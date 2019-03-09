open Bitcoinml;;
open Utils;;
open Block;;
open Tx;;
open Params;;
open Block;;
open Block.Header;;
open Hash;;
open Timediff;;
open Stdint;;
open Storage;;
open Cqueue;;

module Resource = struct
	type t = 
	| RES_TX of Tx.t
	| RES_BLOCK of Block_lazy.t
	| RES_HBLOCKS of Block.Header.t list * Unix.inet_addr
	| RES_INV_TX of Hash.t * Unix.inet_addr
	| RES_INV_BLOCK of Hash.t * Unix.inet_addr
	| REQ_HBLOCKS of Hash.t list * Hash.t * Unix.inet_addr
	| REQ_TX of Hash.t * Unix.inet_addr
	;;
end

module Request = struct
	type t =
	| REQ_TX of Hash.t * Unix.inet_addr option
	| REQ_BLOCKS of Hash.t list * Unix.inet_addr option
	| REQ_HBLOCKS of Hash.t list * Unix.inet_addr option
	| REQ_DATA of Hash.t list * Unix.inet_addr option
	| RES_HBLOCKS of Block.Header.t list * Unix.inet_addr
	| RES_INV_TX of Hash.t
	| RES_TX of Tx.t * Unix.inet_addr
	;;
end	

type t = {
	params	: Params.t;
	config	: Config.t;
	basedir	:	string;
	
	storage :	Storage.t;

	(* Sync status *)
	mutable sync_headers	:	bool;
	mutable sync			:	bool;

	(* Branches *)
	mutable branches			: Branch.t list;
		
	(* Last header status *)
	mutable header_height	:	int64;
	mutable header_last		: 	Header.t;

	(* Last block status *)
	mutable block_height 	:	int64;
	mutable block_last 		:	Block.t;
	mutable block_last_received : float;
	mutable blocks_requested	:	int;
	
	mempool			:	Mempool.t;
	
	(* Queue for incoming resources*)
	resources		:	(Resource.t) Cqueue.t;
	
	(* Queue for data request *)
	requests		:	(Request.t) Cqueue.t;

	mutable run : bool;
};;

let genesis path config p = 
	let genesis_header : Block.Header.t = {
		hash				= p.genesis.hash;
		version			= p.genesis.version;
		prev_block	= p.genesis.prev_block;
		merkle_root = p.genesis.merkle_root;
		time		= p.genesis.time;
		bits		= p.genesis.bits;
		nonce		= p.genesis.nonce;
	} in
	let bc = {
		params			= p;
		config			= config;
		basedir			= "";
		
		storage				= Storage.load path config;
		sync_headers	= false;
		sync					= false;

		branches = [];
		
		header_height	= 0L;
		header_last		= genesis_header;

		block_height 	= 0L;
		block_last 		= {
			size= 1;
			txs	= [];
			header= {
				hash				= Hash.zero;
				time				= 0.0;
				version			= Int32.zero;
				prev_block	= Hash.zero;
				merkle_root	= Hash.zero;
				bits				= "ffffff";
				nonce				= Uint32.zero
			}
		};
		block_last_received = Unix.time ();
		blocks_requested 		= 0;
		
		mempool			= Mempool.create ();
		
		resources		= Cqueue.create ();
		requests		= Cqueue.create ();
		run = true;
	} in 
	bc
;;

let load path config p = 
	let res bcg =
		Log.info "Blockchain" "Starting from block header %s at height %d" bcg.header_last.hash (Int64.to_int bcg.header_height);
		Log.info "Blockchain" "Starting from block %s at height %d" bcg.block_last.header.hash (Int64.to_int bcg.block_height);
		Log.info "Blockchain" "Got %d active side branches" @@ List.length bcg.branches;
		bcg
	in
	let bcg = genesis path config p in
	bcg.branches <- [];
		
	let header_last = Storage.get_header bcg.storage bcg.storage.chainstate.Chainstate.header in
	let block_last = Storage.get_block bcg.storage bcg.storage.chainstate.Chainstate.block in

	match (header_last, bcg.storage.chainstate.Chainstate.header_height, block_last, bcg.storage.chainstate.Chainstate.block_height) with
	| (None, hh, b, bh) -> res bcg
	| (Some (h), hh, None, bh) ->
		bcg.header_last <- h;
		bcg.header_height <- Uint32.to_int64 hh;
		res bcg
	| (Some (h), hh, Some (b), bh) ->
		bcg.header_last <- h;
		bcg.header_height <- Uint32.to_int64 hh;
		bcg.block_last <- b;
		bcg.block_height <- Uint32.to_int64 bh;
		res bcg
;;


let verify_block_header params lhh lh h =
	let check_checkpoint index hash =
		try
			let hash' = List.assoc index params.checkpoints in
			match hash' = hash with
			| true -> Log.debug "Blockchain" "Checkpoint: %s" hash'; true
			| false -> Log.warn "Blockchain" "Checkpoint failed: %s <> %s" hash' hash; false
		with | _ -> true
	in

	(* Check that nBits value matches the difficulty rules *)
	(* Reject if timestamp is the median time of the last 11 blocks or before *)

	(* Check if prev block (matching prev hash) is in main branch or side branches. *)
	h.Header.prev_block = lh.Header.hash  
	(* Block hash must satisfy claimed nBits proof of work *)
	&& Block.Header.check_target h
	(* Block timestamp must not be more than two hours in the future *)
	&& h.Header.time < (Unix.time () +. 60. *. 60. *. 2.)
	(* Check that hash matches known values *)
	&& check_checkpoint ((Int64.to_int lhh) + 1) h.Header.hash
;;


(* Remove the last header / block (if detected a fork) *)
let rollback_block bc =
	let rollback_block' () = 
		Storage.remove_last_block bc.storage bc.params bc.block_last.header.prev_block;
		bc.block_height <- Int64.pred bc.block_height;
		match Storage.get_block bc.storage @@ bc.block_last.header.prev_block with
		| Some (h) -> bc.block_last <- h 
		| None -> failwith "impossible situation - prev block not found"
	in
	let rollback_header' dontremove =
		(if not dontremove then Storage.remove_last_header bc.storage bc.header_last.prev_block);
		bc.header_height <- Int64.pred bc.header_height;
		match Storage.get_header bc.storage @@ bc.header_last.prev_block with
		| Some (h) -> bc.header_last <- h 
		| None -> failwith "impossible situation - prev header not found"
	in
	Log.debug "Blockchain" "Removing last block: %s" bc.header_last.hash;
	if bc.header_height > bc.block_height then 
		rollback_header' false
	else 
		(rollback_header' true; rollback_block' ())
;;
	



let broadcast_tx bc tx = 
	Mempool.add bc.mempool tx;
	bc.requests << RES_INV_TX (tx.hash);
;;


let step bc = 
	let check_branch_updates h = match (Branch.find_parent bc.branches h, Branch.find_fork bc.branches h) with
	| (Some (br), _) -> (* Insert into a branch (if present) *)
		Log.info "Blockchain ←" "Branch %s updated with new block: %s" br.fork_hash h.hash;
		if verify_block_header bc.params br.header_height br.header_last h then (
			Branch.push br h |> ignore
		)
	| (None, Some (br)) -> (* Branch parent not found, but there is already a branch with the same fork_block *)
		()
	| (None, None) -> (* Find if this block is connected with an already connected block *)
		match Storage.get_header bc.storage h.prev_block with
		| None -> ()
		| Some (banc) ->
			let height = Storage.get_block_height bc.storage h.prev_block in
			if height >= ((Int64.to_int bc.header_height) - 1) then (* This is unable to detect old valid branches *)
				(match Storage.get_header bc.storage h.hash with
					| Some (x) -> ()
					| None -> (* Found a valid new branch *)
						let branch = Branch.create banc.hash (Int64.of_int height) h in
						bc.branches <- bc.branches @ [ branch ];
						Log.info "Blockchain ←" "New branch created from %s to %s" banc.hash h.hash;
						()
				) (* else ()*)
	in

	let consume_block (blazy:Block_lazy.t) = if bc.block_last.header.time = 0.0 then ( (* First block *)
		let first_block_height = Int64.sub bc.header_height @@ Int64.of_int bc.config.cache_size in
		match Storage.get_headeri bc.storage first_block_height with
		| Some (bh) when bh.hash = blazy.header.hash -> (match Block_lazy.force blazy with
			| Some (b) -> (
				bc.block_height <- first_block_height;
				bc.block_last <- b;
				Storage.insert_block bc.storage bc.params bc.block_height b)
			| None -> ()
		)
		| _ -> ()
	) else (
		match (blazy, bc.block_last, bc.header_last) with
		| (blazy, block, hl) when blazy.header.prev_block = block.header.hash -> ( (* Next block, header present *)
			match Block_lazy.force blazy with
			| Some (b) -> (
				bc.blocks_requested <- bc.blocks_requested - 1;
				bc.block_height <- Int64.succ bc.block_height;
				bc.block_last <- b;
				let a = Unix.time () in
				Storage.insert_block bc.storage bc.params bc.block_height b;
				Log.debug "Blockchain ←" "Block %d processed in %d seconds (%d transactions, %d KB)" (Int64.to_int bc.block_height) 
					(int_of_float ((Unix.time ()) -. a)) (List.length b.txs) (b.size / 1024);
				bc.block_last_received <- Unix.time ();
				Mempool.remove_txs bc.mempool b.txs;

				let percentage = 100. *. (Int64.to_float bc.block_height) /. (Int64.to_float bc.header_height) in
				Log.info "Blockchain ←" "Block %s - %d - %f%%" b.header.hash (Int64.to_int bc.block_height) percentage; 
				(*(Int64.to_int bc.block_height) @@ Timediff.diffstring (Unix.time ()) block.header.time ~munit:"weeks";*)
				()
			)
			| None -> ()
		)
		| (blazy, block, hl) when blazy.header.prev_block = hl.hash -> (* New block *)
			if verify_block_header bc.params bc.header_height bc.header_last blazy.header then (
				bc.header_last <- blazy.header;
				bc.header_height <- Int64.succ bc.header_height;

				match Block_lazy.force blazy with
				| Some (b) ->
					if verify_block_header bc.params bc.block_height bc.block_last.header b.header then (
						bc.block_height <- Int64.succ bc.block_height;
						bc.block_last <- b;
						Storage.insert_block bc.storage bc.params bc.block_height b;
						Mempool.remove_txs bc.mempool b.txs;
						bc.block_last_received <- Unix.time ();
						Log.debug "Blockchain ←" "Block %s - %d, time: %s ago" block.header.hash (Int64.to_int bc.block_height) @@ Timediff.diffstring (Unix.time ()) block.header.time ~munit:"weeks"
					) else (
						Storage.insert_header bc.storage bc.header_height bc.header_last;
						Log.debug "Blockchain ←" "Block %s - %d, time: %s ago" block.header.hash (Int64.to_int bc.block_height) @@ Timediff.diffstring (Unix.time ()) block.header.time ~munit:"weeks"
					)
				| None -> ()
			) else (
				Log.warn "Blockchain" "Block header validation failed: %s" blazy.header.hash
			);
			()
		| (blazy, block, hl) when blazy.header.prev_block <> hl.hash -> (* New block maybe on side-branch *)
			(*Log.debug "Blockchain" "Skip block %s %s %s" b.header.hash b.header.prev_block block.header.hash;*)
			check_branch_updates blazy.header; ()
		| (blazy, block, hl) -> ()
	) in
	let consume_header h = 
		if verify_block_header bc.params bc.header_height bc.header_last h then (
			(* Insert in the chain *)
			bc.header_last <- h;
			bc.header_height <- Int64.succ bc.header_height;
			Storage.insert_header bc.storage bc.header_height bc.header_last
		) else ( 
			if Block.Header.check_target h then check_branch_updates h else ()
		)
	in
		Cqueue.clear bc.requests;
		
		(* Handle new resources *)
		Cqueue.iter bc.resources (fun res -> match (res : Resource.t) with 
		| REQ_TX (txhash, addr) ->
			(if Mempool.has bc.mempool txhash then bc.requests << Request.RES_TX (Mempool.get bc.mempool txhash, addr))
		| REQ_HBLOCKS (hl, stop, addr) ->
			bc.requests << Request.RES_HBLOCKS ([], addr);
		| RES_INV_BLOCK (bs, addr) -> 
			(if bc.sync then bc.requests << Request.REQ_BLOCKS ([bs], Some (addr)));
		| RES_INV_TX (txs, addr) ->
			(if bc.sync && not (Mempool.has bc.mempool txs) then bc.requests << Request.REQ_TX (txs, Some (addr)));
		| RES_BLOCK (bs) -> if bc.run then consume_block bs
		| RES_TX (tx) -> 
			Mempool.add bc.mempool tx;
			broadcast_tx bc tx
		| RES_HBLOCKS (hbs, addr) when List.length hbs = 0 -> ()
		| RES_HBLOCKS (hbs, addr) -> (
			let oldhh = Int64.to_int bc.header_height in
			(*Log.debug "Blockchain ←" "Headers %d" (List.length hbs);*)
			List.iter (fun h -> consume_header h) @@ List.rev hbs;
			if oldhh < (Int64.to_int bc.header_height) then (
				Log.info "Blockchain ←" "%d Block Headers processed. Last is %d: %s behind" (List.length hbs) (Int64.to_int bc.header_height) 
					(Timediff.diffstring (Unix.time ()) bc.header_last.time);
				Storage.sync bc.storage	
			)
		)
		) |> ignore;

		(* Request old headers for branch verification *)
		if bc.header_last.time < (Unix.time () -. 60. *. 60. *. 5.) then (
			match Storage.get_headeri bc.storage (Int64.sub bc.header_height @@ Int64.of_int 64) with
			| None -> ()
			| Some (h) ->
				(*Log.debug "Blockchain" "Requesting periodic ancestor headers for fork detection";*)
				bc.requests << Request.REQ_HBLOCKS ([h.hash], None);
				bc.requests << Request.REQ_HBLOCKS ([h.hash], None);
				bc.requests << Request.REQ_HBLOCKS ([h.hash], None);
				bc.requests << Request.REQ_HBLOCKS ([h.hash], None)
		);

		(* Check sync status *)
		if bc.header_last.time < (Unix.time () -. 60. *. 60.) then (
			Log.debug "Blockchain" "Headers not in sync: %s behind" @@ Timediff.diffstring (Unix.time ()) bc.header_last.time;
			bc.sync_headers <- false;
			bc.requests << Request.REQ_HBLOCKS ([bc.header_last.hash], None)
		) else (
			if bc.header_last.time < (Unix.time () -. 60. *. 10.) then 
				bc.requests << Request.REQ_HBLOCKS ([bc.header_last.hash], None);
				
			Log.debug "Blockchain" "Headers in sync: last block is %s" @@ Timediff.diffstring (Unix.time ()) bc.header_last.time;
			bc.sync_headers <- true
		);

		(match bc.block_last.header.time, bc.sync_headers with
		| 0.0, true -> (
			Log.debug "Blockchain" "Blocks not in sync, waiting for genesis";
			bc.sync <- false;
			match Storage.get_headeri bc.storage (Int64.sub bc.header_height (Int64.of_int bc.config.cache_size)) with
			| Some (bh) -> bc.requests << Request.REQ_BLOCKS ([bh.hash], None)
			| None -> ()
		)
		| _, true -> (
			(*if bc.block_last.header.time < (Unix.time () -. 60. *. 10.) then ( *)
			if bc.block_last.header.hash <> bc.header_last.hash then (
				Log.info "Blockchain" "Blocks not in sync: %s behind" @@ Timediff.diffstring (Unix.time ()) bc.block_last.header.time;
				bc.sync <- false;

				(* Ask the storage for next n blocks hashes *)
				let rec getblockhashes h n acc = match n with
				| 0 -> acc
				| n ->
					let succ = Int64.succ h in
					let nh = Storage.get_headeri bc.storage succ in
					match nh with
					| None -> acc
					| Some (bh) -> getblockhashes succ (n-1) (bh.hash::acc)
				in 
				if bc.block_last_received < (Unix.time () -. 5.) && bc.blocks_requested > 0 || bc.blocks_requested = 0 then (
					let hashes = getblockhashes (bc.block_height) 500 [] in
					bc.blocks_requested <- 500;
					bc.requests << Request.REQ_BLOCKS (hashes, None))
			) else (
				Log.info "Blockchain" "Blocks in sync: last block is %s" @@ Timediff.diffstring (Unix.time ()) bc.block_last.header.time;
				bc.sync <- true
			)
		)
		| _, _ -> ());

		(* Check branch status *)
		(* Check if a branch is too old, then delete it *)
		bc.branches <- (List.filter (fun bi ->
			if bi.Branch.header_height < (Int64.sub bc.header_height @@ Int64.of_int 12) then (
				Log.info "Branch" "Removing branch %s because is too old" bi.header_last.hash;
				false
			) else true			
		)) bc.branches;

		(* Check if a branch need updates (HBLOCKS) *)
		List.iter (fun b ->
			if b.Branch.header_last.time < (Unix.time () -. 60. *. 10.) then (
				bc.requests << Request.REQ_HBLOCKS ([b.Branch.header_last.hash], None)
			)
		) bc.branches;
			
		(* Check if a branch is longer than the best chain *)
		(match Branch.best_branch bc.branches with
		| None -> ()
		| Some (br) when br.header_height <= bc.header_height -> ()
		| Some (br) when br.header_height > bc.header_height ->
			let rec rollback rolled =
				rollback_block bc;
				match bc.header_last.hash with
				| l when l = br.fork_hash -> rolled
				| l -> rollback @@ (bc.header_last) :: rolled 
			in
			Log.info "Branch" "Found that branch %s is the main branch, rollback" bc.header_last.hash;
			let rolled_back = rollback [] in

			(* Push branch headers to the main branch *)
			List.iter (fun h -> consume_header h) br.header_list;

			(* Remove branch *)
			bc.branches <- (List.filter (fun bi -> br.fork_hash <> bi.Branch.fork_hash) bc.branches);

			(* Move old blocks (if any) to new branch *)
			if List.length rolled_back > 0 then (
				let branch = Branch.create (List.hd rolled_back).hash bc.header_height @@ List.hd rolled_back in
				List.iter (fun h -> Branch.push branch h |> ignore) @@ List.tl rolled_back;
				bc.branches <- bc.branches @ [ branch ];
			);
			()
		| _ -> ()
		);
		(*Storage.update_branches bc.storage bc.branches;*)

		(*Log.debug "Blockchain" "Last block header is %d : %s" (Int64.to_int bc.header_height) bc.header_last.hash;
		Log.debug "Blockchain" "Last block is %d : %s" (Int64.to_int bc.block_height) bc.block_last.header.hash;
		Log.debug "Blockchain" "There are %d active side-branches" @@ List.length bc.branches;
		List.iter (fun b ->
			Log.debug "Branch" "Last block of branch %s (%d blocks) header is %d (diff: %d)" (b.Branch.header_last.hash) (List.length b.Branch.header_list) (Int64.to_int b.header_height)
				(Int64.to_int @@ Int64.sub bc.header_height b.Branch.header_height); 
		) bc.branches;
		Mempool.print_stats bc.mempool;	*)
		()
;;


let rec loop bc =
	if bc.run then (
		Thread.delay 4.;
		step bc;
		loop bc
	) else (
		Storage.sync bc.storage;
		Storage.close bc.storage
	)
;;


let shutdown bc = 
	Log.fatal "Blockchain" "Shutdown...";
	bc.run <- false;
	Storage.sync bc.storage;
	Storage.close bc.storage
;;

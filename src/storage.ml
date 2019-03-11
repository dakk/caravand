open Utils;;
open Store;;
open Bitcoinml;;
open Stdint;;
open Block;;
open Block.Header;;
open Tx;;

module Chainstate = struct
	type t = {
		mutable header        	: Hash.t;
		mutable header_height   : uint32;
		mutable block       	: Hash.t;
		mutable block_height    : uint32;
		mutable prune_height	: uint32
	};;

	let serialize cs = 
		Bitstring.string_of_bitstring [%bitstring {|
			Hash.to_bin cs.header         : 32*8 : string;
			Uint32.to_int32 cs.header_height			: 32 : littleendian;
			Hash.to_bin cs.block         : 32*8 : string;
			Uint32.to_int32 cs.block_height			: 32 : littleendian;
			Uint32.to_int32 cs.prune_height			: 32 : littleendian
		|}]
	;;

	let parse csb = 
		let bdata = Bitstring.bitstring_of_string csb in
		match%bitstring bdata with
		| {|
			header 	        : 32*8 : string;
			header_height   : 32 : string;
			block 	        : 32*8 : string;
			block_height    : 32 : string;
			prune_height	: 32 : string
		|} ->
		Some ({
			header	    		= Hash.of_bin header;
			header_height 	    = Uint32.of_bytes_little_endian (Bytes.of_string header_height) 0;
			block	    		= Hash.of_bin block;
			block_height 	    = Uint32.of_bytes_little_endian (Bytes.of_string block_height) 0;
			prune_height 		= Uint32.of_bytes_little_endian (Bytes.of_string prune_height) 0;
		})
	;;
end

module Chainstate_index = Store.Make_index 
  (Chainstate) 
  (struct let prefix = "chainstate" end)
;;

let load_or_init st (conf: Config.t) = 
  match Chainstate_index.get st "" with
	| Some (cs) -> cs
		| None -> {
			header= Bitcoinml.Hash.zero;
			header_height= Uint32.zero;
			block= Bitcoinml.Hash.zero;
			block_height= Uint32.zero;
			prune_height= Uint32.zero;
	}
;;


type txout_entry = {
	height: int;
	blockhash: Hash.t;
	coinbase: bool;
	txout: Tx.Out.t;
};;

type t = {
	block_store: Store_raw.t;
	state_store: Store_raw.t;
	txout_store: Store_raw.t;
	config: Config.t;
	mutable chainstate: Chainstate.t;
};;


let load path config = 
	let state_store = Store_raw.load path "state" in
	{
		chainstate= load_or_init state_store config;
		config= config;
		block_store= Store_raw.load path "blocks";
		txout_store= Store_raw.load path "txout";
		state_store= state_store
};;

let save st = Chainstate_index.set st.state_store "" st.chainstate;;

let sync st = 
  save st;
	Store_raw.sync st.block_store;
	Store_raw.sync st.txout_store;
	Store_raw.sync st.state_store
;;

let close st = 
	sync st;
	Store_raw.close st.block_store;
	Store_raw.close st.txout_store;
	Store_raw.close st.state_store
;;


let insert_txout st txhash n height blockhash txout = Storage_txout.insert_txout st.txout_store txhash n height blockhash txout;;
let get_txout st txhash n = Storage_txout.get_txout st.txout_store txhash n;;
let remove_txout st txhash n = Storage_txout.remove_txout st.txout_store txhash n;;


let get_blocks st hashes = Storage_blocks.get_blocks st.block_store hashes;;
let get_headers st hashes = Storage_blocks.get_headers st.block_store hashes;;
let get_block_height st hash = Storage_blocks.get_block_height st.block_store hash;;
let get_blocki st height = Storage_blocks.get_blocki st.block_store height;;
let get_block st hash = Storage_blocks.get_block st.block_store hash;;
let get_block_lazy st hash = Storage_blocks.get_block_lazy st.block_store hash;;
let get_header st hash = Storage_blocks.get_header st.block_store hash;;
let get_headeri st height = Storage_blocks.get_headeri st.block_store height;;


let insert_header storage height (header : Block.Header.t) = 
	let h = Uint32.of_int64 height in
	Storage_blocks.insert_header storage.block_store h header;
	
  storage.chainstate.header <- header.hash;
  storage.chainstate.header_height <- h;
  save storage;
  sync storage
;;

let remove_last_header storage prevhash =
	storage.chainstate.header_height <- Uint32.pred storage.chainstate.header_height;
	storage.chainstate.header <- prevhash;
	Storage_blocks.remove_header storage.block_store storage.chainstate.header_height storage.chainstate.header;
  	save storage;
	sync storage
;;

let insert_block storage params height (block : Block.t) = 
	let rec prune_blocks storage xb = 
		match (Uint32.to_int storage.chainstate.block_height) - xb with
		| x' when x' > Uint32.to_int storage.chainstate.prune_height -> (
			let blprune = try get_blocki storage (Int64.of_uint32 storage.chainstate.prune_height) with | _ -> None in
			match blprune with
			| None -> 
				storage.chainstate.prune_height <- Uint32.succ storage.chainstate.prune_height;
				prune_blocks storage xb
			| Some (block) ->
				let left = (Int64.to_int height) - (Uint32.to_int storage.chainstate.prune_height) - xb in
				Log.debug "Storage" "Pruned block %d (%d txs) - %d blocks left to prune" (Uint32.to_int storage.chainstate.prune_height) (List.length block.txs) left;
				storage.chainstate.prune_height <- Uint32.succ storage.chainstate.prune_height;
				Storage_blocks.remove_block_data storage.block_store block;
				prune_blocks storage xb)
		| _ -> ()
	in

	Storage_blocks.insert_block (storage.block_store) block;
	storage.chainstate.block <- block.header.hash;	
	storage.chainstate.block_height <- Uint32.of_int64 height;

	if (Uint32.to_int storage.chainstate.prune_height) = 0 then
		storage.chainstate.prune_height <- Uint32.of_int64 height
	else
		prune_blocks storage storage.config.cache_size;
  	
	Chainstate_index.set storage.state_store "" storage.chainstate;

  	save storage;
	sync storage
;;


let remove_last_block storage params prevhash =
	storage.chainstate.block_height <- Uint32.pred storage.chainstate.block_height;
	storage.chainstate.block <- prevhash;
	remove_last_header storage prevhash;
	sync storage
;;
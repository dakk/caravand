open Utils;;
open Store;;
open Bitcoinml;;
open Stdint;;
open Block;;
open Block.Header;;
open Tx;;

module Chainstate = struct
	type t = {
		mutable block        	: Hash.t;
		mutable height        	: uint32;
	};;

	let serialize cs = 
		Bitstring.string_of_bitstring [%bitstring {|
			Hash.to_bin cs.block         : 32*8 : string;
			Uint32.to_int32 cs.height			: 32 : littleendian
		|}]
	;;

	let parse csb = 
		let bdata = Bitstring.bitstring_of_string csb in
		match%bitstring bdata with
		| {|
			block 	        : 32*8 : string;
			height          : 32 : string
		|} ->
		Some ({
			block	    	= Hash.of_bin block;
			height 	    	= Uint32.of_bytes_little_endian (Bytes.of_string height) 0
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
			block= "0000000000000000000000000000000000000000000000000000000000000000";
			height= Uint32.of_int 0;
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
let get_header st hash = Storage_blocks.get_header st.block_store hash;;
let get_headeri st height = Storage_blocks.get_headeri st.block_store height;;


let insert_header st height (header : Block.Header.t) = 
	let h = Uint32.of_int64 height in
	Storage_blocks.insert_header st.block_store h header;
	
  st.chainstate.block <- header.hash;
	st.chainstate.height <- h;
  save st
;;

let remove_last_header st prevhash =
	st.chainstate.height <- Uint32.pred st.chainstate.height;
	st.chainstate.block <- prevhash;
	Storage_blocks.remove_header st.block_store st.chainstate.height st.chainstate.block;
  save st;
	sync st
;;

let insert_block storage params height (block : Block.t) = 
	Storage_blocks.insert_block (storage.block_store) block;
	storage.chainstate.block <- block.header.hash;	
	storage.chainstate.height <- Uint32.of_int64 height;
  Chainstate_index.set storage.state_store "" storage.chainstate;
	sync storage
;;


let remove_last_block storage params prevhash =
	storage.chainstate.height <- Uint32.pred storage.chainstate.height;
	storage.chainstate.block <- prevhash;
	remove_last_header storage prevhash;
	sync storage
;;
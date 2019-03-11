open LevelDB;;
open Bitcoinml;;
open Stdint;;
open Utils;;
open Store;;

type t = Store_raw.t;;

module Block_index = Store.Make_index 
  (Block)
  (struct let prefix = "bk" end)
;;

module Block_lazy_index = Store.Make_index 
  (struct 
    type t = Block_lazy.t;;
    let serialize ?(hex=false) b = match Block_lazy.force b with 
    | Some (b) -> Block.serialize b
    | None -> ""
    ;;

    let parse = Block_lazy.parse;; 
  end)
  (struct let prefix = "bk" end)
;;

module Block_header_index = Store.Make_index 
  (Block.Header)
  (struct let prefix = "bk" end)
;;


module Block_height_index = Store.Make_index 
  (struct 
    type t = string;;
    let serialize ?(hex=false) o = o;;
    let parse ?(hex=false) o = Some o;;
  end) 
  (struct let prefix = "bi" end)
;;

module Block_height_index_reverse = Store.Make_index 
  (struct 
    type t = Uint32.t;;
    let serialize ?(hex=false) o = Printf.sprintf "%d" @@ Uint32.to_int o;;
    let parse ?(hex=false) o = Some (Uint32.of_int @@ int_of_string o);;
  end) 
  (struct let prefix = "bh" end)
;;


let insert_header block_store height (header : Block.Header.t) = 
	Block_header_index.set block_store (Hash.to_bin_norev header.hash) header;
	Block_height_index_reverse.set block_store (Hash.to_bin_norev header.hash) height;
	Block_height_index.set block_store (Printf.sprintf "%d" (Uint32.to_int height)) header.hash;
;;

let remove_block_data block_store (block : Block.t) = 
  Block_header_index.set block_store block.header.hash block.header
;;


let insert_block block_store (block : Block.t) = 
  Block_index.set block_store (Hash.to_bin_norev block.header.hash) block
;;

let remove_header block_store height hash = 
  Block_height_index.remove block_store @@ Printf.sprintf "%d" (Uint32.to_int height);
	Block_height_index_reverse.remove block_store @@ Hash.to_bin_norev hash;
	Block_header_index.remove block_store @@ Hash.to_bin_norev hash;	
;;

let get_block_height block_store hash =
	match Block_height_index_reverse.get block_store (Hash.to_bin_norev hash) with
	| Some (d) -> Uint32.to_int d
	| None -> 0
;;

let get_block block_store hash = Block_index.get block_store @@ Hash.to_bin_norev hash;;
let get_block_lazy block_store hash = Block_lazy_index.get block_store @@ Hash.to_bin_norev hash;;

let get_blocki block_store height = 
  match Block_height_index.get block_store @@ Printf.sprintf "%d" (Int64.to_int height) with
  | Some (h) -> get_block block_store h 
  | None -> None
;;

let get_header block_store hash = Block_header_index.get block_store @@ Hash.to_bin_norev hash;;

let get_headeri block_store height = 
  match Block_height_index.get block_store @@ Printf.sprintf "%d" (Int64.to_int height) with
  | Some (h) -> get_header block_store h 
  | None -> None
;;

let get_blocks block_store hashes = 
	let rec get_blocks' hs acc = match hashes with
	| [] -> acc
	| h::hs' -> match get_block block_store h with
		| None -> get_blocks' hs' acc
		| Some (h') -> get_blocks' hs' (h'::acc)
	in get_blocks' hashes []
;;

let get_headers block_store hashes = 
	let rec get_headers' hs acc = match hashes with
	| [] -> acc
	| h::hs' -> match get_header block_store h with
		| None -> get_headers' hs' acc
		| Some (h') -> get_headers' hs' (h'::acc)
	in get_headers' hashes []
;;
open Utils
open Bitcoinml
open Stdint
open Store


module Chainstate : sig
	type t = {
		mutable block       	: Hash.t;
		mutable height        	: uint32;
	};;

	val serialize: t -> string
  val parse: string -> t option
end

type t = {
	block_store: Store_raw.t;
	state_store: Store_raw.t;
	config: Config.t;
	mutable chainstate: Chainstate.t;
}

val load: string -> Config.t -> t
val close: t -> unit
val sync: t -> unit


val insert_header     : t -> int64 -> Block.Header.t -> unit
val remove_last_header: t -> Hash.t -> unit
val insert_block      : t -> Params.t -> int64 -> Block.t -> unit
val remove_last_block : t -> Params.t -> Hash.t -> unit

val get_blocki        : t -> Int64.t -> Block.t option
val get_block         : t -> Hash.t -> Block.t option
val get_block_height	:	t -> Hash.t -> int
val get_header				:	t -> Hash.t -> Block.Header.t option
val get_headeri				:	t -> Int64.t -> Block.Header.t option
val get_blocks 				:	t -> Hash.t list -> Block.t list
val get_headers				:	t -> Hash.t list -> Block.Header.t list
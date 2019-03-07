open Bitcoinml

type t = {
  (* Fork status *)
	fork_hash	    :	Hash.t;
  fork_height	  :	int64;

	(* Last header status *)
	mutable header_height	:	int64;
  mutable header_last		: Block.Header.t;

  mutable header_list   : Block.Header.t list;
}

val create      : Hash.t -> int64 -> Block.Header.t -> t
val last        : t -> Hash.t
val push        : t -> Block.Header.t -> bool

val serialize   : t -> string
val parse       : string -> string * t option

val find_parent : t list -> Block.Header.t -> t option
val find_fork   : t list -> Block.Header.t -> t option
val best_branch : t list -> t option
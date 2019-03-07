(* RPC api interface *)
open Utils

type t

val init 		: Config.rpc -> Chain.t -> Net.t -> t
val loop 		: t -> unit
val shutdown 	: t -> unit

type rpc = {
	port	: int;
	user	: string;
	password: string;
}
	
type t = {
	peers	 		: int;
	chain			: string;
	base_path	: string;
	path			: string;
	
	rpc				: rpc;
	log_level			: int;
	cache_size : int;
}

val parse_base_path			: unit -> string
val load_or_init				:	string -> t
val parse_command_line	: t -> t
val create_dirs					: t -> bool
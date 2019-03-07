open Printf;;

type level = FATAL | ERROR | WARN | INFO | DEBUG | DEBUG2;;
type color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White | Default;;

let color_to_string = function
	| Black   -> "\027[30m"
	| Red     -> "\027[31m"
	| Green   -> "\027[32m"
	| Yellow  -> "\027[33m"
	| Blue    -> "\027[34m"
	| Magenta -> "\027[35m"
	| Cyan    -> "\027[36m"
	| White   -> "\027[37m"
	| Default -> "\027[39m"
;;
let color_reset = "\027[0m";;

let color_of_level lev = match lev with
	| INFO -> Green
	| DEBUG -> Yellow 
	| DEBUG2 -> Cyan 
	| _ -> Red
;;

let level_str lev = match lev with
	| INFO -> color_to_string Blue ^ "ℹ" ^ color_reset
	| DEBUG -> color_to_string Yellow ^ "⚙" ^ color_reset
	| DEBUG2 -> color_to_string Cyan ^ "⚙" ^ color_reset
	| ERROR -> color_to_string Red ^ "⚡" ^ color_reset
	| FATAL -> color_to_string Red ^ "⚠" ^ color_reset
	| _ -> " "
;;


let log_level = ref 4;;

let head_str lev sec =
		let ts = Unix.gettimeofday() in
		let tm = Unix.localtime ts in
		let us, _s = modf ts in
		sprintf 
		 " %s %s[%04d-%02d-%02d %02d:%02d:%02d.%03d]%s %s%s%s %s%s" (*→*)
		(level_str lev)
		(color_to_string Magenta)
			(1900 + tm.Unix.tm_year)
			(1    + tm.Unix.tm_mon)
			tm.Unix.tm_mday
			tm.Unix.tm_hour
			tm.Unix.tm_min
			tm.Unix.tm_sec
			(int_of_float (1_000. *. us))
		color_reset
		(color_to_string (color_of_level lev))
		sec
		(color_reset)
		(color_to_string Yellow)
		(color_reset)
;;

let log lev sec fmt =
	let now = head_str lev sec in
	match lev, !log_level with
	| FATAL, l when l > 0 -> fprintf stdout ("%s" ^^ fmt ^^ "\n%!") now
	| ERROR, l when l > 0 -> fprintf stdout ("%s" ^^ fmt ^^ "\n%!") now
	| WARN, l when l > 2 -> fprintf stdout ("%s" ^^ fmt ^^ "\n%!") now
	| INFO, l when l > 2 -> fprintf stdout ("%s" ^^ fmt ^^ "\n%!") now
	| DEBUG, l when l > 3 -> fprintf stdout ("%s" ^^ fmt ^^ "\n%!") now
	| DEBUG2, l when l > 4 -> fprintf stdout ("%s" ^^ fmt ^^ "\n%!") now
	| _ -> ifprintf stdout ("%s" ^^ fmt ^^ "\n%!") now
;;


let fatal sec fmt = log FATAL sec fmt;;
let error sec fmt = log ERROR sec fmt;;
let warn  sec fmt = log WARN  sec fmt;;
let info  sec fmt = log INFO  sec fmt;;
let debug sec fmt = log DEBUG sec fmt;;
let debug2 sec fmt = log DEBUG2 sec fmt;;

let set_level l = log_level := l;;
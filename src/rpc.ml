open Utils;;
open Bitcoinml;;
open Log;;
open Unix;;
open Chain;;
open Block;;
open Block.Header;;
open Tx;;
open Tx.In;;
open Tx.Out;;
open Params;;
open Stdint;;
open Yojson.Basic.Util;;


let send socket str = 
  let len = String.length str in
  send socket (str |> Bytes.of_string) 0 len [] |> ignore
;;

let recv socket = 
  let data_raw = Bytes.create 4096 in
	let reqlen = Unix.recv socket data_raw 0 4096 [] in
  String.sub (data_raw |> Bytes.to_string) 0 reqlen
;;


let shutdown socket =
  try ( Unix.shutdown socket Unix.SHUTDOWN_ALL ) with | _ -> ();
;;

let listen socket port ns =
  bind socket (ADDR_INET (inet_addr_of_string "0.0.0.0", port));
  listen socket ns
;;

module JSONRPC = struct
  type req = {
    methodn : string;
    params  : Yojson.Basic.t list;
    id      : string;
		socket 	: Unix.file_descr;
  };;

  let reply req result = 
		`Assoc [
			("id", `String req.id);
			("result", result);
		] |> to_string |> send req.socket
  ;;

  let parse_request socket = 
		try (
			let data_raw = recv socket in
			let data_split = String.split_on_char '{' data_raw in
			let body = "{" ^ List.nth data_split 1 in
			let j = Yojson.Basic.from_string body in
			Some ({
				socket= socket;
				id= j |> member "id" |> to_string;
				params= []; (*j |> member "params" |> to_list;*)
				methodn= j |> member "method" |> to_string;
			})
		) with _ -> None
	;;
end

let handle_request bc net req = 
	let reply = JSONRPC.reply req in

	Printf.printf "%s\n%!" req.methodn;
	match req.methodn with
	| "getblockcount" -> 
		let bl = Int64.to_int bc.block_height in
		reply (`Int bl)
	| "getblockhash" -> (
		match req.params with
		| [`String b] -> (
			match Storage.get_blocki bc.storage @@ Int64.of_string b with
			| None -> ()
			| Some (b) -> reply (`String b.header.hash)
		)
		| _ -> ()
	)
	| "getrawblock" -> (
		match req.params with
		| [`String b] -> (
			match Storage.get_block bc.storage b with
			| None -> ()
			| Some (b) -> reply (`String (Block.serialize b))
		)
		| _ -> ()
	)
	| _ -> ()
;;

type t = {
	blockchain : Chain.t;
  network 	 : Net.t;
  conf       : Config.rpc;
	mutable run: bool;
	socket		 : Unix.file_descr;
};;

let init (rconf: Config.rpc) bc net = { 
	blockchain= bc; 
	conf= rconf; 
	network= net; 
	run= true;
	socket= socket PF_INET SOCK_STREAM 0
};;

let loop a =
	let rec do_listen socket =
		let (client_sock, _) = accept socket in
		match JSONRPC.parse_request client_sock with
		| None -> 
			(*close client_sock;*)
			if a.run then do_listen socket
		| Some (req) ->
			handle_request a.blockchain a.network req;
			(*close client_sock;*)
			if a.run then do_listen socket
	in
	Log.info "Api.Rpc" "Binding to port: %d" a.conf.port;
	listen a.socket a.conf.port 8;
  try do_listen a.socket with _ -> ()
;;

let shutdown a = 
  Log.fatal "Api.Rpc" "Shutdown...";
	shutdown a.socket;
	a.run <- false
;;
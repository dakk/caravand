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
	let data = Bytes.of_string ("HTTP/1.1 200 OK\nContent-type: application/json-rpc\n\n" ^ str) in
  let len = Bytes.length data in
	Log.info "Rpc" "%s" str;
  send socket data 0 len [] |> ignore
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
    id      : int;
		socket 	: Unix.file_descr;
  };;

  let reply req result = 
		`Assoc [
			("jsonrpc", `String "2.0");
			("result", result);
			("id", `Int req.id)
		] |> Yojson.Safe.to_string |> send req.socket;
		close req.socket
  ;;

  let reply_err req code message = 
		`Assoc [
			("id", `Int req.id);
			("error", `Assoc [
				("code", `Int code);
				("message", `String message);
			]);
			("jsonrpc", `String "2.0")
		] |> to_string |> send req.socket;
		close req.socket
  ;;

  let parse_request socket = 
		try (
			let data_raw = recv socket in
			let data_split = String.split_on_char '{' data_raw in
			let body = "{" ^ List.nth data_split 1 in
			let j = Yojson.Basic.from_string body in
			Some ({
				socket= socket;
				id= j |> member "id" |> to_int;
				params= j |> member "params" |> to_list;
				methodn= j |> member "method" |> to_string;
			})
		) with _ -> None
	;;
end

let handle_request bc net req = 
	let reply = JSONRPC.reply req in
	let na () = reply (`String "Not handled") in
	let nf () = reply (`String "Not found") in

	Log.info "Rpc" "Request %s: %s" req.methodn (Yojson.Basic.to_string (`List req.params));
	match (req.methodn, req.params) with
	| "echo", [`String hdata] -> reply @@ `String hdata
	| "echo", [] -> reply @@ `String "Hello from caravand"
	| "estimatesmartfee", [`Int target; `String mode] -> (
		na ()
	)
	| "gettxout", [`String txid; `Int n] -> (
		na ()
	)
	| "sendrawtransaction", [`String hex] -> (
		match Tx.parse hex with
		| _, Some (tx) ->
			Chain.broadcast_tx bc tx;
			reply (`String tx.hash)
		| _, _ -> na ()
	)
	| "getblockhash", [`Int height] -> (
		match Storage.get_headeri bc.storage @@ Int64.of_int height with 
		| None -> nf ()
		| Some (bh) -> reply (`String bh.hash)
	)
	| "getblockcount", [] -> 
		let bl = Int64.to_int bc.block_height in
		reply (`Int bl)
	| "getrawblock", [`String b]
	| "getblock", [`String b; `Bool false] -> (
		match Storage.get_block bc.storage b with
		| None -> nf ()
		| Some (b) -> reply (`String (Block.serialize b))
	)
	| _ -> 
		Log.info "Rpc" "Request %s not handled" req.methodn;
		na ()
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
			close client_sock;
			if a.run then do_listen socket
		| Some (req) ->
			handle_request a.blockchain a.network req;
			Log.info "Rpc" "Request handled, closed connection";
			if a.run then do_listen socket
	in
	Log.info "Rpc" "Binding to port: %d" a.conf.port;
	listen a.socket a.conf.port 8;
  try do_listen a.socket with _ -> ()
;;

let shutdown a = 
  Log.fatal "Rpc" "Shutdown...";
	close a.socket;
	shutdown a.socket;
	a.run <- false
;;
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
	let rec send_inner estr = match Bytes.length estr with
	| l when l <= 65536 -> send socket estr 0 l []
	| l when l > 65536 ->
		send socket (Bytes.sub estr 0 65536) 0 65536 [] |> ignore;
		65536 + send_inner (Bytes.sub estr 65536 @@ l - 65536)
	| _ -> 0
	in
	let data = Bytes.of_string ("HTTP/1.1 200 OK\nContent-type: application/json-rpc\n\n" ^ str) in
	send_inner data
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
		methodn: string;
		params: Yojson.Basic.t list;
		id: int;
		socket: Unix.file_descr;
	};;

	let reply req result =
		let asstr = Yojson.Safe.to_string result in
		Log.debug "Rpc ↔" "%s %s → %s" req.methodn (Yojson.Basic.to_string (`List req.params)) @@ if String.length asstr > 64 then String.sub asstr 0 64 else asstr;
		`Assoc [
			("jsonrpc", `String "2.0");
			("result", result);
			("id", `Int req.id)
		] |> Yojson.Safe.to_string |> send req.socket |> ignore;
		close req.socket
	;;

	let reply_err req code message =
		Log.error "Rpc ↔" "%s %s → %d %s" req.methodn (Yojson.Basic.to_string (`List req.params)) code message;
		`Assoc [
			("id", `Int req.id);
			("error", `Assoc [
				("code", `Int code);
				("message", `String message);
			]);
			("jsonrpc", `String "2.0")
		] |> Yojson.Safe.to_string |> send req.socket |> ignore;
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
	let reply_err = JSONRPC.reply_err req in
	let nosync () = reply_err (-28) "Synching..." in
	let generror () = reply_err (-28) "Generic error" in

	match (req.methodn, req.params, bc.sync_headers) with
	| "echo", [`String hdata], _ -> reply @@ `String hdata
	| "echo", [], _ -> reply @@ `String "Hello from caravand"
	| "estimatesmartfee", [`Int target; `String mode], false -> nosync ()
	| "estimatesmartfee", [`Int target; `String mode], true -> (
		reply @@ `Assoc [
			("blocks", `Int target);
			("feerate", `Float 0.00013008)
		]
	)
	| "gettxout", [`String txid; `Int n], _ -> (
		match Storage.get_txout bc.storage txid n with 
		| None -> reply_err (-5) "Txout not found"
		| Some (txo) -> reply @@ `String (Tx.Out.serialize txo.txout)
	)
	| "sendrawtransaction", [`String hex], _ -> (
		match Tx.parse ~hex:true hex with
		| _, Some (tx) ->
			Chain.broadcast_tx bc tx;
			reply (`String tx.hash)
		| _, _ -> generror ()
	)
	| "getblockhash", [`Int height], _ -> (
		match Storage.get_blocki bc.storage @@ Int64.of_int height with 
		| None -> reply_err (-8) "Block height out of range"
		| Some (b) -> reply (`String b.header.hash)
	)
	| "getblockcount", [], true -> reply (`Int (Int64.to_int bc.block_height))
	| "getblockcount", [], false -> nosync ()
	| "getrawblock", [`String b], _
	| "getblock", [`String b; `Bool false], _
	| "getblock", [`String b; `Int 0], _ -> (
		match Storage.get_block bc.storage b with
		| None -> 
			(*bc.requests << Request.REQ_BLOCKS ([b], None));*)
			reply_err (-5) "Block not found"
		| Some (b) -> reply @@ `String (Block.serialize ~hex:true b)
	)
	| _ -> 
		Log.error "Rpc ↚" "Request %s not handled" req.methodn;
		reply_err (-32601) "Method not found"
;;

type t = {
	blockchain : Chain.t;
	network 	 : Net.t;
	conf			 : Config.rpc;
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
			(try handle_request a.blockchain a.network req
			with | e -> Log.error "Rpc" "Error handling reqest: %s %s" req.methodn @@ Printexc.to_string e);
			if a.run then do_listen socket
	in
	Log.info "Rpc" "Binding to port: %d" a.conf.port;
	listen a.socket a.conf.port 8;
	try do_listen a.socket with _ -> ()
;;

let shutdown a = 
	Log.fatal "Rpc" "Shutdown...";
	close a.socket;
	a.run <- false
;;

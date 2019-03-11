open Utils;;
open Thread;;
open Random;;
open Config;;
open Bitcoinml;;

type run = { mutable run: bool };;


let main () =
	let running = { run=true } in
	let loop n bc = 
		while running.run do (
			Chain.step bc;
			Net.step n bc
			(*Unix.sleep 1*)
		) done;
		Chain.shutdown bc;
		Net.shutdown n
	in
	Random.self_init ();
	Log.info Constants.name "Starting %s" Constants.version;
	let conf = Config.parse_base_path () |> Config.load_or_init |> Config.parse_command_line in
	let _ = Config.create_dirs conf in
	let cn = Params.abbr_to_network conf.chain in
	Log.set_level conf.log_level;
	if cn = NOTFOUND then
		Log.info Constants.name "Invalid chain"
	else 	
		Log.info Constants.name "Selected network: %s" (Params.name_of_network cn);
		let p = Params.of_network cn in	
				
		let bc = Chain.load conf.path conf p in
		let n = Net.init bc.Chain.params conf in 
		(*let chain_thread = Thread.create (fun bc -> Chain.loop bc) bc in
		let net_thread = Thread.create (fun (n, bc) -> Net.loop n bc) (n, bc) in*)

		let rpc = Rpc.init conf.rpc bc n in
		let rpc_thread = Thread.create (fun rpc -> Rpc.loop rpc) rpc in

		let loop_thread = Thread.create (fun () -> loop n bc) () in

		let sighandler signal = match running.run with
		| false -> Log.fatal Constants.name "Quit signal, shutdown already in progress, please wait..."
		| true -> (
			Log.fatal Constants.name "Quit signal, shutdown. Please wait for the secure shutdown procedure...";
			running.run <- false;
			Chain.shutdown_pre bc;
			Log.info Constants.name "Waiting for childs";
			Rpc.shutdown rpc;
			(* Thread.join rpc_thread; *)
			(*Thread.join net_thread;
			Thread.join chain_thread;*)
			Log.info Constants.name "Exit.";
		) in
		Sys.set_signal Sys.sigint @@ Signal_handle (sighandler);
		
		Thread.join loop_thread;
;;

main ();;

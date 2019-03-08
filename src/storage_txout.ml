open LevelDB;;
open Bitcoinml;;
open Stdint;;
open Utils;;
open Store;;

type t = Store_raw.t;;


type txout_t = {
  height: int;
  blockhash: Hash.t;
  coinbase: bool;
  txout: Tx.Out.t
};;

module Txout_index = Store.Make_index 
  (struct 
    type t = txout_t;;
    let serialize o = "";;
    let parse o = Some ({ height=0; blockhash=""; coinbase=false; txout={value=Int64.of_int 0; script= ([], 0)} });;
  end)
  (struct let prefix = "txout" end)
;;


let insert_txout txout_store txhash n height blockhash (txout : Tx.Out.t) = ();;

let remove_txout txout_store txhash n = ();;

let get_txout txout_store txhash n = None;;


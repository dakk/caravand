cd ~/Repositories/lightning
lightningd/lightningd --network=$1 --log-level=debug --bitcoin-rpcuser=test \
--bitcoin-rpcpassword=test --bitcoin-rpcport=$2 --alias=Caravand

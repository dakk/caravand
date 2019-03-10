cd ~/Repositories/lightning
lightningd/lightningd --network=$1 --log-level=debug --bitcoin-rpcuser=test \
--bitcoin-rpcpassword=test --bitcoin-rpcport=8332 --alias=Caravand

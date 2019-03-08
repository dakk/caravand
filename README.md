# caravand

<!-- ![Logo of caravand](https://github.com/dakk/caravand/raw/master/docs/logo/logo.png) -->

[![Build Status](https://travis-ci.org/dakk/caravand.svg)](https://travis-ci.org/dakk/caravand)
[![MIT License](http://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/dakk/caravand/blob/master/LICENSE)
[![docs](https://img.shields.io/badge/doc-online-blue.svg)](https://github.com/dakk/caravand/wiki)

Bitcoin light node for c-lightning with bitcoin-core compatible jsonrpc. 

Unlike [spruned](https://github.com/gdassori/spruned) who connects both to electrum server and bitcoind nodes,
caravand only connects to bitcoind nodes, and act more or less as a pruned node, avoiding data-miss 
situations: the disk footprint is larger than spruned (since we keep more blocks and txout indexes) but less 
than a bitcoind node (or a bitcoind pruned node).
The software is still in development and currently **it does not work**; please refer to this 
[issue](https://github.com/dakk/caravand/issues/1) for the project status.

Caravand is based on the bitcoinml ocaml library https://github.com/dakk/bitcoinml/

<!--
## Documentation
An updated documentation is available [here](https://github.com/dakk/caravand/wiki).
-->

## License

```
Copyright (c) 2019 Davide Gessa

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
```


## Donations

Feel free to donate bitcoin to the developer: 13TRVwiqLMveg9aPAmZgcAix5ogKVgpe4T

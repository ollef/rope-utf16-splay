# rope-utf16-splay [![Build Status](https://travis-ci.org/ollef/rope-utf16-splay.svg?branch=master)](https://travis-ci.org/ollef/rope-utf16-splay) [![Hackage](https://img.shields.io/hackage/v/rope-utf16-splay.svg)](https://hackage.haskell.org/package/rope-utf16-splay)


Thick strings optimised for indexing and updating using UTF-16 code units and
row/column pairs.

This implementation uses splay trees instead of the usual finger trees, which
is faster according to [the benchmarks](bench.html).

## Contact

Olle Fredriksson - https://github.com/ollef

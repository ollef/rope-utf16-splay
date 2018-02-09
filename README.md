# rope-utf16-splay

Thick strings optimised for indexing and updating using UTF-16 code points and
row/column pairs.

This implementation uses splay trees instead of the usual finger trees, which
is faster according to [the benchmarks](bench.html).

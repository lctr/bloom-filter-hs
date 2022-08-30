# Bloom filter
A simple implementation of a *Bloom* filter in Haskell.

A [*Bloom* filter](https://en.wikipedia.org/wiki/Bloom_filter) is a space-efficient probabilistic set-like data structure that supports *insertion* and
*membership querying* and is used to test whether an element if a member of a
set.

Notably, false positives are possible in Bloom filters, but false *negatives*
are impossible.

This implementation is a code-along for the corresponding chapter 26 tutorial in [Real
World
Haskell](http://book.realworldhaskell.org/read/advanced-library-design-building-a-bloom-filter.html),
and as such does *not* implement the "counting Bloom filter* variant, therefore
element removal is not supported.


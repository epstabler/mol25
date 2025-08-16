# Modular minimalist grammar with realizational morphology

This is a preliminary implementation 
**still under development**
based on the
[MOL-25](https://mol2025.molweb.xyz/programproceedings.html) paper of
the same name.

Mg.hs defines a function g that maps binary trees over roots to
binary trees over pronounceable morphemes, if the structure is
grammatical.

**Listings.pdf** shows the definition of the grammar g and
the 6 linguistic modules it composes, followed by
**an example session**. 

I installed ghc with [ghcup](https://www.haskell.org/ghcup/), and then used
[ghci 9.6.7](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html)
to develop and test the code in this directory.
No exotic language features are used, so this code should
run with ghcup-recommended ghci versions for some time.

To run some examples -- a couple that succeed, and one that fails:

```
> ghci -package time
ghci> :l Examples
ghci> ex 8
ghci> ex 40
ghci> xx 0
```

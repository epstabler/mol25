# Modular minimalist grammar with realizational morphology

This is a preliminary implementation 
**still under development**
based on the
[MOL-25](https://mol2025.molweb.xyz/programproceedings.html) paper of
the same name, in Haskell.

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

See Examples.hs for many examples. There,
Figure 1 = ex 8;
Figure 2 is similar to ex 17, ex 18, ex 19, ex 20, ex 21, ex 22;
Figure 3 = ex 50;
Figure 4 = ex 8;
Figure 5 = ex 53;
Figure 6a = ex 58;
Figure 6b = ex 66.

Here we run one of these examples that succeeds, and
one that fails because it violates smc:

```
> ghci -package time
ghci> :l Examples
ghci> ex 8
ghci> xx 3
```

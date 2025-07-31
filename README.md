# Modular minimalist grammar with realizational morphology

** IN PREPARATION **

Based on [MOL-25](https://mol2025.molweb.xyz/programproceedings.html)
paper of the same name, here are slides and a Haskell implementation
of the function g that maps binary trees over roots to binary trees
over pronounceable morphemes, if the structure is grammatical.

**Listings.pdf** shows the 6 linguistic modules and the definition of g on 4 pretty-printed pages, with lots of white space, followed by **an example session**. 

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

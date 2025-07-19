# Modular minimalist grammar with realizational morphology

[MOL-25](https://mol2025.molweb.xyz/programproceedings.html) slides and a Haskell implementation.

I installed ghc with [ghcup](https://www.haskell.org/ghcup/), and then used
[ghci 9.6.7](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html)
to develop and test the code in this directory.
No exotic language features are used, so hopefully this code will
run with the ghcup-recommended ghci versions for some time.

To run some examples, some of which are mentioned in the slides, type:

```
> ghci -package time
ghci> :l Examples
ghci> ex 7
ghci> xx 2
```

See the code and comments for many other examples.

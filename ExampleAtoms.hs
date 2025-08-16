module ExampleAtoms where
import MgTypes (SO(L))

{- syntactic atoms -- language-specific -- these tailored for examples in the paper -}
atom :: Int -> SO -- produce example atoms by number
atom i = [
  -- items for first example, Figures 1 and 4
  L (["√Jo"], ([], ["D"]), []),             -- 0
  L (["√which"], (["N"], ["D","wh"]), []),  -- 1
  L (["√the"], (["N"], ["D"]), []),         -- 2
  L (["√cat"], ([], ["N"]), []),            -- 3
  L (["√rat"], ([], ["N"]), []),            -- 4
  L (["√food"], ([], ["N"]), []),           -- 5 
  L (["√like"], (["D","D"], ["V"]), []),    -- 6
  L (["√know"], (["C","D"], ["V"]), []),    -- 7
  L (["√decl"], (["V"], ["C"]), []),        -- 8
  L (["√q"], (["V","wh"], ["C"]), []),      -- 9
  -- a second example,just slightly more elaborate
  L (["√the"], (["N"], ["D","K"]), []),     -- 10
  L (["√chase"], (["D","K"], ["V*"]), []),  -- 11
  L (["-√v"], (["V*","D"], ["v*"]), []),    -- 12
  L (["-√pres"], (["v*","K"], ["T"]), []),  -- 13
  -- for testing head movement, Figure 3
  L (["√have"], ([], ["HAVE"]), []),        -- 14
  L (["√have$"], ([], ["HAVE"]), []),       -- 15
  L (["-√pres"], (["HAVE"], ["T"]), []),    -- 16
  L (["-√pres$"], (["HAVE"], ["T"]), []),   -- 17
  L (["√q"], (["T"], ["C"]), []),           -- 18
  L (["-√q"], (["T"], ["C"]), []),          -- 19
  L (["-√q$"], (["T"], ["C"]), []),         -- 20
  -- variations on second example:
  L (["√who"], ([], ["D","K","wh"]), []),   -- 21
  L (["√decl"], (["T","wh"], ["C"]), []),   -- 22
  -- for testing agr, Figure 2
  L (["√the"], (["N"], ["D","K"]), [([["D"],["N"]],"φ:_"),([["D"],["T"],["C"]],"κ:_")]),        -- 23
  L (["√which"], (["N"], ["D","K","wh"]), [([["D"],["N"]],"φ:_"),([["D"],["T"],["C"]],"κ:_")]), -- 24
  L (["√cat"], ([], ["N"]), [([["T"],["D"],["C"]],"φ:3s")]),   -- 25
  L (["√rat"], ([], ["N"]), [([["D"],["N"]],"φ:3p")]),              -- 26
  L (["√chase$"], (["D","K"], ["V*"]), [([["D"],["T"],["C"]],"κ:acc"),([["T"],["D"],["C"]],"φ:_")]),    -- 27
  L (["-√v$"], (["V*","D"], ["v*"]), []),                  -- 28 -v*
  L (["-√pres"], (["v*","K"], ["T"]), [([["D"]],"φ:3s"),([["D"],["T"],["C"]],"κ:nom")]),   -- 29 -T
  L (["√wh"], (["T","wh"], ["C"]), [])                                              -- 30 -C*
  ]!!i

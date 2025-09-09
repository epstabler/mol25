module ExampleAtoms where
import MgTypes (SO(L))

{- syntactic atoms -- language-specific -- these tailored for examples in the paper -}
atom :: Int -> SO -- return example atoms by their position in the lexicon
atom i = [
  -- items for first example, Figures 1 and 4
  L (["√Jo"], ([], ["D"]), ([],[])),             -- 0
  L (["√which"], (["N"], ["D","wh"]), ([],[])),  -- 1
  L (["√the"], (["N"], ["D"]), ([],[])),         -- 2
  L (["√cat"], ([], ["N"]), ([],[])),            -- 3
  L (["√rat"], ([], ["N"]), ([],[])),            -- 4
  L (["√food"], ([], ["N"]), ([],[])),           -- 5 
  L (["√like"], (["D","D"], ["V"]), ([],[])),    -- 6
  L (["√know"], (["C","D"], ["V"]), ([],[])),    -- 7
  L ([], (["V"], ["C"]), ([],[])),               -- 8
  L ([], (["V","wh"], ["C"]), ([],[])),          -- 9
  -- a second example,just slightly more elaborate
  L (["√the"], (["N"], ["D","K"]), ([],[])),     -- 10
  L (["√chase"], (["D","K"], ["V*"]), ([],[])),  -- 11
  L (["-√v"], (["V*","D"], ["v*"]), ([],[])),    -- 12
  L (["-√pres"], (["v*","K"], ["T"]), ([],[])),  -- 13
  -- testing head movement, Figure 2
  L (["√have"], ([], ["HAVE"]), ([],[])),        -- 14
  L (["√have@"], ([], ["HAVE"]), ([],[])),       -- 15
  L (["-√pres"], (["HAVE"], ["T"]), ([],[])),    -- 16
  L (["-√pres@"], (["HAVE"], ["T"]), ([],[])),   -- 17
  L (["√q"], (["T"], ["C"]), ([],[])),           -- 18
  L (["-√q"], (["T"], ["C"]), ([],[])),          -- 19
  L (["-√q@"], (["T"], ["C"]), ([],[])),         -- 20
  -- more variations on second example:
  L (["√who"], ([], ["D","K","wh"]), ([],[])),   -- 21
  L ([], (["T","wh"], ["C"]), ([],[])),   -- 22
  -- testing agr, Figure 3
  L (["√the"], (["N"], ["D","K"]),               -- 23
     ([([["D"],["N"]],"φ:_")],
      []
     )),
  L (["√which"], (["N"], ["D","K","wh"]),        -- 24
     ([([["D"],["N"]],"φ:_")],
      []
     )),
  L (["√cat"], ([], ["N"]),                      -- 25
     ([],
      ["φ:3s"]
     )),
  L (["√rat"], ([], ["N"]),                      -- 26
     ([],
      ["φ:3p"]
     )),
  L (["√chase"], (["D","K"], ["V*"]), ([],[])),  -- 27
  L (["-"], (["V*","D"], ["v*"]), ([],[])),   -- 28
  L (["-√pres"], (["v*","K"], ["T"]),            -- 29
     ( [([["C"],["T"],["D"]],"φ:_")],
       []
     )),
  L ([], (["T","wh"], ["C"]), ([],[])),          -- 30
  -- English BE example, "was laugh -ing", Figure 5
  L (["√past"], (["Asp","K"], ["T"]), ([],[])),  -- 31
  L (["-√-ing"], (["v"], ["Asp"]),               -- 32
     ( [([["T"],["V"]],"ι:_")],
       []
     )),
  L (["-√v@"], (["V"], ["v"]), ([],[])),         -- 33
  L (["√laugh"], ([], ["V"]),                    -- 34
     ( [],
       ["ι:prog"]
     )),
  -- French example, Figure 6a, "elle-s sont mor-ts" (Mixing Langs -- should really be sep from Eng)
  L (["-√pres"], (["Aux","K"], ["T"]), ([],[])), -- 35
  L (["√aux"], (["v"], ["Aux"]), ([],[])),       -- 36
  L (["-√-t"], (["V"], ["v"]), ([],[])),         -- 37
  L (["√mor"], (["D"], ["V"]), ([],[])),         -- 38
  L (["√elle"], ([], ["D","K"]),                 -- 39
     ( [],
       ["φ:3p"]
     )),
  -- French example, Figure 6b, "Camille a pri-s la fleur"  (Mixing Langs -- should really be sep)
  L (["√la"], (["N"], ["D","K"]),                -- 40
     ([([["D"],["N"]],"φ:_")],
      []
     )),
  L (["√fleur"], ([], ["N"]),                    -- 41
     ([],
      ["φ:3s"]
     )),
  L (["√pri"], (["D","K"], ["V*"]), ([],[])),    -- 42
  L (["-√-s"], (["V*","D"], ["v*"]), ([],[])),    -- 43
  L (["√Camille"], ([], ["D","K"]),              -- 44
     ([([["D"],["N"]],"φ:3s")],
      []
     )),
  L (["√v*"], (["v*"], ["Aux"]), ([],[])),      -- 45
  L (["√pres"], (["Aux","K"], ["T"]), ([],[]))   -- 46
  ]!!i

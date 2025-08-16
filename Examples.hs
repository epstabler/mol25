module Examples where
import MgTypes (SO(L,S))
import Mrg (mrg)
import Sel (sel,lbl)
import Agr (instantiate,hasProbe,typeVal)
import Hm (hm)
import Vi (vi,ph,joinstr)
import Mg (g)
import ExampleAtoms (atom)
import PrettyPrint (lbl2str,so2pretty)
import Data.Time (getCurrentTime, diffUTCTime)

{- build some examples with atoms from ExampleAtoms.hs -}
exs :: [SO]
exs = [
  -- first example, Figures 1 and 4
  S (atom 1) (atom 5),                     -- 0 (which food): D
  S (atom 6) (head exs),                   -- 1 like (which food)
  S (atom 2) (atom 3),                     -- 2 the cat
  S (exs!!1) (exs!!2),                     -- 3 the cat like (which food)
  S (atom 9) (exs!!3),                     -- 4 C+wh the cat like (which food)
  S (exs!!4) (head exs),                   -- 5 which food the cat like
  S (atom 7) (exs!!5),                     -- 6 know which food the cat like
  S (exs!!6) (atom 0),                     -- 7 Jo know which food the cat like
  S (atom 8) (exs!!7),                     -- 8 Jo know which food the cat like
  -- slightly less simple example
  S (atom 10) (atom 4),                    -- 9 "the rat"
  S (atom 10) (atom 3),                    -- 10 "the cat"
  S (atom 11) (exs!!10),                   -- 11 "chase (the cat)"
  S (exs!!11) (exs!!10),                   -- 12 "the cat chase" V*
  S (atom 12) (exs!!12),                   -- 13 "v* chase the cat" D,v*
  S (exs!!13) (exs!!9),                    -- 14 "(the rat) v* chase the cat" v*,K
  S (atom 13) (exs!!14),                   -- 15 "chase pres (the rat) chase the cat" K-oT,K
  S (exs!!15) (exs!!9),                    -- 16 "the rat chase pres the cat" T
  -- test hm, examples in Figure 3
  S (atom 17) (atom 15),                   -- 17 "have -pres" in T
  S (atom 20) (exs!!17),                   -- 18 "have -pres -q" in C
  S (atom 19) (S (atom 16) (atom 15)),     -- 19 "have* -pres -q" in V
  S (atom 19) (S (atom 17) (atom 14)),     -- 20 "have -pres* -q" in T
  S (atom 20) (S (atom 16) (atom 14)),     -- 21 "have -pres -q*" in C
  S (atom 20) (S (atom 17) (atom 15)),     -- 22 "have* -pres* -q*" in C
  -- variations on first example
  S (atom 11) (atom 21),                   -- 23 "chase (who)" V*
  S (exs!!23) (atom 21),                   -- 24 "(who) chase" V*
  S (atom 12) (exs!!24),                   -- 25 "v* chase who" D,v* (cf exs 4)
  S (exs!!25) (exs!!10),                   -- 26 "(the cat) chase (who)" v*,K
  S (atom 13) (exs!!26),                   -- 27 "chase pres (the cat) chase (who)" K-oT,K
  S (exs!!27) (exs!!10),                   -- 28 "the cat chase pres (who)" T
  S (atom 22) (exs!!28),                   -- 29 "the cat chase pres (who)" T
  S (exs!!29) (atom 21),                   -- 30 "who the cat chase -s" T
  -- which cat chase-s the rat
  S (atom 23) (atom 26),                   -- 31 "the rat"
  S (atom 24) (atom 25),                   -- 32 "which cat"
  S (atom 27) (exs!!31),                   -- 33 "chase (the rat)"
  S (exs!!33) (exs!!31),                   -- 34 "the rat chase" V*
  S (atom 28) (exs!!34),                   -- 35 "v* chase the rat" D,v*
  S (exs!!35) (exs!!32),                   -- 36 "v* chase the rat (which cat)" v*,K
  S (atom 29) (exs!!36),                   -- 37 "chase pres the rat (which cat)" K-oT,K
  S (exs!!37) (exs!!32),                   -- 38 "(which cat) chase pres the rat (which cat)" T,K
  S (atom 30) (exs!!38),                   -- 39 "(which cat) chase pres the rat" C,wh
  S (exs!!39) (exs!!32),                    -- 40 "which cat chase-s the rat" C,
  -- agr: which rat-s the cat chase
  S (atom 24) (atom 26),                   -- 41 "which rat"
  S (atom 23) (atom 25),                   -- 42 "the cat"
  S (atom 27) (exs!!41),                   -- 43 "chase (which rat)"
  S (exs!!43) (exs!!41),                   -- 44 "(which rat) chase" V*
  S (atom 28) (exs!!44),                   -- 45 "v* (which rat) chase (which rat)" D,v*
  S (exs!!45) (exs!!42),                   -- 46 "v* chase (the cat) (which rat)" v*,K
  S (atom 29) (exs!!46),                   -- 47 "(the cat) (which cat-s) chase pres" K-oT,K
  S (exs!!47) (exs!!42),                   -- 48 "the cat-s (the cat-s) (which rat) chase pres ()" T,K
  S (atom 30) (exs!!48),                   -- 49 "(which rat) the cat-s () () chase pres ()" C,wh
  S (exs!!49) (exs!!41),                   -- 50 "which rat the cat-s chase" C,
  -- testing agr
  S (L (["√which"], (["N"], ["D","K","wh"]), [([],"φ:3s")])) --51 -- reversing probe-goal of 41
    (L (["√rat"], ([], ["N"]), [([["D","N"]],"φ:_")]))
  ]

xxs = [ -- CHECKING THAT PROBLEM CASES ARE APPROPRIATELY HANDLED
  S (atom 2) (atom 2),                     -- 0 "the the", no match -- UNLABELABLE
  S (exs!!25) (atom 21),                   -- 1 "(who) (who) chase" v*,(wh,K.wh) -- OK, but trouble later if SMC
  S (atom 13) (xxs!!1),                    -- 2 "pres (who) (who) chase" v*,(wh,wh) -- OK, but trouble later if SMC
  S (xxs!!2) (atom 21)                     -- 3 ERROR if SMC enforced in Sel.hs, OK otherwise
  ]

ex i = if i >= length exs then putStrLn "No such example" else tst (exs!!i)
xx i = if i >= length xxs then putStrLn "No such bad example" else tst (xxs!!i)

tst so = do
  putStrLn $ "\ninput so =\n" ++ so2pretty so
  putStrLn $ "\nlbl so =\n" ++ lbl2str (lbl so)
  putStrLn $ "\ninstantiate so =\n" ++ so2pretty (instantiate so)
  startTime <-  getCurrentTime
  putStrLn $ "\ng so =\n" ++ so2pretty (g so)
  endTime <- getCurrentTime
  let diff = diffUTCTime endTime startTime
  putStrLn $ "\nexecution time for computing (g so) and printing that result = " ++ show diff
  putStrLn $ "\n(ph.g) so =\n" ++ (ph.g) so
  putStrLn ""

test =  S (L (["√which"], (["N"], ["D","K","wh"]), [([],"φ:3s")])) --51 -- reversing probe-goal of 41
           (L (["√rat"], ([], ["N"]), [([["D"],["N"]],"φ:_")]))

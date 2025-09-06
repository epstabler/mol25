module Vi where
import MgTypes (F,Morph,SO(L,S,E),subList,joinStr)
import Agr (leafFeatures)
import Hm (rmHmDiacritics)

{- In development ... Vi rules integrated into function definitions here, facilitating experimentation.
 They should be separated in future work, to support parametric grammar definition, as in the paper. -}

vi :: SO -> SO
--vi so = so
vi E = E
vi (S (L (["√de"],s,a)) (S (L (["√el"],s',a')) so)) = S (L (["del"],s,a)) (S (L ([],s',a')) so)
vi (S so so') = S (vi so) (vi so')
vi (L (rs,s,a)) = L (m (rmHmDiacritics rs) (leafFeatures (rs,s,a)), s, a)

m :: Morph -> [F] -> Morph
m rs lfs = if (not.null) rs && head rs `elem` ["√pres","√past"] then "DO":mDefault rs lfs  else mDefault rs lfs

mDefault :: Morph -> [F] -> Morph
mDefault rs lfs = case rs of
  ("√pres":rs') -> if "φ:3s" `elem` lfs then "-s":mDefault rs' lfs else mDefault rs' lfs
  ("√pres.T.φ:3s":rs') -> "-s":mDefault rs' lfs
  ("√past":rs') -> "-ed":mDefault rs' lfs
  ("√q":rs') -> mDefault rs' lfs           -- unpronounced
  ("√wh":rs') -> mDefault rs' lfs          -- unpronounced
  ("√v":rs') -> mDefault rs' lfs           -- unpronounced
  ("√v@.v*":rs') -> mDefault rs' lfs       -- unpronounced
  (r:rs') -> if subList ["N","φ:3p"] lfs then rDefault r:"-s":mDefault rs' lfs else rDefault r:mDefault rs' lfs
  [] -> []
  where
  rDefault cs = rmFs ("",case cs of {'√':cs' -> cs' ; _ -> cs})
  rmFs (t,f) = case f of {'.':_ -> t; c:cs -> rmFs (t++[c],cs); _ -> t}

ph :: SO -> String  -- return string of phonologically specified morphs
ph (S so so') = joinStr " " [ph so, ph  so']
ph (L (w,_,_)) = if not (null w) && head w == "(" then "" else joinStr " " w
ph E = ""

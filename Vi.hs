module Vi where
import MgTypes (F,Morph,SO(L,S,E),subList,joinStr)
import Agr (leafFeatures)
import Hm (rmHmDiacritics)

vi :: SO -> SO
vi E = E
vi (S (L (["√de"],s,a)) (S (L (["√el"],s',a')) so)) = S (L (["del"],s,a)) (S (L ([],s',a')) (vi so))
vi (S (L (["√pres.T"],s,a)) (S (L (["√v*.Aux"],s',a')) so)) = S (L (["a"],s,a)) (S (L ([],s',a')) (vi so)) -- Fig 6b
vi (S so so') = S (vi so) (vi so')
vi (L (rs,s,a)) = L (m (rmHmDiacritics rs) (leafFeatures (rs,s,a)), s, a)

m :: Morph -> [F] -> Morph
m rs lfs = case rs of
  ("√past.T":_) -> mDefault ("BE":rs) lfs  -- for ex 53, Figure 5, English BE inserted
  _ -> mDefault rs lfs

mDefault :: Morph -> [F] -> Morph
mDefault rs lfs = case rs of
  ("√pres.T.φ:3s":rs') -> "-s":mDefault rs' lfs
  ("BE":"√past.T":rs') -> "was":mDefault rs' lfs -- for ex 53
  ("√past.T":rs') -> "was":mDefault rs' lfs
  ("√aux.Aux": "√pres.T" : rs') -> "sont":mDefault rs' lfs -- Fig 6a
  ("√elle":rs') -> if "φ:3p" `elem` lfs then "elle":"-s":mDefault rs' lfs else "elle":mDefault rs' lfs -- Fig 6a
  (r:rs') -> if subList ["N","φ:3p"] lfs then rDefault r:"-s":mDefault rs' lfs else rDefault r:mDefault rs' lfs
  [] -> []
  where
  rDefault cs = rmFs ("",case cs of {'√':cs' -> cs' ; _ -> cs})
  rmFs (t,f) = case f of {'.':_ -> t; c:cs -> rmFs (t++[c],cs); _ -> t}

ph :: SO -> String  -- return string of phonologically specified morphs
ph (S so so') = joinStr " " [ph so, ph  so']
ph (L (w,_,_)) = if not (null w) && head w == "(" then "" else joinStr " " w
ph E = ""

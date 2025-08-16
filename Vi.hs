module Vi where
import MgTypes (Morph,Sel,Agr,SO(L,S,E))
import Agr (instantiate,agrees)

{- In development ... Vi rules integrated into function definitions here, facilitating experimentation.
 They should be separated in future work, to support parametric grammar definition, as in the paper. -}

vi :: SO -> SO        -- vocabulary insertion
vi so = vi' (instantiate so)
  where
  vi' E = E
  vi' (S so so') = S (vi' so) (vi' so')
  vi' (L ([],sel,agr)) = L ([],sel,agr)
  vi' (L (r:rs,sel,agr)) = 
    if strip r `elem` ["√pres","√past"]
    then L ("DO":viDefault (strip r:rs) sel agr,sel,agr)
    else L (viDefault (strip r:rs) sel agr, sel, agr)
  strip ('-':cs) = stripSuf cs             -- remove morph diacritics
  strip cs = stripSuf cs
  stripSuf r = if not (null r) && last r == '$' then init r else r

  viDefault :: Morph -> Sel -> Agr -> Morph
  viDefault rs sel agr = case rs of
    ("-√pres":rs') -> if "φ:3s" `agrees` agr then "-s":viDefault rs' sel agr else viDefault rs' sel agr
    ("-√past":rs') -> "-ed":viDefault rs' sel agr
    ("√decl":rs') -> viDefault rs' sel agr        -- unpronounced
    ("√q":rs') -> viDefault rs' sel agr           -- unpronounced
    ("√wh":rs') -> viDefault rs' sel agr          -- unpronounced
    ("-√v$":rs') -> viDefault rs' sel agr         -- unpronounced
    (r:rs') ->
      if "N" `elem` snd sel && "φ:3p" `agrees` agr
      then defStrip r:"-s":viDefault rs' sel agr
      else defStrip r:viDefault rs' sel agr
    [] -> []

  defStrip ('√':cs) = cs  -- use root name as default
  defStrip cs = cs        -- else id

ph :: SO -> String  -- return string of phonologically specified morphs
ph (S so so') = joinstr " " [ph so, ph  so']
ph (L (w,_,_)) = if not (null w) && head w == "(" then "" else joinstr " " w
ph E = ""

joinstr :: String -> [String] -> String  -- join strings, separated by sep
joinstr sep = foldr (\x y -> if null y then x else x ++ sep ++ y) ""

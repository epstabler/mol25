module Agr where
import Data.List (partition,union)
import MgTypes (F,Agr,Leaf,SO(L,S,E),Label,fstOf3,sndOf3)

{- In development ... this interim version not working as intended yet -}

agr :: SO -> SO
agr so = let isos = instantiate so in if isos == isos then so else error "agr"  --  == forces computation of isos

instantiate so = let (iso, ispine) = i (unionTiers so) [] so in iso  -- return fully instantiated agreement
  where
  i tier ders so = case updateSpine tier (hd so) ders of -- i tier dcommanders so -> (so',dcommandees)
    Just spine -> let (so',dees) = i' tier spine so in iLeaf ders dees so'
    Nothing -> i' tier ders so

  i' tier ders (L leaf) = (L leaf,[])  -- i' tier dcommanders so -> (so',dcommandees)
  i' tier ders (S so so') = case (lblfs so, lblfs so') of  -- headFirst?
    ( ((f:ns, ps), agrfs, movers), (([], _:ps'), agrfs', movers') ) -> i'' f  so movers so' tier ders True
    ( (([], _:ps'), agrfs', movers'), ((f:ns, ps), agrfs, movers) ) -> i'' f so' movers so  tier ders False

  i'' f so movers so' tier spine headFirst = case partition ((== f).head.sndOf3) movers of -- IM or EM?
    ( [(y',_:ps'',agrfs'')], movers'') ->        -- IM
      let (iso,ispine) = i tier [] so  in if headFirst then (S iso so',ispine) else (S so' iso,ispine)
    _ -> case updateSpine tier (hd so') spine of -- EM
      Just spine' -> let (iso,ispine) = i tier spine' so in let (iso', _) = i tier [] so' in
        if headFirst then (S iso iso', ispine) else (S iso' iso, ispine)
      Nothing -> let (iso,ispine) = i tier spine so in let (iso', _) = i tier [] so' in
        if headFirst then (S iso iso', ispine) else (S iso' iso, ispine)

iLeaf ders dees so' = let (r,sel,agr) = hd so' in -- iLeaf dcommanders dcommandees so -> (so',dcommandees')
  let (so'',dees',agr') = iAgr ders (so',dees,agr) in (putHd (r,sel,agr') so'',dees')

iAgr _ (so,dees,[]) = (so,dees,[])
iAgr ders (so,dees,(tier,f'):agr) = let (so',dees',f'') = iF ders (so,dees) tier f' in
  let (so'',dees'',agr') = iAgr ders (so',dees',agr) in (so'',dees'',(tier,f''):agr')

iF ders (so,dees) tier f' = case nextInTier tier dees of
  Just (r,sel,agr) -> let (t,v) = typeVal f' in let (agr',so',f'') = matchAgr t v agr so in
      if head v == '_' then (putTier tier (r,sel,agr') so', dees, f'') else (so,(r,sel,agr'):dees,f'')
  Nothing -> case nextInTier tier ders of
    Just (r,sel,agr) -> let (t,v) = typeVal f' in let (agr',so',f'') = matchAgr t v agr so in
        if head v == '_' then (putTier tier (r,sel,agr') so', dees, f'') else (so,(r,sel,agr'):dees,f'')
    Nothing -> (so,dees,f')

matchAgr t v [] so = ([],so,t++":"++v)
matchAgr t v ((tier,f):agr) so = let (t',v') = typeVal f in if t == t' 
  then let x = matchF v v' in ((tier,t++":"++x):agr,so,t++":"++x)
  else let (agr',so',f') = matchAgr t v agr so in ((tier,f):agr',so',f')

matchF ('_':x) x' = case x of {"" -> x'; _ -> if x == x' then x else error "matchF downward conflict"}
matchF x' ('_':x) = case x of {"" -> x'; _ -> if x == x' then x else error "matchF upward conflict"}
matchF x x' = if x == x' then x else error "matchF"

hd (L leaf) = leaf
hd (S so so') = if (not.null.fst.fstOf3.lblfs) so then hd so else hd so'

putHd leaf (L _) = L leaf
putHd leaf (S so so') = if (not.null.fst.fstOf3.lblfs) so then S (putHd leaf so) so' else S so (putHd leaf so')

putTier _ leaf (L _) = L leaf
putTier tier leaf (S so so')
 | leafInTier (hd (S so so')) tier = putHd leaf (S so so')
 | (not.null.fst.fstOf3.lblfs) so = S (putTier tier leaf so) so'
 | otherwise = S so (putTier tier leaf so')

updateSpine tier leaf spine = if leafInTier leaf tier then Just (leaf:spine) else Nothing

nextInTier tier spine = case spine of {[] -> Nothing; lf:spine -> if leafInTier lf tier then Just lf else nextInTier tier spine}

leafInTier (_,(_,p:ps),agr) = foldr (\x sofar -> subList x (p:map snd agr) || sofar) False

typeVal f = tV' "" f where tV' pre f = case f of {"" -> (pre,""); (c:cs) -> if c == ':' then (pre,cs) else tV' (pre++[c]) cs}

unionTiers (S so so') = unionTiers so `union` unionTiers so' -- return union of tiers and probe host categories
unionTiers (L (_,sel,agr)) = foldr (union.fst) [[(head.snd) sel] | hasProbe agr] agr

hasProbe = foldr (\x sofar -> ((== '_').head.snd.typeVal.snd) x || sofar) False -- agr has a probe?

subList a b = foldr (\x sofar -> (x `elem` b) && sofar) True a  -- a is a sublist of b?

lblfs (L (_, (ns,ps), agr)) = ((ns,ps), agr, []) -- since lbl already checked by sel, this is simpler: no smc, etc
lblfs (S so so') = case (lblfs so, lblfs so') of -- head-first?
  ( ((f:ns, ps), agr, movers), (([], _:ps'), agr', movers') ) -> lblfs' so f (ns,ps) agr movers so' ps' agr' movers'
  ( (([], g:ps'), agr', movers'), ((f:ns, ps), agr, movers) ) -> lblfs' so' f (ns,ps) agr movers so  ps' agr' movers'
  where 
  lblfs' so f headFs agr movers spec ps' agr' movers' =  case partition ((== f).head.sndOf3) movers of -- IM or EM?
    ( [(y',_:ps'',agr'')], movers'') ->  (headFs, agr, newmoversfs spec movers'' [] ps'' agr'')
    _ -> (headFs, agr, newmoversfs spec movers movers' ps' agr')
  newmoversfs spec mover mover' ps' agr' = case ps' of { [] -> mover++mover'; _ -> mover++[(spec,ps',agr')]++mover' }

agrees f agr = case agr of {[] -> False; ((_,f'):agr) -> f == f' || agrees f agr} -- is f in agr?

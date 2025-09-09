module Agr where
import MgTypes (F,Tier,Agr,Leaf,SO(L,S,E),typeVal,subList)
import Sel (hd)

agr :: SO -> SO
agr E = E
agr (L leaf) = L leaf
agr (S hso cso) = let (hso',cso') = (agr hso, agr cso) in
                  let (r,s,a) = hd hso' in
                  let (cso'',a') = iAgr cso' a in
                  let hso'' = putAgrInHead a' hso' in
                    S hso'' cso''
  where
  iAgr E a = (E, a)                              -- instantiate agr probe features, returning (newComp, newAgr)
  iAgr so ([],gs) = (so, ([],gs))
  iAgr so ((tier,f):ps,gs) = case nextLeafInTier tier so of
      Nothing -> let (so',(ps',gs')) = iAgr so (ps,gs) in (so', ((tier,f):ps', gs'))
      Just leaf -> let (f',so',changed) = iL so tier leaf f in
                   let (so'',(ps',gs')) = iAgr so' (ps,gs) in
                     if changed then (so'', (ps', f':gs')) else (so'', ((tier,f):ps', gs'))

  iL so tier (r,s,(ps,gs)) f =                   -- given probe f and leaf, search for value in leaf gs features
    let (ftype,fval) = typeVal f in let (fval',gs',changed) = matchAndFlag (ftype,fval) gs in
    if changed then (ftype++":"++fval', putAgrInTier tier (ps, gs') so, True) else (f, so, False)
    where
    matchAndFlag (_,fval) [] = (fval, [], False)
    matchAndFlag (ftype,fval) (g:gs) = let (gtype,gval) = typeVal g in
      if gtype == ftype
      then (gval, (gtype ++ ":_" ++ gval):gs, True)
      else let (gval',gs',changed) = matchAndFlag (ftype,fval) gs in (gval', g:gs', changed)

  nextLeafInTier _ E = Nothing
  nextLeafInTier tier (L h) = if leafIsInTier h tier then Just h else Nothing
  nextLeafInTier tier (S hso cso) = let h = hd hso in
    if leafIsInTier h tier then Just h else nextLeafInTier' tier (S hso cso)
    where -- after checking head of (S hso cso) above, now check heads-of-specs, then first-merged comp (if any)
    nextLeafInTier' tier (S (L _) cso) = nextLeafInTier tier cso
    nextLeafInTier' tier (S hso cso) = let ch = hd cso in if leafIsInTier ch tier then Just ch else nextLeafInTier' tier hso

  putAgrInTier tier a (L (r,s,a')) = L (r,s,a)
  putAgrInTier tier a (S hso cso) = let (r,s,a') = hd hso in
    if leafIsInTier (r,s,a') tier then S (putAgrInHead a hso) cso else putAgrInTier' tier a (S hso cso)
    where -- after checking head of (S hso cso) above, now check heads-of-specs, then first-merged comp (if any)
    putAgrInTier' tier a (S (L _) cso) = putAgrInTier tier a cso
    putAgrInTier' tier a (S hso cso) = let (r,s,a') = hd cso in
      if leafIsInTier (r,s,a') tier then S hso (putAgrInHead a cso) else S (putAgrInTier' tier a hso) cso

  putAgrInHead a (L (r,s,_)) = L (r,s,a)
  putAgrInHead a (S so so') = S (putAgrInHead a so) so'

  leafIsInTier leaf = foldr (\x sofar -> subList x (leafFeatures leaf) || sofar) False

leafFeatures :: Leaf -> [F] -- first positive sel feature ('category') and instantiated agr features (without goal-flagging)
leafFeatures (_,(_,p:_),(_,gs)) = p:map rmFlag gs where
  rmFlag f = let (ftype,fval) = typeVal f in if (not.null) fval && head fval == '_' then ftype++":"++tail fval else f

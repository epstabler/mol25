module Sel where
import Data.List (partition)
import MgTypes (SO(L,S),Label,sndOf3)

sel :: SO -> SO
sel so = let lso = (lbl so, lbl so) in
  if lso == lso then so else error "ls"     -- Haskell is lazy. Here == forces computation of lso

lbl :: SO -> Label
lbl (L (_, (negs,poss), agrfs)) = ((negs, poss), agrfs, [])
lbl (S so so') = case (lbl so, lbl so') of
  ( ((f:ns, ps), agrfs, movers), (([], g:ps'), agrfs', movers') ) -> case partition ((== f).head.sndOf3) movers of
      ( [(y',_:ps'',agrfs'')], movers'') ->        -- internal merge
         if y' == so' then ((ns,ps), agrfs, newmovers so' movers'' [] ps'' agrfs'') else error "move-over-merge"
      _ ->                                         -- external merge
         if f == g then ((ns,ps), agrfs, newmovers so' movers movers' ps' agrfs') else error "unlabelable, no match"
  _ -> error "unlabelable"
  where
  newmovers so' ms ms' ps' agrfs' = let nms = case ps' of {[] -> ms++ms'; _ -> ms++[(so',ps',agrfs')]++ms'} in
    if smc nms then nms else error "smc"
  -- smc _ = True                                   -- uncomment this line to remove smc check
  smc movers = noduplicates (map (head.sndOf3) movers)
  noduplicates fs = case fs of {[] -> True; f:fs -> f `notElem` fs && noduplicates fs}

module Sel where
import Data.List (partition)
import MgTypes (Leaf,SO(L,S,E),Label,fstOf3,sndOf3)

sel :: SO -> SO
sel so = let lso = (lblck so, lblck so) in
  if lso == lso then so else error "ls"     -- Haskell is lazy. Here == forces computation of lso
  where
  lblck (L (_, selfs, agrfs)) = (selfs, agrfs, [])
  lblck (S so so') = case (lbl so, lbl so') of
    ( ((f:ns, ps), agrfs, movers), (([], g:ps'), agrfs', movers') ) -> case partition ((== f).head.sndOf3) movers of
        ( [(y',_:ps'',agrfs'')], movers'') ->        -- IM
             if y' == so' then ((ns,ps), agrfs, smc (newmovers so' movers'' [] ps'' agrfs'')) else error "move-over-merge"
        _ -> if f == g then ((ns,ps), agrfs, smc (newmovers so' movers movers' ps' agrfs')) else error "unlabelable, no match"
    _ -> error "unlabelable"
  smc movers = if noduplicates (map (head.sndOf3) movers) then movers else error "smc"
  noduplicates fs = case fs of {[] -> True; f:fs -> f `notElem` fs && noduplicates fs}

lbl (L (_, selfs, agrfs)) = (selfs, agrfs, []) -- after lblck, lbl can be calculated without identities or smc
lbl (S so so') = case (lbl so, lbl so') of
  ( ((f:ns, ps), agrfs, movers), (([], _:ps'), agrfs', movers') ) -> case partition ((== f).head.sndOf3) movers of
      ( [(_,_:ps'',agrfs'')], movers'') -> ((ns,ps), agrfs, newmovers so' movers'' [] ps'' agrfs'') -- IM
      _ -> ((ns,ps), agrfs, newmovers so' movers movers' ps' agrfs')                                 -- EM

newmovers so' ms ms' ps' agrfs' = case ps' of {[] -> ms++ms'; _ -> ms++[(so',ps',agrfs')]++ms'}

module Lin where
import MgTypes (SO(L,S,E),fstOf3,sndOf3)
import Data.List (partition)
import Sel (lbl)

lin :: SO -> SO
lin (L leaf) = L leaf
lin (S so so') = let ((f:_,ps),agrfs,movers) = lbl so in case partition ((== f).head.sndOf3) movers of
    ( [(_,ps'',_)], movers'' ) -> ord ps'' so so'  -- internal merge
    _ -> ord ((snd.fstOf3.lbl) so') so so'         -- external merge
  where
    ord posFeats so so' = case (posFeats, so) of
      ([_], L _) -> S (lin so) (lin so')     -- first merge, nonmoving complement
      ([_], S _ _) -> S (lin so') (lin so)   -- nonfirst merge, nonmoving complement
      _ -> S (lin so) (del so')              -- moving element
    -- del _ = E                             -- uncomment this line to make del = removal
    del (S so so') = S (del so) (del so')
    del (L (morph,sel,agr)) = if (not.null) morph && head morph == "(" then L (morph,sel,agr) else L (["("]++morph++[")"],sel,agr)

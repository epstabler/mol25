module Hm where
import MgTypes (SO(L,S),Morph)

hm :: SO -> SO
hm (L leaf) = L leaf
hm so = case h False False [] so of { ([], so') -> so' ; (rs,so') -> error ("hm: " ++ show rs) }
  where
  dependent morph = not (null morph)  &&  (head.head) morph == '-'
  strong morph = not (null morph) && (last.last) morph == '$'
  -- h dep-above? strong-above? heads-from-above input-SO -> (span-heads-still-to-be-placed, output-SO)
  h :: Bool -> Bool -> Morph -> SO -> (Morph, SO)
  h hiDep hiStrong rs (L (r,sel,agr)) =
    if hiDep && not (strong r && not hiStrong)
    then (r++rs, L ([],sel,agr))
    else ([], L (r++rs,sel,agr))
  h hiDep hiStrong rs (S (L (r,sel,agr)) so) =             -- note: relies on head-first, must precede lin
    if dependent r
    then let (rs',so') = h True (hiStrong || strong r) (r++rs) so in
      if not hiDep || (strong r && not hiStrong)
      then ([], S (L (rs',sel,agr)) so')
      else (rs', S (L ([],sel,agr)) so')
    else case h False False [] so of
      ([], so' ) ->
        if hiDep
        then (r++rs, S (L ([],sel,agr)) so')
        else ([], S (L (r++rs,sel,agr)) so')
      (rs'', _ ) -> error ("h: not dep but heads moved from below: " ++ show rs'')
  h hiDep hiStrong rs (S so so') =
    let (rs',so'') = h hiDep hiStrong rs so in (rs', S so'' so')

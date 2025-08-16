module MgTypes where

type Morph = [String]                                     -- E.g. initially ["√cat"], then phon specified ["cat"]
type F = String                                           -- feature -- E.g. "N", "V", "φ:3p", "-φ:_", "-φ:_3p"
type Sel = ([F], [F])                                     -- (neg features -o pos features)
type Agr = [([[F]], F)]                                   -- list of (tier, feature) pairs
type Leaf = (Morph, Sel, Agr)                             -- basic syntactic objects
data SO = L Leaf | S SO SO | E deriving (Show, Eq, Ord)   -- syntactic objects (E is for SO deleted by lin)
type Mover = (SO, [F], Agr)                               -- (movingSO, positive sel features, agr features)
type Label = (Sel, Agr, [Mover])                          -- label computed by sel

fstOf3 (x,_,_) = x
sndOf3 (_,x,_) = x
thrOf3 (_,_,x) = x

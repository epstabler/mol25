module MgTypes where

type Morph = [String]                                   -- E.g. initially ["√cat"], then phon specified ["cat"]
type F = String                                         -- Features. E.g. "N", "V", or (for Agr) "φ:_", "φ:3p", "κ:nom", "φ:_3p"
type Sel = ([F], [F])                                   -- (negative features, positive features)
type Goal = F                                           -- Agr Goal is a typed feature: "φ:3p", "φ:_", "φ:_3p"
type Tier = [[F]]                                       -- Tier is list of feature lists, e.g.: [["D"],["N"]], [["D","κ:nom"],["C"]]
type Probe = (Tier,F)                                   -- Probe (tier, typed feature) pair
type Agr = ([Probe],[Goal])                             -- Agr features are a list of probes and list of goals
type Leaf = (Morph, Sel, Agr)                           -- basic syntactic objects
data SO = L Leaf | S SO SO | E deriving (Show, Eq, Ord) -- hierarchical syntactic objects (E is for SO deleted by lin)
type Mover = (SO, [F], Agr)                             -- (movingSO, positive sel features, agr features)
type Label = (Sel, Agr, [Mover])                        -- label computed by sel

typeVal :: F -> (F,F)  -- map agr feature to (type,value)
typeVal f = typeVal' ("",f) where typeVal' (t,f) = case f of {':':cs -> (t,cs); c:cs -> typeVal' (t++[c],cs); _ -> (t,"")}

fstOf3 (x,_,_) = x
sndOf3 (_,x,_) = x

subList a b = foldr (\x sofar -> (x `elem` b) && sofar) True a

joinStr separator = foldr (\x y -> if null y then x else x ++ separator ++ y) ""

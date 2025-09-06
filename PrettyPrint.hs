module PrettyPrint where
import MgTypes (F,SO(L,S,E),Sel,Agr,Mover,Label,Leaf)
-- import Vi (joinstr)
{- joinstr is used by ph and also in PrettyPrint.hs -}
joinstr :: String -> [String] -> String  -- join strings, separated by sep
joinstr sep = foldr (\x y -> if null y then x else x ++ sep ++ y) ""

{- PRINT UTILITIES -}
leaf2str :: Leaf -> String
--leaf2str (morph,lbl) = "(" ++ joinstr " " morph ++ "," ++ sel2str lbl ++ ")"
leaf2str (morph,lbl,agr) = joinstr " " morph ++ ":" ++ sel2str lbl ++ agrfs2str agr

agrfs2str :: Agr -> String
agrfs2str ([],[]) = ""
agrfs2str (p,g) = " " ++ joinstr "," (map pair2str p ++ g) where
  pair2str (tier,f) = "{" ++ joinstr "," (map (joinstr ".") tier) ++ "}:" ++ f

sel2str :: Sel -> String
sel2str (n,p) = case (n, p) of
  ([],[]) -> "[],[]"
  ([],_) -> joinstr "." p
  _ -> joinstr "." n ++ " -o " ++ joinstr "." p

so2str :: SO -> String
so2str (L leaf) = leaf2str leaf
so2str (S x y) = "{ " ++ so2str x ++ "," ++ so2str y ++ " }"

tab2str :: Int -> String
tab2str n = case n of { 0 -> "" ; _ -> " " ++ tab2str (n-1)}

so2pretty :: SO -> String           -- like so2str, but prettified with newlines and indent
so2pretty =  so2pretty' 0 where
  so2pretty' _ E = ""
  so2pretty' _ (L leaf) = leaf2str leaf
  so2pretty' i (S x y) = "{ " ++ so2pretty' (i+2) x ++ ",\n" ++ tab2str (i+1) ++ so2pretty' (i+2) y ++ " }"

lbl2str :: Label -> String
lbl2str (sel, ([],[]), movers) = case movers of {[] -> sel2str sel; _ -> sel2str sel ++ "," ++ movers2str movers}
lbl2str (sel, agr, movers) = case movers of {[] -> sel2str sel; _ -> sel2str sel ++ "," ++  agrfs2str agr ++ "," ++ movers2str movers}

mover2str :: Mover -> String
mover2str (so,pos,([],[])) = "(" ++ so2str so ++ "," ++ joinstr "." pos ++ ")"
mover2str (so,pos,agrfs) = "(" ++ so2str so ++ "," ++ joinstr "." pos ++ "," ++  agrfs2str agrfs ++ ")"

movers2str :: [Mover] -> String
movers2str os = joinstr "," (map mover2str os)


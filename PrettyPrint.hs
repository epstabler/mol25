module PrettyPrint where
import MgTypes (F,SO(L,S,E),Sel,Agr,Mover,Label,Leaf,joinStr)

leaf2str :: Leaf -> String
leaf2str (morph,lbl,agr) = joinStr " " morph ++ ":" ++ sel2str lbl ++ agrfs2str agr

agrfs2str :: Agr -> String
agrfs2str ([],[]) = ""
agrfs2str (p,g) = " " ++ joinStr "," (map pair2str p ++ g) where
  pair2str (tier,f) = "{" ++ joinStr "," (map (joinStr ".") tier) ++ "}:" ++ f

sel2str :: Sel -> String
sel2str (n,p) = case (n, p) of
  ([],[]) -> "[],[]"
  ([],_) -> joinStr "." p
  _ -> joinStr "." n ++ " -o " ++ joinStr "." p

so2str :: SO -> String
so2str (L leaf) = leaf2str leaf
so2str (S x y) = "{ " ++ so2str x ++ "," ++ so2str y ++ " }"

tab2str :: Int -> String
tab2str n = case n of
  0 -> ""
  _ -> " " ++ tab2str (n-1)

so2pretty :: SO -> String           -- like so2str, but prettified with newlines and indent
so2pretty =  so2pretty' 0 where
  so2pretty' _ E = ""
  so2pretty' _ (L leaf) = leaf2str leaf
  so2pretty' i (S x y) = "{ " ++ so2pretty' (i+2) x ++ ",\n" ++ tab2str (i+1) ++ so2pretty' (i+2) y ++ " }"

lbl2str :: Label -> String
lbl2str (sel, agr, movers) = case (movers,agr) of
  ([],_) -> sel2str sel
  (_,([],[])) -> sel2str sel ++ "," ++ movers2str movers
  _ -> sel2str sel ++ "," ++  agrfs2str agr ++ "," ++ movers2str movers

mover2str :: Mover -> String
mover2str (so,pos,([],[])) = "(" ++ so2str so ++ "," ++ joinStr "." pos ++ ")"
mover2str (so,pos,agrfs) = "(" ++ so2str so ++ "," ++ joinStr "." pos ++ "," ++  agrfs2str agrfs ++ ")"

movers2str :: [Mover] -> String
movers2str os = joinStr "," (map mover2str os)

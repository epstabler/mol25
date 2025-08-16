module Mg where
import MgTypes (SO)
import Mrg (mrg)
import Sel (sel)
import Agr (agr)
import Hm (hm)
import Lin (lin)
import Vi (vi)

g :: SO -> SO
g = vi . lin . hm . agr . sel . mrg               -- g is the composition of 6 functions

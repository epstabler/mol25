module Mrg where
import MgTypes (SO)

mrg :: SO -> SO
mrg = id         -- Haskell *already enforces* the binary SO structure of MgTypes.hs, so this suffices

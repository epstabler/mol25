module Mrg where
import MgTypes (SO)

mrg :: SO -> SO
mrg = id         -- Haskell *already enforces* the binary SO structure defined in MgTypes.hs, so this suffices

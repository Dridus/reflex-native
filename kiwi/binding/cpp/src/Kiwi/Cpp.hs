-- |Convenience module re-exporting both the "Kiwi.Dsl" and "Kiwi.DslBinding" modules.
--
-- See "Kiwi.Dsl" for an introduction to using the higher level DSL to represent constraints.
--
-- See "Kiwi.DslBinding" for functions which use the underlying Kiwi library via the raw FFI to solve constraints.
--
-- See "Kiwi.Raw.Constraint", "Kiwi.Raw.Errors", "Kiwi.Raw.Expression", "Kiwi.Raw.Solver", "Kiwi.Raw.Strength", "Kiwi.Raw.Term", "Kiwi.Raw.Types", and
-- "Kiwi.Raw.Variable" for the raw FFI bindings to the Kiwi C++ library.
--
-- For example (from the Cassowary technical report):
--
-- @
--     import Kiwi
--
--     main :: IO ()
--     main = 'withNewSolver' $ do
--       xl <- 'variable' "xl"
--       xm <- 'editVariable' strong "xm"
--       xr <- 'variable' "xr"
--       'constrain' 'required' $ 'varT' xm '*.' 2 '==@' 'varT' xl '+:' 'varT' xr
--       'constrain' 'required' $ 'varT' xl '+:' 10 '<=@' 'varT' xr
--       'constrain' 'required' $ 'varT' xr '<=@' 100
--       'constrain' 'required' $ 0 '<=@' 'varT' xl
--       'updateVariables'
--       liftIO . print =<< 'getValue' xl
--       liftIO . print =<< 'getValue' xm
--       liftIO . print =<< 'getValue' xr
-- @
module Kiwi.Cpp (module Export) where

import Kiwi.Dsl as Export
import Kiwi.Cpp.DslBinding as Export


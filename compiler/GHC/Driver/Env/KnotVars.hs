{-# LANGUAGE DeriveFunctor #-}
module GHC.Driver.Env.KnotVars( KnotVars(..)
                              , emptyKnotVars
                              , knotVarsFromModuleEnv
                              , knotVarElems
                              , lookupKnotVars
                              , knotVarsWithout
                              ) where

import GHC.Prelude
import GHC.Unit.Types ( Module )
import GHC.Unit.Module.Env
import Data.Maybe

data KnotVars a = KnotVars { kv_domain :: [Module]
                           , kv_lookup :: Module -> Maybe a }
                           deriving Functor

emptyKnotVars :: KnotVars a
emptyKnotVars = KnotVars [] (const Nothing)

knotVarsFromModuleEnv :: ModuleEnv a -> KnotVars a
knotVarsFromModuleEnv me = KnotVars (moduleEnvKeys me) (lookupModuleEnv me)

knotVarElems :: KnotVars a -> [a]
knotVarElems (KnotVars keys lookup) = mapMaybe lookup keys

lookupKnotVars :: KnotVars a -> Module -> Maybe a
lookupKnotVars (KnotVars _ lookup) = lookup

knotVarsWithout :: Module -> KnotVars a -> KnotVars a
knotVarsWithout this_mod (KnotVars loop_mods lkup) = KnotVars
  (filter (/= this_mod) loop_mods)
  (\that_mod -> if that_mod == this_mod then Nothing else lkup that_mod)


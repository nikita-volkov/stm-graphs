module STMGraph.DynamicStore where

import STMGraph.Prelude
import qualified ListT
import qualified GHC.Exts
import qualified STMContainers.Multimap as Multimap
import qualified STMContainers.Map as Map
import qualified Focus


newtype DynamicStore =
  DynamicStore (Map.Map TypeRep GHC.Exts.Any)

new :: STM DynamicStore
new =
  DynamicStore <$> Map.new


-- * Key
-------------------------

-- |
-- A simplified type identifier, 
-- which is expected to be unique in the context of the application
-- of the DynamicStore.
-- 
-- Useful in cases such as the following:
-- 
-- > type Key (Map (Edge a) (Node a)) = a
-- 
-- Such simplification allows to reduce the constraints on the stored type.
type family Key a


-- * On
-------------------------

type On =
  ReaderT DynamicStore STM

on :: DynamicStore -> On a -> STM a
on = flip runReaderT

focus :: forall a r. Typeable (Key a) => Focus.StrategyM STM a r -> On r
focus s =
  ReaderT $ \(DynamicStore m) ->
    let 
      tr = typeOf (undefined :: (Key a))
      in
        Map.focus (unsafeCoerce s) tr m



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


-- * On
-------------------------

type On =
  ReaderT DynamicStore STM

on :: DynamicStore -> On a -> STM a
on = flip runReaderT

lookup :: forall a. Typeable a => On (Maybe a) 
lookup =
  focus Focus.lookupM

focus :: forall a r. Typeable a => Focus.StrategyM STM a r -> On r
focus s =
  ReaderT $ \(DynamicStore m) ->
    let 
      tr = typeOf (undefined :: a)
      in
        Map.focus (unsafeCoerce s) tr m



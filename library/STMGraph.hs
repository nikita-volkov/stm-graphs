-- |
-- A heterogenous directed mutable graph in STM.
module STMGraph
where

import STMGraph.Prelude
import qualified ListT
import qualified GHC.Exts
import qualified STMContainers.Multimap as Multimap
import qualified STMContainers.Set as Set
import qualified STMContainers.Map as Map
import qualified STMGraph.DynamicStore as DynamicStore
import qualified Focus


data Node a =
  Node {
    unique :: !Unique,
    value :: !(TVar a),
    targets :: !DynamicStore.DynamicStore,
    sources :: !DynamicStore.DynamicStore
  }

instance Eq (Node a) where
  a == b = unique a == unique b

instance Hashable (Node a) where
  hashWithSalt s n = 
    combine s (hashUnique (unique n))
    where
      combine h1 h2 = (h1 * 16777619) `xor` h2
  hash n = 
    hashUnique (unique n)

type instance DynamicStore.Key (Set.Set (Node a)) = a
type instance DynamicStore.Key (Multimap.Multimap (Edge a) (Node a)) = a


-- |
-- An edge to a node with a value @a@.
data family Edge a


new :: a -> STM (Node a)
new a =
  Node <$> newUniqueSTM <*> newTVar a <*> DynamicStore.new <*> DynamicStore.new
  where
    newUniqueSTM = pure $ unsafePerformIO newUnique


-- * On
-------------------------

-- |
-- A computation in a context of a node.
type On a m r =
  ReaderT (Node a) m r

on :: Node a -> On a m r -> m r
on node reader =
  runReaderT reader node

remove :: On a STM ()
remove =
  undefined

get :: On a STM a
get =
  ReaderT $ \n -> readTVar (value n)

set :: a -> On a STM ()
set a =
  ReaderT $ \n -> writeTVar (value n) a

addTarget :: (Typeable a, Multimap.Key (Edge a)) => Edge a -> Node a -> On a STM ()
addTarget edge target =
  ReaderT $ \source -> do
    DynamicStore.on (sources target) $ DynamicStore.focus $ \case
      Nothing -> 
        (,) <$> pure () <*> (Focus.Replace <$> Set.new)
      Just s -> 
        (Set.insert source s) *>
        (pure ((,) () Focus.Keep))
    DynamicStore.on (targets source) $ DynamicStore.focus $ \case
      Nothing ->
        (,) <$> pure () <*> (Focus.Replace <$> Multimap.new)
      Just m ->
        (Multimap.insert target edge m) *>
        (pure ((,) () Focus.Keep))

removeTarget :: Edge a -> On a STM Bool
removeTarget edge =
  ReaderT $ \source -> do
    undefined

streamTargets :: Edge b -> On a (ListT.ListT STM) (Node b)
streamTargets e =
  undefined


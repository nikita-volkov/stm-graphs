module STMGraphs.Heterogenous where

import STMGraphs.Prelude
import qualified ListT
import qualified STMContainers.Multimap as Multimap
import qualified STMContainers.Set as Set
import qualified STMContainers.Map as Map
import qualified Focus
import qualified STMGraphs.Homogenous as H
import qualified STMGraphs.Heterogenous.DynKey as DynKey


-- |
-- A heterogenous directed mutable graph in STM.
newtype Node a =
  Node { unpack :: H.Node DynKey.DynKey Any }
  deriving (Eq, Hashable)

new :: a -> STM (Node a)
new a =
  Node <$> H.new (unsafeCoerce a)


-- |
-- An edge to a node with a value @a@.
data family Edge a

deriving instance Typeable Edge


-- * On
-------------------------

-- |
-- A computation in a context of a node.
type On a m r =
  ReaderT (Node a) m r

on :: Node a -> On a m r -> m r
on node reader =
  runReaderT reader node

get :: On a STM a
get =
  withReaderT unpack $ 
  fmap unsafeCoerce $ H.get

set :: a -> On a STM ()
set a =
  withReaderT unpack $ 
  H.set $ unsafeCoerce a

addEdge :: (Multimap.Key (Edge b), Typeable b) => Node b -> Edge b -> On a STM ()
addEdge target edge =
  withReaderT unpack $
  H.addEdge (unsafeCoerce target) (DynKey.dynKey edge)

removeEdge :: (Multimap.Key (Edge b), Typeable b) => Node b -> Edge b -> On a STM ()
removeEdge target edge =
  withReaderT unpack $ 
  H.removeEdge (unsafeCoerce target) (DynKey.dynKey edge)

remove :: On a STM ()
remove =
  withReaderT unpack $ 
  H.remove

streamTargets :: (Multimap.Key (Edge b), Typeable b) => Edge b -> On a (ListT.ListT STM) (Node b)
streamTargets e =
  withReaderT unpack $
  fmap unsafeCoerce $
  H.streamTargets (DynKey.dynKey e)


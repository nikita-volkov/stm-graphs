module STMGraphs.Homogenous where

import STMGraphs.Prelude
import qualified ListT
import qualified GHC.Exts
import qualified STMContainers.Multimap as Multimap
import qualified STMContainers.Set as Set
import qualified STMContainers.Map as Map
import qualified Focus


-- |
-- A homogenous directed mutable graph in STM.
data Node e v =
  Node {
    unique :: !Unique,
    value :: !(TVar v),
    targets :: !(Multimap.Multimap e (Node e v)),
    sources :: !(Multimap.Multimap (Node e v) e)
  }

instance Eq (Node e v) where
  a == b = 
    unique a == unique b

instance Hashable (Node e v) where
  hashWithSalt s n = 
    combine s (hashUnique (unique n))
    where
      combine h1 h2 = (h1 * 16777619) `xor` h2
  hash n = 
    hashUnique (unique n)


new :: v -> STM (Node e v)
new a =
  Node <$> newUniqueSTM <*> newTVar a <*> Multimap.new <*> Multimap.new
  where
    newUniqueSTM = pure $ unsafePerformIO newUnique


-- * On
-------------------------

-- |
-- A computation in a context of a node.
type On e v m r =
  ReaderT (Node e v) m r

on :: Node e v -> On e v m r -> m r
on node reader =
  runReaderT reader node

get :: On e v STM v
get =
  ReaderT $ \n -> readTVar (value n)

set :: v -> On e v STM ()
set a =
  ReaderT $ \n -> writeTVar (value n) a

addEdge :: (Multimap.Key e) => Node e v -> e -> On e v STM ()
addEdge target edge =
  ReaderT $ \source -> do
    Multimap.insert target edge (targets source)
    Multimap.insert edge source (sources target)

removeEdge :: (Multimap.Key e) => Node e v -> e -> On e v STM ()
removeEdge target edge =
  ReaderT $ \source -> do
    Multimap.delete target edge (targets source)
    Multimap.delete edge source (sources target)

remove :: (Multimap.Key e) => On e v STM ()
remove =
  ReaderT $ \target -> do
    flip ListT.traverse_ (Multimap.stream (sources target)) $ \(source, edge) ->
      on source $ removeEdge target edge

streamTargets :: (Multimap.Key e) => e -> On e v (ListT.ListT STM) (Node e v)
streamTargets e =
  ReaderT $ \source ->
    Multimap.streamByKey e (targets source)


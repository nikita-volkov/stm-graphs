-- |
-- A heterogenous directed mutable graph in STM.
module STMGraph
where

import STMGraph.Prelude
import qualified ListT
import qualified GHC.Exts
import qualified STMContainers.Multimap as Multimap


data Node a =
  Node {
    unique :: !Unique,
    value :: !(TVar a),
    targets :: !(Multimap.Multimap GHC.Exts.Any GHC.Exts.Any)
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


-- |
-- An edge to a node with a value @a@.
data family Edge a


new :: a -> STM (Node a)
new a =
  Node <$> newUniqueSTM <*> newTVar a <*> Multimap.new
  where
    newUniqueSTM = pure $ unsafePerformIO newUnique


-- * On
-------------------------

-- |
-- A computation in a context of a node.
type On m r =
  forall a. ReaderT (Node a) m r

on :: Node a -> On m r -> m r
on node reader =
  runReaderT reader node

remove :: On STM ()
remove =
  undefined

get :: On STM a
get =
  undefined

set :: a -> On STM ()
set =
  undefined

addTarget :: Edge a -> Node a -> On STM ()
addTarget =
  undefined

removeTarget :: Edge a -> On STM Bool
removeTarget =
  undefined

streamTargets :: Edge b -> On (ListT.ListT STM) (Node b)
streamTargets e =
  undefined


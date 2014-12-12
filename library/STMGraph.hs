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
    targets :: Multimap.Multimap GHC.Exts.Any GHC.Exts.Any
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

remove :: Node a -> STM ()
remove =
  undefined

add :: Edge a -> Node a -> Node b -> STM ()
add =
  undefined

list :: Edge b -> Node a -> ListT.ListT STM (Node b)
list e n =
  undefined

get :: Node a -> STM a
get =
  undefined

set :: a -> Node a -> STM ()
set =
  undefined


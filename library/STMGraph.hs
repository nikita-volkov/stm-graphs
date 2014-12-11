-- |
-- A heterogenous directed mutable graph in STM.
module STMGraph
(
  Node(..),
  new,
  remove,
  addTarget,
  getTargetsByType,
  getValue,
  setValue,
)
where

import STMGraph.Prelude
import qualified ListT


data Node a =
  Node {
    unique :: !Unique
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

new :: a -> STM (Node a)
new =
  undefined

remove :: Node a -> STM ()
remove =
  undefined

addTarget :: Node a -> Node b -> STM ()
addTarget =
  undefined

getTargetsByType :: Node a -> ListT.ListT STM (Node b)
getTargetsByType =
  undefined

getValue :: Node a -> STM a
getValue =
  undefined

setValue :: a -> Node a -> STM ()
setValue =
  undefined


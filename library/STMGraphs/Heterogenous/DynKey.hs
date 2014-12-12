module STMGraphs.Heterogenous.DynKey where

import STMGraphs.Prelude


-- |
-- A dynamic value usable as a key in a hash map.
data DynKey =
  forall a. (Eq a) => DynKey !Int !TypeRep !a

instance Hashable DynKey where
  hashWithSalt s = hashWithSalt s . hash
  hash (DynKey h _ _) = h

instance Eq DynKey where
  DynKey _ t1 v1 == DynKey _ t2 v2 =
    t1 == t2 && unsafeCoerce v1 == v2

dynKey :: (Typeable a, Hashable a, Eq a) => a -> DynKey
dynKey a =
  DynKey (hash a) (typeOf a) a

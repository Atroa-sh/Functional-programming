module Queue
  ( Queue
  , emptyQ   -- :: Queue a
  , isEmptyQ -- :: Queue a -> Bool
  , addQ     -- :: a -> Queue a -> Queue a
  , remQ     -- :: Queue a -> (a, Queue a)
  )where

emptyQ :: Queue a
isEmptyQ :: Queue a -> Bool
addQ :: a -> Queue a -> Queue a
remQ :: Queue a -> (a, Queue a)


newtype Queue a = MkQueue [a] deriving Show

emptyQ = MkQueue []
isEmptyQ (MkQueue s) = null s
addQ x (MkQueue s) = MkQueue (x:s)
remQ (MkQueue s) = (last s, MkQueue (take (length s-1) s))
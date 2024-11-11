module State where

newtype ST st a = S (st -> (a, st))

app :: ST st a -> st -> (a,st)
app (S st) x = st x

instance Functor (ST st) where
   -- fmap :: (a -> b) -> ST a -> ST b
   fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))

instance Applicative (ST st) where
   -- pure :: a -> ST a
   pure x = S (\s -> (x,s))

   -- (<*>) :: ST (a -> b) -> ST a -> ST b
   stf <*> stx = S (\s ->
      let (f,s')  = app stf s
          (x,s'') = app stx s' in (f x, s''))

instance Monad (ST st) where
   -- return :: a -> ST a
   return = pure

   -- (>>=) :: ST a -> (a -> ST b) -> ST b
   st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')

stUpdate :: st -> ST st ()
stUpdate st1 = S (\st0 -> ((),st1))

stState :: ST st st
stState = S (\st -> (st,st))

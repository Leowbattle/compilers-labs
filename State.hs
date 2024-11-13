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



newtype StateIO st a = ST (st -> IO (a,st))

appT :: StateIO st a -> st -> IO (a,st)
appT (ST st) x = st x

instance Functor (StateIO st) where
   -- fmap :: (a -> b) -> ST a -> ST b
   -- fmap g st = ST (\s -> let (x,s') = appT st s in pure (g x, s'))
   fmap g st = ST (\s -> do (x,s') <- appT st s
                            return (g x, s'))

instance Applicative (StateIO st) where
   -- pure :: a -> ST a
   pure x = ST (\s -> pure (x, s))

   -- (<*>) :: ST (a -> b) -> ST a -> ST b
   stf <*> stx = ST (\s -> do (f, s') <- appT stf s
                              (x, s'') <- appT stx s'
                              return (f x, s''))

instance Monad (StateIO st) where
   -- return :: a -> ST a
   return = pure

   -- (>>=) :: ST a -> (a -> ST b) -> ST b
   st >>= f = ST (\s -> do (x,s') <- appT st s
                           appT (f x) s')

stUpdateT :: st -> StateIO st ()
stUpdateT st1 = ST (\st0 -> return ((),st1))

stStateT :: StateIO st st
stStateT = ST (\st -> return (st,st))

lift :: IO a -> StateIO st a
lift mx = ST (\s -> do x <- mx
                       return (x,s))

-- Counter
data Counter a = Counter {
  value :: a,
  step :: a
} deriving Show
newtype CounterOp a b = CounterOp {
  run :: Counter a -> (b, Counter a)
}

next :: Num a => Counter a -> (a, Counter a)
next (Counter v s) = (s, Counter v' s)
  where
    v' = v + s

-- モナドモドキ
class MyMonad m where
  bind :: m b -> (b -> m c) -> m c
  bind_ :: m b -> m c -> m c
  unit :: b -> m b

instance MyMonad (CounterOp a) where
  method `bind` lambda = CounterOp $ \counter ->
    let
      (x, c) = run method $ counter
      (x', c') = run (lambda x) $ c
    in
      (x', c')

  method `bind_` inLambda = method `bind` \_ -> inLambda

  unit x = CounterOp $ \c -> (x, c)

main = do
  let counter = Counter 0 1
  print
    $ run ((CounterOp next)
    `bind_` (CounterOp next)
    `bind_` (CounterOp next))
    $ counter

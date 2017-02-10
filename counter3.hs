-- State
newtype MyState s a = MyState {
  run :: s -> (a, s)
}

-- モナドモドキ
class MyMonad m where
  bind :: m a -> (a -> m b) -> m b
  bind_ :: m a -> m b -> m b
  unit :: a -> m a

-- Stateモナド
instance MyMonad (MyState s) where
  method `bind` lambda = MyState $ \state ->
    let
      (a, s) = run method $ state
      (a', s') = run (lambda a) $ s
    in
      (a', s')

  method `bind_` inLambda = method `bind` \_ -> inLambda

  unit a = MyState $ \s -> (a, s)

get :: MyState s s
get = MyState $ \s -> (s, s)

put :: s -> MyState s ()
put s = MyState $ \_ -> ((), s)

-- Counter
data Counter a = Counter {
  value :: a,
  step :: a
} deriving Show

type CounterOp a b = MyState (Counter a) b

next :: Num a => CounterOp a a
next = get
  `bind` (\(Counter v s) -> (put (Counter (v + s) s))
  `bind_` (unit s))

main = do
  let counter = Counter 0 1
  print
    $ run (next
    `bind_` next
    `bind_` next)
    $ counter

import Control.Monad.State

-- Counter
data Counter a = Counter {
  value :: a,
  step :: a
} deriving Show

type CounterOp a b = State (Counter a) b

next :: Num a => CounterOp a a
next = get
  >>= (\(Counter v s) -> (put (Counter (v + s) s))
  >> (return s))

main = do
  let counter = Counter 0 1
  print
    $ runState (next
    >> next
    >> next)
    $ counter

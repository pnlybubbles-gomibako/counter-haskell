import Control.Monad.State

-- Stack
newtype Stack a = Stack [a] deriving Show

type StackOp a b = State (Stack a) b

-- Stackの先頭に要素を追加
-- (追加する要素) -> (初期Stack) -> (先端の要素, 更新されたStack)
push :: a -> StackOp a a
push c = do
  (Stack cs) <- get
  put (Stack (c:cs))
  return c

-- Stackの専用の要素を取り出す
-- (初期Stack) -> (取り出された要素, 更新されたStack)
pop :: StackOp a a
pop = do
  (Stack (c:cs)) <- get
  put (Stack cs)
  return c

-- popした要素を調べる
headIs :: (a -> Bool) -> StackOp a Bool
headIs checker = do
  i <- pop
  return (checker i)

-- 空にする
empty :: StackOp a ()
empty = do
  put (Stack [])
  return ()

main = do
  let stack = Stack [1, 2, 3, 4, 5]
  print
    $ (`runState` stack)
    $ do
      i1 <- headIs (> 4)
      i2 <- headIs (> 3)
      i3 <- headIs (> 2)
      return (and [i1, i2, i3])
  print
    $ (`runState` stack)
    $ do
      i1 <- pop
      i2 <- pop
      return (i1 + i2)
  print
    $ (`runState` stack)
    $ do
      push 10
      push 12
      i <- pop
      return i
  print
    $ (`runState` stack)
    $ do
      i <- pop
      push 10
      push 12
      return i
  print
    $ (`runState` stack)
    $ do
      i1 <- pop
      i2 <- pop
      push 10
      push 12
      return (i1 + i2)
  print
    $ (`runState` stack)
    $ do
      push 10
      empty
      push 11
      push 12

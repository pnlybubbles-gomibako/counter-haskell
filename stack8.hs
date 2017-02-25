import Control.Monad.State

-- Stack
newtype Stack a = Stack [a] deriving Show

type StackOp a b = State (Stack a) b

-- Stackの先頭に要素を追加
-- (追加する要素) -> (初期Stack) -> (先端の要素, 更新されたStack)
push :: a -> StackOp a a
push c =
  get
  >>= \(Stack cs) -> put (Stack (c:cs))
  >> return c

-- Stackの専用の要素を取り出す
-- (初期Stack) -> (取り出された要素, 更新されたStack)
pop :: StackOp a a
pop =
  get
  >>= \(Stack (c:cs)) -> put (Stack cs)
  >> return c

-- popした要素を調べる
headIs :: (a -> Bool) -> StackOp a Bool
headIs checker =
  pop
  >>= \i -> return (checker i)

-- 空にする
empty :: StackOp a ()
empty =
  put (Stack [])
  >> return ()

main = do
  let stack = Stack [1, 2, 3, 4, 5]
  print
    $ (`runState` stack)
    $ headIs (> 4)
      >>= \i1 -> headIs (> 3)
      >>= \i2 -> headIs (> 2)
      >>= \i3 -> return (and [i1, i2, i3])
  print
    $ (`runState` stack)
    $ pop
      >>= \i1 -> pop
      >>= \i2 -> return (i1 + i2)
  print
    $ (`runState` stack)
    $ push 10
      >> push 12
      >> pop
      >>= \i -> return i
  print
    $ (`runState` stack)
    $ pop
      >>= \i -> push 10
      >> push 12
      >> return i
  print
    $ (`runState` stack)
    $ pop
      >>= \i1 -> pop
      >>= \i2 -> push 10
      >> push 12
      >> return (i1 + i2)
  print
    $ (`runState` stack)
    $ push 10
      >> empty
      >> push 11
      >> push 12

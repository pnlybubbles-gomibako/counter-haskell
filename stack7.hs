-- Stack
newtype Stack a = Stack [a] deriving Show
newtype StackOp a b = StackOp {
  run :: Stack a -> (b, Stack a)
}

-- Stackの先頭に要素を追加
-- (追加する要素) -> (初期Stack) -> (先端の要素, 更新されたStack)
push :: a -> Stack a -> (a, Stack a)
push c (Stack cs) = (c, Stack (c:cs))

-- Stackの専用の要素を取り出す
-- (初期Stack) -> (取り出された要素, 更新されたStack)
pop :: Stack a -> (a, Stack a)
pop (Stack (c:cs)) = (c, Stack cs)

-- popした要素を調べる
headIs :: (a -> Bool) -> Stack a -> (Bool, Stack a)
headIs checker s = (i', s')
  where
    (i, s') = pop s
    i' = checker i

-- 空にする
empty :: Stack a -> ((), Stack a)
empty _ = (i, s)
  where
    i = ()
    s = Stack []

-- モナドモドキ
class MyMonad m where
  bind :: m b -> (b -> m c) -> m c
  bind_ :: m b -> m c -> m c
  unit :: b -> m b

-- a ... Stack
-- b ... 付属
instance MyMonad (StackOp a) where
  -- bind :: StackOp a b -> (b -> StackOp a c) -> StackOp a c
  method `bind` lambda = StackOp $ \stack ->
    let
      (i, s) = run method $ stack
      (i', s') = run (lambda i) $ s
    in
      (i', s')

  -- bind :: StackOp b a -> StackOp c a -> StackOp c a
  method `bind_` inLambda = method `bind` \_ -> inLambda

  -- unit :: b -> StackOp a b
  --   b =           \a  -> (b, a )
  unit i = StackOp $ \cs -> (i, cs)

main = do
  let stack = Stack [1, 2, 3, 4, 5]
  print
    $ run ((StackOp $ headIs (> 4))
      `bind` \i1 -> (StackOp $ headIs (> 3))
      `bind` \i2 -> (StackOp $ headIs (> 2))
      `bind` \i3 -> unit $ and [i1, i2, i3])
    $ stack
  print
    $ run (StackOp pop
      `bind` \i1 -> StackOp pop
      `bind` \i2 -> unit $ i1 + i2)
    $ stack
  print
    $ run ((StackOp $ push 10)
      `bind_` (StackOp $ push 12)
      `bind_` StackOp pop
      `bind` \i -> unit i)
    $ stack
  print
    $ run (StackOp pop
      `bind` \i -> (StackOp $ push 10)
      `bind_` (StackOp $ push 12)
      `bind_` unit i)
    $ stack
  print
    $ run (StackOp pop
      `bind` \i1 -> StackOp pop
      `bind` \i2 -> (StackOp $ push 10)
      `bind_` (StackOp $ push 12)
      `bind_` (unit $ i1 + i2))
    $ stack
  print
    $ run ((StackOp $ push 10)
      `bind_` StackOp empty
      `bind_` (StackOp $ push 11)
      `bind_` (StackOp $ push 12))
    $ stack

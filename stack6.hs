-- Stack
newtype Stack a = Stack [a] deriving Show
type StackOp b a = Stack a -> (b, Stack a)

-- Stackの先頭に要素を追加
-- (追加する要素) -> (初期Stack) -> (先端の要素, 更新されたStack)
push :: a -> StackOp a a
push c (Stack cs) = (c, Stack (c:cs))

-- Stackの専用の要素を取り出す
-- (初期Stack) -> (取り出された要素, 更新されたStack)
pop :: StackOp a a
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

-- なにもしない
unit :: b -> StackOp b a
unit i cs = (i, cs)

--      method  -> (lambda            ) -> (stack -> (i'     , s'   ))
bind :: StackOp b a -> (b -> StackOp c a) -> StackOp c a
bind method lambda = \stack ->
  let
    (i, s) = method stack
    (i', s') = lambda i s
  in
    (i', s')

-- 結果を必要としないbind (結果が必要ないので第2引数はlambda式の戻り値だけ)
bind_ :: StackOp b a -> StackOp c a -> StackOp c a
bind_ method inLambda = method `bind` \_ -> inLambda

main = do
  let stack = Stack [1, 2, 3, 4, 5]
  print
    $ (headIs (> 4)
      `bind` \i1 -> headIs (> 3)
      `bind` \i2 -> headIs (> 2)
      `bind` \i3 -> unit $ and [i1, i2, i3])
    $ stack
  print
    $ (pop
      `bind` \i1 -> pop
      `bind` \i2 -> unit $ i1 + i2)
    $ stack
  print
    $ (push 10
      `bind_` push 12
      `bind_` pop
      `bind` \i -> unit i)
    $ stack
  print
    $ (pop
      `bind` \i -> push 10
      `bind_` push 12
      `bind_` unit i)
    $ stack
  print
    $ (pop
      `bind` \i1 -> pop
      `bind` \i2 -> push 10
      `bind_` push 12
      `bind_` (unit $ i1 + i2))
    $ stack
  print
    $ (push 10
      `bind_` empty
      `bind_` push 11
      `bind_` push 12)
    $ stack

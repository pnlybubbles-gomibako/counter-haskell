-- Stack
type Stack = [Integer]
type StackOp = Stack -> (Integer, Stack)

-- Stackの先頭に要素を追加
-- (追加する要素) -> (初期Stack) -> (先端の要素, 更新されたStack)
push :: Integer -> StackOp
push c cs = (c, c:cs)

-- Stackの専用の要素を取り出す
-- (初期Stack) -> (取り出された要素, 更新されたStack)
pop :: StackOp
pop (c:cs) = (c, cs)

-- なにもしない
unit :: Integer -> StackOp
unit i cs = (i, cs)

-- 先頭から2つ取り出して2項演算を行い、なにもしない
--      (2項演算子                     ) -> StackOp
calc :: (Integer -> Integer -> Integer) -> StackOp
calc op = pop `bind` \i1 -> pop `bind` \i2 -> unit (op i1 i2)

--      method  -> (lambda            ) -> (stack -> (i'     , s'   ))
bind :: StackOp -> (Integer -> StackOp) -> StackOp
bind method lambda = \stack ->
  let
    (i, s) = method stack
    (i', s') = lambda i s
  in
    (i', s')

-- 結果を必要としないbind (結果が必要ないので第2引数はlambda式の戻り値だけ)
bind_ :: StackOp -> StackOp -> StackOp
bind_ method inLambda = method `bind` \_ -> inLambda

ml = calc (*)
ad = calc (+)
sb = calc (-)
dv = calc div

main = do
  let stack = [1, 2, 3, 4, 5]
  print $ ad stack

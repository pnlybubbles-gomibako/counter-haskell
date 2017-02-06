-- Stack
type Stack = [Integer]

-- Stackの先頭に要素を追加
-- (追加する要素) -> (初期Stack) -> (先端の要素, 更新されたStack)
push :: Integer -> Stack -> (Integer, Stack)
push c cs = (c, c:cs)

-- Stackの専用の要素を取り出す
-- (初期Stack) -> (取り出された要素, 更新されたStack)
pop :: Stack -> (Integer, Stack)
pop (c:cs) = (c, cs)

-- 先頭から2つ取り出して2項演算を行い、先頭に結果を追加する
--      (2項演算子                     ) -> Stack -> (Integer, Stack)
calc :: (Integer -> Integer -> Integer) -> Stack -> (Integer, Stack)
calc op = pop `bind` \i1 -> pop `bind` \i2 -> push (op i1 i2)

--      (method                   ) -> (lambda                                ) -> (stack -> (i'     , s'   ))
bind :: (Stack -> (Integer, Stack)) -> (Integer -> (Stack -> (Integer, Stack))) -> (Stack -> (Integer, Stack))
bind method lambda stack = (i', s')
  where
    (i, s) = method stack
    (i', s') = lambda i s

ml = calc (*)
ad = calc (+)
sb = calc (-)
dv = calc div

main = do
  let stack = [1, 2, 3, 4, 5]
  print $ ad stack

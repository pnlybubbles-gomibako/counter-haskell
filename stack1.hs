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

main = do
  let stack = [1, 2, 3, 4, 5]
  let (i1, stack1) = pop stack
  let (i2, stack2) = pop stack1
  let (i3, stack3) = push (i1 + i2) stack2
  print (i3, stack3)

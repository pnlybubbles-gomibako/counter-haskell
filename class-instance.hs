-- newtype Str = Str {
--   getStr :: String
-- } deriving (Show)

-- newtype Innnnt = Innnnt {
--   getInnnnt :: Integer
-- } deriving (Show)

-- -- data Str = Str String deriving (Show)
-- -- data Innnnt = Innnnt Integer deriving (Show)

-- newtype StrWith a = StrWith {
--   getStrWith :: (String, a)
-- } deriving (Show)

-- class Tatsumi m where
--   desc :: m -> (String, m)

-- instance Tatsumi Str where
--   desc x = ("hi string", x)

-- instance Tatsumi Innnnt where
--   desc x = ("hi integer", x)

-- instance Tatsumi (StrWith a) where
--   -- desc (StrWith x) = ("hi" ++ (fst $ x), StrWith x)
--   desc x = ("hi" ++ (fst $ getStrWith x), x)

-- newtype Lambda a b = Lambda {
--   getLambda :: (String, a) -> ((String, a), b)
-- }

-- class Add a where
--   plus :: a -> a -> a

-- instance Add Integer where
--   plus x y = x + y

-- instance Add Double where
--   plus x y = x + y

newtype IntWrap = IntWrap {
  unwrap :: Integer
} deriving (Show)

class Couple m where
  couple :: a -> (m, a)

instance Couple IntWrap where
  couple x = (IntWrap 1, x)

-- newtype Stack a = Stack [a] deriving (Show)

-- newtype StackOp a b = StackOp {
--   run :: Stack a -> (b, Stack a)
-- }

-- class MyMonad m where
--   ret :: a -> m a
--   comb :: m a -> (a -> m b) -> m b

-- instance MyMonad (StackOp a) where
--   ret x = StackOp $ \stack -> (x, stack)
--   m `comb` n = StackOp $ \stack0 ->
--    let (x1, stack1) = run m stack0
--        (x2, stack2) = run (n x1) stack1
--    in  (x2, stack2)

-- pop :: Stack a -> (a, Stack a)
-- pop (Stack (x:xs)) = (x, Stack xs)

main = do
  print $ unwrap $ fst $ couple undefined
  -- let f = (+1) $ fst $ (1, 2)
  -- let s = (+1) $ snd $ wrap 20
  -- ret = plus $ wrap 2
  -- print $ plus (1 :: Integer) 2
  -- print $ plus (1.1 :: Double) 2.0
  -- let sample2 = Innnnt 1
  -- let sample1 = Str "Tatsumi"
  -- let sample3 = StrWith ("TatsumiWith", 1)
  -- print $ sample1
  -- print $ desc sample1
  -- print $ sample2
  -- print $ desc sample2
  -- print $ sample3
  -- print $ desc sample3
  -- let sampleLambda = wrap 1
  -- print $ sampleLambda ("hiLambda", False)
  -- let s = Stack [1, 2, 3, 4, 5]
  -- print $ run (StackOp $ topis (> 4)) s
  -- print $ run (StackOp $ ret 1) s
  -- print $ run (StackOp pop `comb` \i -> ret (i * 2)) $ s

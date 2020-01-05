Monad-IO :: Monad IO
Monad-List :: Monad List
bind :: Monad m -> m a -> (a -> m b) -> m b
seq :: Monad m -> m a -> m b -> m b
seq m a b = bind m a (\_ -> b)
getLine :: IO String
putStrLn :: String -> IO ()
main = seq Monad-IO (bind Monad-IO getLine putStrLn) getLine
repeat :: a -> Int -> [a]
repeat _ 0 = []
repeat a n = a : repeat a (n - 1)
cons :: a -> [a] -> [a]
plus :: Int -> Int -> Int
inf :: Int -> [Int]
inf i = i : inf (i + 1)
test :: a -> [a]
test a = bind Monad-List inf (repeat a)

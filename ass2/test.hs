test1 :: IO a -> IO a
test1 a = do a' <- a
             return a'
    
import TexifyMatrix2

mat = from2dArr [[0,1,2]
                ,[2,3,4]
                ,[4,5,6]
                ,[6,7,8]
                ,[8,9,10]
                ,[10,11,12]]

mat2 = from2dArr [[2, 4, 6]
                 ,[8, 10, 12]
                 ,[14, 16, 18]]

mat3 = from2dArr [[1, 2, 3]
                 ,[4, 5, 6]
                 ,[7, 8, 9]]

testSlices :: Matrix -> IO ()
testSlices mat = do
    putStrLn $ "Original: " ++ (showTex mat)
    putStrLn $ "sliceR (1, 3): " ++ (showTex $ sliceR (1, 3) mat)
    putStrLn $ "sliceC (2, 4): " ++ (showTex $ sliceC (2, 4) mat)
    putStrLn $ "Composed slice: " ++ (showTex $ sliceR (1, 3) $ sliceC (2, 4) mat)
    putStrLn $ "Slice: " ++ (showTex $ slice (1, 3) (2, 4) mat)

testTranspose :: Matrix -> IO ()
testTranspose mat = do
    putStrLn $ "Original: " ++ (showTex mat)
    putStrLn $ "Transpose: " ++ (showTex $ transpose mat)

testMul :: Matrix -> Matrix -> IO ()
testMul m1 m2 = do
    putStrLn . show $ m1 * m2 

testId :: Matrix -> IO ()
testId m = do
    putStrLn . show $ identity 2
    putStrLn . show $ identity 5

testSwap :: Matrix -> IO ()
testSwap m = do
    putStrLn $ "Original: " ++ (showTex m)
    putStrLn $ "swap 1 3" ++ (showTex $ swap 1 3 m)
    putStrLn $ "swap 2 4" ++ (showTex $ swap 2 4 m)

testStacks :: Matrix -> Matrix -> IO ()
testStacks m1 m2 = do
    putStrLn $ "hstack: " ++ (showTex $ hstack m1 m2)
    putStrLn $ "vstack: " ++ (showTex $ vstack m1 m2)

main = do
    testStacks mat2 mat3
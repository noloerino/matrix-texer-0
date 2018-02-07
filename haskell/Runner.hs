import TexifyMatrix2
import Data.Ratio

arr = from2dArr $ [[1,0,0,0,0]
                  ,[0,0,0,0,1]
                  ,[-10,0,0,1,-1]
                  ,[0,0,1,-1,0]
                  ,[10,-5,0,1,0]]

-- Write out a 2d array of all functions here, used as they are defined in TexifyMatrix2
rc = rowCombo
s = swap
rs = rowScale
-- (All methods that manipulate rows are 1-indexed unless otherwise specified)
-- m :: Rational -> Matrix -> Matrix (multiplies a matrix by a scalar)
-- swap :: Int -> Int -> Matrix -> Matrix (swaps specified rows in a matrix)
-- rowCombo :: rowCombo :: Int -> (Rational, Int) -> Matrix -> Matrix (performs a linear combination of 2 rows)
-- transpose :: Matrix -> Matrix
-- identity :: Int -> Matrix
funcs = [[rs 1 (1%2), rc 2 (-1, 1)]
        ,[rs 2 (2%3), rc 1 (-1%2, 2)]]

-- CHANGE THIS for augmented/non-augmented
display = display0

display0 :: ShowTex a => a -> IO ()
display0 = putStrLn . showTex
display1 = putStrLn . showAugmented
display2 = putStrLn . showAugHalf

runFuncs = do
    let matrices = foldl (\acc fs -> (foldl (\acc2 f -> f acc2) (head acc) fs) : acc) [arr] funcs
    mapM_ display (reverse matrices)

main = do
    -- runFuncs
    display arr

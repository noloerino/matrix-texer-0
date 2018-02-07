module TexifyMatrix2
    ( ShowTex
    , showTex
    , showAugmented
    , showAugHalf
    , Scalar
    , Vector
    , vectorSum
    , dot
    , mulElemWise
    , Matrix
    , identity
    , transpose
    , rowCount
    , swap
    , rowCombo
    , rowScale
    , sliceR
    , sliceC
    , slice
    , matMul
    , hstack
    , vstack
    , augWithId
    , from2dArr
    , to2dArr
    , tagNums
    , fracTagNum
    , mulSV
    , mulVS
    , mulSM
    , mulMS
    , m
    , getSwapper
    , getRowComboer
    , getRowScaler ) where

import qualified Data.List as List
import Data.Ratio

default (Rational) -- Literally all the functionality depends on things being rational (bad practice, I know)

class ShowTex a where
    showTex :: a -> String

data Scalar = Scalar Rational deriving (Eq)
instance Show Scalar where
    show (Scalar n)
        | denominator n == 1 = show $ numerator n
        | otherwise = (show $ numerator n) ++ "/" ++ (show $ denominator n)
instance ShowTex Scalar where
    showTex (Scalar n)
        | denominator n == 1 = show $ numerator n
        | otherwise = fracTagNum (numerator n) (denominator n)
instance Num Scalar where
    (Scalar a) + (Scalar b) = Scalar $ a + b
    (Scalar a) * (Scalar b) = Scalar $ a * b
    abs (Scalar a) = Scalar $ abs a
    signum (Scalar a) = Scalar $ signum a
    fromInteger a = Scalar (a % 1)
    negate (Scalar a) = Scalar $ negate a

type Vector = [Scalar]

showTexRow :: Vector -> String
showTexRow v = "\n\t" ++ (List.intercalate " & " (map showTex v)) ++ " \\\\"

vectorSum :: Vector -> Vector -> Vector
vectorSum v1 v2 = zipWith (+) v1 v2

dot :: Vector -> Vector -> Scalar
dot v1 v2 = sum $ zipWith (*) v1 v2

mulElemWise :: Vector -> Vector -> Vector
mulElemWise v1 v2 = zipWith (*) v1 v2

data Matrix = Matrix [Vector] deriving (Eq, Show)
instance ShowTex Matrix where
    showTex mat = "$$ \\begin{bmatrix}" ++ showMatElems mat ++ "\n\\end{bmatrix} $$\n"
instance Num Matrix where
    (Matrix a) + (Matrix b) = Matrix $ zipWith vectorSum a b
    a * b = matMul a b
    abs (Matrix a) = Matrix $ map (map abs) a
    signum (Matrix a) = Matrix $ map (map signum) a
    fromInteger a = Matrix [[Scalar (a % 1)]]
    negate (Matrix a) = Matrix $ map (map negate) a

showMatElems :: Matrix -> String
showMatElems (Matrix mat) = unwords (map showTexRow mat)

-- | Displays the matrix with a bar before the last column
showAugmented :: Matrix -> String
showAugmented (Matrix mat) = augmentor cs (showMatElems (Matrix mat))
    where cs = replicate rows 'c' ++ "|c"
          rows = pred . length $ head mat

-- | Displays the matrix with a bar down the middle
showAugHalf :: Matrix -> String
showAugHalf (Matrix mat) = augmentor (cs ++ "|" ++ cs) (showMatElems (Matrix mat))
    where cs = replicate (length (head mat) `quot` 2) 'c'

augmentor :: String -> String -> String
augmentor cs s = "$$ \\left[\\begin{array}{" ++ cs ++ "}" ++ s ++ "\n\\end{array}\\right] $$"

rowCount :: Matrix -> Int
rowCount (Matrix m) = length m

identity :: Int -> Matrix
identity n = Matrix $ map (\i -> (replicate i 0) ++ [Scalar 1] ++ (replicate (n - i - 1) 0)) [0..n - 1]

transpose :: Matrix -> Matrix
transpose (Matrix mat) = Matrix $ map (\j -> foldr (\i acc -> ((mat !! i) !! j) : acc) [] [0..rows - 1]) [0..cols - 1]
    where rows = length mat
          cols = length $ mat !! 0

-- | Swaps the specified rows of the matrix. 1-indexed.
swap :: Int -> Int -> Matrix -> Matrix
swap j k (Matrix mat)
    | b >= length mat || a < 0 = undefined -- oob
    | otherwise = (getSwapper j k (Matrix mat)) * (Matrix mat)
    where a = (min j k) - 1
          b = (max j k) - 1

getSwapper :: Int -> Int -> Matrix -> Matrix
getSwapper j k (Matrix mat) = Matrix $ (take a id) ++ rowb ++ (drop (a + 1) $ take b id) ++ rowa ++ (drop (b + 1) id)
    where a = (min j k) - 1
          b = (max j k) - 1
          rowb = [id !! b]
          rowa = [id !! a]
          (Matrix id) = identity $ length mat

-- | Adds a scalar multiple of the second row to another. 1-indexed.
rowCombo :: Int -> (Rational, Int) -> Matrix -> Matrix
rowCombo a (n, b) mat = (getRowComboer a (n, b) mat) * mat

getRowComboer :: Int -> (Rational, Int) -> Matrix -> Matrix
getRowComboer a (n, b) (Matrix mat) = Matrix $ (take (a - 1) id) ++ [newRow] ++ (drop a id)
    where (Matrix id) = identity $ length mat
          r1 = id !! (pred a)
          r2 = id !! (pred b)
          newRow = (take (b - 1) r1) ++ [Scalar n] ++ (drop b r1)

-- | Multiplies the specified row of a matrix by a given amount. 1-indexed.
rowScale :: Int -> Rational -> Matrix -> Matrix
rowScale a n mat = (getRowScaler a n mat) * mat

getRowScaler :: Int -> Rational -> Matrix -> Matrix
getRowScaler a n (Matrix mat) = Matrix $ take i id ++ [Scalar n `mulSV` (id !! i)] ++ drop (i + 1) id
    where (Matrix id) = identity $ length mat
          i = a - 1

-- | Slices the row from start index to end index - 1 (like Python's ":")
sliceR :: (Int, Int) -> Matrix -> Matrix
sliceR (start, end) (Matrix mat) = Matrix $ take (end - start) $ drop start mat

sliceC :: (Int, Int) -> Matrix -> Matrix
sliceC (start, end) (Matrix mat) = Matrix $ map (\x -> take (end - start) $ drop start x) mat

slice :: (Int, Int) -> (Int, Int) -> Matrix -> Matrix
slice (r1, r2) (c1, c2) mat = sliceR (r1, r2) $ sliceC (c1, c2) mat

-- | Multiplies two matrices together; will not fail gracefully if the dimensions are not appropriate
-- If somebody messed up and the # of rows in the first matrix is not the same as the number of cols in the second,
-- then returns undefined.
matMul :: Matrix -> Matrix -> Matrix
matMul (Matrix m1) (Matrix m2)
    | length (m1 !! 0) /= length m2 = undefined
    | otherwise = Matrix $ map (\j -> foldr (\i acc -> (m1 !! j) `dot` (m2' !! i) : acc) [] [0..width - 1]) [0..height - 1]
         where (Matrix m2') = transpose (Matrix m2)
               height = length m1
               width = length m2'

-- | Attaches the second matrix to the right of the first matrix.
hstack :: Matrix -> Matrix -> Matrix
hstack (Matrix m1) (Matrix m2) = Matrix $ zipWith (++) m1 m2

-- | Attaches the second matrix to the bottom of the first matrix.
vstack :: Matrix -> Matrix -> Matrix
vstack (Matrix m1) (Matrix m2) = Matrix $ m1 ++ m2

-- | Augments the matrix with the appropriate identity matrix.
augWithId :: Matrix -> Matrix
augWithId mat = hstack mat (identity $ rowCount mat)

from2dArr :: [[Rational]] -> Matrix
from2dArr arr = Matrix $ map (map Scalar) arr

to2dArr :: Matrix -> [[Scalar]]
to2dArr (Matrix mat) = mat

tagNums :: (Show a, Num a) => String -> [a] -> String
tagNums tag args = '\\' : tag ++ (concat $ map (\x -> "{" ++ show x ++ "}") args)

fracTagNum :: (Show a, Ord a, Num a) => a -> a -> String
fracTagNum a b
    | a >= 0 = tagNums "frac" [a, b]
    | otherwise = "-" ++ fracTagNum (-a) b

-- | Multiplies a vector by a scalar.
mulSV :: Scalar -> Vector -> Vector
mulSV sc v = map (*sc) v
mulVS :: Vector -> Scalar -> Vector
mulVS = flip mulSV

-- | Multiplies a matrix by a scalar.
mulSM :: Scalar -> Matrix -> Matrix
mulSM sc (Matrix mat) = Matrix $ map (mulSV sc) mat
mulMS :: Matrix -> Scalar -> Matrix
mulMS = flip mulSM
m :: Rational -> Matrix -> Matrix
a `m` b = mulSM (Scalar a) b

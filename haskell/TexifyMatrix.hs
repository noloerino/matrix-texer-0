-- module TexifyMatrix
-- ( ratMatToStr
-- , dMatToStr
-- , rowToStr
-- , applyTag
-- , fracTag
-- , sqrtTag
-- , addRow
-- , subRow
-- , mulMwS
-- , divMwS
-- , divSwS
-- , mulSwS
-- , TexString
-- , TexMatRow
-- , TexMatrix
-- ) where

import qualified Data.List as List
import Data.Ratio -- Creates Rational numbers with % operator

-- | The TexString class essentially acts as an alias for String, allowing show to work properly 
data TexString = TSString String | TSRat Rational | TSDouble Double  deriving (Eq)
instance Show TexString where
    show (TSString s) = s
    show (TSRat n)
        | denominator n == 1 = show $ numerator n
        | otherwise = show $ fracTag (show $ numerator n) (show $ denominator n)
    show (TSDouble n) = show n

-- | Takes a string and its arguments and converts it into a tag.
-- For example, `applyTag "frac" ["1", "2"]` returns "\frac{1}{2}".
applyTag :: String -> [String] -> TexString
applyTag tag args = TSString $ '\\' : tag ++ (concat $ map (\x -> "{" ++ x ++ "}") args)

fracTag :: String -> String -> TexString
fracTag s1 s2 = applyTag "frac" [s1, s2]

sqrtTag :: String -> TexString
sqrtTag n = applyTag "sqrt" [n]

-- DEFINING MATRIX OPERATIONS
-- | The RatMatRow type is the internal representation of all the numbers in the matrix.
type RatMatRow = [Rational]
type RatMatrix = [RatMatRow]

type DMatRow = [Double]
type DMatrix = [DMatRow]

addRow :: Fractional a => [a] -> [a] -> [a]
addRow a b = zipWith (+) a b

subRow :: Fractional a => [a] -> [a] -> [a]
subRow a b = zipWith (-) a b

-- SCALAR OPERATIONS
mulMwS :: Fractional a => [a] -> a -> [a]
mulMwS row n = map (*n) row

divMwS :: Fractional a => [a] -> a -> [a]
divMwS row n = map (/n) row

divSwS :: Fractional a => a -> a -> a
divSwS a b = a / b

mulSwS :: Fractional a => a -> a -> a
mulSwS a b = a * b

-- | Takes a matrix and two indices, and swaps the rows at those indices.
-- The arguments provided should assume the matrix is one-indexed.
swap :: (Fractional a, Eq a) =>[[a]] -> Int -> Int -> [[a]]
swap mat a b = foldr (\x acc -> (if x == swap1 then swap2 else if x == swap2 then swap1 else x) : acc) [] mat
    where swap1 = mat !! (pred a)
          swap2 = mat !! (pred b)

-- DEFINING TOSTRING STUFF
data TexMatRow = TexRatRow RatMatRow | TexDRow DMatRow deriving (Eq)
instance Show TexMatRow where
    show (TexRatRow row) = "\n    " ++ (List.intercalate " & " (map show $ map TSRat row)) ++ " \\\\"
    show (TexDRow row) = "\n    " ++ (List.intercalate " & " (map show $ map TSDouble row)) ++ " \\\\"
data TexMatrix = TexMatrix [TexMatRow] deriving (Eq)
instance Show TexMatrix where
    show (TexMatrix mat) = showMat mat

showMat :: Show a => [a] -> String
showMat x = "\\begin{bmatrix}" ++ unwords (map show x) ++ "\n\\end{bmatrix}"

-- | Displays a Rational number as a fraction, such as "1/2".
showRat :: Rational -> TexString
showRat n
    | denominator n == 1 = TSString . show $ numerator n
    | otherwise = TSString $ (show $ numerator n) ++ "/" ++ (show $ denominator n)

-- | Converts a TexMatRow to its string representation.
showTexRow :: TexMatRow -> String
showTexRow trow = show trow

-- | Converts a full matrix to LaTeX form.
-- The boolean flag sets whether or not to wrap the matrix with dollar signs.
ratMatToStr :: RatMatrix -> Bool -> String
ratMatToStr mat True = "$$ " ++ (ratMatToStr mat False) ++ " $$"
ratMatToStr mat False = show . TexMatrix $ map TexRatRow mat

dMatToStr :: DMatrix -> Bool -> String
dMatToStr mat True = "$$ " ++ (dMatToStr mat False) ++ " $$"
dMatToStr mat False = show . TexMatrix $ map TexDRow mat

rowToStr :: RatMatRow -> [TexString]
rowToStr ns = map showRat ns

rmatrix = [[1 % 2, 2 % 3, 3 % 4], [3, 4, 5], [4, 5, 6]]
dmatrix :: [[Double]]
dmatrix = [[1.2, 2.3, 3.4], [3,4,5], [4,5,6]]

main = do
    putStrLn $ ratMatToStr (swap rmatrix 1 2) True
    putStrLn $ dMatToStr (swap dmatrix 1 2) True

{--
output:
$$ "\\begin{bmatrix}\n    \"3 % 1 & 4 % 1 & 5 % 1\\\\\\\\\"\n    \"1 % 5 & 2 % 3 & 3 % 1\\\\\\\\\"\n    \"4 % 1 & 5 % 1 & 6 % 1\\\\\\\\\"\n\\end{bmatrix}" $$
Problem: members of matrix are of rational type, not TSRat, causing show's recursive calls to be insufficient
Solution: when converting to string, map TSRat or TSString constructor to all members of the matrix

therefore two separate types: one type Matrix = [[Rational]], other type StrMatrix = [[TexString]]
--}


{--
Brainstorm:
    Create two stacks of arrays from parsing an equation:
    1st stack is 1-argument lambda expressions
    2nd stack is index representing row of array to apply function to
    Generate comments based on what functions are executed
--}
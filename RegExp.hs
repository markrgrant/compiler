module RegExp where

data RegExp = Epsilon |
              Literal Char |
              Union RegExp RegExp |
              Concat RegExp RegExp |
              Star RegExp
              deriving Eq


-- Return True if the regular expression exactly matches the entire string,
-- false otherwise.
matches :: RegExp -> String -> Bool
matches Epsilon [] = True
matches (Literal c1) [c2] = c1 == c2
matches (Union r1 r2) str = matches r1 str || matches r2 str
matches (Concat r1 r2) str =
    any (\(str1,str2) -> matches r1 str1 && matches r1 str2)
        (allSplits str)
matches (Star r) "" = True
matches (Star r) str =
    any (\(str1, str2) -> matches r str1 && matches (Star r) str2)
        (allSplits str)

allSplits :: String -> [(String, String)]
allSplits str = map (\i -> splitAt i str) [0..length str]

-- Express a regular expression as a string.  Avoid including
-- parantheses when they are unnecessary.
instance Show RegExp where
    show Epsilon = ""
    show (Literal c) = [c]
    show (Union r1 r2) = show r1 ++ "|" ++ show r2
    show (Concat (Literal c1) (Literal c2)) = c1:[c2]
    show (Concat r@(Union _ _) (Literal c)) = "(" ++ show  r ++ ")" ++ [c]
    show (Concat (Literal c) r@(Union _ _)) = [c] ++ "(" ++ show r ++ ")"
    show (Concat r1 r2) = show r1 ++ show r2
    show (Star (Literal c)) = [c] ++ "*"
    show (Star r) = "(" ++ show r ++ ")*"

-- Make RegExp an instance of the Read type class and respect
-- the regexp operator binding strength star > concat > union.
-- Also identify parentheses and assign them the highest priority
-- when parsing.
-- readsPrec :: Int -> ReadS a
-- type ReadS a = String -> [(a, String)]

-- parsing a regular expression
-- (ab)+|c*d
-- read parenthesis, and parse all of the contents

-- instance (Read a) => Read (RegExp a) where
--    readsPrec i [] = []
--    readsPrec i (x:xs)
--        | x in ['a'..'z'] = (Literal x, xs):(readsPrec)
--        | x in [
--        | otherwise = []


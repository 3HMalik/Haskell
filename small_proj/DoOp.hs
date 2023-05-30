import Data.Maybe
import Data.Char
import Data.List

myElem :: Eq a => a -> [a] -> Bool
myElem a [] = False
myElem a (x:xs)
    | a == x = True
    | otherwise = myElem a xs

safeDiv :: Int -> Int -> Maybe Int
safeDiv a 0 = Nothing
safeDiv a b = Just (div a b)

safeNth :: [a] -> Int -> Maybe a
safeNth [] _ = Nothing
safeNth (x:_) 0 = Just x
safeNth (x:xs) b
    | b < 0 = Nothing
    | otherwise = safeNth xs (b-1)

safeSucc :: Maybe Int -> Maybe Int
safeSucc a = fmap succ a

myLookup :: Eq a => a -> [(a,b)] -> Maybe b
myLookup y [] = Nothing
myLookup y ((a,b):xs)
    | y == a = Just b
    |otherwise = myLookup y xs

maybeDo :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeDo f a b = pure f <*> a <*> b

readInt :: [Char] -> Maybe Int
readInt [] = Nothing
readInt str
    | all isDigit str == True = Just (read str :: Int)
    | otherwise = Nothing

getLineLength :: IO Int
getLineLength = do
    l <- getLine
    let a = length (l)
    return a

printAndGetLength :: String -> IO Int
printAndGetLength str =
    putStrLn str >>
    return (length str)

getInt :: IO (Maybe Int)
getInt = do
    l <- getLine
    if (all isDigit l) == True
        then return (Just (read l :: Int))
        else
            return (Nothing)

myconcat :: String -> Int -> IO String
myconcat str 0 = return str
myconcat str x = do
    line <- getLine
    myconcat (str ++ line) (x-1)

concatLines :: Int -> IO String
concatLines x 
    | x < 0 = return ""
    | otherwise = do 
        l <- myconcat "" x
        return l

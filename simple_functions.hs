mySucc :: Int -> Int
mySucc a = a + 1

myIsNeg :: Int -> Bool
myIsNeg a
    | a < 0 = True
    | otherwise = False

myAbs :: Int -> Int
myAbs a
    | a < 0 = -a 
    | otherwise = a

myMin :: Int -> Int -> Int
myMin a b 
    | a < b = a 
    | otherwise = b 

myMax :: Int -> Int -> Int
myMax a b 
    | a > b = a 
    | otherwise = b 

myTuple :: a -> b -> (a, b)
myTuple a b = (a,b)

myTruple :: a -> b -> c -> (a, b, c)
myTruple a b c = (a, b, c)

myFst :: (a, b) -> a
myFst (a, b) = a

mySnd :: (a, b) -> b
mySnd (a, b) = b 

mySwap :: (a, b) -> (b, a)
mySwap (a, b) = (b, a)

myHead :: [a] -> a
myHead (a:_) = a
myHead [] = error "liste vide"

myTail :: [a] -> [a]
myTail (_:a) = a
myTail [] = error "liste vide"

myLength :: [a] -> Int
myLength [] = 0
myLength (_:a) = 1 + myLength(a)

myNth :: [a] -> Int -> a
myNth [] _ = error "liste vide"
myNth (x:_) 0 = x 
myNth (x:xs) b 
    | b < 0 = error "negative index"
    | otherwise = myNth xs (b-1)

myTake :: Int -> [a] -> [a]
myTake b (x:xs)
    | b <= 0 = []
    | b >= myLength(x:xs) = (x:xs)
    | otherwise = x : myTake (b-1) xs

myDrop :: Int -> [a] -> [a]
myDrop b (x:xs)
    | b <= 0 = (x:xs)
    | b >= myLength(x:xs) = []
    | otherwise = myDrop (b-1) xs

myAppend :: [a] -> [a] -> [a]
myAppend [] [] = []
myAppend [] (x:xs) = (x:xs)
myAppend (x:xs) [] = (x:xs)
myAppend (x:xs) (a:b) = x : myAppend xs (a:b)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse [a] = [a]
myReverse (x:xs) = myAppend (myReverse xs) [x]

myInit :: [a] -> [a]
myInit [] = error "liste vide"
myInit [a] = []
myInit (x:xs) = x : myInit(xs)

myLast :: [a] -> a
myLast [] = error "liste vide"
myLast [a] = a
myLast (x:xs) = myLast (xs) 

myZip :: [a] -> [b] -> [(a, b)]
myZip [] [] = []
myZip [a] [] = []
myZip [] [b] = []
myZip [a] [b] = [(a,b)]
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

myUnzip :: [(a,b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip ((a, b):xs) = (a : myFst (myUnzip xs), b : mySnd (myUnzip xs))

myMap :: (a -> b) -> [a] -> [b]
myMap fun [] = []
myMap fun (x:xs) = fun x : myMap fun xs
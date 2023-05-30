import System.Environment
import Data.Maybe
import Data.Char
import Data.List
import System.Exit

setInts :: [String] -> [Int]
setInts [] = []
setInts a = fmap read a :: [Int] 

isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

split_list :: ([Int], [Int]) -> Bool
split_list (xa, []) = isSorted xa
split_list (xa, yb) = False

display_list :: Bool -> IO ()
display_list True = putStrLn "OK"
display_list False = putStrLn "KO"

check_int :: [Char] -> Bool -> IO ()
check_int str True = return ()
check_int str False = exitWith (ExitFailure 84)

check_errors :: [String] -> [Char] -> IO ()
check_errors [] _ = exitWith (ExitFailure 84)
check_errors [x] _ = check_int x (all isDigit x)
check_errors (x:xs) com = check_errors xs com

main :: IO ()
main = do 
    l <- getArgs
    a <- getLine
    check_errors l a
    let list = setInts l
    let action = words a
    let (x,y) = call_actions (list, []) action
    display_list (split_list (x,y))

call_actions :: ([Int], [Int]) -> [String] -> ([Int], [Int]) 
call_actions couple [] = couple
call_actions couple actions = foldl do_actions couple actions

do_actions :: ([Int], [Int]) -> String -> ([Int], [Int])
do_actions (a,b) "sa" = (sa a, b)
do_actions (a,b) "sb" = (a, sb b)
do_actions (a,b) "sc" = (sa a, sb b)
do_actions (a,b) "pa" = pa (a, b)
do_actions (a,b) "pb" = pb (a, b)
do_actions (a,b) "ra" = (ra a, b)
do_actions (a,b) "rb" = (a, rb b)
do_actions (a,b) "rr" = (ra a, rb b)
do_actions (a,b) "rra" = (rra a, b)
do_actions (a,b) "rrb" = (a, rrb b)
do_actions (a,b) "rrr" = (rra a, rrb b)

sa :: [a] -> [a]
sa [] = []
sa (x:[]) = (x:[])
sa (x:y:xs) = (y:x:xs)

sb :: [b] -> [b]
sb [] = []
sb (x:[]) = (x:[])
sb (x:y:xs) = (y:x:xs)

pa :: ([Int], [Int]) -> ([Int], [Int])
pa (x, []) = (x, [])
pa (xs, (y:ys)) = ((y:xs), ys)

pb :: ([Int], [Int]) -> ([Int], [Int])
pb ([], xs) = ([], xs)
pb ((x:xs), ys) = (xs, (x:ys))

ra :: [Int] -> [Int]
ra [] = []
ra (a:as) = reverse $ a:(reverse as)

rb :: [Int] -> [Int]
rb [] = []
rb (b:bs) = reverse $ b:(reverse bs)

rra :: [Int] -> [Int]
rra [] = []
rra a = (last a):(init a)

rrb :: [Int] -> [Int]
rrb [] = []
rrb b = (last b):(init b)

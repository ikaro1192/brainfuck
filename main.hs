import Data.Char
import System.IO

--純粋・不純を分けるための型・関数
data Purity = Pure | Dirty deriving (Eq,Show)
type Program = [(Char,Purity)]


isPure :: Char -> Purity
isPure '+' = Pure
isPure '-' = Pure
isPure '>' = Pure
isPure '<' = Pure
isPure ' ' = Pure
isPure x = Dirty

separatePurity :: String -> Program 
separatePurity "" = []
separatePurity (x:xs) = (x,isPure x) : (separatePurity xs)

--括弧関係の処理

data Bracket = LeftBracket Char | RightBracket Char

--BracketL BracketR Level treated input
findLeftBracket :: Bracket -> Bracket -> Int -> Program -> Program -> (Program, Program) 
findLeftBracket leftBracket@(LeftBracket left) rightBracket@(RightBracket right) 0 xs (y:ys)
	| fst y == left = (xs, y:ys)
	| fst y == right = findLeftBracket leftBracket  rightBracket  1 (y : xs) ys
	| otherwise = findLeftBracket leftBracket rightBracket 0 (y : xs) ys

findLeftBracket leftBracket@(LeftBracket left) rightBracket@(RightBracket right) level [] (y:ys)
	| fst y == left = findLeftBracket leftBracket rightBracket (level - 1) (y : []) ys
	| fst y == right = findLeftBracket leftBracket rightBracket (level + 1) (y : []) ys
	| otherwise = findLeftBracket leftBracket rightBracket level (y : []) ys


findLeftBracket leftBracket@(LeftBracket left) rightBracket@(RightBracket right) level all@(x:xs) (y:ys)
	| fst y == left = findLeftBracket leftBracket rightBracket (level - 1) (y : all) ys
	| fst y == right = findLeftBracket leftBracket rightBracket (level + 1) (y : all) ys
	| otherwise = findLeftBracket leftBracket rightBracket level (y : all) ys


findLeftAngledBracket :: Program -> Program -> (Program, Program)
findLeftAngledBracket = findLeftBracket (LeftBracket '[') (RightBracket ']') 0

findRightAngledBracket :: Program  -> Program -> (Program, Program)
findRightAngledBracket  x y = (reverse.fst $ result,snd $ result)
	where result = findLeftBracket (LeftBracket ']') (RightBracket '[') 0 x $ reverse y


----擬似メモリ操作
type Unit = (Bool, Int)
type Memory = [Unit]

--純粋な処理
pureRun :: Char -> Memory -> Memory
pureRun c [] = []
pureRun c (x:y:z:xs)
	| fst y == True = (command c x y z) ++ xs
	| otherwise = x : pureRun c (y: z : xs)
	where
		command '+' x y z = x : (True, snd y + 1 ) : z : []
		command '-' x y z = x : (True, snd y - 1 ) : z : []
		command '>' x y z = x : (False, snd y ) : (True, snd z ) : []
		command '<' x y z = (True, snd x ) : (False, snd y ) : z : []
		command ' ' x y z = x : y : z : []


--不純な処理
dirtyRun :: Char -> Memory ->IO Memory
dirtyRun c [] = return []
dirtyRun c (x:y:z:xs)
	| fst y == True = do
		result <-(command c x y z)
		return $ result ++ xs
	| otherwise =do
		result <- dirtyRun c (y: z : xs)
		return $ x : result
	where
		command '.' x y z = do
			putStr $ (Data.Char.chr $ snd y) : []
			return $ x : y : z : []
		command a x y z = do
			return $ x : y : z : []


--イテレータの値を取得
getIterVal :: Memory -> Int
getIterVal ((True,x):xs) =  x
getIterVal ((False,x):xs) = getIterVal xs


--実行
run :: Memory -> Program -> Program -> IO Memory
run memory treatedProgram [] = return memory

run memory treatedProgram nonTreatedProgram@((']',_):xs) = do

	run memory (tail (snd result)) ( ('[',Pure) : ( fst result))
	where result = findLeftAngledBracket nonTreatedProgram treatedProgram 

run memory treatedProgram nonTreatedProgram@(('[',_):xs) = do
	
	run memory ( (fst result)) (snd result)
	where
		sub = findRightAngledBracket [] xs
		result = if (getIterVal memory == 0)
			then  ((snd sub) ++ (('[',Pure) : treatedProgram) , reverse (fst sub))
			else (('[',Pure):treatedProgram,xs)
		

run memory treatedProgram nonTreatedProgram@(x:xs) = if snd x == Pure
	then do
		run (pureRun (fst x) memory) (x:treatedProgram) xs
	else do

		result1 <- dirtyRun (fst x) memory
		run result1 (x:treatedProgram) xs

main = do
	handle <- openFile "hoge.bf" ReadMode
	code <- hGetContents handle
	let memory = ((False,0) : (True,0) : ( repeat (False,0) ))
	let program = separatePurity code
	run memory [] program


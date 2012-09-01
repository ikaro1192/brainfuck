
import Data.Char

--純粋・不純を分けるための型・関数
data Purity = Pure | Dirty deriving (Eq,Show)

isPure :: Char -> Purity
isPure '+' = Pure
isPure '-' = Pure
isPure '>' = Pure
isPure '<' = Pure
isPure ' ' = Pure
isPure x = Dirty

separatePurity :: String -> [(Char,Purity)]
separatePurity "" = []
separatePurity (x:xs) = (x,isPure x) : (separatePurity xs)



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
			putStr $ (Data.Char.chr $ snd y + 65) : []
			return $ x : y : z : []

--実行
run :: Memory -> [(Char, Purity)] -> IO Memory
run memory [] = return memory
run memory (x:xs) = if snd x == Pure
	then do
		result <- run (pureRun (fst x) memory) xs
		return result
	else do
		result1 <- dirtyRun (fst x) memory
		result <- run result1 xs
		return result

main = do
	foo <- getLine
	code <- return foo
	let memory = ((False,0) : (True,0) : ( replicate 20 (False,0) ))
	let program = separatePurity code
	run memory program



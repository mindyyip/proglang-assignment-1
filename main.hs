import PA1Helper
import System.Environment (getArgs)

-- Haskell representation of lambda expression
-- data Lexp = Atom String | Lambda String Lexp | Apply Lexp  Lexp

-- Given a filename and function for reducing lambda expressions,
-- reduce all valid lambda expressions in the file and output results.
-- runProgram :: String -> (Lexp -> Lexp) -> IO ()

-- This is the identity function for the Lexp datatype, which is
-- used to illustrate pattern matching with the datatype. "_" was
-- used since I did not need to use bound variable. For your code,
-- however, you can replace "_" with an actual variable name so you
-- can use the bound variable. The "@" allows you to retain a variable
-- that represents the entire structure, while pattern matching on
-- components of the structure.
id' :: Lexp -> Lexp
id' v@(Atom _) = v
id' lexp@(Lambda _ _) = lexp
id' lexp@(Apply _ _) = lexp

-- You will need to write a reducer that does something more than
-- return whatever it was given, of course!
-- @ is for pattern matching

reducer :: Lexp -> Lexp
reducer lexp@(Atom _) = lexp
reducer lexp@(Lambda x (Apply e m))  = reducerSubfunction lexp $ eta_convert lexp x e m
reducer lexp@(Lambda x e)            = reducerSubfunction lexp $ Lambda x $ reducer e
reducer lexp@(Apply (Lambda x e) e1) = reducerSubfunction lexp $ betaReduce x (reducer e1) e
reducer lexp@(Apply e1 e2)           = reducerSubfunction lexp $ Apply (reducer e1) $ reducer e1


-- Helper Function for reducer
reducerSubfunction :: Lexp -> Lexp -> Lexp
reducerSubfunction e1 e2
    | e1 == e2 = e2
    | otherwise = reducer e2


-- Remove code (given to us from file iscombinator.hs)
remove :: (Eq a) => a -> [a] -> [a]
remove x = filter (\v -> v/=x)

-- Code given in class for identifying free variables
freevars :: Lexp -> [String]
freevars (Atom s)          = [s]
freevars (Lambda v e)      = remove v (freevars e)
freevars (Apply e1 e2)     = (freevars e1)++(freevars e2)

-- Function that applies beta reduction
betaReduce :: String -> Lexp -> Lexp -> Lexp
betaReduce var e1 e2@(Atom v)
            | var == v      = e1
            | otherwise     = e2
betaReduce var e1 (Apply expr1 expr2)                     = Apply (betaReduce var e1 expr1) $ betaReduce var e1 expr2
betaReduce var e1 expr@(Lambda var' expr')
            | var' /= var && var' `notElem` freevars e1  = Lambda var' $ betaReduce var e1 expr'
            | var' /= var && var' `elem` freevars e1     = betaReduce var e1 $ alpha_rename expr $ freevars e1
            | otherwise    = e2



etaConvert :: Lexp -> Lexp
etaConvert x@(Atom _) = x
etaConvert expr@(Apply expr1 expr2) = Apply (etaConvert expr1) (etaConvert expr2)
etaConvert lexp@(Lambda x (Apply f y))                                                                    
  | not x `elem` freeVars f && (x == y) = etaConvert f
  | otherwise = lexp
etaConvert (Lambda x e) =  Lambda x (etaConvert e) 

alphaRename :: Lexp->[String]->Lexp
alphaRename (Lambda x e) usedNames = Lambda(getName x usedNames) $ betaReduce x (Atom $ getName x usedNames) e
alphaRename lexp usedNames = lexp


getName :: String -> [String] -> String
getName x usedNames = head $ dropWhile (`elem` useNames) [x ++ show i | i <- [1...]]

-- Entry point of program
main = do
    arssgs <- getArgs
    let inFile = case args of { x:_ -> x; _ -> "input.lambda" }
    let outFile = case args of { x:y:_ -> y; _ -> "output.lambda"}
    -- id' simply returns its input, so runProgram will result
    -- in printing each lambda expression twice.
    runProgram inFile outFile reducer

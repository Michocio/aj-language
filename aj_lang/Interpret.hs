{-
  Michał Jaroń
  mj348711
  File with whole functions connected with interpreting
  I tried, I tried hardly, believe me, to separate
  expressions, declarartions and stmts to another files.
  But as we talked at during classes - interpreter is like
  one big recursive functions, so that was almost impossible
  to divide functions to another files.
  For example expressions needs instructions (call, inc etc.)
  So, please forgive me length of that file :),
  I tried to divide functions in space and to include
  solid amount of comments, hope it will help.
  Have a good time :)
-}

module Interpret where

import Control.Monad.Writer
import Control.Monad.Writer.Lazy
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import Control.Monad.Except
import qualified Data.Map as M
import Data.Char
import Data.List
import Lexcalc
import Parcalc
import Skelcalc
import Printcalc
import Abscalc
import ErrM
import Data.Typeable
import Control.Exception
import Control.Monad.Trans.Maybe
import Control.Monad.Identity
import Data.Maybe
import qualified Data.Char as Char


import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )

import Lexcalc
import Parcalc
import Skelcalc
import Printcalc
import Abscalc


import Storables
import Core

-- Errors messages
divideZero s = liftIO $ throwIO $  ErrorCall  $ "Próba dzielenia przez 0: " ++ (printTree s)
outOfBounds s =  liftIO $ throwIO $  ErrorCall $ "Próba odwołania poza tablicę: " ++ (printTree s)


{-
-------------------------------
------------------------------------------
----------------------------------------------
   Block of General functions
----------------------------------------------
------------------------------------------
-------------------------------
-}

startProgram :: Prog -> IO ()
startProgram (Program p) =  (liftIO $ evalStateT (runInstr (p)) emptyKernel)

runInstr  :: [Inst] -> StateT Kernel IO ()
runInstr [] = return ()
runInstr (x:xs) = do
    doInstr x
    y <- get
    -- Uncomment for debuggin purposes
    --liftIO $ putStrLn $ show "stan"
    --liftIO $ putStrLn $ show x
    --liftIO $ putStrLn $ show y
    runInstr xs

-- Parsowanie instrukcji
doInstr :: Inst ->  StateT Kernel IO ()

doInstr (Print e) = do
    val <- evalExpr e
    liftIO $ putStr $ show val
    return ()

doInstr (PrintLn e) = do
    val <- evalExpr e
    liftIO $ putStrLn $ show val
    return ()

doInstr (InstrD d) = do
    execDecl d
    return ()

doInstr (InstrExpr e) = do
    x <- evalExpr e
    return ()

doInstr (InstrS stmt) = do
    execStmt stmt
    return ()

doInstr (InstrClass d) = do
    declClass d
    return ()

doInstr (InstrFun fun) = do
    declFun fun
    return ()

doInstr (InstrLoop loop) = do
    execLoop loop
    return ()

doInstr (InstrFor for) = do
    execFor for
    return ()

doInstr (Read var) = do
    liftIO $ throwIO $ ErrorCall $  "Czytanie nie jest obecnie wspierane"
    return ()

doInstr (IApply _ _) = do
    liftIO $ throwIO $ ErrorCall $  "Apply obecnie nie wspierany"
    return ()
doInstr (IApplyAn _ _) = do
    liftIO $ throwIO $ ErrorCall $  "Apply obecnie nie wspierany"
    return ()

-----------------------------
----Block semantics----------
-----------------------------
replaceValues :: [Ident] -> [Storable] -> StateT Kernel IO ()
replaceValues [] _ = return ()
replaceValues (x:xs) (v:vs) = do
    assignment x v
    replaceValues xs vs

--changeMap ::

getOldVals :: StateT Kernel IO ()
getOldVals = do
    (env, t, s, c, fun, scopes, level, existance) <- get
    lista <- return $ M.toList level
    return ()

-- semantic of block
-- possible overriding of variables etc
execBlock :: [Inst] ->  StateT Kernel IO ()
execBlock insts = do
    (env, t, s, c, fun, scopes, level, existance) <- get
    put(env, t, s, c, fun, (env, s):scopes, level, M.empty)
    runInstr insts
    (env', _, s', _, _, _, level', ex) <- get
    -- which was overrided, and which values was changed
    restore <- return $ (M.keys  env)  \\ (M.keys ex)
    zz <- mapM (getValueByIdent) restore
    put (env, t, s, c, fun, scopes, level, existance)
    replaceValues restore zz
    (env, t, s, c, fun, scopes, level, existance) <- get
    replaceValues (M.keys level') (M.elems level')
    return ()


-----------------------------
----Call function----------
-----------------------------

-- used for dividing funs from fun params
isFun :: VEnv -> FEnv -> ((Ident, Type), Exp) -> Bool
isFun env fun (v, EVar var)=
    case loc var env of
        (Just val) -> False
        otherwise ->
         case (M.lookup var fun) of
             (Just x) -> True
             otherwise -> False
isFun _ _ (v, ELam lambda) = True
isFun _ _ _= False


-- name of class connected with fun returns
capitFirst :: String -> String
capitFirst x = (toUpper $ head x):(tail x)

translateToFunDecl ::  ((Ident, Type), Exp) -> StateT Kernel IO ()
translateToFunDecl ((name@(Ident x), t), EVar fun) = do
    (e, t, s, c, funs, scope, l, existance) <- get
    (Ident str_name) <- return $ fun
    -- Class for returing
    class_name <- return $ capitFirst str_name
    -- Get body of passed as param fun
    (Just copied_class) <- return $ M.lookup (Ident class_name) c
    current <- return $ fromJust $ M.lookup fun funs
    put(e, t, s, M.insert (Ident $ capitFirst x) copied_class c, M.insert name current funs,
        scope, l, existance)
    return ()

translateToFunDecl ((name, tt),(ELam (LamFun t params e))) = do
    runInstr $
        [InstrFun $ FunDecl name (toParamsList params t)
            [InstrS (FieldAssin (Ident "returns") (Ident "x") e)]]
    return ()


toParamsList :: [LambdaParams] -> Type -> ParamsList
toParamsList params t =
    FunParamList (fmap (\(LambdaParam tt name) -> FunVar tt name) params) [(FunVar t (Ident "x"))]


--TFun [(Ident "x",TInt)] [(Ident "y",TInt)])
callFun :: Call -> StateT Kernel IO Storable
callFun (CallFun name@(Ident fun) params) = do
    initital@(env, types, s, classes, funs, scopes, level, existance) <- get
    -- Get function definition
    (body, paramsIn, paramsOut, level) <- return $ fromJust $ M.lookup name funs
    -- Translate from tree representation into expr
    input <- return $ fmap (\(RealParamVal x) -> x) params
    withVals <- return $ zip paramsIn input
    -- functions vs vars as param
    (functions, values) <-  return $ partition (isFun env funs) withVals
    remember <- get
    defs <- mapM translateToDecls values
    funsdecls <- mapM translateToFunDecl functions
    toReturn <- return $ InstrD (Declare (CustomType (ClassName (capitFirst fun))) [(VarName (Ident "returns"))])
    -- prepare Kernel
    (_, _, _, cl, funs', _, _, _) <- get
    (env, _, nowe, _, _, _, _, _) <-
        (liftIO $ execStateT (runInstr  (defs ++ toReturn:body))
            (M.empty, M.empty, M.empty, cl, funs', [], M.empty, M.empty))
    pos <- return $ fromJust $ M.lookup (Ident "returns") env
    ret <- return $ evalObj nowe (fromJust $ M.lookup pos nowe)
    put remember--restore
    -- extract value to return
    -- If one value to return then return as simple value, else return object
    if(length ret == 1) then return $ head ret
    else return $ PVals ret

-----------------------------
----For functions----------
-----------------------------
execFor :: For -> StateT Kernel IO ()
execFor (ForTo t var init cond instr) =
    execFor (ForToMulti t var init cond [instr])
execFor (ForToMulti t var init cond instr) = do
    inst <- return $  [InstrD ( Declare t [VarAssin var init])]
    work <- return $ InstrExpr $ Epostinc (EVar var)
    (env, t, s, c, fun, scopes, level, existance) <- get
    put(env, t, s, c, fun, scopes, level, M.empty)
    runInstr inst
    koniec <- evalExpr cond
    for greater koniec var (instr++[work])
    (env', _, s', _, _, _, level', ex) <- get
    restore <- return $ (M.keys  env)  \\ (M.keys ex)
    zz <- mapM (getValueByIdent) restore
    put (env, t, s, c, fun, scopes, level, existance)
    replaceValues restore zz
    (env, t, s, c, fun, scopes, level, existance) <- get
    replaceValues (M.keys level') (M.elems level')
    return ()

execFor (ForDownTo t var init cond instr) =
    execFor (ForDownToMulti t var init cond [instr])
execFor (ForDownToMulti t var init cond instr) = do
    inst <- return $  [InstrD ( Declare t [VarAssin var init])]
    work <- return $ InstrExpr $ Epostdec (EVar var)
    (env, t, s, c, fun, scopes, level, existance) <- get
    put(env, t, s, c, fun, scopes, level, M.empty)
    runInstr inst
    koniec <- evalExpr cond
    for less koniec var (instr++[work])
    (env', _, s', _, _, _, level', ex) <- get
    restore <- return $ (M.keys  env)  \\ (M.keys ex)
    zz <- mapM (getValueByIdent) restore
    put (env, t, s, c, fun, scopes, level, existance)
    replaceValues restore zz
    (env, t, s, c, fun, scopes, level, existance) <- get
    replaceValues (M.keys level') (M.elems level')
    return ()

for :: (Storable -> Storable -> Storable)
            -> Storable -> Ident -> [Inst] -> StateT Kernel IO ()
for how koniec var body = do
    val <- getValueByIdent var
    cond <- return $ how val koniec
    if(cond == (PBool False)) then do
        runInstr body
        for how koniec var body
    else return ()


-----------------------------
----Loop functions----------
-----------------------------


execLoop :: Loop -> StateT Kernel IO ()
execLoop (LoopInst vars exp how instr) = execLoop (LoopInstMulti vars exp how [instr])

execLoop (LoopInstMulti vars exp work instrs) = do
    (env, t, s, c, fun, scopes, level, existance) <- get
    put(env, t, s, c, fun, scopes, level, M.empty)
    case vars of
        -- create counting variables
        (LoopDecli decls) -> runInstr $ fmap tranlsateLoop decls
        otherwise -> runInstr [] -- LoopDeclEps
    case work of
        -- instructions to do after every iteration
        (LoopDo how) -> let body = instrs ++ (fmap tranlsateHow how) in
                            loop 0 exp body -- start loop
        otherwise -> loop 0 exp instrs  --LoopDoEps
    -- because semantic of block is a little bit diffrent that exec block:
    -- clean and restore values after loop
    (env', _, s', _, _, _, level', ex) <- get
    restore <- return $ (M.keys  env)  \\ (M.keys ex)
    zz <- mapM (getValueByIdent) restore
    put (env, t, s, c, fun, scopes, level, existance)
    replaceValues restore zz
    (env, t, s, c, fun, scopes, level, existance) <- get
    replaceValues (M.keys level') (M.elems level')
    return ()


-- As I mentioned in grammar description - every declaration in loop is done only once
clearLoop :: Inst -> [Inst]
clearLoop (InstrD d) = []
clearLoop x = [x]

-- Single interation
loop :: Integer -> Exp -> [Inst] -> StateT Kernel IO ()
loop i e body = do
    val <- evalExpr e
    -- another iteration?
    if(val == (PBool True)) then do
        runInstr body
        -- After first iteration, delete all declarations
        if(i==0) then let body' = foldl (++) [] (fmap clearLoop body) in loop (i+1) e body'
        else loop (i+1) e body
    else return ()

-- helpers

-- vars == LoopAssin Ident Exp --> VarAssin Ident Exp
tranlsateLoop :: LoopDecl -> Inst
tranlsateLoop (DeclareLoop t vars) =
    let parsed = (fmap (\(LoopAssin name e)-> (VarAssin name e)) vars) in
        (InstrD (Declare t parsed))

tranlsateHow :: IterExp -> Inst
tranlsateHow (IterExpr e) = (InstrExpr e)

tranlsateHow (IterExpr2 (VAssinPlusL var e)) = (InstrS (VAssinPlus var e))
tranlsateHow (IterExpr2 (VAssinMultiL var e)) = (InstrS (VAssinMulti var e))
tranlsateHow (IterExpr2 (VAssinMinusL var e)) = (InstrS (VAssinMinus var e))
tranlsateHow (IterExpr2 (VAssinDivL var e)) = (InstrS (VAssinDiv var e))

{-
-------------------------------
------------------------------------------
----------------------------------------------
   Block of Stmts
----------------------------------------------
------------------------------------------
-------------------------------
-}

execStmt:: Stmt ->  StateT Kernel IO ()

-- x = val
-- Assign value to named variable
execStmt (VAssin var exp) = do
    val <- evalExpr exp
    t <- getType var
    assignment var (casts val t)
    return ()


-- obj.tab[][][] = val
-- Ident"." Ident  "[" Exp "]" [Elem] "=" Exp ";";
execStmt that@(VAssinArrField obj@(Ident name) tab@(Ident arr) index which exp) = do
    -- compose name of desired object
    what <- return $ (capitFirst name) ++ "." ++ arr
    array <- return $ ((EArr (Ident what) index which))
    -- translate to array form of Ident
    look <- toArrIdent False array
    val <- evalExpr exp
    -- Check if within bounds
    (env, _, s, _, _, _, _, _) <- get
    addr <- return $ M.lookup look env
    case addr of
        (Just addr') -> do -- ok
            t <- getType look
            assignment look  (casts val t)
            return ()
        otherwise -> outOfBounds that


-- Ident "[" Exp "]" [Elem]"."Ident ;
-- tab[][][].field
-- Array of objects
execStmt that@(VAssinFieldArr tab@(Ident arr) index which obj@(Ident name) exp) = do
    -- translate to array form of ident
    array <- return $ ((EArr tab index which))
    (Ident table) <- toArrIdent True array
    -- now add specific ident, as object
    field <- return $ Ident $ table ++ "." ++ name
    val <- evalExpr exp
    (env, _, s, _, _, _, _, _) <- get
    addr <- return $ M.lookup field env
    case addr of-- if index in array exists?
        (Just addr') -> do
            t <- getType field --ok
            assignment field  (casts val t)
            return ()
        otherwise -> outOfBounds that

-- object.x = val
execStmt (FieldAssin var (Ident member) exp) = do
    val <- evalExpr exp
    t <- getType (toIdent ((fromIdent var) ++ member))
    assignment (toIdent ((fromIdent var) ++ member)) (casts val t)
    return ()

-- tab[0][1] = val
execStmt that@(VAssinArr (Ident name) index which exp) = do
    -- Very similar to VAssinFieldArr
    val <- evalExpr exp
    array <- return $ ((EArr (Ident name) index which))
    table <- toArrIdent False  array
    (env, _, s, _, _, _, _, _) <- get
    addr <- return $ M.lookup table env
    case addr of
        (Just addr') -> do
            t <- getType table
            assignment table (casts val t)
            return ()
        otherwise -> outOfBounds that

-- x += val
execStmt (VAssinPlus var exp) = do
    -- at first compute value to add
    val <- evalExpr exp
    old_val <- getValueByIdent var
    t <- getType var
    assignment var (casts (old_val `plus` val) t)
    return ()

-- x *= val
execStmt (VAssinMulti var exp) = do
    val <- evalExpr exp
    old_val <- getValueByIdent var
    t <- getType var
    assignment var (casts (old_val `multi` val) t)
    return ()

-- x -= val
execStmt (VAssinMinus var exp) = do
    val <- evalExpr exp
    old_val <- getValueByIdent var
    t <- getType var
    assignment var (casts (old_val `minus` val) t)
    return ()

-- x /= val
execStmt s@(VAssinDiv var exp) = do
    val <- evalExpr exp
    old_val <- getValueByIdent var
    t <- getType var
    if(val == (PInt 0) || val == (PDouble 0)) then
        divideZero s
    else do
        assignment var (casts (old_val `divide` val) t)
        return ()

-----------------------------
----if----------
-----------------------------

-- Translate to "wrappers"
execStmt (SIfOne exp inst elifs) =
    execStmt (SIfOneMore exp [inst] elifs)

execStmt (SIfTwo exp inst elifs elseInst) =
    execStmt (SIfThreeMore exp [inst] elifs [elseInst])

execStmt (SIfTwoMore exp block elifs elseInst) =
    execStmt (SIfThreeMore exp block elifs [elseInst])

execStmt (SIfFourMore exp inst elifs elseInst) =
    execStmt (SIfThreeMore exp [inst] elifs elseInst)

-- Wrappers
execStmt s@(SIfOneMore exp block elifs) = do
    val <- evalExpr exp
    val <- return $ casts val TBool
    -- If cond in if is true
    if(val == (PBool True)) then
        do
            execBlock block
            return ()
    else
        do-- else, try to find first true elseif
            which <- whichElif (fmap (translateElif) elifs ) 0
            if(which >= 0) then execBlock $ translateElifInst (elifs !! which)
            else return ()

execStmt (SIfThreeMore exp block elifs elseBlock) = do
    val <- evalExpr exp
    val <- return $ casts val TBool
    -- If cond in if is true
    if(val == (PBool True)) then
        do
            execBlock block
            return ()
    else
        do-- else, try to find first true elseif
            which <- whichElif (fmap (translateElif) elifs ) 0
            if(which >= 0) then execBlock $ translateElifInst (elifs !! which)
            else execBlock elseBlock

-----------------------------
----switch----------
-----------------------------
-- Just translate to ifs
execStmt (Switch exp cases elseBlock) = do
    (CasesVal first_e inst1) <- return $ head cases
    trans <- return $ fmap (\(CasesVal e instrs)-> SElIfMore (Eeq e exp) instrs) (tail cases)
    execStmt(SIfThreeMore (Eeq exp first_e) inst1 trans elseBlock)
    return ()

execStmt (Switch2 exp cases) = do
    execStmt (Switch exp cases [])
    return ()

-- Helper functions for infs
whichElif :: [Exp] -> Int -> StateT Kernel IO Int
whichElif [] _= return (-1)
whichElif (x:xs) i = do
    val <- evalExpr x
    val <- return $ casts val TBool
    if(val == (PBool True)) then return i
    else whichElif xs (i+1)

-- Extract from box
translateElifInst ::  ElIf -> [Inst]
translateElifInst  (SElIf exp inst) = [inst]
translateElifInst (SElIfMore exp insts) =insts

translateElif ::  ElIf -> Exp
translateElif (SElIf exp inst) = exp
translateElif (SElIfMore exp insts) = exp


{-
-------------------------------
------------------------------------------
----------------------------------------------
   Block of Expressions
----------------------------------------------
------------------------------------------
-------------------------------
-}

-- Helper function for translating "tree" form of array into ident
toArrIdent :: Bool -> Exp -> StateT Kernel IO Ident
toArrIdent up ((EArr (Ident name) index which)) = do
    -- ident, at the end of fun should looks like
    -- name[0][e][e][e]name
    indexes <- return $ index:(fmap (\(ArrElem x) -> x) which)
    indexes <- return $ index:(fmap (\(ArrElem x) -> x) which)
    eval_ind <- mapM evalExpr indexes
    eval_ind <- return $ fmap (\(PInt x) -> x) eval_ind
    look <- return $ fmap (\x -> "[" ++ show (x) ++ "]") eval_ind
    if up then do-- used for array of objects
        table <- return $ capitFirst name ++ "[0]" ++ foldl (++) "" look ++ name
        return $ Ident table
    else do
        table <- return $ name ++ "[0]" ++ foldl (++) "" look ++ name
        return $ Ident table


-----Expressions
evalExpr :: Exp ->  StateT Kernel IO Storable

evalExpr Etrue = return (PBool True)
evalExpr Efalse = return (PBool False)
evalExpr (EInt n) = return (PInt n)
evalExpr (EDouble n) = return (PDouble n)
evalExpr (EString str) = return (PText str)

-- kind of contructor - not used generally, only in backend
evalExpr (ECons es) = do
    vals <- mapM evalExpr es
    return $ PVals vals

-- ++x
-- Firstly increment and return new value
evalExpr (Epreinc e@(EVar var)) = do
    val1 <- evalExpr e
    case val1 of
        (PText s) -> do -- if text, than duplicate last char
            letter <- return $ last s
            val <- evalExpr (EAdd e (EString [letter]))
            assignment var val
            return val
        otherwise -> do
            val <- evalExpr (EAdd e (EInt 1))
            assignment var val
            return val

-- --x
-- Firstly decrement and return new value
evalExpr (Epredec e@(EVar var)) = do
    val <- evalExpr (ESub e (EInt 1))
    assignment var val
    return val

-- x++
-- increment, but return old value
evalExpr (Epostinc e@(EVar var)) = do
    pre <- evalExpr e
    evalExpr (Epreinc e)
    return pre

-- x--
-- decrement, but return old value
evalExpr (Epostdec e@(EVar var)) = do
    pre <- evalExpr e
    evalExpr (Epredec e)
    return pre


-- Logical operations

evalExpr (LogicalAnd e1 e2) = do
    val1 <- evalExpr e1
    val1 <- return $ casts val1 TBool
    val2 <- evalExpr e2
    val2 <- return $ casts val2 TBool
    return $ PBool $ (castBool val1) && (castBool val2)

evalExpr (LogicalOr e1 e2) = do
    val1 <- evalExpr e1
    val1 <- return $ casts val1 TBool
    val2 <- evalExpr e2
    val2 <- return $ casts val2 TBool
    return $ PBool $ (castBool val1) || (castBool val2)

evalExpr (LogicalNot e) = do
    val <- evalExpr e
    val <- return $ casts val TBool
    return $ PBool $ not (castBool val)


-- Comparsions

evalExpr (Eeq e1 e2) = do
    val1 <- evalExpr e1
    val2 <- evalExpr e2
    case val1 of
        (PObject a) -> do
            -- if object, than compare valus, not pointers to values
            case val2 of
                (PObject b) -> do
                    (_, _, s, _, _, _, _, _) <- get
                    -- eval object from pointers to basic values
                    obj1 <- return $ fmap (flip getValueByLoc s) a
                    obj2 <- return $ fmap (flip getValueByLoc s) b
                    cmps <- return $ zipWith (\x y -> (cmpStorable s x y)) obj1 obj2
                    -- and now compare
                    return $ PBool $ all ((==)True) cmps
        otherwise ->  do
            return $ PBool $ val1 == val2


evalExpr (Eneq e1 e2) = do
    (PBool val) <- evalExpr (Eeq e1 e2)
    return (PBool $ not val)

-- Use overrided operators for Storables
evalExpr (ELess  e1 e2) = do
    val1 <- evalExpr e1
    val2 <- evalExpr e2
    return (val1 `less` val2)

evalExpr (EBigg  e1 e2) = do
    val1 <- evalExpr e1
    val2 <- evalExpr e2
    return (val1 `greater` val2)

evalExpr (ELessEq e1 e2) = do
    val1 <- evalExpr e1
    val2 <- evalExpr e2
    (PBool ls) <- return $ (val1 `less` val2)
    eq <- return $ (Eeq e1 e2)
    (PBool valEq) <- evalExpr eq
    return $ PBool (ls || valEq)

evalExpr (EBiggEq e1 e2) = do
    val1 <- evalExpr e1
    val2 <- evalExpr e2
    (PBool ls) <- return $ (val1 `greater` val2)
    eq <- return $ (Eeq e1 e2)
    (PBool valEq) <- evalExpr eq
    return $ PBool (ls || valEq)


-- Arithmetic operations

evalExpr (EAdd e1 e2) = do
    val1 <- evalExpr e1
    val2 <- evalExpr e2
    return (val1 `plus` val2)

evalExpr (ESub e1 e2) = do
    val1 <- evalExpr e1
    val2 <- evalExpr e2
    return (val1 `minus` val2)

evalExpr (EMul e1 e2) = do
    val1 <- evalExpr e1
    val2 <- evalExpr e2
    return (val1 `multi` val2)

evalExpr s@(EDiv e1 e2) = do
    val1 <- evalExpr e1
    val2 <- evalExpr e2
    -- check if second value is not 0
    if(val2 == (PInt 0) || val2 == (PDouble 0.0)) then
        divideZero s
    else
        return (val1 `divide` val2)

evalExpr (EMod e1 e2) = do
    (PInt val1) <- evalExpr e1
    (PInt val2) <- evalExpr e2
    return $ PInt (val1 `mod` val2)

-- It has to be ints, but it's checked by typechecker
evalExpr (EPow e1 e2) = do
    (PInt val1) <- evalExpr e1
    (PInt val2) <- evalExpr e2
    return $ PInt (val1 ^ val2)

evalExpr (ESqrt e) = do
    (PInt val) <- evalExpr e
    return $ PInt (floor (sqrt (fromIntegral  val)))

evalExpr (EAbs e) = do
    val <- evalExpr e
    return (absolute val)

-- variable
evalExpr (EVar var) = do
    val <- (getValueByIdent var)
    (env, _, s, _, _, _, _, _) <- get
    case val of
        -- if object that we want values, not pointers
        x@(PObject locs) -> return $ PVals (evalObj s x)
        otherwise -> return val



-- Ident"." Ident "[" Exp "]" [Elem];
-- x.tab[][][]
evalExpr that@(EArrField obj@(Ident name) tab@(Ident arr) index which) = do
    what <- return $ (capitFirst name) ++ "." ++ arr
    array <- return $ ((EArr (Ident what) index which))
    look <- toArrIdent False  array
    --val <- (getValueByIdent look)
    (env, _, s, _, _, _, _, _) <- get
    addr <- return $ M.lookup look env
    case addr of
        (Just addr') -> do
            val <- (getValueByIdent look)
            case val of
                x@(PObject locs) -> return $ PVals (evalObj s x)
                otherwise -> return val
        otherwise -> outOfBounds that

-- Ident"." Ident "[" Exp "]" [Elem];
--- tab[][][].x
evalExpr that@(EFieldAr tab@(Ident arr) index which obj@(Ident name)) = do
    array <- return $ ((EArr tab index which))
    (Ident look) <- toArrIdent True array
    table <- return $ Ident $ look ++ "." ++ name
    val <- (getValueByIdent table)
    (env, _, s, _, _, _, _, _) <- get
    addr <- return $ M.lookup table env
    case addr of
        (Just addr') -> do
            val <- (getValueByIdent table)
            case val of
                x@(PObject locs) -> return $ PVals (evalObj s x)
                otherwise -> return val
        otherwise -> outOfBounds that

-- value from cell of array
evalExpr look@(EArr (Ident name) index which) = do
    table <- toArrIdent False  look
    (env, _, s, _, _, _, _, _) <- get
    addr <- return $ M.lookup table env
    case addr of
        (Just addr') -> do
            (Just val) <- return $ M.lookup  addr' s
            case val of
                x@(PObject locs) -> return $ PVals (evalObj s x)
                otherwise -> return val
        otherwise -> outOfBounds look

-- obj.field, operates on fields, because we can have obj in obj
evalExpr (EObj var member) = do
    (env, types, store, classes, _, _, _, _) <- get
    -- Get type name of var
    CustomType (ClassName type') <- return $ fromJust $ M.lookup var types
    -- Class declaration
    class' <- return $ (fromJust $ M.lookup (toIdent type') classes)
    -- Get number (in order) of member in class declataration
    (Just which) <- return $ findPair member class'
    -- Address of particular object
    pos <- return $ fromJust $ M.lookup var env
    -- get what is store in pos addres
    (PObject fields) <- return $ ((fromJust (M.lookup pos store)))
    -- return field
    return (fromJust $ M.lookup (fields !! which) store)

-- fun()
evalExpr (ECall fun) = callFun fun

-- not fully supported - lambda possible only as fun parametr
evalExpr (ELam (LamFun t params e)) = do
    return PEmpty

evalExpr (EScope _) = do
    liftIO $ throwIO $ ErrorCall $  "Scope obecnie nie wspierany"


{-
-------------------------------
------------------------------------------
------------Declarations
----------------------------------------------
------------------------------------------
-------------------------------
-}

-- General functions connected with declarations

execDecl :: Decl -> StateT Kernel IO ()
execDecl (Declare t (vars)) = declaration vars t

-- "Loop" of decl excecution
declaration :: [Var] -> Type -> StateT Kernel IO ()
declaration [] _ = return ()
declaration (d:ds) t = do
    declare t d
    declaration ds t

toBeHide :: Ident -> StateT Kernel IO Bool
toBeHide var = do
    (env, _, _, _, _, _, _, _) <- get
    if(isJust $ M.lookup var env) then return True
    else return False

overrideVar :: Ident -> StateT Kernel IO ()
overrideVar name = do
    exists <- toBeHide name
    if(exists) then do
        (env, types, s, classes, funs, scopes, level, existance) <- get
        val <- getValueByIdent name
        put (env, types, s, classes, funs, scopes, M.insert name val level, existance)
        return ()
    else
        return ()

declare :: Type -> Var ->  StateT Kernel IO ()
-- only declaration
-- int x
declare t (VarName var) = do
    alloc var t
    return ()
-- definiton
-- int x = val
declare t (VarAssin var exp) = do
    alloc var t
    val <- evalExpr exp
    assignment var (casts val t)
    return ()

-- creates new space
alloc :: Ident -> Type -> StateT Kernel IO Loc

-- array
alloc name@(Ident var) (TArray kind dimensions) = do
    exists <- existsInBlock name
    -- that should be done by typechecker
    --if(exists) then liftIO $ throwIO $  ErrorCall  "Próba deklaracji tablicy o już użytej nazwie"
    --else do
    (dims', t) <- treeIntoList [] (TArray kind dimensions)
    array <- listIntoTree(reverse dims') t
    gdzie <- alloc name TInt
    (env, types, s, classes, funs, scopes, level, existance) <- get
    s' <- return $ assinToLoc gdzie (PObject []) s
    put (env, types, s', classes, funs, scopes, level, existance)
    createArray 0 var name array
    return 1

-- Obiekt
alloc var (CustomType (ClassName name)) = do
    exists <- existsInBlock var
    (env, types, s, classes, funs, scopes, level, existance) <- get
    -- that should be done by typechecker
    --if(exists) then liftIO $ throwIO $  ErrorCall  "Próba deklaracji obiektu o już użytej nazwie"
    --else do
    -- look for class definition
    params <- return $ M.lookup (toIdent name) classes
    case params of
        (Just attr) -> do
                -- alloc space for every field of obj
                z <- mapM (uncurry alloc) $ map (\((Ident x),y) ->
                    ((Ident ((fromIdent var) ++ x)),y))  attr
                (env, types, s, classes, funs, scopes, level, existance) <- get
                pos <- return $ loc var env
                -- modify env
                put (M.insert var (justVal pos (M.size env)) env,
                            M.insert var (CustomType (ClassName name)) types,
                            M.insert (justVal pos (M.size env)) (PObject z) s,
                            classes, funs, scopes, level, M.insert var True existance)
                return $ justVal pos (M.size env)
        otherwise -> liftIO $ throwIO $ ErrorCall  "Typ nie istnieje"

-- simple type
alloc var t = do
    overrideVar var
    (env, types, s, classes, funs, scopes, level, existance) <- get
    pos <- return $ loc var env
    --if(exists) then liftIO $ throwIO $  ErrorCall  "Próba deklaracji zmiennej o już użytej nazwie"
    --else do
    put (M.insert var (justVal pos (M.size env)) env, M.insert var t types,
                M.insert (justVal pos (M.size env)) (defaultValue t) s, classes,
                funs, scopes, level, M.insert var True existance)
    return $ justVal pos (M.size env)


-- Generally is not needed, because that's work for typechecker,
-- but it is residuum from times without TypeChecker :)
existsInBlock :: Ident -> StateT Kernel IO Bool
existsInBlock var = do
    (_, _, _, _, _, _, _, exists) <- get
    if(isJust $ M.lookup var exists) then return True
    else return False


-- Translate tree form of array into list form and extract type of array
treeIntoList ::  [Integer] -> Type -> StateT Kernel IO ([Integer], Type)
treeIntoList dims (TArray kind@(TArray _ _) (ArraySymbol e)) = do
    (arr, t) <- (treeIntoList dims kind)
    (PInt n) <- evalExpr e
    return (n:arr, t)

treeIntoList dims (TArray t@_ (ArraySymbol e)) = do
    (PInt n) <- evalExpr e
    return (n:dims,t)


-- List form to tree form
listIntoTree :: [Integer] -> Type -> StateT Kernel IO Type
listIntoTree (x:[]) t = return (TArray t (ArraySymbol (EInt x)))
listIntoTree (x:xs) t = do
    count <- (listIntoTree xs t)
    return (TArray count (ArraySymbol (EInt x)))


createArray :: Int -> String -> Ident -> Type -> StateT Kernel IO Loc
createArray i tab name@(Ident var) (TArray kind (ArraySymbol e)) = do
    -- Creates proper name for cell
    my_name <- return $ ((tab ++ "["++(show i)++"]"))
    index <- alloc (Ident my_name) (TInt)
    (PInt n) <- evalExpr e
    -- Because we can have many dimensions, allocate cells recursively
    x <- mapM (\j -> createArray  (j) my_name name kind) [0..(fromInteger (n-1))]
    (env, types, s, classes, funs, scopes, level, existance) <- get
    s' <- return $ assinToLoc index (PArray x) s
    put(env, types, s', classes, funs, scopes, level, existance)
    return index

createArray i my name@(Ident var) t = do
    l <- alloc (Ident (my ++ "["++(show $i)++"]" ++ var)) t
    (a, b, c, defs, funs, scopes, level, existance) <- get
    (PObject tab) <- getValueByIdent name
    (Just gdzie) <- return $ M.lookup name a
    (env, types, s, classes, funs, scopes, level, existance) <- get
    s' <- return $ assinToLoc gdzie (PObject $ tab++[l]) s
    put (env, types, s', classes, funs, scopes, level, existance)
    return l



-- Functions connected with class declaration

declClass :: ClassHeader -> StateT Kernel IO ()
declClass (ClassDec (ClassName name) body) = do
    (a, b, c, defs, funs, scopes, level, existance) <- get
    -- empty body
    put (a, b, c, M.insert (toIdent name) [] defs, funs, scopes, level, existance)
    execClassBody (toIdent name) body

execClassBody :: Ident -> [ClassStmt] -> StateT Kernel IO ()
execClassBody _ [] = return ()
execClassBody name (x:xs) = do
    declareMembers name x
    execClassBody name xs
    return ()

declareMembers :: Ident -> ClassStmt -> StateT Kernel IO ()
declareMembers name (ClassVar (Declare t [])) = return ()
declareMembers name (ClassVar (Declare t (x:xs))) = do
    (a, b, c, defs, funs, scopes, level, existance) <- get
    let defs' = saveField t x (fromJust $ M.lookup name defs) in
        put(a, b, c, M.insert name defs' defs, funs, scopes, level, existance)
    declareMembers name (ClassVar (Declare t (xs)))
    return ()
    where
        saveField :: Type -> Var -> [ClassMember] -> [ClassMember]
        saveField t (VarName var) list = (var, t):list


-- some helping functions
-- name are rather self explaining
storableToExp :: Storable -> Exp
storableToExp val = case val of
    (PVals vals) -> ECons $ fmap storableToExp vals
    (PBool True) -> Etrue
    (PBool False) -> Efalse
    (PText txt) -> EString txt
    (PDouble d) -> EDouble d
    (PInt i) -> EInt i


translateToDecls :: ((Ident, Type), Exp) -> StateT Kernel IO Inst
translateToDecls ((var, t), e) = do
    val <- evalExpr e
    return (InstrD (Declare t [(VarAssin var (storableToExp val))]))



-- Function declaration
translateFun :: FunArgs -> (Ident, Type)
translateFun (FunVar t var) = (var, t)
translateFun (FunFunny var (FunParamList paramsIn paramsOut)) =
    let ins = fmap translateFun paramsIn
        outs = fmap translateFun paramsOut in
            (var, TFun ins outs)
-- Declare object for returning
returnClass :: (Ident, Type) -> ClassStmt
returnClass (var, t) = ClassVar (Declare t [(VarName var)])

-- heart of fun declaring
declFun :: Fun -> StateT Kernel IO ()
declFun (FunDecl name@(Ident fun) (FunParamList args returns) body) = do
    (env, types, s, classes, funs, scopes, level, existance) <- get

    paramsIn <- return $ fmap translateFun args
    paramsOut <- return $ fmap translateFun returns
    put (env, types, s, classes, M.insert name (body, paramsIn, paramsOut, level) funs,
            scopes, level, existance)
    returns_defs <- return $ fmap returnClass paramsOut
    declClass (ClassDec (ClassName $ capitFirst fun) returns_defs)
    return ()

{-
  Michał Jaroń
  mj348711
  Static control of types.
-}

module TypeChecker where

import Control.Monad.Writer
import Control.Monad.Writer.Lazy
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import Control.Monad.Except
import qualified Data.Map as M
import Data.Char
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
import Data.List
import Data.Maybe
import Errors

type Base = Int

type ClassMember =  (Ident, Type)
type ClassDefs = M.Map Ident [ClassMember]

-- Base is here, because of possible futher modification to variable scopes
type FunDef = ([Inst], [(Ident, Type)], [(Ident, Type)], Base)

type EEnv = M.Map Ident Bool
type FEnv = M.Map Ident FunDef
type TEnv = M.Map Ident Type
type CEnv = ClassDefs

type Kernel = (TEnv, CEnv, FEnv, EEnv, EEnv, Ident)

-- The last Ident is name of function, inside which we are.
-- That's needed to avoid recursive typechecking (in funs)
initKernel :: Kernel
initKernel =  (M.empty, M.empty, M.empty, M.empty, M.empty, Ident "")

-- Some kind of casting and helping functions

tNum = [TInt, TDouble]

trans :: Type -> [Type]
trans TInt = [TInt, TBool, TDouble]
trans TDouble = [TDouble, TBool]
trans TText = [TText, TInt, TBool]
trans TBool = [TBool]
trans (TArray t a)= [(TArray t  a)]
trans (CustomType name) = [CustomType name]

whatType :: [Type] -> Type
whatType [TInt, TBool, TDouble] = TInt
whatType [TDouble, TBool] = TDouble
whatType [TText, TInt, TBool] = TText
whatType [TBool] = TBool
whatType [(TArray t a)]= (TArray t  a)
whatType [(CustomType name)] = CustomType name


commutative :: (([Type] -> Bool), ([Type] -> Bool)) -> ([Type], [Type]) -> Bool
commutative (checkT1, checkT2) (t1, t2) = if (checkT1 t1) &&  (checkT2 t2) then True
    else False

checkCommutative :: (([Type] -> Bool), ([Type] -> Bool)) -> ([Type], [Type]) -> Bool
checkCommutative (checkT1, checkT2) (t1, t2) =
    if (commutative (checkT1, checkT2) (t1, t2)) then True
    else commutative (checkT2, checkT1) (t1, t2)


-- If we are not sure if object exists
getTypeSafe :: Ident -> StateT Kernel IO (Maybe Type)
getTypeSafe name = do
    (types, classes, funs, existance, funex, level) <- get
    res <- return $ M.lookup name types
    return res

getType :: Ident -> StateT Kernel IO Type
getType name = do
    (types, classes, funs, existance, funex, level) <- get
    res <- return $ M.lookup name types
    if(isJust res) then return $ fromJust res
    else undeclaredObjErr name

ifNum :: [Type] -> Bool
ifNum t = if (not $ null $ t `intersect` tNum) then True else False

ifText ::  [Type] -> Bool
ifText t = if (not $ null $ t `intersect` [TText]) then True else False



-- Main functions
typeCorrect :: Prog -> IO [Type]
typeCorrect p =  (liftIO $ evalStateT (checkTypes p) initKernel)


checkTypes :: Prog -> StateT Kernel IO [Type]
checkTypes (Program []) = return []
checkTypes (Program (x:xs)) = checkMultiInstr (x:xs)


checkMultiInstr :: [Inst] ->StateT Kernel IO [Type]
checkMultiInstr [] = return []
checkMultiInstr (x:xs) = do
    checkInstr x
    checkMultiInstr xs


checkBlock :: [Inst] ->StateT Kernel IO [Type]
checkBlock x = do
    (types, classes, funs, existance, funex, level) <- get
    put(types, classes, funs, M.empty, M.empty, level)
    checkMultiInstr x
    put (types, classes, funs, existance, funex, level)
    return []

checkInstr :: Inst -> StateT Kernel IO [Type]
checkInstr (InstrClass c) = declClass True c
checkInstr (InstrD d)= declType d
checkInstr (InstrExpr expr)= exprType expr
checkInstr (InstrS s)= stmtType s
checkInstr (InstrFun fun) = funDeclType fun
checkInstr (InstrLoop loop) = loopType loop
checkInstr (Print e) = checkInstr (InstrExpr e)
checkInstr (PrintLn e) = checkInstr (InstrExpr e)
checkInstr (InstrFor f) = forType f
-- not supported
checkInstr (Read var) = exprType (EVar var)
checkInstr (IApply _ _) = return []
checkInstr (IApplyAn _ _) = return []


{-
-------------------------------
------------------------------------------
----------------------------------------------
   Check function declaration correctness
----------------------------------------------
------------------------------------------
-------------------------------
-}
tranformParam :: (Ident, Type) -> FunArgs
tranformParam (name, t) = case t of
    (TFun ins outs) -> FunFunny name (FunParamList (fmap tranformParam ins) (fmap tranformParam ins))
    otherwise -> FunVar t name

toFunDecl :: (Ident, Type) -> Fun
toFunDecl (name, (TFun ins outs)) =
    (FunDecl name (FunParamList (fmap tranformParam ins) (fmap tranformParam outs)) [])

checkFunBody :: Ident -> StateT Kernel IO [Type]
checkFunBody name@(Ident fun) = do
        (types, classs, funs, existance, funex, level) <- get
        Just (body, paramsIn, paramsOut, _) <- return $ M.lookup name funs
        (functions, values) <- return $ partition (divideFun) paramsIn
        instrDecl <- return $ fmap translateToDecls values
        bodies <- return $ fmap toFunDecl functions
        (_, cl', f', _, _, _) <- get
        put (M.empty, cl', f', M.empty, M.empty, name)
        checkMultiInstr $ fmap (\x -> InstrFun x) bodies
        toReturn <- return $ InstrD (Declare (CustomType (ClassName (capitFirst fun)))
            [(VarName (Ident "returns"))])
        checkMultiInstr ([toReturn] ++ instrDecl)
        checkBlock body
        put (types, classs, funs, M.empty, funex, level)
        return []

funDeclType :: Fun -> StateT Kernel IO [Type]
funDeclType (FunDecl name@(Ident fun) params body) = do
    (types, classes, funs, existance, funex, level) <- get
    name_fun <- return $ M.lookup name funex
    -- check if name of fun is unique
    case name_fun of
        (Just x) -> duplicatedFunErr fun
        Nothing -> return []
    -- Check all parametrs correctness
    checkFunParams fun params
    -- Everything seems to be ok, declare fun
    (FunParamList ins outs) <- return params
    ins' <- return $ fmap translateFun ins
    outs' <- return $ fmap translateFun outs
    put(types, classes, M.insert name (body, ins', outs' ,0) funs,
        existance, M.insert name True funex, level)
    -- create returns values class
    returns_defs <- return $ fmap returnClass outs'
    declClass False (ClassDec (ClassName $ capitFirst fun) returns_defs)
    checkFunBody name
    return []

-- helper functions for calling

-- That's how we decode returns object of functions
capitFirst :: String -> String
capitFirst x = (toUpper $ head x):(tail x)

-- Divide functions from objects
isFun :: FunArgs -> Bool
isFun (FunVar _ _) = False
isFun _ = True

hasDupl :: (Eq a) => [a] -> Bool
hasDupl xs = not $ nub xs == xs


checkFunParams :: String -> ParamsList -> StateT Kernel IO [Type]
checkFunParams fun what@(FunParamList ins outs) = do
    (funs, vars) <- return $ partition isFun ins
    -- Input parametrs
    -- unique name of variable parametrs
    duplicate <- return $ hasDupl (fmap (\(FunVar t name) -> name) vars)
    -- unique name of fun's parametrs
    duplicateFuns <- return $ hasDupl (fmap (\(FunFunny name _) -> name) funs)
    if duplicateFuns then duplicatedFunParamErr fun
    else do
        case duplicate of
            True -> duplicatedFunParamErr fun
            False -> do
                -- Outputs parametrs
                (funs, vars) <- return $ partition isFun outs
                duplicate <- return $ hasDupl (fmap (\(FunVar t name) -> name) vars)
                case duplicate of
                    True -> duplicatedFunParamErr fun
                    False -> do
                        assin <- return $ not $ null $ funs
                        -- funtion to be returned - not supperted yet
                        if (assin) then funReturnErr fun
                        else return []



-- Declare Fun
translateFun :: FunArgs -> (Ident, Type)
translateFun (FunVar t var) = (var, t)
translateFun (FunFunny var (FunParamList paramsIn paramsOut)) =
    let ins = fmap translateFun paramsIn
        outs = fmap translateFun paramsOut in
            (var, TFun ins outs)

returnClass :: (Ident, Type) -> ClassStmt
returnClass (var, t) = ClassVar (Declare t [(VarName var)])


{-
-------------------------------
------------------------------------------
----------------------------------------------
   Class definition
----------------------------------------------
------------------------------------------
-------------------------------
-}

declClass ::  Bool -> ClassHeader -> StateT Kernel IO [Type]
declClass check (ClassDec (ClassName name) fields) = do
    (types, classes, funs, existance, funex, level) <- get
    memebers <- (mapM (declFields name) fields)
    -- Check if name is unique
    exists <- return $ M.lookup (Ident name) classes
    case exists of
        (Just x) -> if (check) then duplicatedClassErr name else do
                    classes' <- return $  M.insert (Ident name) (foldl (++) [] memebers )classes
                    put (types, classes', funs, existance, funex,level)
                    return []
        Nothing -> do
            classes' <- return $  M.insert (Ident name) (foldl (++) [] memebers )classes
            put (types, classes', funs, existance, funex,level)
            return []

addField :: String -> Type -> Var -> StateT Kernel IO ClassMember
addField _ t (VarName name) = return $ (name, t)
addField name t (VarAssin _ _) = funAsFieldErr name

declFields :: String -> ClassStmt -> StateT Kernel IO [ClassMember]
declFields name (ClassVar (Declare t vars)) = do
     fields <- mapM (addField name t) vars
     return fields


{-
-------------------------------
------------------------------------------
----------------------------------------------
   Block of Expressions
----------------------------------------------
------------------------------------------
-------------------------------
-}


-- Wrappers are here because, some functions are translated to another,
-- but still in case of error we want to display orginal source of error
exprWrapper :: Exp -> Exp -> StateT Kernel IO [Type]
exprWrapper origin e@(LogicalAnd e1 e2) = do
    te1 <- exprType e1
    te2 <- exprType e2
    -- If we are capable to cast values to bool
    if(TBool `elem` te1 && TBool `elem` te2) then return $ trans TBool
    else notLogicalErr origin

exprWrapper origin e@(Eeq e1 e2) = do
    te1 <- exprType e1
    te2 <- exprType e2
    -- same type
    if(whatType te1 == whatType te2) then do
        case whatType te1 of
            -- array comparing is not possible
            (TArray t a) -> arrEqErr e
            otherwise -> return $ trans TBool
    else do
        if(whatType te1 == TBool && TBool `elem` te2) ||
            (whatType te2 == TBool && TBool `elem` te1) then return $ trans TBool
        else do
            if(whatType te1 == TDouble && TDouble `elem` te2) ||
                (whatType te2 == TDouble && TDouble `elem` te1) then return $ trans TBool
            else do
                if(whatType te1 == TInt && TInt `elem` te2) ||
                    (whatType te2 == TInt && TInt `elem` te1) then return $ trans TBool
                else eqErr origin

exprWrapper origin (ELess e1 e2) = do
    te1 <- exprType e1
    te2 <- exprType e2
    if (ifNum te1 && ifNum te2) then do
        if(whatType te1 == TDouble || whatType te2 == TDouble) &&
            (whatType te1 == TText || whatType te2 == TText)
            then  eqErr origin
        else
            return $ trans TBool
    else eqErr origin

exprWrapper origin (EMod e1 e2) = do
    te1 <- exprType e1
    te2 <- exprType e2
    if(whatType te1 == TInt && whatType te2 == TInt) then return $ trans TInt
    else arithErr origin


exprType :: Exp -> StateT Kernel IO [Type]

exprType (EString _) = return $ trans TText
exprType (EInt _) = return $ trans TInt
exprType (EDouble _) = return $ trans TDouble
exprType (Etrue) = return $ trans TBool
exprType (Efalse) = return $ trans TBool


exprType (EVar name) = do
    t <- getTypeSafe name
    case t of
        (Just x) -> return $ trans $ fromJust t
        otherwise -> undeclaredVarErr name

exprType (EObj obj field) = do
    t <- objType (EObj obj field)
    return $ trans t

-- Ident"." Ident "[" Exp "]" [Elem];
--- tab[][][].x
exprType (EFieldAr tab@(Ident arr) index which obj@(Ident name)) = do
    t <- arrayBounds (EArr tab index which)
    return $ trans t

-- Ident"." Ident "[" Exp "]" [Elem];
-- x.tab[][][]
exprType s@(EArrField obj@(Ident name) tab@(Ident arr) index which) = do
    objT <- objType (EObj obj tab)
    case objT of
        table@(TArray t a) -> do
            objT <- return $ traverseArray table
            return $ trans objT
        otherwise ->  notArrErr arr

exprType (EArr name index which) = do
    t <- arrayBounds (EArr name index which)
    return $ trans t

-- Jeżeli oba wyrażanie mogą być rzutowane do bool
exprType e@(LogicalAnd e1 e2) = exprWrapper e e
exprType e@(LogicalOr e1 e2) = exprWrapper e (LogicalAnd e1 e2)

exprType x@(LogicalNot e) = do
    t <- exprType e
    if(TBool `elem` t) then return $ trans TBool
    else notLogicalSingleErr x

exprType x@(ECons es) = do
    return [TBool, TInt, TText, TDouble]

exprType e@(Eeq e1 e2) = exprWrapper e e
exprType e@(Eneq e1 e2) = exprWrapper e (Eeq e1 e2)

-- Capablitiy of transforming to int/double
exprType e@(ELess e1 e2) = exprWrapper e e
exprType e@(EBigg e1 e2) = exprWrapper e (ELess e1 e2)
exprType e@(ELessEq e1 e2) = exprWrapper e (ELess e1 e2)
exprType e@(EBiggEq e1 e2) = exprWrapper e (ELess e1 e2)

-- If both evaluations are numeric types or both are Text
exprType e@(EAdd e1 e2) = do
    te1 <- exprType e1
    te2 <- exprType e2
    if(whatType te1 == TBool || whatType te1 == TBool) then arithErr e
    else do
        if(ifText te1 || ifText te2) then return $ trans TText
        else do
            if (whatType te1 == TDouble || whatType te2 == TDouble) then return $ trans TDouble
            else return $ trans TInt

exprType e@(ESub e1 e2) = do
    te1 <- exprType e1
    te2 <- exprType e2
    if(ifNum te1 && ifNum te2) then do
        if(whatType te1 == TText || whatType te2 == TText) then arithErr e
        else do
            if (whatType te1 == TDouble || whatType te2 == TDouble) then return $ trans TDouble
            else return $ trans TInt
    else arithErr e

exprType e@(EMul e1 e2) = do
    te1 <- exprType e1
    te2 <- exprType e2
    if(whatType te1 == TText && whatType te2 == TText) then arithErr e
    else do
        if (ifNum te1 && ifNum te2) then do
            if (whatType te1 == TDouble || whatType te2 == TDouble) then return $ trans TBool
            else if (TText `elem` te1 || TText `elem` te2) then return $ trans TText
            else return $ trans TInt
        else arithErr e

exprType e@(EDiv e1 e2) = do
    te1 <- exprType e1
    te2 <- exprType e2
    if (ifNum te1 && ifNum te2) then do
        if(whatType te1 == TText || whatType te2 == TText) then arithErr e
        else do
            if (whatType te1 == TDouble || whatType te2 == TDouble) then return $ trans TBool
            else return $ trans TInt
    else arithErr e

exprType e@(EMod e1 e2) = exprWrapper e e

exprType e@(EPow e1 e2) = exprWrapper e (EMod e1 e2)

exprType what@(ESqrt e) = do
    t <- exprType e
    if(whatType t == TInt) then return $ trans TInt
    else arithErr what

exprType what@(EAbs e) = do
    t <- exprType e
    if(whatType t == TInt || whatType t == TDouble) then return t
    else arithErr what

-- Only variable++ suported
exprType s@(Epreinc e) = do
    case e of
        (EVar _) -> exprType (EAdd e (EInt 1))
        otherwise -> incErr s

exprType s@(Epredec e) = do
    case e of
        (EVar _) -> exprType (ESub e (EInt 1))
        otherwise -> incErr s

exprType (Epostinc e) = exprType (Epreinc e)
exprType (Epostdec e) = exprType (Epredec e)


---------------------------
---- funciton calling
---------------------------
exprType call@(ECall (CallFun name@(Ident fun) params)) = do
    (types, classs, funs, existance, funex, level) <- get
    fun_def <- return $ M.lookup name funs
    case fun_def of
        Nothing -> undeclaredCallErr name
        -- Info about base fun
        (Just (body, paramsIn, paramsOut, _)) -> do
            -- Check if definied params and real params has matching types
            real_params <- mapM paramType (fmap (\(RealParamVal e) -> e) params)
            def_params <- return $ fmap (\(name, t) -> t) paramsIn
            cmp <- return $ zip (fmap blankIdent def_params) real_params
            gat <- return $ fmap commonType cmp
            -- same number of params?
            if(not (length real_params  == length def_params)) then paramsNumErr call
            else
                if (any (==False) gat) then unmatchingParamsErr call
                else do
                    rets <- return $ paramsOut
                    -- extract type of return
                    if(length rets == 1) then do
                        (_, rets) <- return $ head rets
                        return $ trans rets
                    else return $ trans (CustomType (ClassName (capitFirst fun)))


-- Classes for return
attachClass :: (Ident, [ClassMember]) -> StateT Kernel IO Type
attachClass (name, body) = do
    (types, classs, funs, existance, funex, level) <- get
    put(types, M.insert name body classs, funs, existance, funex, level)
    return TInt

-- Lambda function to named function
transLam :: (Exp, Ident) -> StateT Kernel IO Exp
transLam ((EVar x), _) = return (EVar x)
transLam (ELam (LamFun t params e), var@(Ident name)) = do
    (types, classs, funs, existance, funex, level) <- get
    parametrs <- return $ fmap (\(LambdaParam tt i) -> (i, tt)) params
    def <- return $ ([], parametrs, [(Ident "x", t)], 0)
    put (types, classs, M.insert var def funs, existance, funex,level)
    return (EVar var)

-- Returins value of name
getClass :: CEnv -> (Ident) -> [ClassMember]
getClass env name = let look = M.lookup name env in
    case look of
        (Just x) -> x
        otherwise -> []

attachBody :: (Ident, FunDef) -> StateT Kernel IO Type
attachBody (name, body) = do
    (types, classs, funs, existance, funex, level) <- get
    put(types, classs, M.insert name body funs, existance, funex, level)
    return TInt

-- body of particular fun
findBody :: FEnv -> Ident -> FunDef
findBody env name = let look = M.lookup name env in
    case look of
        (Just x) -> x
        otherwise -> ([],[],[],0)

-- Used for list manipulation - take olny listed indexes from list
takeIndexes :: [Int] -> [a] -> [a]
takeIndexes [] _  = []
takeIndexes (x:xs) list = (list !! x):(takeIndexes xs list)

-- Used in dividinf functions from concrete objects
funType :: [Type] -> Bool
funType  [(TFun ins outs)] = True
funType _ = False

translateToDecls :: (Ident, Type) -> Inst
translateToDecls (var, t) = (InstrD (Declare t [(VarName var )]))

divideFun :: (Ident, Type) -> Bool
divideFun (name, TFun _ _) = True
divideFun _= False


-- Needed for checking if function parametr has the same type,
-- names of parametrs aren't important
blankIdent :: Type -> Type
blankIdent (TFun ins outs) =
    (TFun (fmap (\(var, t) -> (Ident "", t)) ins) (fmap (\(var, t) -> (Ident "", t)) outs))
blankIdent t = t


-- Used for checking list of fun params
paramType :: Exp -> StateT Kernel IO [Type]
--paramType (ECall (CallFun name _)) = do
--    return []
paramType x@(ELam (LamFun t params e)) = do
    ins <- return $ fmap (\(LambdaParam tt var) -> (var,tt)) params
    (types, classs, funs, existance, funex, level) <- get
    (var,tt) <-return $  head ins
    put(M.insert var tt types, classs, funs, existance, funex, level)
    et <- exprType e
    put (types, classs, funs, existance, funex, level)
    if(not $ commonType (t,et)) then lambdaErr x
    else return [(TFun (fmap (\(var, t) -> (Ident "", t)) ins) [(Ident "", t)])]

paramType e@(EVar var)  = do
    (types, classs, funs, existance, funex, level) <- get
    exists <- getTypeSafe var
    case exists of
        (Just x) ->  exprType e
        otherwise -> do
            (types, classs, funs, existance, funex, level) <- get
            already_exists <- return $ M.lookup var funs
            case already_exists of
                (Just (_, paramsIn, paramsOut, _)) -> do
                    extractIn <- return $ fmap (\(var, t) -> (Ident "", t)) paramsIn
                    extractOut <- return $ fmap (\(var, t) -> (Ident "", t)) paramsOut
                    return [(TFun extractIn extractOut)]
                otherwise -> undeclaredCallErr e

paramType e = exprType e

commonType :: (Type,[Type]) -> Bool
commonType (t, types) = if t `elem` types then True else False
---------------------------
---- obj.field
---------------------------
-- Type of obj.field
objType :: Exp -> StateT Kernel IO Type
objType what@(EObj obj field) = do
    a@(types, classes, funs, existance, funex, level) <- get
    x <- getTypeSafe obj
    case x of
        (Just t) -> do
            case t of
                (CustomType (ClassName className)) -> do
                    def <- return $ M.lookup (Ident className) classes
                    case def of
                        (Just fields) -> do
                            exists <- findPair className fields field
                            return exists
                        otherwise -> undeclaredClassErr (className)
                otherwise -> notObjErr obj
        otherwise -> notObjErr obj

findPair :: String -> [ClassMember] -> Ident -> StateT Kernel IO Type
findPair name [] x = notFieldErr name x
findPair name ((field, fieldType):xs) match = if(field == match) then return fieldType
    else findPair name xs match

-----------------------------
----- Array calling
-----------------------------
arrayBounds :: Exp -> StateT Kernel IO Type
arrayBounds e@(EArr name index which) = do
    (types, classes, funs, existance, funex, level) <- get
    t <- getType name
    case t of
        (TArray tt arr) -> do
            ts <- return $ traverseArray t
            return ts
        otherwise -> notArrErr name

-- Extract type of array
traverseArray :: Type -> Type
traverseArray (TArray kind@(TArray _ _) (ArraySymbol n)) =
        let t = (traverseArray kind) in
        (t)

traverseArray (TArray t@_ (ArraySymbol n)) = t


{-
-------------------------------
------------------------------------------
----------------------------------------------
   Block of declarations
----------------------------------------------
------------------------------------------
-------------------------------
-}


declType :: Decl -> StateT Kernel IO [Type]
declType (Declare t (vars)) = do
        decl vars t

-- Main "loop" of declarations
decl :: [Var] -> Type -> StateT Kernel IO [Type]
decl [] _ = return []
decl (d:ds) t = do
    (_, classes, funs, existance, funex, level) <- get
    case t of
        (CustomType (ClassName name)) -> do
            if(isJust $ M.lookup (Ident name) classes) then do
                declV t d
                decl ds t
            else undeclaredClassErr name
        otherwise -> do
            declV t d
            decl ds t


declV :: Type -> Var ->  StateT Kernel IO [Type]
-- just declaration
declV t (VarName var) = do
    (types, classes, funs, existance, funex, level) <- get
    if(isJust $ M.lookup var existance) then duplicatedVarErr var
    else do
        -- Include existance
        put (M.insert var t types, classes, funs, M.insert var True existance, funex, level)
        return [t]

--definition
declV t s@(VarAssin var exp) = do
    (types, classes, funs, existance, funex, level) <- get
    te <- exprType exp
    if(not $ t `elem` te) then notAssinErr te t s
    else do
        if(isJust $ M.lookup var existance) then duplicatedVarErr var
        else do
            put (M.insert var t types, classes, funs, M.insert var True existance, funex, level)
            val <- exprType exp
            stmtType (VAssin var exp)
            return [t]


{-
-------------------------------
------------------------------------------
----------------------------------------------
   Block of Stmts
----------------------------------------------
------------------------------------------
-------------------------------
-}

stmtType :: Stmt -> StateT Kernel  IO [Type]

--Ident "=" Exp ";";
stmtType s@(VAssin var e) = do
    (types, classes, funs, existance, funex, level) <- get
    t <- getType var
    te <- exprType e
    if(t `elem` te) then return [t]
    else notAssinErr te t s

--Ident"." Ident  "[" Exp "]" [Elem] "=" Exp ";";
stmtType s@(VAssinArrField obj@(Ident name) tab@(Ident arr) index which exp) = do
    objT <- objType (EObj obj tab)
    te <- exprType exp
    case objT of
        table@(TArray t a) -> do
            objT <- return $ traverseArray table
            if(objT `elem` te) then return [objT]
            else notAssinFieldErr objT te s
        otherwise ->  notArrErr s

-- Ident "[" Exp "]" [Elem]"."Ident  "=" Exp ";";
stmtType s@(VAssinFieldArr name index which obj exp) = do
    (types, classes, funs, existance, funex, level) <- get
    t <- arrayBounds (EArr name index which)
    te <- exprType exp

    (CustomType (ClassName className)) <- return t
    def <- return $ M.lookup (Ident className) classes
    case def of
        (Just fields) -> do
            exists <- findPair className fields obj
            if(exists `elem` te) then return [exists]
            else notAssinFieldErr t te s
        otherwise -> undeclaredClassErr (className)

-- obj.field = val
stmtType s@(FieldAssin obj field e) = do
    objT <- objType (EObj obj field)
    te <- exprType e
    if(objT `elem` te) then return [objT]
    else notAssinFieldErr objT te s

-- tab[] = val
stmtType s@(VAssinArr name index which e) = do
    t <- arrayBounds (EArr name index which)
    te <- exprType e
    if(t `elem` te) then return [t]
    else notAssinFieldErr t te s

--x+=
stmtType (VAssinPlus var e) = do
    (types, classes, funs, existance, funex, level) <- get
    t <- getType var
    exprType (EAdd (EInt 0) e)

-- x -=
stmtType (VAssinMinus  var e) = do
    (types, classes, funs, existance, funex, level) <- get
    t <- getType var
    exprType (ESub (EInt 0) e)

-------------------
---------- block of ifs
-------------------
stmtType s@(SIfThreeMore e inst elifs elsei) = do
    te <- exprType e
    -- main if cond has to be Bool value
    if(not $ TBool `elem` te) then condErr s
    else do
        elseifs <- return $ fmap collectConds elifs
        x <- mapM (exprType) elseifs
        -- if every cond has ability to be casted to bool
        if (any (== False )  (fmap checkBool x)) then condErr s
        else do
            -- check body of very block
            checkBlock inst
            mapM checkBlock (fmap extractInstr elifs)
            checkBlock elsei
            return $ trans TBool
        where
            -- used functions
            checkBool x = if TBool `elem` x then True else False
            collectConds (SElIf exp instr) = exp
            collectConds (SElIfMore exp instrs) = exp
            extractInstr (SElIf exp instr) = [instr]
            extractInstr (SElIfMore exp instrs) = instrs

stmtType (SIfTwo e inst elifs elsei) = stmtType (SIfThreeMore e [inst] elifs [elsei])
stmtType (SIfFourMore e inst elifs elsei) = stmtType (SIfThreeMore e [inst] elifs elsei)
stmtType (SIfOne e inst elifs) = stmtType (SIfThreeMore e [inst] elifs [])
stmtType (SIfTwoMore e inst elifs elsei) = stmtType (SIfThreeMore e inst elifs [elsei])
stmtType (SIfOneMore e inst elifs) = stmtType (SIfThreeMore e inst elifs [])

-- Tranlsated to ifs
stmtType (Switch exp cases elseBlock) = do
    (CasesVal first_e inst1) <- return $ head cases
    trans <- return $ fmap (\(CasesVal e instrs)-> SElIfMore (Eeq e exp) instrs) (tail cases)
    stmtType (SIfThreeMore (Eeq exp first_e) inst1 trans elseBlock)

stmtType (Switch2 e cases) = stmtType (Switch e cases [])

-----------------------------
-----------Block of for
-----------------------------
forType  :: For -> StateT Kernel  IO [Type]
forType (ForTo t var init cond instr) =
    forType (ForToMulti t var init cond [instr])

forType f@(ForToMulti t var init cond instr) =
    if (not $ t `elem` [TInt, TText]) then
        forToErr t f
    else do
        --typ <- exprType cond
        --if(t `elem` typ) then do
        inst <- return $  [InstrD ( Declare t [VarAssin var init])]
        work <- return $ InstrExpr $ Epostinc (EVar var)
        checkBlock (inst ++ instr ++ [work])
        --else forTypesErr f

forType (ForDownTo t var init cond instr) =
    forType (ForDownToMulti t var init cond [instr])

forType f@(ForDownToMulti t var init cond instr) =
    if (not $ t `elem` [TInt]) then
        forToErr t f
    else do
        --typ <- exprType cond
        --if(t `elem` typ) then do
        inst <- return $  [InstrD ( Declare t [VarAssin var init])]
        work <- return $ InstrExpr $ Epostdec (EVar var)
        checkBlock (inst ++ instr ++ [work])
        --else forTypesErr f
-----------------------------
-----------Block of loops
-----------------------------
loopType :: Loop -> StateT Kernel  IO [Type]
loopType s@(LoopInst loopvars exp how instr) =
    loopType (LoopInstMulti loopvars exp how [instr])


loopType s@(LoopInstMulti loopvars exp how instrs) = do
    (types, classes, funs, existance, funex, level) <- get
    put (types, classes, funs, M.empty, M.empty, level)
    decls <- return $  varsLoop loopvars
    names <- return $ fmap (\(n, t, e)->n) decls
    -- every name unique?
    duplicate <- return $ hasDupl names
    if(duplicate) then duplicatedLoopParamErr s
    else do
        -- check instructions to do every iteration
        mapM checkInstr (fmap tranlsateLoop decls)
        cond <- exprType exp
        -- cond is bool
        if(not $ TBool `elem` cond) then condErr s
        else do
            work <- return $ parseWork how
            real_instrs <- return $ fmap tranlsateHow work
            checkMultiInstr real_instrs
            checkBlock instrs
            put (types, classes, funs, existance, funex, level)
            return []

-- Instruction in "work" section
divideWork :: IterExp -> Bool
divideWork (IterExpr _) = True
divideWork (IterExpr2 _) = False


parseWork :: LoopHow -> [IterExp]
parseWork LoopDoEps = []
parseWork (LoopDo x) = x

-- Declarations for loop
varsLoop :: LoopVars -> [(Ident, Type, Exp)]
varsLoop LoopDeclEps = []
varsLoop (LoopDecli vars) =
    let x = fmap (\(DeclareLoop t v) -> (t,v)) vars
        y = [(name, z, e) |  (z,vs) <- x, (LoopAssin name e)<- vs] in
        y

tranlsateHow :: IterExp -> Inst
tranlsateHow (IterExpr e) = (InstrExpr e)

tranlsateHow (IterExpr2 (VAssinPlusL var e)) = (InstrS (VAssinPlus var e))
tranlsateHow (IterExpr2 (VAssinMultiL var e)) = (InstrS (VAssinMulti var e))
tranlsateHow (IterExpr2 (VAssinMinusL var e)) = (InstrS (VAssinMinus var e))
tranlsateHow (IterExpr2 (VAssinDivL var e)) = (InstrS (VAssinDiv var e))


tranlsateLoop :: (Ident, Type, Exp) -> Inst
tranlsateLoop (name, t, val) = (InstrD (Declare t [(VarAssin name val)]))

allowedLoopExp :: Exp -> Bool
allowedLoopExp (Epreinc _) = True
allowedLoopExp (Epredec _) = True
allowedLoopExp (Epostinc _) = True
allowedLoopExp (Epostdec _) = True

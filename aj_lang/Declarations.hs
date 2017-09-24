module Declarations where

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

import Core
import Storables
import Expressions


-- General functions connected with declarations
execDecl :: Decl -> StateT Kernel IO ()
execDecl (Declare t (vars)) = declaration vars t

declaration :: [Var] -> Type -> StateT Kernel IO ()
declaration [] _ = return ()
declaration (d:ds) t = do
    declare t d
    declaration ds t

declare :: Type -> Var ->  StateT Kernel IO ()
declare t (VarName var) = do
    alloc var t
    return ()

declare t (VarAssin var exp) = do
    alloc var t
    val <- evalExpr exp
    assignment var val
    return ()

-- creates new space
alloc :: Ident -> Type -> StateT Kernel IO Loc

--array
alloc name@(Ident var) (TArray kind dimensions) = do
    exists <- existsInBlock name
    if(exists) then liftIO $ throwIO $  ErrorCall  "Próba deklaracji tablicy o już użytej nazwie"
    else do
        (dims', t) <- return $ treeIntoList [] (TArray kind dimensions)
        array <- return $ listIntoTree(reverse dims') t
        createArray 1 var name array
        return 1

-- Obiekt
alloc var (CustomType (ClassName name)) = do
    exists <- existsInBlock var
    (env, types, s, classes, funs, scopes, level) <- get
    -- Find declaration of class
    if(exists) then liftIO $ throwIO $  ErrorCall  "Próba deklaracji obiektu o już użytej nazwie"
    else do
        params <- return $ M.lookup (toIdent name) classes
        case params of
            (Just attr) -> do
                    z <- mapM (uncurry alloc) $ map (\((Ident x),y) -> ((Ident ((fromIdent var) ++ x)),y))  attr
                    (env, types, s, classes, funs, scopes, level) <- get
                    pos <- return $ loc var env
                    put (M.insert var (justVal pos (M.size env)) env, M.insert var (CustomType (ClassName name)) types,
                                M.insert (justVal pos (M.size env)) (PObject z) s, classes, funs, scopes, level)
                    return $ justVal pos (M.size env)
            otherwise -> liftIO $ throwIO $ ErrorCall  "Typ nie istnieje"


alloc var t = do
    (env, types, s, classes, funs, scopes, level) <- get
    pos <- return $ loc var env
    exists <- existsInBlock var
    if(exists) then liftIO $ throwIO $  ErrorCall  "Próba deklaracji zmiennej o już użytej nazwie"
    else do
        --CHANGE
        put (M.insert var (justVal pos (M.size env)) env, M.insert var t types,
                    M.insert (justVal pos (M.size env)) (defaultValue t) s, classes, funs, scopes, level)
        return $ justVal pos (M.size env)


existsInBlock :: Ident -> StateT Kernel IO Bool
existsInBlock var = do
    (_, t, _, _, _, _, _) <- get
    if(isJust $ M.lookup var t) then return True
    else return False



-- Functions connected with class declaration

declClass :: ClassHeader -> StateT Kernel IO ()
declClass (ClassDec (ClassName name) body) = do
    (a, b, c, defs, funs, scopes, level) <- get
    put (a, b, c, M.insert (toIdent name) [] defs, funs, scopes, level)
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
    (a, b, c, defs, funs, scopes, level) <- get
    let defs' = saveField t x (fromJust $ M.lookup name defs) in
        put(a, b, c, M.insert name defs' defs, funs, scopes, level)
    declareMembers name (ClassVar (Declare t (xs)))
    return ()
    where
        saveField :: Type -> Var -> [ClassMember] -> [ClassMember]
        saveField t (VarName var) list = (var, t):list


-- Functions connected with fun declaration

translateToDecls :: ((Ident, Type), Exp) -> Inst
translateToDecls ((var, t), val) = (InstrD (Declare t [(VarAssin var (evalExpr val)))]))

extraParams :: ExtraParams -> Bool
extraParams MoreParams= True
extraParams _ = False

translateFun :: FunArgs -> (Ident, Type)
translateFun (FunVar t var) = (var, t)
translateFun (FunFunny var (FunParamList paramsIn inMore paramsOut outMore)) =
    let ins = fmap translateFun paramsIn
        outs = fmap translateFun paramsOut in
            (var, TFun ins outs (extraParams inMore) (extraParams outMore))


returnClass :: (Ident, Type) -> ClassStmt
returnClass (var, t) = ClassVar (Declare t [(VarName var)])

declFun :: Fun -> StateT Kernel IO ()
declFun (FunDecl name@(Ident fun) (FunParamList args more1 returns more2) body) = do
    (env, types, s, classes, funs, scopes, level) <- get
    paramsIn <- return $ fmap translateFun args
    paramsOut <- return $ fmap translateFun returns
    put (env, types, s, classes, M.insert name (body, paramsIn, paramsOut, level) funs, scopes, level)
    returns_defs <- return $ fmap returnClass paramsOut
    declClass (ClassDec (ClassName $ "#" ++ fun) returns_defs)
    return ()


-- Przetłumacz drzewo tablicy do listy z wymiarami i ostatecznym typem tablicy
treeIntoList ::  [Integer] -> Type -> ([Integer], Type)
treeIntoList dims (TArray kind@(TArray _ _) (ArraySymbol n)) =
    let (arr, t) = (treeIntoList dims kind) in
    (n:arr, t)

treeIntoList dims (TArray t@_ (ArraySymbol n)) = (n:dims,t)


-- Zmień reprezantacje tablicową tablicy na drzewo
listIntoTree :: [Integer] -> Type -> Type
listIntoTree (x:[]) t = (TArray t (ArraySymbol x))
listIntoTree (x:xs) t = (TArray (listIntoTree xs t) (ArraySymbol x))


createArray :: Int -> String -> Ident -> Type -> StateT Kernel IO Loc
createArray i tab name@(Ident var) (TArray kind (ArraySymbol n)) = do
    my_name <- return $ ((tab ++ "["++(show $i-1)++"]"))
    index <- alloc (Ident my_name) (TInt)
    x <- mapM (\j -> createArray  (j+i) my_name name kind) [1..(fromInteger n)]
    (env, types, s, classes, funs, scopes, level) <- get
    s' <- return $ assinToLoc index (PArray x) s
    put(env, types, s', classes, funs, scopes, level)
    return index

createArray i my name@(Ident var) t = do
    alloc (Ident (my ++ "["++(show $i-1)++"]" ++ var)) t

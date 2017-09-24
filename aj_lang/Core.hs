module Core where

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

import Storables


type ClassMember =  (Ident, Type)
type ClassDefs = M.Map Ident [ClassMember]

type FunDef = ([Inst], [(Ident, Type)], [(Ident, Type)], Level)

type VEnv = M.Map Ident Loc
type TEnv = M.Map Ident Type
type CEnv = ClassDefs
type FEnv = M.Map Ident FunDef
type Scope = [(VEnv, Store)]
type Level = M.Map Ident Storable
type Existance = M.Map Ident Bool

type Store = M.Map Loc Storable

type Kernel = (VEnv, TEnv, Store, CEnv, FEnv, Scope, Level, Existance)


emptyKernel :: Kernel
emptyKernel = (M.empty, M.empty, M.empty, M.empty, M.empty, [], M.empty, M.empty)


getType :: Ident -> StateT Kernel IO Type
getType var = do
    (_, t, _, _, _, _, _, _) <- get
    return $ fromJust $ M.lookup var t
-- returns address of given variable
loc :: Ident -> VEnv -> Maybe Loc
loc var env = M.lookup var env

getValueByLoc :: Loc -> Store -> Storable
getValueByLoc pos s = let value = M.lookup pos s in
                        case value of
                            (Just x) -> x
                            otherwise -> PEmpty

-- Extratrcts vaar with name ident from store
getValueByIdent :: Ident -> StateT Kernel IO Storable
getValueByIdent var = do
    (env, _, s, _, _, _, _, _) <- get
    exists <- return $ loc var env
    case exists of
        (Just pos)  -> return $ getValueByLoc pos s
        otherwise   -> liftIO $ throwIO $ ErrorCall "Zmienna nie istnieje"


-- Put val into loc in store s
assinToLoc :: Loc -> Storable -> Store -> Store
assinToLoc addr val s = M.insert addr val s



findPair :: (Eq a) => a -> [(a,b)] -> Maybe Int
findPair cmp list = findIndex (\(p,q) -> if(p == cmp) then True else False) list

-- Custom eq operator
cmpStorable :: Store -> Storable -> Storable -> Bool
cmpStorable s x@(PObject a) y@(PObject b) = (evalObj s x) == (evalObj s y)
cmpStorable s a b = a == b

-- Redukuje obiekt do typów prostych
evalObj :: Store -> Storable -> [Storable]
evalObj s (PObject a) = foldl (++) [] (fmap (evalObj s) (fmap (flip getValueByLoc s) a))
evalObj s a = return a



evalLoc :: Store -> Loc -> [Loc]
evalLoc s l = let x = fromJust $  M.lookup l s in
                case x of
                    (PObject locs) -> foldl (++) [] (fmap ((evalLoc s)) locs)
                    otherwise -> [l]

copyObj :: Store -> [Loc] -> [Storable] -> Store
copyObj s [] _= s
copyObj s (x:xs) (y:ys) = let s' = assinToLoc x y s in
                            copyObj s' (xs) (ys)

assignment :: Ident -> Storable ->  StateT Kernel IO Storable

assignment var obj@(PVals vals) = do
    (env, types, s, classes, funs, scopes, level, existance) <- get
    basic <- return $ vals
    (Just pos) <- liftM2 loc (return var) (return env)
    (PObject thats) <- return $ fromJust $ M.lookup pos s
    here <- return $  foldl (++) [] $ fmap (evalLoc s) thats
    put (env, types, copyObj s here basic, classes, funs, scopes, level, existance)
    return PEmpty

assignment var obj@(PObject locs) = do
    (env, types, s, classes, funs, scopes, level, existance) <- get
    basic <- return $ evalObj s obj
    (Just pos) <- liftM2 loc (return var) (return env)
    (PObject thats) <- return $ fromJust $ M.lookup pos s
    here <- return $  foldl (++) [] $ fmap (evalLoc s) thats
    put (env, types, copyObj s here basic, classes, funs, scopes, level, existance)
    return PEmpty


assignment var val = do
        (env, types, s, classes, funs, scopes, level, existance) <- get
        exists <- liftM2 loc (return var) (return env)
        case exists of
            (Just pos)  ->
                --CHANGE
                put (env, types, (assinToLoc pos val s), classes, funs, scopes, level, existance)
            otherwise   -> liftIO $ throwIO $
                ErrorCall $ "Zmienna " ++ show var ++ " nie istnieje"
        return val

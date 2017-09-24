module Storables where

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
import qualified Data.Text as TEXT

type Loc = Int-- adresses

data PVal = PVals [Storable] | PObject [Loc] | PArray [Loc] | PBool Bool | PInt Integer |
                    PDouble Double | PText String | PFun ([PVal], [PVal]) | PMore | PEmpty


type Storable = PVal -- values that can be stored in VENV


defaultValue :: Type -> Storable
defaultValue  (TInt) = (PInt 0)
defaultValue  (TText) = (PText "")
defaultValue  (TBool) = (PBool True)
defaultValue  (TDouble) = (PDouble 0.0)
--defaultValue  (TArray t dim) = (PArray [])
defaultValue  (CustomType _) = (PObject [])

casts :: Storable -> Type -> Storable
casts (PBool x) t = (PBool x)
casts (PInt x) TDouble = PDouble (fromIntegral x)
casts (PInt x) TBool = if(x > 0) then (PBool True) else (PBool False)
casts (PInt x) _ = (PInt x)
casts (PText x) TBool = if(length x > 0) then (PBool True) else (PBool False)
casts (PText x) TInt = PInt (toInteger $ length x)
casts (PText x) _ = (PText x)
casts (PDouble x) TBool =  if(x > 0) then (PBool True) else (PBool False)
casts (PDouble x) _ =  (PDouble x)
casts (PVals x) _ =  (PVals x)


instance Show PVal where
    show (PEmpty) = "_"
    show (PBool val) = show val
    show (PInt val)  = show val
    show (PDouble val)  = show val
    show (PText val) = val
    show (PObject members) = "Obiekt " ++ show  members
    show (PArray members) = "Tablica " ++ show  members
    show (PVals vals) = "Trans obj " ++ (show (fmap show vals))

instance Eq PVal where
    (==) (PInt a) (PInt b)          = a == b
    (==) (PBool a) (PBool b)        = a == b
    (==) (PText a) (PText b)        = a == b
    (==) (PDouble a) (PDouble b)    = a == b

    (==) (PBool a) b = a == castBool b
    (==) a (PBool b) = b == castBool a

    (==) (PDouble a) b = a == castDouble b
    (==) b (PDouble a) = a == castDouble b

    (==) (PInt a) b = a == castInt b
    (==) b (PInt a) = a == castInt b

    (==) a@(PVals vals1) b@(PVals vals2) = vals1 == vals2


plus :: Storable -> Storable -> Storable
plus (PInt a) (PInt b) = PInt (a+b)
plus (PDouble a) (PDouble b) = PDouble (a+b)
plus (PInt a) (PDouble b) = PDouble (fromIntegral a +b)
plus (PDouble a) (PInt b) = PDouble (a+ (fromIntegral b))

plus (PText a) (PText b) = PText (a++b)
plus (PInt a) (PText b) = PText ((show a) ++b)
plus (PText a) (PInt b) = PText (a ++  (show b))

plus (PDouble a) (PText b) = PText ((show a) ++b)
plus (PText a) (PDouble b) = PText (a ++  (show b))


minus :: Storable -> Storable -> Storable
minus (PInt a) (PInt b) = PInt (a - b)
minus (PDouble a) (PDouble b) = PDouble (a - b)
minus (PInt a) (PDouble b) = minus (PDouble $ fromIntegral a) (PDouble b)
minus (PDouble b) (PInt a) = minus (PDouble b) (PDouble $ fromIntegral a)


multi :: Storable -> Storable -> Storable
multi (PInt a) (PInt b) = PInt (a*b)
multi (PDouble a) (PInt b) = PDouble (a* (fromIntegral b))
multi (PInt a) (PDouble b) = PDouble ((fromIntegral a)*b)
multi (PDouble a) (PDouble b) = PDouble (a*b)
multi (PText a) (PDouble b) = PText (TEXT.unpack $ TEXT.replicate (floor b) (TEXT.pack a))
multi (PDouble b) (PText a) = PText (TEXT.unpack $ TEXT.replicate(floor b) (TEXT.pack a))
multi (PText a) (PInt b) = PText (TEXT.unpack $ TEXT.replicate (fromInteger b)(TEXT.pack a))
multi (PInt b) (PText a) = PText (TEXT.unpack $ TEXT.replicate (fromInteger b) (TEXT.pack a))


divide :: Storable -> Storable -> Storable
divide  (PInt a) (PInt b) = PDouble (fromIntegral $ a `div` b)
--divide  (PDouble a) (PDouble b) = PDouble (a `div` b)
--divide  (PDouble a) (PInt b) = PDouble (a `div` b)
--divide  (PInt a) (PDouble b) = PDouble (a `div` b)

absolute :: Storable  -> Storable
absolute  (PInt a) = PInt (abs a)


less :: Storable -> Storable -> Storable
less (PInt a) (PInt b) = PBool (a<b)
less (PInt a) (PDouble b) = PBool ((fromIntegral a) < b)
less (PDouble a) (PInt b) = PBool (a < (fromIntegral b))
less (PDouble a) (PDouble b) = PBool (a<b)

less (PText a) (PText b) = PBool ((length a) < (length b))
less (PText a) (PInt b) = PBool ((length a) < (fromInteger b))
less (PInt a) (PText b) = PBool ((fromInteger a) < (length b))


greater :: Storable -> Storable -> Storable
greater (PInt a) (PInt b) = PBool (a>b)

greater (PText a) (PText b) = PBool ((length a) > (length b))
greater (PInt a) (PText b) = PBool (fromInteger a > (length b))
greater (PText b) (PInt a) = PBool (fromInteger a < (length b))


castInt :: PVal -> Integer
castInt (PInt val)  = val
castInt (PText val)  = toInteger $ length val

castDouble :: PVal -> Double
castDouble (PDouble val) = val
castDouble (PInt val) = realToFrac val

castBool :: PVal -> Bool
castBool (PBool True) = True
castBool (PBool False) = False
castBool (PInt n) = if n > 0 then True else False
castBool (PDouble n) = if n > 0 then True else False
castBool (PText str) = if(length str > 0) then True else False

justVal :: Maybe a -> a -> a
justVal (Just x) _ = x
justVal _ x = x


capitalized :: String -> String
capitalized (head:tail) = Char.toUpper head : map Char.toLower tail
capitalized [] = []


fromIdent :: Ident -> String
fromIdent (Ident x) = capitalized $ x ++ "."

toIdent :: String -> Ident
toIdent name = Ident name

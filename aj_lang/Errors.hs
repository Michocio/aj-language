{-
  Michał Jaroń
  mj348711
  File with errors definitions
-}

module Errors where

import Printcalc
import Control.Monad (MonadPlus(..), liftM)
import Control.Monad.Except
import Control.Exception

duplicatedLoopParamErr s = liftIO $ throwIO $ ErrorCall $ "Parametry w definicji loop muszą mieć różne nazwy: " ++ (printTree s)
condErr s = liftIO $ throwIO $ ErrorCall $ "Warunek logiczny musi mieć typ bool: " ++ (printTree s)
notAssinErr te t s =
    liftIO $ throwIO $ ErrorCall $ "Nie można przypisać obiektu typu " ++ printTree (head te) ++ " do obiektu typu " ++ printTree t ++" :" ++ (printTree s)
notAssinFieldErr te t s =
    liftIO $ throwIO $ ErrorCall $ "Nie można przypisać obiektu typu " ++ printTree (te) ++ " do obiektu typu " ++ printTree t ++" :" ++ (printTree s)
arrEqErr x = liftIO $ throwIO $ ErrorCall $ "Nie można porównać tablic: " ++ (printTree x)
notFieldErr name x = liftIO $ throwIO $ ErrorCall $ "Nie istnieje pole: " ++ (printTree x) ++ " w klasie " ++ name
outOfBoundsErr e = liftIO $ throwIO $ ErrorCall $ "Odwołanie poza zakres tablicy: " ++ (show e)
undeclaredObjErr name = liftIO $ throwIO $ ErrorCall $ "Próba użycia niezadeklarowanego obiektu: " ++ show name
duplicatedVarErr var = liftIO $ throwIO $ ErrorCall $ "Próba deklaracji zajętej nazwy " ++ (printTree var)
duplicatedClassErr name =  liftIO $ throwIO $ ErrorCall $ "Istnieje już klasa o nazwie " ++ name
duplicatedFunErr name = liftIO $ throwIO $ ErrorCall $ "Funkcja o nazwie " ++ name  ++" juz istnieje"
duplicatedFunParamErr name = liftIO $ throwIO $ ErrorCall $ "Parametry funkcji muszą mieć różne nazwy: " ++ name
funReturnErr name = liftIO $ throwIO $ ErrorCall $ "Zwracanie funkcji nie jest obecnie wspierane: " ++ name
notLogicalErr e = liftIO $ throwIO $ ErrorCall $ "Operacja logiczna wymaga dwóch wartości bool: " ++ (printTree e)
notLogicalSingleErr e = liftIO $ throwIO $ ErrorCall $ "Operacja logiczna wymaga wartości bool: " ++ (printTree e)
eqErr e = liftIO $ throwIO $ ErrorCall $ "Nie można porównać danych typów: " ++ (printTree e)
arithErr e = liftIO $ throwIO $ ErrorCall $ "Niepoprawne typy dla operacji: " ++ (printTree e)
undeclaredCallErr name = liftIO $ throwIO $ ErrorCall $ "Funkcja " ++ (printTree name) ++ " nie istnieje"
lambdaErr x = liftIO $ throwIO $ ErrorCall $ "Funkcja anonimowa zwraca niepoprawny typ wyrażenia: " ++ printTree x
undeclaredClassErr name =  liftIO $ throwIO $ ErrorCall $ "Nie istnieje klasa o nazwie " ++ (name)
notObjErr obj = liftIO $ throwIO $ ErrorCall $ (printTree obj) ++ " nie jest obiektem"
notArrErr arr = liftIO $ throwIO $ ErrorCall $ (printTree arr) ++ " nie jest tablicą"
unmatchingParamsErr call = liftIO $ throwIO $ ErrorCall $ "Podane typy parametrów nie zgadzają się z def. funkcji: " ++ (printTree call)
funAsFieldErr name = liftIO $ throwIO $ ErrorCall $ "W definicji klasy możliwe tylko deklaracje pól " ++ name
undeclaredVarErr name = liftIO $ throwIO $ ErrorCall $ "Nie istnieje zmienna " ++ (printTree name)
incErr s = liftIO $ throwIO $ ErrorCall $ "Niepoprawne wyrażanie " ++ (printTree s)
paramsNumErr s = liftIO $ throwIO $ ErrorCall $ "Zła liczba parametrów: " ++ (printTree s)
forToErr t f = liftIO $ throwIO $ ErrorCall $ "Niedozwolony typ " ++  (printTree t) ++ " dla for: " ++ (printTree f)
forTypesErr f = liftIO $ throwIO $ ErrorCall $ "Niezgoność typów dla for: " ++ (printTree f)

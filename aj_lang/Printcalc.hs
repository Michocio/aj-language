{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Printcalc where

-- pretty-printer generated by the BNF converter

import Abscalc
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: [a] -> Doc
  prtList = concatD . map (prt 0)

instance Print a => Print [a] where
  prt _ = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)


instance Print Ident where
  prt _ (Ident i) = doc (showString ( i))


instance Print ClassName where
  prt _ (ClassName i) = doc (showString ( i))


instance Print Negative where
  prt _ (Negative i) = doc (showString ( i))



instance Print Prog where
  prt i e = case e of
   Program insts -> prPrec i 0 (concatD [prt 0 insts])


instance Print Type where
  prt i e = case e of
   CustomType classname -> prPrec i 0 (concatD [prt 0 classname])
   TInt  -> prPrec i 0 (concatD [doc (showString "int")])
   TText  -> prPrec i 0 (concatD [doc (showString "text")])
   TAny  -> prPrec i 0 (concatD [doc (showString "any")])
   TBool  -> prPrec i 0 (concatD [doc (showString "bool")])
   TDouble  -> prPrec i 0 (concatD [doc (showString "double")])
   TArray type' array -> prPrec i 0 (concatD [prt 0 type' , prt 0 array])

  prtList es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print Array where
  prt i e = case e of
   ArraySymbol exp -> prPrec i 0 (concatD [doc (showString "[") , prt 0 exp , doc (showString "]")])


instance Print Inst where
  prt i e = case e of
   InstrD decl -> prPrec i 0 (concatD [prt 0 decl])
   InstrS stmt -> prPrec i 0 (concatD [prt 0 stmt])
   InstrExpr exp -> prPrec i 0 (concatD [prt 0 exp])
   InstrLoop loop -> prPrec i 0 (concatD [prt 0 loop])
   InstrFor for -> prPrec i 0 (concatD [prt 0 for])
   InstrFun fun -> prPrec i 0 (concatD [prt 0 fun])
   InstrClass classheader -> prPrec i 0 (concatD [prt 0 classheader])
   IApply id0 id -> prPrec i 0 (concatD [doc (showString "apply") , prt 0 id0 , prt 0 id])
   IApplyAn lambda id -> prPrec i 0 (concatD [doc (showString "apply") , prt 0 lambda , prt 0 id , doc (showString ";")])
   Print exp -> prPrec i 0 (concatD [doc (showString "print") , doc (showString "(") , prt 0 exp , doc (showString ")") , doc (showString ";")])
   PrintLn exp -> prPrec i 0 (concatD [doc (showString "println") , doc (showString "(") , prt 0 exp , doc (showString ")") , doc (showString ";")])
   Read id -> prPrec i 0 (concatD [doc (showString "read") , doc (showString "(") , prt 0 id , doc (showString ")") , doc (showString ";")])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print Decl where
  prt i e = case e of
   Declare type' vars -> prPrec i 0 (concatD [prt 0 type' , prt 0 vars , doc (showString ";")])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print Var where
  prt i e = case e of
   VarName id -> prPrec i 0 (concatD [prt 0 id])
   VarAssin id exp -> prPrec i 0 (concatD [prt 0 id , doc (showString "=") , prt 0 exp])

  prtList es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print Stmt where
  prt i e = case e of
   VAssin id exp -> prPrec i 0 (concatD [prt 0 id , doc (showString "=") , prt 0 exp , doc (showString ";")])
   FieldAssin id0 id exp -> prPrec i 0 (concatD [prt 0 id0 , doc (showString ".") , prt 0 id , doc (showString "=") , prt 0 exp , doc (showString ";")])
   VAssinArrField id0 id exp1 elems exp -> prPrec i 0 (concatD [prt 0 id0 , doc (showString ".") , prt 0 id , doc (showString "[") , prt 0 exp1 , doc (showString "]") , prt 0 elems , doc (showString "=") , prt 0 exp , doc (showString ";")])
   VAssinArr id exp0 elems exp -> prPrec i 0 (concatD [prt 0 id , doc (showString "[") , prt 0 exp0 , doc (showString "]") , prt 0 elems , doc (showString "=") , prt 0 exp , doc (showString ";")])
   VAssinFieldArr id0 exp1 elems id exp -> prPrec i 0 (concatD [prt 0 id0 , doc (showString "[") , prt 0 exp1 , doc (showString "]") , prt 0 elems , doc (showString ".") , prt 0 id , doc (showString "=") , prt 0 exp , doc (showString ";")])
   VAssinPlus id exp -> prPrec i 0 (concatD [prt 0 id , doc (showString "+=") , prt 0 exp , doc (showString ";")])
   VAssinMulti id exp -> prPrec i 0 (concatD [prt 0 id , doc (showString "*=") , prt 0 exp , doc (showString ";")])
   VAssinMinus id exp -> prPrec i 0 (concatD [prt 0 id , doc (showString "-=") , prt 0 exp , doc (showString ";")])
   VAssinDiv id exp -> prPrec i 0 (concatD [prt 0 id , doc (showString "/=") , prt 0 exp , doc (showString ";")])
   SIfOne exp inst elifs -> prPrec i 0 (concatD [doc (showString "if") , doc (showString "(") , prt 0 exp , doc (showString ")") , prt 0 inst , prt 0 elifs])
   SIfTwo exp inst0 elifs inst -> prPrec i 0 (concatD [doc (showString "if") , doc (showString "(") , prt 0 exp , doc (showString ")") , prt 0 inst0 , prt 0 elifs , doc (showString "else") , prt 0 inst])
   SIfFourMore exp inst elifs insts -> prPrec i 0 (concatD [doc (showString "if") , doc (showString "(") , prt 0 exp , doc (showString ")") , prt 0 inst , prt 0 elifs , doc (showString "else") , doc (showString "{") , prt 0 insts , doc (showString "}")])
   SIfOneMore exp insts elifs -> prPrec i 0 (concatD [doc (showString "if") , doc (showString "(") , prt 0 exp , doc (showString ")") , doc (showString "{") , prt 0 insts , doc (showString "}") , prt 0 elifs])
   SIfTwoMore exp insts elifs inst -> prPrec i 0 (concatD [doc (showString "if") , doc (showString "(") , prt 0 exp , doc (showString ")") , doc (showString "{") , prt 0 insts , doc (showString "}") , prt 0 elifs , doc (showString "else") , prt 0 inst])
   SIfThreeMore exp insts0 elifs insts -> prPrec i 0 (concatD [doc (showString "if") , doc (showString "(") , prt 0 exp , doc (showString ")") , doc (showString "{") , prt 0 insts0 , doc (showString "}") , prt 0 elifs , doc (showString "else") , doc (showString "{") , prt 0 insts , doc (showString "}")])
   Switch exp cases insts -> prPrec i 0 (concatD [doc (showString "switch") , doc (showString "(") , prt 0 exp , doc (showString ")") , doc (showString "{") , prt 0 cases , doc (showString "}") , doc (showString "else") , doc (showString "{") , prt 0 insts , doc (showString "}")])
   Switch2 exp cases -> prPrec i 0 (concatD [doc (showString "switch") , doc (showString "(") , prt 0 exp , doc (showString ")") , doc (showString "{") , prt 0 cases , doc (showString "}")])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print ElIf where
  prt i e = case e of
   SElIf exp inst -> prPrec i 0 (concatD [doc (showString "elseif") , doc (showString "(") , prt 0 exp , doc (showString ")") , prt 0 inst])
   SElIfMore exp insts -> prPrec i 0 (concatD [doc (showString "elseif") , doc (showString "(") , prt 0 exp , doc (showString ")") , doc (showString "{") , prt 0 insts , doc (showString "}")])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print Case where
  prt i e = case e of
   CasesVal exp insts -> prPrec i 0 (concatD [doc (showString "case") , prt 0 exp , doc (showString ":") , doc (showString "{") , prt 0 insts , doc (showString "}")])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print Elem where
  prt i e = case e of
   ArrElem exp -> prPrec i 0 (concatD [doc (showString "[") , prt 0 exp , doc (showString "]")])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print Exp where
  prt i e = case e of
   Etrue  -> prPrec i 18 (concatD [doc (showString "true")])
   Efalse  -> prPrec i 18 (concatD [doc (showString "false")])
   LogicalAnd exp0 exp -> prPrec i 2 (concatD [prt 2 exp0 , doc (showString "and") , prt 3 exp])
   LogicalOr exp0 exp -> prPrec i 2 (concatD [prt 2 exp0 , doc (showString "or") , prt 3 exp])
   LogicalNot exp -> prPrec i 13 (concatD [doc (showString "not") , prt 13 exp])
   Eeq exp0 exp -> prPrec i 8 (concatD [prt 8 exp0 , doc (showString "==") , prt 9 exp])
   Eneq exp0 exp -> prPrec i 8 (concatD [prt 8 exp0 , doc (showString "!=") , prt 9 exp])
   ELess exp0 exp -> prPrec i 9 (concatD [prt 9 exp0 , doc (showString "<") , prt 10 exp])
   EBigg exp0 exp -> prPrec i 9 (concatD [prt 9 exp0 , doc (showString ">") , prt 10 exp])
   ELessEq exp0 exp -> prPrec i 9 (concatD [prt 9 exp0 , doc (showString "<=") , prt 10 exp])
   EBiggEq exp0 exp -> prPrec i 9 (concatD [prt 9 exp0 , doc (showString ">=") , prt 10 exp])
   EAdd exp0 exp -> prPrec i 10 (concatD [prt 10 exp0 , doc (showString "+") , prt 11 exp])
   ESub exp0 exp -> prPrec i 10 (concatD [prt 10 exp0 , doc (showString "-") , prt 11 exp])
   EMul exp0 exp -> prPrec i 11 (concatD [prt 11 exp0 , doc (showString "*") , prt 12 exp])
   EDiv exp0 exp -> prPrec i 11 (concatD [prt 11 exp0 , doc (showString "/") , prt 12 exp])
   EMod exp0 exp -> prPrec i 11 (concatD [prt 11 exp0 , doc (showString "%") , prt 12 exp])
   EPow exp0 exp -> prPrec i 12 (concatD [prt 12 exp0 , doc (showString "^") , prt 13 exp])
   ESqrt exp -> prPrec i 13 (concatD [doc (showString "sqrt") , doc (showString "(") , prt 13 exp , doc (showString ")")])
   EAbs exp -> prPrec i 13 (concatD [doc (showString "abs") , prt 13 exp])
   Epreinc exp -> prPrec i 14 (concatD [doc (showString "++") , prt 14 exp])
   Epredec exp -> prPrec i 14 (concatD [doc (showString "--") , prt 14 exp])
   Epostinc exp -> prPrec i 15 (concatD [prt 15 exp , doc (showString "++")])
   Epostdec exp -> prPrec i 15 (concatD [prt 15 exp , doc (showString "--")])
   ECons exps -> prPrec i 16 (concatD [doc (showString "<<") , prt 0 exps , doc (showString ">>")])
   EString str -> prPrec i 16 (concatD [prt 0 str])
   EInt n -> prPrec i 16 (concatD [prt 0 n])
   EDouble d -> prPrec i 16 (concatD [prt 0 d])
   ECall call -> prPrec i 16 (concatD [prt 0 call])
   ENeg negative -> prPrec i 16 (concatD [prt 0 negative])
   EScope id -> prPrec i 16 (concatD [doc (showString "scope") , doc (showString ".") , prt 0 id])
   EObj id0 id -> prPrec i 17 (concatD [prt 0 id0 , doc (showString ".") , prt 0 id])
   EVar id -> prPrec i 18 (concatD [prt 0 id])
   EArr id exp elems -> prPrec i 19 (concatD [prt 0 id , doc (showString "[") , prt 0 exp , doc (showString "]") , prt 0 elems])
   EFieldAr id0 exp elems id -> prPrec i 19 (concatD [prt 0 id0 , doc (showString "[") , prt 0 exp , doc (showString "]") , prt 0 elems , doc (showString ".") , prt 0 id])
   EArrField id0 id exp elems -> prPrec i 19 (concatD [prt 0 id0 , doc (showString ".") , prt 0 id , doc (showString "[") , prt 0 exp , doc (showString "]") , prt 0 elems])
   ELam lambda -> prPrec i 19 (concatD [prt 0 lambda])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print For where
  prt i e = case e of
   ForTo type' id exp0 exp inst -> prPrec i 0 (concatD [doc (showString "for") , prt 0 type' , prt 0 id , doc (showString ":=") , prt 0 exp0 , doc (showString "to") , prt 0 exp , prt 0 inst])
   ForToMulti type' id exp0 exp insts -> prPrec i 0 (concatD [doc (showString "for") , prt 0 type' , prt 0 id , doc (showString ":=") , prt 0 exp0 , doc (showString "to") , prt 0 exp , doc (showString "{") , prt 0 insts , doc (showString "}")])
   ForDownTo type' id exp0 exp inst -> prPrec i 0 (concatD [doc (showString "for") , prt 0 type' , prt 0 id , doc (showString ":=") , prt 0 exp0 , doc (showString "downto") , prt 0 exp , prt 0 inst])
   ForDownToMulti type' id exp0 exp insts -> prPrec i 0 (concatD [doc (showString "for") , prt 0 type' , prt 0 id , doc (showString ":=") , prt 0 exp0 , doc (showString "downto") , prt 0 exp , doc (showString "{") , prt 0 insts , doc (showString "}")])


instance Print Loop where
  prt i e = case e of
   LoopInst loopvars exp loophow inst -> prPrec i 0 (concatD [doc (showString "loop") , doc (showString "(") , prt 0 loopvars , prt 0 exp , prt 0 loophow , doc (showString ")") , prt 0 inst])
   LoopInstMulti loopvars exp loophow insts -> prPrec i 0 (concatD [doc (showString "loop") , doc (showString "(") , prt 0 loopvars , prt 0 exp , prt 0 loophow , doc (showString ")") , doc (showString "{") , prt 0 insts , doc (showString "}")])


instance Print LoopVars where
  prt i e = case e of
   LoopDecli loopdecls -> prPrec i 0 (concatD [prt 0 loopdecls , doc (showString ";")])
   LoopDeclEps  -> prPrec i 0 (concatD [])


instance Print LoopHow where
  prt i e = case e of
   LoopDo iterexps -> prPrec i 0 (concatD [doc (showString ";") , prt 0 iterexps])
   LoopDoEps  -> prPrec i 0 (concatD [])


instance Print LoopDecl where
  prt i e = case e of
   DeclareLoop type' varloops -> prPrec i 0 (concatD [prt 0 type' , prt 0 varloops])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString "|") , prt 0 xs])

instance Print VarLoop where
  prt i e = case e of
   LoopAssin id exp -> prPrec i 0 (concatD [prt 0 id , doc (showString "=") , prt 0 exp])

  prtList es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print IterExp where
  prt i e = case e of
   IterExpr exp -> prPrec i 0 (concatD [prt 0 exp])
   IterExpr2 loopstmt -> prPrec i 0 (concatD [prt 0 loopstmt])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print LoopStmt where
  prt i e = case e of
   VAssinPlusL id exp -> prPrec i 0 (concatD [prt 0 id , doc (showString "+=") , prt 0 exp])
   VAssinMultiL id exp -> prPrec i 0 (concatD [prt 0 id , doc (showString "*=") , prt 0 exp])
   VAssinMinusL id exp -> prPrec i 0 (concatD [prt 0 id , doc (showString "-=") , prt 0 exp])
   VAssinDivL id exp -> prPrec i 0 (concatD [prt 0 id , doc (showString "/=") , prt 0 exp])


instance Print Fun where
  prt i e = case e of
   FunDecl id paramslist insts -> prPrec i 0 (concatD [prt 0 id , doc (showString "::") , prt 0 paramslist , doc (showString "{") , prt 0 insts , doc (showString "}")])


instance Print ParamsList where
  prt i e = case e of
   FunParamList funargss0 funargss -> prPrec i 0 (concatD [doc (showString "[") , prt 0 funargss0 , doc (showString "]") , doc (showString "->") , doc (showString "[") , prt 0 funargss , doc (showString "]")])


instance Print FunArgs where
  prt i e = case e of
   FunVar type' id -> prPrec i 0 (concatD [prt 0 type' , prt 0 id])
   FunFunny id paramslist -> prPrec i 0 (concatD [prt 0 id , doc (showString "::") , prt 0 paramslist])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print FunParam where
  prt i e = case e of
   FunFun id paramslist -> prPrec i 0 (concatD [prt 0 id , doc (showString "::") , doc (showString "(") , prt 0 paramslist , doc (showString ")")])


instance Print Call where
  prt i e = case e of
   CallFun id realparams -> prPrec i 0 (concatD [prt 0 id , doc (showString "(") , prt 0 realparams , doc (showString ")")])


instance Print RealParam where
  prt i e = case e of
   RealParamVal exp -> prPrec i 0 (concatD [prt 0 exp])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print Lambda where
  prt i e = case e of
   LamFun type' lambdaparamss exp -> prPrec i 0 (concatD [doc (showString "(") , prt 0 type' , doc (showString "::") , doc (showString "[") , prt 0 lambdaparamss , doc (showString "]") , doc (showString "->") , doc (showString "(") , prt 0 exp , doc (showString ")") , doc (showString ")")])


instance Print LambdaParams where
  prt i e = case e of
   LambdaParam type' id -> prPrec i 0 (concatD [prt 0 type' , prt 0 id])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print ClassHeader where
  prt i e = case e of
   ClassDec classname classstmts -> prPrec i 0 (concatD [doc (showString "class") , prt 0 classname , doc (showString "{") , prt 0 classstmts , doc (showString "}")])


instance Print ClassBlock where
  prt i e = case e of
   ClassBody classstmts -> prPrec i 0 (concatD [doc (showString "{") , prt 0 classstmts , doc (showString "}")])


instance Print ClassStmt where
  prt i e = case e of
   ClassVar decl -> prPrec i 0 (concatD [prt 0 decl])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])



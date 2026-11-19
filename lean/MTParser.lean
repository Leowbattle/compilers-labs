/-
  Direct port of `MTParser.hs`.

  `Type` from the Haskell source is called `MTType` here because `Type` is a
  reserved name in Lean.
-/

import Parser

abbrev Identifier := String

inductive MTType
  | TyInt
  | TyBool
  deriving BEq, Repr, Inhabited

inductive BinOperator
  | Addition
  | Subtraction
  | Multiplication
  | Division
  | BinAnd
  | BinOr
  | Less
  | LessEq
  | Greater
  | GreaterEq
  | Equals
  | NotEquals
  deriving BEq, Repr, Inhabited

inductive UnOperator
  | Negation
  | UnNot
  deriving BEq, Repr, Inhabited

inductive Expr
  | Identifier (id : Identifier)
  | LitInteger (value : Int)
  | LitBool (value : Bool)
  | BinOp (op : BinOperator) (lhs rhs : Expr)
  | UnOp (op : UnOperator) (arg : Expr)
  | Conditional (cond yes no : Expr)
  | FunctionCall (id : Identifier) (args : List Expr)
  deriving BEq, Repr, Inhabited

inductive Command
  | Assignment (id : Identifier) (e : Expr)
  | IfThenElse (cond : Expr) (yes no : Command)
  | While (cond : Expr) (cmd : Command)
  | GetInt (id : Identifier)
  | PrintInt (e : Expr)
  | BeginEnd (cmds : List Command)
  deriving BEq, Repr, Inhabited

inductive Declaration
  | VarDecl (id : Identifier) (ty : MTType)
  | VarInit (id : Identifier) (ty : MTType) (e : Expr)
  | FunctionDecl (id : Identifier) (args : List (Identifier × MTType)) (ret : MTType) (e : Expr)
  deriving BEq, Repr, Inhabited

inductive VFType
  | VarType (ty : MTType)
  | FunType (args : List MTType) (ret : MTType)
  deriving BEq, Repr, Inhabited

inductive Program
  | LetIn (decls : List Declaration) (cmd : Command)
  deriving BEq, Repr, Inhabited

open MTType BinOperator UnOperator Command Declaration VFType Program
-- Excludes `Identifier`, which would otherwise shadow the `Identifier` type alias.
open Expr (LitInteger LitBool BinOp UnOp Conditional FunctionCall)

def declName : Declaration → String
  | VarDecl id _ => id
  | VarInit id _ _ => id
  | FunctionDecl id _ _ _ => id

def declType : Declaration → VFType
  | VarDecl _ ty => VarType ty
  | VarInit _ ty _ => VarType ty
  | FunctionDecl _ args ret _ => FunType (args.map Prod.snd) ret

def int : Parser Expr := LitInteger <$> natural

def operator (ops : List (String × BinOperator)) (p : Parser Expr) : Parser Expr := do
  let x ← p
  let alts := ops.map (fun (sym, op) => do
    let _ ← string sym
    let m ← p
    return (op, m))
  let xs ← many (alts.foldl (· <|> ·) empty)
  return xs.foldl (fun a (op, m) => BinOp op a m) x

def identifier : Parser Identifier := do
  let c ← satisfy item Char.isAlpha
  let cs ← many (satisfy item Char.isAlphanum)
  let s : Str := c :: cs
  return String.ofList s

partial def separatedBy {α : Type} (p : Parser α) (s : String) : Parser (List α) :=
  (do
    let x ← p
    let _ ← token (string s)
    let xs ← separatedBy p s
    return (x :: xs)) <|>
  ((fun x => [x]) <$> p) <|>
  return []

def parenList {α : Type} (opn : String) (p : Parser α) (cls : String) : Parser (List α) := do
  let _ ← token (string opn)
  let xs ← separatedBy p ","
  let _ ← token (string cls)
  return xs

mutual
  partial def expr : Parser Expr := condExp <|> orExp

  partial def condExp : Parser Expr := do
    let cond ← orExp
    let _ ← token (string "?")
    let x ← orExp
    let _ ← token (string ":")
    let y ← expr
    return Conditional cond x y

  partial def orExp : Parser Expr := operator [("||", BinOr)] andExp

  partial def andExp : Parser Expr := operator [("&&", BinAnd)] notExp

  partial def notExp : Parser Expr :=
    (do
      let _ ← token (string "!")
      let e ← term
      return UnOp UnNot e) <|>
    compExp

  partial def compExp : Parser Expr := operator [("<", Less),
                                                  ("<=", LessEq),
                                                  (">", Greater),
                                                  (">=", GreaterEq),
                                                  ("==", Equals),
                                                  ("!=", NotEquals)] addExp

  partial def addExp : Parser Expr := operator [("+", Addition), ("-", Subtraction)] mexp

  partial def mexp : Parser Expr := operator [("*", Multiplication), ("/", Division)] term

  partial def term : Parser Expr := token (do
    (do
      let _ ← token (string "false")
      return LitBool false) <|>
    (do
      let _ ← token (string "true")
      return LitBool true) <|>
    (do
      let id ← identifier
      let args ← parenList "(" expr ")"
      return FunctionCall id args) <|>
    int <|>
    (Expr.Identifier <$> identifier) <|>
    (do
      let _ ← token (string "-")
      let t ← term
      return UnOp Negation t) <|>
    (do
      let _ ← token (string "(")
      let e ← expr
      let _ ← token (string ")")
      return e))
end

partial def exprs : Parser (List Expr) :=
  (do
    let e ← expr
    let _ ← token (string ",")
    let es ← exprs
    return (e :: es)) <|>
  ((fun e => [e]) <$> expr) <|>
  pure []

def typeP : Parser MTType :=
  (do
    let _ ← string "Integer"
    return TyInt) <|>
  (do
    let _ ← string "Boolean"
    return TyBool)

def argDeclaration : Parser (Identifier × MTType) := do
  let id ← identifier
  let _ ← token (string ":")
  let ty ← typeP
  return (id, ty)

def declaration : Parser Declaration :=
  (do
    let _ ← token (string "var")
    let id ← identifier
    let _ ← token (string ":")
    let ty ← typeP
    let _ ← token (string ":=")
    let e ← expr
    return VarInit id ty e) <|>
  (do
    let _ ← token (string "var")
    let id ← identifier
    let _ ← token (string ":")
    let ty ← typeP
    return VarDecl id ty) <|>
  (do
    let _ ← token (string "fun")
    let id ← identifier
    let args ← parenList "(" argDeclaration ")"
    let _ ← token (string ":")
    let ty ← typeP
    let _ ← token (string "=")
    let e ← expr
    return FunctionDecl id args ty e)

partial def declarations : Parser (List Declaration) :=
  (do
    let d ← declaration
    let _ ← token (string ";")
    let ds ← declarations
    return (d :: ds)) <|>
  (do
    let d ← declaration
    return [d])

mutual
  partial def command : Parser Command :=
    (do
      let _ ← token (string "if")
      let e ← expr
      let _ ← token (string "then")
      let yes ← command
      let _ ← token (string "else")
      let no ← command
      return IfThenElse e yes no) <|>
    (do
      let _ ← token (string "while")
      let e ← expr
      let _ ← token (string "do")
      let c ← command
      return While e c) <|>
    (do
      let _ ← token (string "getint")
      let _ ← token (string "(")
      let id ← identifier
      let _ ← token (string ")")
      return GetInt id) <|>
    (do
      let _ ← token (string "printint")
      let _ ← token (string "(")
      let e ← expr
      let _ ← token (string ")")
      return PrintInt e) <|>
    (do
      let _ ← token (string "begin")
      let cs ← commands
      let _ ← token (string "end")
      return BeginEnd cs) <|>
    (do
      let id ← identifier
      let _ ← token (string ":=")
      let e ← expr
      return Assignment id e)

  partial def commands : Parser (List Command) :=
    (do
      let c ← command
      let _ ← token (string ";")
      let cs ← commands
      return (c :: cs)) <|>
    (do
      let c ← command
      return [c])
end

def program : Parser Program := do
  let _ ← token (string "let")
  let ds ← declarations
  let _ ← token (string "in")
  let c ← command
  return LetIn ds c

def parseMT (s : String) : Program :=
  match parse program s with
  | (t, _) :: _ => t
  | [] => panic! "parseMT: no parse"

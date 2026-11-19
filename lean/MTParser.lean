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

/--
A total separated list.

The recursive call is made through `parserFix`, so it only runs on a strictly
shorter suffix. If the element parser succeeds but the separator does not
consume input, the result stops with the single element.
-/
def separatedBy {α : Type} (p : Parser α) (s : String) : Parser (List α) :=
  parserFix fun self => fun src =>
    (p src).flatMap fun (x, rest) =>
      match token (string s) rest with
      | [] => [([x], rest)]
      | (_, rest') :: _ =>
          let xs := self rest'
          if xs.isEmpty then [([x], rest)]
          else xs.map fun (ys, rest'') => (x :: ys, rest'')

def parenList {α : Type} (opn : String) (p : Parser α) (cls : String) : Parser (List α) := do
  let _ ← token (string opn)
  let xs ← separatedBy p ","
  let _ ← token (string cls)
  return xs

/-! ### Total recursive expression grammar

`expr` is the only fixed point. `termFor` turns a parser for nested
expressions into a term parser; its own negative/term recursion is another
fixed point. This keeps all recursion on the input-length measure without a
fuel parameter.
-/

def mexpWith (termP : Parser Expr) : Parser Expr :=
  operator [("*", Multiplication), ("/", Division)] termP

def addExpWith (termP : Parser Expr) : Parser Expr :=
  operator [("+", Addition), ("-", Subtraction)] (mexpWith termP)

def compExpWith (termP : Parser Expr) : Parser Expr :=
  operator
    [("<", Less), ("<=", LessEq), (">", Greater),
     (">=", GreaterEq), ("==", Equals), ("!=", NotEquals)]
    (addExpWith termP)

def notExpWith (termP : Parser Expr) : Parser Expr :=
  (do
    let _ ← token (string "!")
    let e ← termP
    return UnOp UnNot e) <|>
  compExpWith termP

def andExpWith (termP : Parser Expr) : Parser Expr :=
  operator [("&&", BinAnd)] (notExpWith termP)

def orExpWith (termP : Parser Expr) : Parser Expr :=
  operator [("||", BinOr)] (andExpWith termP)

def condExpWith (exprP termP : Parser Expr) : Parser Expr := do
  let cond ← orExpWith termP
  let _ ← token (string "?")
  let x ← orExpWith termP
  let _ ← token (string ":")
  let y ← exprP
  return Conditional cond x y

def exprBody (exprP termP : Parser Expr) : Parser Expr :=
  condExpWith exprP termP <|> orExpWith termP

def termBody (exprP termP : Parser Expr) : Parser Expr :=
  token (do
    (do
      let _ ← token (string "false")
      return LitBool false) <|>
    (do
      let _ ← token (string "true")
      return LitBool true) <|>
    (do
      let id ← identifier
      let args ← parenList "(" exprP ")"
      return FunctionCall id args) <|>
    int <|>
    (Expr.Identifier <$> identifier) <|>
    (do
      let _ ← token (string "-")
      let t ← termP
      return UnOp Negation t) <|>
    (do
      let _ ← token (string "(")
      let e ← exprP
      let _ ← token (string ")")
      return e))

def termFor (exprP : Parser Expr) : Parser Expr :=
  parserFix fun termP => termBody exprP termP

def expr : Parser Expr :=
  parserFix fun exprP => exprBody exprP (termFor exprP)

def term : Parser Expr := termFor expr

-- `exprs` is not used by the grammar; the generic `separatedBy` covers it.

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

def declarations : Parser (List Declaration) :=
  separatedBy declaration ";"

def commandsFor (cmdP : Parser Command) : Parser (List Command) :=
  separatedBy cmdP ";"

def commandBody (cmdP : Parser Command) (commandsP : Parser (List Command)) : Parser Command :=
  (do
    let _ ← token (string "if")
    let e ← expr
    let _ ← token (string "then")
    let yes ← cmdP
    let _ ← token (string "else")
    let no ← cmdP
    return IfThenElse e yes no) <|>
  (do
    let _ ← token (string "while")
    let e ← expr
    let _ ← token (string "do")
    let c ← cmdP
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
    let cs ← commandsP
    let _ ← token (string "end")
    return BeginEnd cs) <|>
  (do
    let id ← identifier
    let _ ← token (string ":=")
    let e ← expr
    return Assignment id e)

def command : Parser Command :=
  parserFix fun cmdP => commandBody cmdP (commandsFor cmdP)

def commands : Parser (List Command) :=
  commandsFor command

def program : Parser Program := do
  let _ ← token (string "let")
  let ds ← declarations
  let _ ← token (string "in")
  let c ← command
  return LetIn ds c

def parseMT (s : String) : Except String Program :=
  match parseExact program s with
  | t :: _ => .ok t
  | [] => .error "parse error"

/-- Successful `parseMT` results consume the entire input. -/
theorem parseMT_consumes (s : String) (p : Program) (h : parseMT s = .ok p) :
    (p, []) ∈ parse program s := by
  unfold parseMT at h
  split at h
  · rename_i t ts hx
    have hp : t ∈ parseExact program s := by
      rw [hx]
      exact List.mem_cons_self
    cases h
    exact mem_parseExact hp
  · simp at h

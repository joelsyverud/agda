
module Agda.Utils.Haskell.Syntax where

-- Modules --

data Module = Module ModuleName [ModulePragma] [ImportDecl] [Decl]

data ModulePragma = LanguagePragma [Name]

data ImportDecl = ImportDecl
      { importModule    :: ModuleName
      , importQualified :: Bool
      , importSpecs     :: Maybe (Bool, [ImportSpec]) }

data ImportSpec = IVar Name

-- Declarations --

data Decl = TypeDecl Name [TyVarBind] Type
          | DataDecl DataOrNew Name [TyVarBind] [ConDecl] [Deriving]
          | TypeSig [Name] Type
          | FunBind [Match]
  deriving (Eq)

data DataOrNew = DataType | NewType
  deriving (Eq)

data ConDecl = ConDecl Name [Type]
  deriving (Eq)

type Deriving = (QName, [Type])

data Binds = BDecls [Decl]
  deriving (Eq)

data Rhs = UnGuardedRhs Exp
         | GuardedRhss [GuardedRhs]
  deriving (Eq)

data GuardedRhs = GuardedRhs [Stmt] Exp
  deriving (Eq)

data Match = Match Name [Pat] Rhs (Maybe Binds)
  deriving (Eq)

-- Expressions --

data Type = TyForall [TyVarBind] Type
          | TyFun Type Type
          | TyCon QName
          | TyVar Name
          | TyApp Type Type
  deriving (Eq)

data Pat = PVar Name
         | PLit Literal
         | PAsPat Name Pat
         | PWildCard
         | PBangPat Pat
         | PApp QName [Pat]
         | PatTypeSig Pat Type
         | PIrrPat Pat
  deriving (Eq)


data Stmt = Qualifier Exp
          | Generator Pat Exp
  deriving (Eq)

data Exp = Var QName
         | Con QName
         | Lit Literal
         | InfixApp Exp QOp Exp
         | App Exp Exp
         | Lambda [Pat] Exp
         | Let Binds Exp
         | If Exp Exp Exp
         | Case Exp [Alt]
         | ExpTypeSig Exp Type
         | NegApp Exp
  deriving (Eq)

data Alt = Alt Pat Rhs (Maybe Binds)
  deriving (Eq)

data Literal = Int Integer | Frac Rational | Char Char | String String
  deriving (Eq)

-- Names --

data ModuleName = ModuleName String
  deriving (Eq, Ord)

data QName = Qual ModuleName Name | UnQual Name
  deriving (Eq)

data Name = Ident String | Symbol String
  deriving (Eq)

data QOp = QVarOp QName
  deriving (Eq)

data TyVarBind = UnkindedVar Name
  deriving (Eq)

unit_con :: Exp
unit_con = Con (UnQual (Ident "()"))
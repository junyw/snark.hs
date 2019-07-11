module Compiler.AST 
  (
  -- * Aliases for types
    Text
  , Id
  , Tag

  -- * Abstract syntax 
  , Program (..), AProgram(..)
  , Decl (..)   , ADecl(..)
  , Bind (..)
  , Expr (..)   , AExpr(..), CExpr(..), ImmExpr(..)
  , Prim1 (..)
  , Prim2 (..)
  
  -- * Constructors
  , bindsExpr, bindsCExpr
  -- * Destructiors
  , exprBinds
  , bindId
  , isPublicParam
  
  -- * Labels
  , getLabel, getCLabel
  ) where

import qualified Data.List as List
import Text.Printf
import Data.Text.Prettyprint.Doc

--------------------------------------------------------------------------------
-- | Abstract Syntax  
--------------------------------------------------------------------------------

type Text = String

-- | 'Id' are program variables
type Id = Text 

-- | 'Tag' 
type Tag = Int

-- | 'Prim1' are unary operatons
data Prim1 = Add1 | Sub1 | Not | IsZero
  deriving (Show)

-- | 'Prim2' are binary operatons
data Prim2 = Plus | Minus | Times | Divide | Less | Greater | Equal
  deriving (Show)

-- | 'Exrp' are single expressions
data Expr a = Number  !Integer  a 
            | Boolean !Bool     a
            | Id      !Id       a
            | Prim1   !Prim1    !(Expr a) a
            | Prim2   !Prim2    !(Expr a) !(Expr a) a
            | If      !(Expr a) !(Expr a) !(Expr a) a
            | Let     !(Bind a) !(Expr a) !(Expr a) a
            | App     !Id       [Expr a]  a
  deriving (Show, Functor)


-- | 'Bind' represent let-bindings or function parameters
data Bind a = Bind !Id a 
            | Parameter !Bool !Id a -- True if public, False if private≈
  deriving (Show, Functor)

-- | 'Decl' are function definitions
data Decl a = Decl { fName  :: !(Bind a)
                   , fArgs  :: [Bind a]
                   , fBody  :: !(Expr a)
                   , fLabel :: a 
                   }
  deriving (Show)
 

data Program a = Prog { pDecls :: [Decl a] }
  deriving (Show)

-- | Constructing `Expr` from let-binds
bindsExpr :: [(Bind a, Expr a)] -> Expr a -> a -> Expr a
bindsExpr bs e l = foldr (\(x, e1) e2  -> Let x e1 e2 l) e bs

-- | Destructing `Expr` into let-binds
exprBinds :: Expr a -> ([(Bind a, Expr a)], Expr a)
exprBinds (Let x e e' _) = ((x, e) : bs, body)
  where
    (bs, body)           = exprBinds e'
exprBinds body = ([] , body)

bindId :: Bind a -> Id
bindId (Bind x _) = x
bindId (Parameter _ x _) = x

isPublicParam :: Bind a -> Bool
isPublicParam (Parameter b _ _) = b
isPublicParam (Bind _ _) = error "isPublicParam: not a Parameter"



-- | Get label from 'Expr'
getLabel :: Expr a -> a
getLabel (Number _ l)    = l
getLabel (Boolean _ l)   = l
getLabel (Id _ l)        = l
getLabel (Prim1 _ _ l)   = l
getLabel (Prim2 _ _ _ l) = l
getLabel (If    _ _ _ l) = l
getLabel (Let _ _ _ l)   = l
getLabel (App _ _ l)     = l

--------------------------------------------------------------------------------
-- | Abstract Syntax - ANF
--------------------------------------------------------------------------------

-- | Immediate Expressions
data ImmExpr a = ImmNum   !Integer  a 
               | ImmBool  !Bool     a
               | ImmId    !Id       a
  deriving (Show, Functor)

-- | Compound Expressions
data CExpr a = CPrim1   !Prim1        !(ImmExpr a)  a
             | CPrim2   !Prim2        !(ImmExpr a)  !(ImmExpr a) a
             | CIf      !(ImmExpr a)  !(ImmExpr a)  !(ImmExpr a) a
             | CApp     !Id           [ImmExpr a]   a
             | CImmExpr !(ImmExpr a)
  deriving (Show, Functor)

-- | ANF Expressions
data AExpr a = ALet     !(Bind a)     !(CExpr a)    !(AExpr a)   a
             | ACExpr   !(CExpr a)
  deriving (Show, Functor)


-- | 'Decl' are function definitions
data ADecl a = ADecl { aName  :: !(Bind a)
                     , aArgs  :: [Bind a]
                     , aBody  :: !(AExpr a)
                     , aLabel :: a 
                     }
  deriving (Show)
 

data AProgram a = AProg { aDecls :: [ADecl a] }
  deriving (Show)


-- | Constructing `CExpr` from let-binds
bindsCExpr :: [(Bind a, CExpr a)] -> CExpr a -> a -> AExpr a
bindsCExpr bs e l = foldr (\(x, e1) e2  -> ALet x e1 e2 l) (ACExpr e) bs

-- | Destructing `AExpr` into let-binds
exprABinds :: AExpr a -> ([(Bind a, CExpr a)], AExpr a)
exprABinds (ALet x e e' _) = ((x, e) : bs, body)
  where
    (bs, body)           = exprABinds e'
exprABinds body = ([] , body)

-- | Get label from 'CExpr'
getCLabel :: CExpr a -> a
getCLabel (CPrim1 _ _ l)   = l
getCLabel (CPrim2 _ _ _ l) = l
getCLabel (CIf    _ _ _ l) = l
getCLabel (CApp _ _ l) = l 
getCLabel (CImmExpr e) = getImmLabel e

-- | Get label from 'ImmExpr'
getImmLabel :: ImmExpr a -> a
getImmLabel (ImmNum _ l)    = l
getImmLabel (ImmBool _ l)   = l
getImmLabel (ImmId _ l)        = l


--------------------------------------------------------------------------------
-- | Pretty Printer - AST
--------------------------------------------------------------------------------

instance Pretty Prim1 where
  pretty Add1  = pretty "add1"
  pretty Sub1  = pretty "sub1"
  pretty Not   = pretty "not"
  pretty IsZero = pretty "isZero"

instance Pretty Prim2 where
  pretty Plus    = pretty "+"
  pretty Minus   = pretty "-"
  pretty Times   = pretty "*"
  pretty Divide  = pretty "/"
  pretty Less    = pretty "<"
  pretty Greater = pretty ">"
  pretty Equal   = pretty "=="

instance Pretty (Bind a) where
  pretty (Bind x _) = pretty x
  pretty (Parameter True x _)  = pretty "public" <+> pretty x
  pretty (Parameter False x _) = pretty "private" <+> pretty x

instance Pretty (Expr a) where
  pretty (Number n _)  = pretty n
  pretty (Boolean b _) = pretty b
  pretty (Id x _)      = pretty x
  pretty (Prim1 o e _) = pretty o <> pretty "(" <> pretty e <> pretty ")"
  pretty (Prim2 o l r _) = pretty l <+> pretty o <+> pretty r
  pretty (If    c t e _) = pretty "(if" <+> pretty c <> pretty ":" <+> pretty t <+> pretty "else:" <+> pretty e <> pretty ")"
  pretty e@(Let {}) = pretty "(let" <+> pretty_binds bs <+> pretty "in" <+> pretty b <> pretty ")"
    where 
      (bs, b) = exprBinds e
      pretty_binds :: [(Bind a, Expr a)] -> Doc b
      pretty_binds bs = sep (punctuate comma (pretty_pair <$> bs))
      pretty_pair :: (Bind a, Expr a) -> Doc b
      pretty_pair (b, e) = pretty b <+> pretty "=" <+> pretty e
  pretty (App f es _) = pretty f <> pretty "(" <> pretty_many es <> pretty ")"

instance Pretty (Decl a) where
  pretty (Decl f xs e _) = pretty "def" <+> pretty f <> pretty "(" <> pretty_many xs <> pretty "):" <>
                             line <> indent 4 (pretty e) 


instance Pretty (Program a) where
  pretty (Prog ds) = vsep (pretty <$> ds)

pretty_many :: (Pretty a) => [a] -> Doc b
pretty_many xs = sep (punctuate comma (pretty <$> xs))


--------------------------------------------------------------------------------
-- | Pretty Printer - ANF
--------------------------------------------------------------------------------

instance Pretty (ImmExpr a) where
  pretty (ImmNum n _)     = pretty n
  pretty (ImmBool b _)    = pretty b
  pretty (ImmId x _)      = pretty x

instance Pretty (CExpr a) where
  pretty (CPrim1 o e _)   = pretty o <> pretty "(" <> pretty e <> pretty ")"
  pretty (CPrim2 o l r _) = pretty l <+> pretty o <+> pretty r
  pretty (CIf    c t e _) = pretty "(if" <+> pretty c <> pretty ":" <+> pretty t <+> pretty "else:" <+> pretty e <> pretty ")"
  pretty (CApp f es _)    = pretty f <> pretty "(" <> pretty_many es <> pretty ")"
  pretty (CImmExpr e)     = pretty e

instance Pretty (AExpr a) where
  pretty e@(ALet {}) = pretty "(let" <+> pretty_binds bs <+> pretty "in" <+> pretty b <> pretty ")"
    where 
      (bs, b) = exprABinds e
      pretty_binds :: [(Bind a, CExpr a)] -> Doc b
      pretty_binds bs = sep (punctuate comma (pretty_pair <$> bs))
      pretty_pair :: (Bind a, CExpr a) -> Doc b
      pretty_pair (b, e) = pretty b <+> pretty "=" <+> pretty e

  pretty (ACExpr e)       = pretty e
 
instance Pretty (ADecl a) where
  pretty (ADecl f xs e _) = pretty "def" <+> pretty f <> pretty "(" <> pretty_many xs <> pretty "):" <>
                             line <> indent 4 (pretty e) 

instance Pretty (AProgram a) where
  pretty (AProg ds) = vsep (pretty <$> ds)



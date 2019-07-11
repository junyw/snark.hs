module Compiler.Compiler
  ( tag
  , rename
  , anormal
  , atag
  , genCircuit
  , compile
  ) where

import Field.Fr
import Compiler.R1CS
import Compiler.QAP
import Compiler.AST
import Compiler.Parser
import Compiler.Circuit 

import Control.Exception (Exception, throw, catch)

import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Text.Printf as Printf

--------------------------------------------------------------------------------
-- | Tagging
--------------------------------------------------------------------------------

tag :: Program a -> Program Tag
tag (Prog ds) = Prog (tag_decl 0 <$> ds)
  where 
    tag_decl :: Tag -> Decl a -> Decl Tag
    tag_decl i Decl{fName, fArgs, fBody, fLabel} = 
      Decl{fName=fName_tag, fArgs=fArgs_tag, fBody=fBody_tag, fLabel=i}
      where
        (i', fName_tag) = tag_bind i fName
        (i'', fArgs_tag) = tag_binds i' fArgs
        (i''', fBody_tag) = tag_expr i'' fBody


tag_expr :: Tag -> Expr a -> (Tag, Expr Tag)
tag_expr i (Number  n _)     = (i+1, Number n i)
tag_expr i (Boolean b _)     = (i+1, Boolean b i)
tag_expr i (Id   x _)        = (i+1, Id x i)
tag_expr i (Prim1 op e1 _)   = (i', Prim1 op e1_tag i)
  where
    (i', e1_tag) = tag_expr i e1
  
tag_expr i (Prim2 op e1 e2 _) = (i'', Prim2 op e1_tag e2_tag i)
  where 
    (i', e1_tag) = tag_expr (i+1) e1
    (i'', e2_tag) = tag_expr i' e2

tag_expr i (If c e1 e2 _)    = (i''', If c_tag e1_tag e2_tag i)
  where 
    (i', c_tag) = tag_expr (i+1) c
    (i'', e1_tag) = tag_expr i' e1
    (i''', e2_tag) = tag_expr i'' e2

tag_expr i (App f args _)    = (i', App f args_tags i)
  where
    (i', args_tags) = tag_exprs (i+1) args 
  
tag_expr i (Let x e b _)       = (i''', Let x_tag e_tag b_tag i)
  where
    (i', x_tag) = tag_bind (i+1) x
    (i'', e_tag) = tag_expr i' e
    (i''', b_tag) = tag_expr i'' b 

tag_exprs :: Tag -> [Expr a] -> (Tag, [Expr Tag])
tag_exprs i [] = (i, [])
tag_exprs i (e:es) = (i'', e_tag:es_tags)
  where
    (i', e_tag) = tag_expr i e
    (i'', es_tags) = tag_exprs i' es

tag_binds :: Tag -> [Bind a] -> (Tag, [Bind Tag])
tag_binds i [] = (i, [])
tag_binds i (b:bs) = (i'', b_tag:bs_tags)
  where
    (i', b_tag) = tag_bind i b
    (i'', bs_tags) = tag_binds i' bs

tag_bind :: Tag -> Bind a -> (Tag, Bind Tag)
tag_bind i (Bind x _) = (i+1, Bind x i)
tag_bind i (Parameter b x _) = (i+1, Parameter b x i)

--------------------------------------------------------------------------------
-- | Renaming
--------------------------------------------------------------------------------

type Env = Map.Map Id Id
find :: Id -> Env -> Id
find k env = case Map.lookup k env of
                   Just v -> v
                   Nothing -> error $ "rename find - cannot find id: " ++ k

rename :: Program Tag -> Program Tag
rename (Prog ds) = Prog (main':funs')
  where
    (mains, funs) = List.partition (\decl -> (bindId (fName decl)) == "main") ds
    main = head mains
    main' = rename_main main
    funs' = rename_decl <$> funs
    rename_main :: Decl Tag -> Decl Tag 
    rename_main Decl {fName=name, fArgs=args, fBody=b, fLabel=tag} =
      Decl {fName=name, fArgs=args', fBody=b', fLabel=tag}
      where     
        env = foldl (\env param -> case param of 
                      (Parameter True  x _) -> Map.insert x (rename_public x) env
                      (Parameter False x _) -> Map.insert x x env) 
                      Map.empty args
        args' = (\param -> case param of 
                    (Parameter True  x tag) -> Parameter True (rename_public x) tag
                    (Parameter False x _)   -> param) <$> args
        b' = rename_expr env b

    rename_public :: Id -> Id
    rename_public x = Printf.printf "&%s" x
    rename_decl :: Decl Tag -> Decl Tag
    rename_decl Decl {fName=name, fArgs=args, fBody=b, fLabel=tag} = 
      Decl {fName=name, fArgs=args, fBody=b', fLabel=tag}
      where 
        env = Map.fromList ((\b -> (bindId b, bindId b)) <$> args)
        b' = rename_expr env b

rename_expr :: Env -> Expr Tag -> Expr Tag
rename_expr env e@(Number _ _)  = e
rename_expr env e@(Boolean _ _) = e
rename_expr env (Id x tag) = Id (find x env) tag
rename_expr env (Prim1 op e tag) = Prim1 op (rename_expr env e) tag
rename_expr env (Prim2 op e1 e2 tag) = Prim2 op (rename_expr env e1) (rename_expr env e2) tag
rename_expr env (If c e1 e2 tag)     = If (rename_expr env c) (rename_expr env e1) (rename_expr env e2) tag

rename_expr env (App f args tag)    = App f (rename_expr env <$> args) tag
  
rename_expr env (Let x e b tag)       = Let x' e' b' tag
  where
    x' = rename_bind x
    e' = rename_expr env e
    env' = Map.insert (bindId x) (bindId x') env 
    b' = rename_expr env' b

rename_bind :: Bind Tag -> Bind Tag
rename_bind (Bind x tag) = Bind x' tag
  where
    x' = Printf.printf "%s_%d" x tag

--------------------------------------------------------------------------------
-- | Convert Program to A-Normal Form
--------------------------------------------------------------------------------

anormal :: Program Tag -> AProgram ()
anormal (Prog ds) = AProg (anfDecl <$> ds)
  where
    anfDecl Decl{fName, fArgs, fBody, fLabel} = 
      ADecl { aName  = untag_bind fName
            , aArgs  = untag_binds fArgs
            , aBody  = anf fBody
            , aLabel = ()
            }

anf :: Expr Tag -> AExpr ()
anf e = stitch ctxt cexpr
  where 
    (ctxt, cexpr) = canf e

type Binds a = [(Bind a, CExpr a)]

canf :: Expr Tag -> (Binds (), CExpr ())
canf (Number  n _)     = ([], CImmExpr(ImmNum n ()))
canf (Boolean b _)     = ([], CImmExpr(ImmBool b ()))
canf (Id   x _)        = ([], CImmExpr(ImmId x ()))
canf (Prim1 op arg _)     = (arg_ctxt, CPrim1 op arg_imm ())
  where
    (arg_ctxt, arg_imm) = imm arg

canf (Prim2 op e1 e2 _) = (e2_ctxt ++ e1_ctxt, CPrim2 op e1_imm e2_imm ())
  where
    (e1_ctxt, e1_imm)     = imm e1
    (e2_ctxt, e2_imm)     = imm e2

canf (If c e1 e2 _)    = (ctxt, CIf c_imm e1_imm e2_imm ())
  where
    (c_ctxt, c_imm)  = imm c
    (e1_ctxt,      e1_imm)  = imm  e1
    (e2_ctxt,     e2_imm)  = imm  e2
    ctxt = e2_ctxt ++ e1_ctxt ++ c_ctxt

canf (App f args _)      = (ctxt, CApp f args_imms ())
  where
    (ctxt, args_imms)  = imms args

canf (Let x e b _)       = (ctx, b_canf)
  where
    (e_ctxt, e_canf) = canf  e
    (b_ctxt, b_canf) = canf  b
    ctx = b_ctxt ++ (untag_bind x, e_canf):e_ctxt

imms :: [Expr Tag] -> (Binds (), [ImmExpr ()])
imms []     = ([], [])
imms (e:es) = (es_ctxt ++ e_ctxt, e_imm : es_imms)
  where 
    (e_ctxt, e_imm) = imm e
    (es_ctxt, es_imms) = imms es

imm :: Expr Tag -> (Binds (), ImmExpr ())
imm (Number  n _) = ([], ImmNum  n ())
imm (Boolean b _) = ([], ImmBool b ())
imm (Id x _)   = ([], ImmId x ())

imm (Prim1 op arg tag)    = (ctxt, mkId tmp)
  where
    (arg_ctxt, arg_imm)    = imm arg
    tmp                    = mkBind "unary" tag
    ctxt = (tmp, CPrim1 op arg_imm ()) : arg_ctxt

imm (Prim2 op e1 e2 tag) = (ctxt, mkId tmp)
  where
    (e_ctxts, [e1_imm, e2_imm]) = imms [e1, e2]
    tmp                      = mkBind "binary" tag
    ctxt = (tmp, CPrim2 op e1_imm e2_imm ()) : e_ctxts

imm (App f args tag) = (ctxt, mkId tmp)
  where
    (args_ctxt, args_imms) = imms args
    tmp = mkBind "app" tag
    ctxt = (tmp, CApp f args_imms ()) : args_ctxt

imm (If c e1 e2 tag) = (ctxt, mkId tmp)
  where
    (c_ctxt, c_imm)  = imm c
    (e1_ctxt,     e1_imm)  = imm  e1
    (e2_ctxt,     e2_imm)  = imm  e2
    tmp = mkBind "if" tag
    ctxt = (tmp, CIf c_imm e1_imm e2_imm ()) : e2_ctxt ++ e1_ctxt ++ c_ctxt 

imm (Let x e0 e1 _) = (ctxt, e1_imm)
  where
    (e0_ctxt, e0_canf) = canf e0
    (e1_ctxt, e1_imm) = imm  e1
    ctxt = e0_ctxt ++ (untag_bind x, e0_canf):e1_ctxt
   

untag_bind :: Bind a -> Bind ()
untag_bind (Bind x _) = (Bind x ())
untag_bind (Parameter b x _) = (Parameter b x ())

untag_binds :: [Bind a] -> [Bind ()]
untag_binds [] = []
untag_binds (b:bs) = b':bs'
  where 
    b' = untag_bind b
    bs' = untag_binds bs

mkId :: Bind a -> ImmExpr ()
mkId x = ImmId (bindId x) ()

-- | 'mkBind' returns a new variable
mkBind :: String -> Tag -> Bind ()
mkBind str i = Bind x ()
  where
    x = "~" ++ str ++ show i


stitch :: Binds a -> CExpr a -> AExpr a
stitch bs e = bindsCExpr (reverse bs) e (getCLabel e)


--------------------------------------------------------------------------------
-- | Tagging - ANF
--------------------------------------------------------------------------------
atag :: AProgram () -> AProgram Tag
atag (AProg ds) = AProg (atag_decl 0 <$> ds)
  where 
    atag_decl i ADecl { aName, aArgs, aBody, aLabel} = 
             ADecl{aName=aName_tag, aArgs=aArgs_tag, aBody=aBody_tag, aLabel=i}
      where
        (i', aName_tag) = btag i aName
        (i'', aArgs_tag) = btags i' aArgs
        (i''', aBody_tag) = etag i'' aBody


etag :: Tag -> AExpr a -> (Tag, AExpr Tag)
etag i (ALet bind cexpr aexpr _) = (i''', ALet bind_tag cexpr_tag aexpr_tag i)
  where 
    (i', bind_tag) = btag i bind
    (i'', cexpr_tag) = ctag i' cexpr
    (i''', aexpr_tag) = etag i'' aexpr 
etag i (ACExpr cexpr) = (i', ACExpr cexpr_tag)
  where
    (i', cexpr_tag) = ctag i cexpr

ctag :: Tag -> CExpr a -> (Tag, CExpr Tag)
ctag i (CPrim1 op e _) = (i', CPrim1 op e_tag i)
  where 
    (i', e_tag) = immtag i e

ctag i (CPrim2 op e1 e2 _) = (i'', CPrim2 op e1_tag e2_tag i)
  where 
    (i', e1_tag) = immtag i e1
    (i'', e2_tag) = immtag i' e2

ctag i (CIf c e1 e2 _) = (i''', CIf c_tag e1_tag e2_tag i)
  where
    (i', c_tag) = immtag i c
    (i'', e1_tag) = immtag i' e1
    (i''', e2_tag) = immtag i'' e2

ctag i (CApp x e _) = (i', CApp x e_tags i)
  where 
    (i', e_tags) = immtags i e

ctag i (CImmExpr e) = (i', CImmExpr e_tag)
  where
    (i', e_tag) = immtag i e

immtags :: Tag -> [ImmExpr a] -> (Tag, [ImmExpr Tag])
immtags i [] = (i, [])
immtags i (e:es) = (i'', e_tag:es_tags)
  where 
    (i', e_tag) = immtag i e
    (i'', es_tags) = immtags i' es

immtag :: Tag -> ImmExpr a -> (Tag, ImmExpr Tag)
immtag i (ImmNum n _) = (i+1, ImmNum n i)
immtag i (ImmBool b _) = (i+1, ImmBool b i)
immtag i (ImmId x _) = (i+1, ImmId x i)

btags :: Tag -> [Bind a] -> (Tag, [Bind Tag])
btags i [] = (i, [])
btags i (b:bs) = (i'', b_tag:bs_tags)
  where 
    (i', b_tag) = btag i b
    (i'', bs_tags) = btags i' bs



btag :: Tag -> Bind a -> (Tag, Bind Tag)
btag i (Bind x _) = (i+1, Bind x i)
btag i (Parameter b x _) = (i+1, Parameter b x i)

--------------------------------------------------------------------------------
-- | Compile to Circuit 
--------------------------------------------------------------------------------

type CEnv = Map.Map Id [CWire]

genCircuit :: AProgram Tag -> Circuit
genCircuit (AProg ds) = circuit
  where
    (main', funs) = List.partition (\decl -> (bindId (aName decl)) == "main") ds
    main = head main'
    (public, private) = List.partition isPublicParam (aArgs main)
    (env, gates) = genMain main
    circuit = Circuit { cPublicParams  = bindToWire <$> public
                      , cPrivateParams = bindToWire <$> private
                      , cGates = gates
                      , cWires = (\x -> CWire x 1) <$> (Map.keys env)
                      }

genMain :: ADecl Tag -> (CEnv, [CGate])
genMain ADecl { aName, aArgs, aBody, aLabel } = (env, gates')
  where
    final_output = CWire "~final" 1
    init_env = Map.fromList $ ((\x -> (bindId x, [])) <$> aArgs)
    (env, gates) = genAExpr init_env final_output aBody
    -- add final constraint
    final_wire = genImm env (ImmId "~final" (-1))
    gates' = gates ++ [(CAssert CTimes final_wire [(CInt 1)] (CInt 1))]
 
genAExpr :: CEnv -> CWire -> AExpr Tag -> (CEnv, [CGate])
genAExpr env out (ALet b cexpr aexpr _) = (env''', gates++gates')
  where
    (env', gates) = genCExpr env (bindToWire b) cexpr
    bexpr = case Map.lookup (bindId b) env' of                                 
              Nothing -> [CWire (bindId b) 1]
              Just wires -> wires

    env'' = Map.insert (bindId b) bexpr env'
    (env''', gates') = genAExpr env'' out aexpr 

genAExpr env out (ACExpr cexpr) = (env', cgates)
  where
    (env', cgates) = genCExpr env out cexpr 


genCExpr :: CEnv -> CWire -> CExpr Tag  -> (CEnv, [CGate])

genCExpr env out (CPrim1 Not e tag) = (env', [gate])
  where
    wires = genImm env e
    tmp = mkWire "wire_not" tag
    gate = CGate CTimes wires wires tmp
    env'  = Map.insert (toName out) [(CInt 1), negateWire tmp] (insertTmpWires [tmp] env)  

genCExpr env out (CPrim1 IsZero e tag) = (env', [gate1, gate2, gate3])
  where
    tmp_m  = mkWire "wire_m" tag
    tmp_y  = mkWire "wire_y" tag
    tmp_yy = mkWire "wire_yy" tag
    x_wires = genImm env e
    gate1 = CNormalize x_wires tmp_m tmp_y
    gate2 = CAssert CTimes [(CInt 1), negateWire tmp_y] x_wires (CInt 0)
    gate3 = CGate CTimes [tmp_y] [tmp_y] tmp_yy
    env' = Map.insert (toName out)  [(CInt 1), negateWire tmp_yy] (insertTmpWires [tmp_m, tmp_y, tmp_yy] env)  

genCExpr env out (CPrim1 Add1 e _) = undefined

genCExpr env out (CPrim1 Sub1 e _) = undefined

genCExpr env out (CPrim2 Times e1 e2 _) = (env, [CGate CTimes wires1 wires2 out])
  where
    wires1 = genImm env e1 
    wires2 = genImm env e2

genCExpr env out (CPrim2 Divide e1 e2 _) = (env, [CGate CDivide wires1 wires2 out])
  where
    wires1 = genImm env e1 
    wires2 = genImm env e2

genCExpr env out (CPrim2 Plus e1 e2 _) = (env', [])
  where
    wires1 = genImm env e1 
    wires2 = genImm env e2
    env'   = Map.insert (toName out) (wires1++wires2) env

genCExpr env out (CPrim2 Minus e1 e2 _) = (env', [])
  where
    wires1 = genImm env e1 
    wires2 = genImm env e2
    env'   = Map.insert (toName out) (wires1++(negateWires wires2)) env

genCExpr env out (CPrim2 Equal a b tag) = (env', [gate1, gate2, gate3])
  where
    tmp_m  = mkWire "wire_m" tag
    tmp_y  = mkWire "wire_y" tag
    tmp_yy = mkWire "wire_yy" tag
    a_wires = genImm env a
    b_wires = genImm env b
    x_wires = a_wires ++ (negateWires b_wires)
    gate1 = CNormalize x_wires tmp_m tmp_y
    gate2 = CAssert CTimes [(CInt 1), negateWire tmp_y] x_wires (CInt 0)
    gate3 = CGate CTimes [tmp_y] [tmp_y] tmp_yy
    env' = Map.insert (toName out)  [(CInt 1), negateWire tmp_yy] (insertTmpWires [tmp_m, tmp_y, tmp_yy] env)  

genCExpr env out (CIf c e1 e2 tag) = (env', [gate1, gate2])
  where
    wires0 = genImm env c
    wires1 = genImm env e1
    wires2 = genImm env e2
    out1 = mkWire "wire_if_then" tag
    out2 = mkWire "wire_if_else" tag
    gate1 = CGate CTimes wires0 wires1 out1
    gate2 = CGate CTimes ((CInt 1):(negateWires wires0)) wires2 out2
    env'  = Map.insert (toName out) [out1, out2] (insertTmpWires [out1, out2] env) 

genCExpr env out (CImmExpr immexpr) = (env, [gate])
  where
    wires = genImm env immexpr
    gate = CGate CTimes wires [CInt 1] out 


genImm :: CEnv -> ImmExpr a -> [CWire]
genImm env (ImmNum n l) = [CInt n]
genImm env (ImmBool True l) = [CInt 1]
genImm env (ImmBool False l) = [CInt 0]

genImm env (ImmId x l) = case Map.lookup x env of
                                Nothing -> error $ "genImm: cannot find id: " ++ x
                                Just []    -> [CWire x 1]
                                Just wires -> wires

insertTmpWires :: [CWire] -> CEnv -> CEnv 
insertTmpWires [] env = env
insertTmpWires (w:ws) env = env''
  where
    env' = Map.insert (toName w) [] env 
    env'' = insertTmpWires ws env'

-- | 'mkWire' returns a new variable as CWire
mkWire :: String -> Tag -> CWire
mkWire str i = CWire x 1
  where
    x = "~" ++ str ++ show i

bindToWire :: Bind a -> CWire
bindToWire (Bind x _) = CWire x 1
bindToWire (Parameter _ x _) = CWire x 1 

--------------------------------------------------------------------------------
-- | Compile
--------------------------------------------------------------------------------
compile :: FilePath -> Text -> Circuit
compile f src = genCircuit . atag . anormal . rename . tag $ parse f src



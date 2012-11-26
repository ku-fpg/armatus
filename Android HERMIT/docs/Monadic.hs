{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE GADTs, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, OverlappingInstances, UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module Monadic where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity

import Data.Proxy

-- Hobbits-related stuff
import Data.Binding.Hobbits
import qualified Data.Type.List.Map as Map
import Data.Type.List.Proof.Member as Member

import Galois

-- misc stuff
instance Liftable Bool where
    mbLift [nuP| True |] = True
    mbLift [nuP| False |] = False


------------------------------------------------------------
-- Interpreter monad
------------------------------------------------------------

type Env ctx = MapC Identity ctx

--newtype InterpM ctx a = InterpM { unInterpM :: ReaderT (Env ctx) Identity a }
newtype InterpM a = InterpM { unInterpM :: StateT Int Identity a }

instance Monad InterpM where
    return = InterpM . return
    m >>= f = InterpM $ unInterpM m >>= unInterpM . f

instance MonadState Int InterpM where
    get = InterpM get
    put = InterpM . put


------------------------------------------------------------
-- Object language types
------------------------------------------------------------

data Type a where
    Bool :: Type Bool
    Int :: Type Int
    Arrow :: Type a -> Type b -> Type (a -> b)
    M :: Type a -> Type (InterpM a)

class IsType a where
    typeRep :: Type a

instance IsType Bool where
    typeRep = Bool

instance IsType Int where
    typeRep = Int

instance (IsType a, IsType b) => IsType (a -> b) where
    typeRep = Arrow typeRep typeRep

instance IsType a => IsType (InterpM a) where
    typeRep = M typeRep


------------------------------------------------------------
-- Intensional type functions
------------------------------------------------------------

type family Apply f a

data ApplyF f a = IsType a => ApplyF (Apply f a)
unApplyF :: ApplyF f a -> Apply f a
unApplyF (ApplyF x) = x

------------------------------------------------------------
-- Interpreter class
------------------------------------------------------------

class Semantics f where
{-
    intLit :: Int -> ApplyF f Int
    boolLit :: Bool -> ApplyF f Bool

    lam :: (ApplyF f a -> ApplyF f b) -> ApplyF f (a -> b)
    app :: ApplyF f (a -> b) -> ApplyF f a -> ApplyF f b

    if_ :: ApplyF f Bool -> ApplyF f a -> ApplyF f a -> ApplyF f a

    lt :: ApplyF f Int -> ApplyF f Int -> ApplyF f Bool
    plus :: ApplyF f Int -> ApplyF f Int -> ApplyF f Int

    ret :: ApplyF f a -> ApplyF f (InterpM a)
    bind :: ApplyF f (InterpM a) -> ApplyF f (a -> InterpM b) -> ApplyF f (InterpM b)
    incr :: ApplyF f (InterpM Int)
-}

    intLit :: Int -> ApplyF f Int
    --intLit i = intLitP Proxy i
    boolLit :: Bool -> ApplyF f Bool
    --boolLitP _ = boolLit

    lam :: (IsType a, IsType b) => (ApplyF f a -> ApplyF f b) -> ApplyF f (a -> b)
    --lamP _ = lam

    app :: IsType b => ApplyF f (a -> b) -> ApplyF f a -> ApplyF f b
    --appP _ = app

    if_ :: ApplyF f Bool -> ApplyF f a -> ApplyF f a -> ApplyF f a
    --if_P _ = if_

    lt :: ApplyF f Int -> ApplyF f Int -> ApplyF f Bool
    --ltP _ = lt
    plus :: ApplyF f Int -> ApplyF f Int -> ApplyF f Int
    --plusP _ = plus

    ret :: ApplyF f a -> ApplyF f (InterpM a)
    --retP _ = ret
    bind :: IsType b => ApplyF f (InterpM a) -> ApplyF f (a -> InterpM b) -> ApplyF f (InterpM b)
    --bindP _ = bind
    incr :: ApplyF f (InterpM Int)
    --incrP _ = incr


------------------------------------------------------------
-- A Simple Interpreter
------------------------------------------------------------

-- Identity type function
data Id
type instance Apply Id a = a

instance Semantics Id where
    intLit i = ApplyF i
    boolLit b = ApplyF b
    lam f = ApplyF $ unApplyF . f . ApplyF
    app (ApplyF f) (ApplyF a) = ApplyF $ f a

    if_ (ApplyF True) x y = x
    if_ (ApplyF False) x y = y

    lt (ApplyF i1) (ApplyF i2) = ApplyF $ i1 < i2
    plus (ApplyF i1) (ApplyF i2) = ApplyF $ i1 + i2

    ret (ApplyF x) = ApplyF $ return x
    bind (ApplyF m) (ApplyF f) = ApplyF $ m >>= f
    incr = ApplyF $ modify ((+) 1) >> get


------------------------------------------------------------
-- The "Code" type
------------------------------------------------------------

data Term a where
    IntLit :: Int -> Term Int
    BoolLit :: Bool -> Term Bool

    Var :: Name a -> Term a
    Lam :: Binding a (Term b) -> Term (a -> b)
    App :: Term (a -> b) -> Term a -> Term b

    If :: Term Bool -> Term a -> Term a -> Term a
    Lt :: Term Int -> Term Int -> Term Bool
    Plus :: Term Int -> Term Int -> Term Int

    Ret :: Term a -> Term (InterpM a)
    Bind :: Term (InterpM a) -> Term (a -> InterpM b) -> Term (InterpM b)
    Incr :: Term (InterpM Int)


eval :: Term a -> a
eval t = mbEval Nil (emptyMb t)

mbEval :: MapC Identity ctx -> Mb ctx (Term a) -> a
mbEval env [nuP| IntLit i |] = mbLift i
mbEval env [nuP| BoolLit b |] = mbLift b
mbEval env [nuP| Var x |]
    | Left inCtx <- mbNameBoundP x
    = runIdentity $ Map.lookup inCtx env
mbEval env [nuP| Var x |] = error "Unbound variable!"
mbEval env [nuP| Lam b |] = \x -> mbEval (env :> Identity x) (combineMb b)
mbEval env [nuP| App t1 t2 |] = (mbEval env t1) (mbEval env t2)
mbEval env [nuP| If b t1 t2 |] =
    if mbEval env b then mbEval env t1 else mbEval env t2
mbEval env [nuP| Lt t1 t2 |] = mbEval env t1 < mbEval env t2
mbEval env [nuP| Plus t1 t2 |] = mbEval env t1 + mbEval env t2
mbEval env [nuP| Ret t |] = return $ mbEval env t
mbEval env [nuP| Bind t1 t2 |] = mbEval env t1 >>= mbEval env t2
mbEval env [nuP| Incr |] = modify ((+) 1) >> get


------------------------------------------------------------
-- A Staged Interpreter
------------------------------------------------------------

-- Term type function
data TermF
type instance Apply TermF a = Term a

instance Semantics TermF where
    intLit i = ApplyF $ IntLit i
    boolLit b = ApplyF $ BoolLit b

    lam f = ApplyF $ Lam $ nu $ unApplyF . f . ApplyF . Var
    app (ApplyF f) (ApplyF a) = ApplyF $ App f a

    if_ (ApplyF b) (ApplyF t1) (ApplyF t2) = ApplyF $ If b t1 t2

    lt (ApplyF t1) (ApplyF t2) = ApplyF $ Lt t1 t2
    plus (ApplyF t1) (ApplyF t2) = ApplyF $ Plus t1 t2

    ret (ApplyF x) = ApplyF $ Ret x
    bind (ApplyF m) (ApplyF f) = ApplyF $ Bind m f
    incr = ApplyF Incr


------------------------------------------------------------
-- Galois connections over just the object language types
------------------------------------------------------------

{-
class GaloisConnection1F spec impl where
    abstr1f :: (Proxy spec, Proxy impl) -> ApplyF impl a -> ApplyF spec a
    repr1f :: (Proxy spec, Proxy impl) -> ApplyF spec a -> ApplyF impl a
-}

class GaloisConnection1F spec impl where
    abstr1f :: Proxy (spec, impl) -> ApplyF impl a -> ApplyF spec a
    repr1f :: Proxy (spec, impl) -> ApplyF spec a -> ApplyF impl a

instance GaloisConnection1F spec impl => GaloisConnection (ApplyF spec a) (ApplyF impl a) where
    abstr = abstr1f Proxy
    repr = repr1f Proxy

{-
class GaloisConnection1F spec impl where
    abstr1f :: IsType a => Proxy a -> ApplyF impl a -> ApplyF spec a
    repr1f :: IsType a => Proxy a -> ApplyF spec a -> ApplyF impl a
-}

------------------------------------------------------------
-- Derived interpreters
------------------------------------------------------------

class (Semantics f, GaloisConnection1F f g) => DerivedSemantics f g where
    intLitD :: (Proxy (f, g)) -> Int -> ApplyF g Int
    intLitD p i = repr1f p $ intLit i

    boolLitD :: (Proxy (f, g)) -> Bool -> ApplyF g Bool
    boolLitD p b = repr1f p $ boolLit b

    lamD :: (IsType a, IsType b) => (Proxy (f, g)) -> (ApplyF g a -> ApplyF g b) -> ApplyF g (a -> b)
    lamD p f = repr1f p $ lam $ abstr1f p . f . repr1f p

    appD :: IsType b => (Proxy (f, g)) -> ApplyF g (a -> b) -> ApplyF g a -> ApplyF g b
    appD p f a = repr1f p $ app (abstr1f p f) (abstr1f p a)

    if_D :: (Proxy (f, g)) -> ApplyF g Bool -> ApplyF g a -> ApplyF g a -> ApplyF g a
    if_D p b x y = repr1f p $ if_ (abstr1f p b) (abstr1f p x) (abstr1f p y)

    ltD :: (Proxy (f, g)) -> ApplyF g Int -> ApplyF g Int -> ApplyF g Bool
    ltD p x y = repr1f p $ lt (abstr1f p x) (abstr1f p y)
    plusD :: (Proxy (f, g)) -> ApplyF g Int -> ApplyF g Int -> ApplyF g Int
    plusD p x y = repr1f p $ plus (abstr1f p x) (abstr1f p y)

    retD :: (Proxy (f, g)) -> ApplyF g a -> ApplyF g (InterpM a)
    retD p x = repr1f p $ ret $ abstr1f p x
    bindD :: IsType b => (Proxy (f, g)) -> ApplyF g (InterpM a) -> ApplyF g (a -> InterpM b) -> ApplyF g (InterpM b)
    bindD p m f = repr1f p $ bind (abstr1f p m) (abstr1f p f)
    incrD :: (Proxy (f, g)) -> ApplyF g (InterpM Int)
    incrD p = repr1f p incr

------------------------------------------------------------
-- Relating the staged and unstaged interpreters
------------------------------------------------------------

bottom = bottom

idToTerm :: Type a -> a -> Term a
idToTerm Int i = IntLit i
idToTerm Bool b = BoolLit b
idToTerm (Arrow tp1 tp2) f =
    Lam $ nu $ \_ -> idToTerm tp2 $ f bottom
--idToTerm (M tp) m = interpMToTerm $ m >>= return . idToTerm tp
idToTerm (M tp) m = bottom

--interpMToTerm :: InterpM (Term a) -> Term (InterpM a)
--interpMToTerm m = Ret $ runIdentity $ (runStateT $ unInterpM m) bottom

instance GaloisConnection1F Id TermF where
    abstr1f _ (ApplyF x) = ApplyF $ eval x
    repr1f _ (ApplyF x) = ApplyF $ idToTerm typeRep x

-- FIXME: I don't think the above is a Galois connection!!!


------------------------------------------------------------
-- online partial evaluation
------------------------------------------------------------

newtype SInterpM f a = SInterpM { unSInterpM :: StateT (ApplyF f Int) Identity a }


{-
data PEF_static f
type instance ApplyF (PEF_static f) Int = ApplyF f Int
type instance ApplyF (PEF_static f) Bool = ApplyF f Bool
type instance ApplyF (PEF_static f) (a -> b) =
    (ApplyF (PEF_static f) a) -> (ApplyF (PEF_static f) b)
type instance ApplyF (PEF_static f) (InterpM a) =
    SInterpM f (ApplyF (PEF_static f) a)

data PEF f
type instance ApplyF (PEF f) a = (Maybe (ApplyF (PEF_static f) a), ApplyF f a)
-}

data PEF f
data PEF_static (f :: *)

type instance Apply (PEF f) a = (Maybe (Apply (PEF_static f) a), ApplyF f a)

type instance Apply (PEF_static f) Int = Int
type instance Apply (PEF_static f) Bool = Bool
type instance Apply (PEF_static f) (a -> b) =
    ApplyF (PEF f) a -> ApplyF (PEF f) b
type instance Apply (PEF_static f) (InterpM a) =
    SInterpM f (ApplyF (PEF f) a)

instance GaloisConnection1F f (PEF f) where
    abstr1f _ (ApplyF (_, x)) = x
    repr1f _ (ApplyF x) = ApplyF (Nothing, ApplyF x)
{-
    abstr1f _ (ApplyF x :: ApplyF (PEF f) a) =
        case typeRep :: Type a of
          Int -> snd x
          Bool -> snd x
          Arrow _ _ -> snd x
          M _ -> snd x
    repr1f _ (ApplyF x :: ApplyF f a) =
        ApplyF $
        case typeRep :: Type a of
          Int -> (Nothing, ApplyF x)
          Bool -> (Nothing, ApplyF x)
          Arrow _ _ -> (Nothing, ApplyF x)
          M _ -> (Nothing, ApplyF x)
-}

instance Semantics f => Semantics (PEF f) where
    intLit i = ApplyF (Just i, intLit i)
    boolLit b = ApplyF (Just b, boolLit b)

    lam f = let p :: Proxy (f, PEF f) = Proxy in
            ApplyF (Just f, lam $ abstr1f p . f . repr1f p)
    app (ApplyF (Just f, ff)) a = f a
    app (ApplyF (Nothing, f)) (ApplyF (_, a)) = repr1f Proxy $ app f a

    if_ (ApplyF (Just True, _)) x y = x
    if_ (ApplyF (Just False, _)) x y = y
    if_ (ApplyF (Nothing, b)) x y =
        repr1f Proxy $ if_ b (abstr x) (abstr y)

{-
    app = App

    if_ = If

    lt = Lt
    plus = Plus

    ret = Ret
    bind = Bind
    incr = Incr
-}

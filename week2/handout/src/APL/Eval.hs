{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module APL.Eval
  ( Val (..),
    runEval,
    eval,
    envEmpty,
    Error,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup = lookup

type Error = String


newtype EvalM a = EvalM (Env -> Either Error a)

instance Functor EvalM where
  fmap f (EvalM x) =
    EvalM $ \env -> case x env of
      Right v -> Right $ f v
      Left err -> Left err


instance Applicative EvalM where
  pure x = EvalM $ \_ -> Right x
  EvalM func <*> EvalM ax = EvalM $ \env ->
    case (func env, ax env) of 
      (Left err, _) -> Left err
      (_, Left err) -> Left err
      (Right f, Right x) -> Right $ f x


-- Type (>>=) (m a -> (a -> m b) -> m b)
instance Monad EvalM where
  return = pure 
  EvalM a >>= f = EvalM $ \env ->
    case a env of 
      Left err -> Left err
      Right x' -> 
        let EvalM y = f x'
        in y env 


askEnv :: EvalM Env
askEnv = EvalM $ \env -> Right env

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM a) = EvalM $ \env -> a (f env)

runEval :: EvalM a -> Either Error a
runEval (EvalM f) = f envEmpty

failure :: String -> EvalM a
failure s = EvalM $ \_ -> Left s 

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env ->
  case m1 env of 
    Left _ -> m2 env
    Right a -> Right a


eval :: Exp -> EvalM Val
eval (CstInt a) = pure $ ValInt a 
eval (CstBool a) = pure $ ValBool a 

-- Addition Case
eval (Add a b) = do
  x <- eval a 
  y <- eval b
  case (x, y) of 
    (ValInt a', ValInt b') -> pure $ ValInt $ a' + b'
    _ -> failure "Non-integer operand"

-- Subtraction Case
eval (Sub a b) = do
  x <- eval a 
  y <- eval b
  case (x, y) of 
    (ValInt a', ValInt b') -> pure $ ValInt $ a' - b'
    _ -> failure "Non-integer operand"

-- Multiplication Case
eval (Mul a b) = do
  x <- eval a 
  y <- eval b
  case (x, y) of 
    (ValInt a', ValInt b') -> pure $ ValInt $ a' * b'
    _ -> failure "Non-integer operand"

-- Multiplication Case
eval (Div a b) = do
  x <- eval a 
  y <- eval b
  case (x, y) of 
    (ValInt a, ValInt b) -> checkedDiv a b
      where 
        checkedDiv _ 0 = failure "Divide by 0 failure"
        checkedDiv u v = pure $ ValInt $ u `div` v 
    _ -> failure "Non-integer operand"

-- Pow Case
eval (Pow a b) = do
  x <- eval a
  y <- eval b
  case (x,y) of
    (ValInt a, ValInt b) -> checkedPow a b
      where
        checkedPow _ y' | y' < 0 = failure "Negative exponent"
        checkedPow x' y' = pure $ ValInt $ x' ^ y'
    _ -> failure "Non-integer operand"

-- TryCatch case
eval (TryCatch e1 e2) = catch (eval e1) (eval e2)

-- Eql Case
eval (Eql a b) = do
  x <- eval a
  y <- eval b
  case (x,y) of
    (ValInt x, ValInt y) -> pure $ ValBool $ x == y
    (ValBool x, ValBool y) -> pure $ ValBool $ x == y
    (_, _) -> failure "Invalid operands to equality"

-- If Case
eval (If cond a b) = do
  cond' <- eval cond
  case cond' of
    ValBool True -> eval a
    ValBool False -> eval b
    _ -> failure "Invalid operands to equality"

-- Var case
eval (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v

-- Let case
eval (Let var a b) = do
  v1 <- eval a
  localEnv (envExtend var v1) $ eval b

eval (Lambda v e) = do
  env <- askEnv
  pure $ ValFun env v e


-- Apply case
eval (Apply funcExpr argExpr) = do
  x <- eval funcExpr 
  y <- eval argExpr
  case (x, y) of
    (ValFun f_env var body, arg) ->
      localEnv (const $ envExtend var arg f_env) $ eval body
    (_,_) ->
      failure "Cannon apply non-function"


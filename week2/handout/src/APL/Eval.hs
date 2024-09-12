module APL.Eval
  ( Val (..),
    eval,
    runEval,
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
envLookup v env = lookup v env

type Error = String

newtype EvalM a = EvalM (Either Error a)

instance Functor EvalM where
  fmap _ (EvalM (Left e)) = EvalM $ Left e
  fmap f (EvalM (Right x)) = EvalM $ Right $ f x 

instance Applicative EvalM where
  pure a = EvalM $ Right a
  EvalM (Left e) <*> _ = EvalM $ Left e
  _ <*> EvalM (Left e) = EvalM $ Left e
  EvalM (Right f) <*> EvalM (Right x) = EvalM $ Right $ f x

-- Type (m a -> (a -> m b) -> m b)
instance Monad EvalM where
  return = pure
  EvalM (Left a) >>= _ = EvalM $ Left a
  EvalM (Right a) >>= f = f a 


runEval :: EvalM a -> Either Error a
runEval (EvalM x) = x

failure :: String -> EvalM a
failure s = EvalM $ Left s


eval :: Env -> Exp -> EvalM Val
eval env (CstInt x) = pure $ ValInt x
eval env (CstBool b) = pure $ ValBool b
eval env (Add e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  case (x,y) of
    (ValInt a, ValInt b) -> pure $ ValInt $ a + b
    _ -> failure "Non-integer operand"

eval env (Sub e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  case (x,y) of
    (ValInt a, ValInt b) -> pure $ ValInt $ a - b
    _ -> failure "Non-integer operand"
  

eval env (Mul e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  case (x,y) of
    (ValInt a, ValInt b) -> pure $ ValInt $ a * b
    _ -> failure "Non-integer operand"
  
eval env (Div a b) = do
  x <- eval env a
  y <- eval env b
  case (x,y) of
    (ValInt a, ValInt b) -> checkedDiv a b
      where
        checkedDiv _ 0 = failure "Division by zero"
        checkedDiv a b = pure $ ValInt $ a `div` b
    _ -> failure "Non-integer operand"


eval env (Pow a b) = do
  x <- eval env a
  y <- eval env b
  case (x,y) of
    (ValInt a, ValInt b) -> checkedPow a b
      where
        checkedPow x y = 
          if y < 0
            then failure "Negative exponent"
            else pure $ ValInt $ a ^ b
    _ -> failure "Non-integer operand"

eval env (Eql a b) = do
  x <- eval env a
  y <- eval env b
  case (x,y) of
    (ValInt x, ValInt y) -> pure $ ValBool $ x == y
    (ValBool x, ValBool y) -> pure $ ValBool $ x == y
    (_, _) -> failure "Invalid operands to equality"



eval env (If cond a b) = do
  cond' <- eval env cond
  case cond' of
    ValBool True -> eval env a
    ValBool False -> eval env b
    _ -> failure "Invalid operands to equality"
    

eval env (Let var a b) = do
  v1 <- eval env a
  eval (envExtend var v1 env) b



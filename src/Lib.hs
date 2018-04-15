module Lib
    ( someFunc
    ) where

import Parser
import Control.Monad.Except(ExceptT, liftEither, runExceptT)
import Control.Monad.Writer(Writer, tell, runWriter)
import Control.Monad.Trans(lift)
import Data.Semigroup((<>))

type Info = Int

type TermExe = (Term, Info)

data Term =
  TmTrue
    | TmFalse
    | TmNot Term
    | TmIf Term Term Term
    | TmZero
    | TmSucc Term
    | TmPred Term
    | TmIsZero Term

instance Show Term where
  show TmTrue = "true"
  show TmFalse = "false"
  show (TmNot term) = "not (" <> show term <> ")"
  show (TmIf termB term1 term2) = "if " <> "("<>show termB <>")"<> "{"<>show term1 <>"}"<> "{" <> show term2 <>"}"
  show TmZero = "0"
  show (TmSucc term) | isNumeric term = show $ (read $show term) + 1
  show (TmSucc term) = "succ (" <> show term <>")"
  show (TmPred term) = "pred (" <> show term<>")"
  show (TmIsZero term) = "isZero (" <> show term<>")"

isNumeric :: Term -> Bool
isNumeric TmZero = True
isNumeric (TmSucc term) = isNumeric term
isNumeric _ = False

isBool :: Term -> Bool
isBool TmTrue = True
isBool TmFalse = True
isBool _ = False

isValue :: Term -> Bool
isValue = (||) <$> isBool <*> isNumeric

-- いっつも値を評価し直すの冗長じゃない?
-- 全部評価するって決めちゃえばいいけどTmIf の後の方まで計算はしないからね
getVal :: Term -> Term
getVal = either id id . evalOnce

evalOnce :: Term -> Either Term Term
evalOnce (TmIf TmTrue t u) = return t
evalOnce (TmIf TmFalse t u) = return u
evalOnce (TmIf notVal t u) | not (isValue notVal) = return (TmIf (getVal notVal) t u)
evalOnce (TmNot TmFalse) = return TmTrue
evalOnce (TmNot TmTrue) = return TmFalse
evalOnce (TmNot notVal) | not (isValue notVal) = return $ TmNot $ getVal notVal
evalOnce (TmSucc notVal) | not (isValue notVal) = return $ TmSucc $ getVal notVal
evalOnce (TmPred TmZero) = return TmZero
evalOnce (TmPred (TmSucc nv)) | isNumeric nv = return nv
evalOnce (TmPred notVal) | not (isValue notVal) = return $ TmPred $ getVal notVal
evalOnce (TmIsZero TmZero) = return TmTrue
evalOnce (TmIsZero (TmSucc nv)) | isNumeric nv = return TmFalse
evalOnce (TmIsZero notVal) | not (isValue notVal) = return $ TmIsZero $ getVal notVal
evalOnce t | isValue t = Left t
evalOnce t = error "Runtime error."


-- 正規評価、遅延評価
-- 簡約は英語でreduction

evalAll :: Term -> Either Term Term
evalAll t = do
  term <- evalOnce t
  t' <- evalAll term
  return t'

evalTerm :: Term -> Term
evalTerm = either id id . evalAll


evalAllWithLog :: Term -> ExceptT Term (Writer String) Term
evalAllWithLog t = do
  lift $ tell $ "Now: " <> show t <> ".\n"
  term <- liftEither $ evalOnce t
  t' <- evalAllWithLog term
  return t'

evalTermLog :: Term -> Writer String Term
evalTermLog = fmap (either id id) . runExceptT . evalAllWithLog

term1 :: Term
term1 = TmSucc$ TmSucc $TmIf (TmNot$ TmNot $ TmNot $ TmIsZero $ TmPred $ TmZero)
  (TmPred TmZero) -- 0
  (TmSucc$TmSucc$TmSucc$TmSucc TmZero) -- 4


someFunc :: IO ()
someFunc = do
  let (v2,log) = runWriter $ evalTermLog term1
  putStrLn log
  putStrLn $ "Result: " ++ show v2

  parser1

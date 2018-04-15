{-# LANGUAGE ScopedTypeVariables #-}

module MyParser (myParser1) where

import Control.Applicative ((<$>), (<*>), pure, liftA2, Const(..))
import Control.Monad.State(StateT(..),get,put,evalStateT,execStateT)
import Control.Monad(mplus,mzero, MonadPlus, guard)
import Control.Monad.Trans(lift)
import Control.Monad.Identity(Identity(..))
import Data.Char (isLetter)


pop :: (MonadPlus m) => StateT [a] m a
pop = do
  (a:as) <- get
  put as
  return a

satisfy :: (MonadPlus m) => (a -> Bool) -> StateT [a] m a
satisfy p = do
  x <- pop
  guard $ p x
  return x

char :: (MonadPlus m, Eq a) => a -> StateT [a] m a
char c = satisfy (==c)

mostPos :: (MonadPlus m) => ([(a, [c])] -> (a, [c])) -> StateT [c] [] a -> StateT [c] m a -- most of possibilities
mostPos most (StateT f) = StateT $ \s ->
  case f s of
    [] -> mzero
    ps -> return $ most ps

fstPos, lastPos :: (MonadPlus m) => StateT [c] [] a -> StateT [c] m a
fstPos = mostPos head -- first of possibilities
lastPos = mostPos last -- last of possibilities

string :: (Eq c, MonadPlus m) => [c] -> StateT [c] m [c]
string [] = return []
string (c:cs) = do
  char c
  string cs
  return $ c:cs

-- 最短マッチ、最長マッチ
many :: (Applicative f, MonadPlus m) => StateT [c] m (f a) -> StateT [c] m (f [a])
many p = many' `mplus` return (pure []) where
  many' = do
    c <- p
    cs <- many p
    return $ liftA2 (:) c cs

manyStr :: (MonadPlus m) => StateT [c] m [a] -> StateT [c] m [a]
manyStr = fmap getConst . many . fmap Const

manyChar :: (MonadPlus m) => StateT [c] m a -> StateT [c] m [a]
manyChar = fmap runIdentity . many . fmap Identity

singleton :: (MonadPlus m) => StateT [c] m a -> StateT [c] m [a]
singleton = fmap (:[])

myParser1 :: IO ()
myParser1 = do
  let parserEx = manyStr $ string "ab" `mplus` string "a"
  let (v::Maybe String) = evalStateT parserEx "aaabbb22"
  print v

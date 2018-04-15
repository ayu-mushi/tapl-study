{-# LANGUAGE ScopedTypeVariables #-}

module MyParser (myParser1) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.State(StateT(..),get,put,evalStateT,execStateT)
import Control.Monad(mplus,mzero, MonadPlus)
import Control.Monad.Trans(lift)
import Data.Char (isLetter)


pop :: (MonadPlus m) => StateT [a] m a
pop = do
  (a:as) <- get
  put as
  return a

satisfy :: (MonadPlus m) => (a -> Bool) -> StateT [a] m a
satisfy p = do
  x <- pop
  if p x
     then return x
     else mzero

char :: (MonadPlus m, Eq a) => a -> StateT [a] m a
char c = satisfy (==c)

mostPos :: (MonadPlus m) => ([(a, String)] -> (a, String)) -> StateT [Char] [] a -> StateT [Char] m a -- most of possibilities
mostPos most (StateT f) = StateT $ \s ->
  case f s of
    [] -> mzero
    ps -> return $ most ps

fstPos, lastPos :: (MonadPlus m) => StateT [Char] [] a -> StateT [Char] m a
fstPos = mostPos head -- first of possibilities
lastPos = mostPos last -- last of possibilities

string :: (MonadPlus m) => String -> StateT [Char] m String
string "" = return ""
string (c:cs) = do
  char c
  string cs
  return $ c:cs

-- 最短マッチ、最長マッチ
many :: (MonadPlus m) => StateT [Char] m String -> StateT [Char] m String
many p = many' `mplus` return [] where
  many' = do
    c <- p
    cs <- many p
    return (c++cs)

singleton :: (MonadPlus m) => StateT [Char] m a -> StateT [Char] m [a]
singleton = fmap (:[])

myParser1 :: IO ()
myParser1 = do
  let (v::Maybe String) = evalStateT (many $ string "aaabbb"`mplus` string "a" ) "aaabbb22"
  print v

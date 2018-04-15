{-# LANGUAGE ScopedTypeVariables #-}

module MyParser (myParser1) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.State(StateT(..),get,put,evalStateT,execStateT)
import Control.Monad(mplus,mzero, MonadPlus)
import Control.Monad.Trans(lift)


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

char c = satisfy (==c)

fstPos :: StateT [Char] [] a -> StateT [Char] [] a
fstPos (StateT f) = StateT $ \s ->
  case f s of
       [] -> mzero
       (p:ps) -> [p]

lastPos :: StateT [Char] [] a -> StateT [Char] [] a
lastPos (StateT f) = StateT $ \s ->
  case f s of
    [] -> mzero
    ps -> [last ps]


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

myParser1 :: IO ()
myParser1 = do
  let (v::Maybe String) = evalStateT (many $ string "a" `mplus` string "b") "aaabbb22"
  print v

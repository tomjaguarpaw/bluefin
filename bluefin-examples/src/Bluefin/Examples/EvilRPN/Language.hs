{-# LANGUAGE DeriveAnyClass #-}

module Bluefin.Examples.EvilRPN.Language where

import Control.Applicative
import Control.Exception
import Text.Read (readMaybe)

data RPN = Plus | Minus | Multiply | Push Int | Evil Int
  deriving (Show, Eq)

data Output = Stack [Int] | InsufficientStack
  deriving (Show, Eq)

parse :: String -> Maybe RPN
parse xs = (Push <$> readMaybe xs) <|> case xs of
  "+" -> Just Plus
  "-" -> Just Minus
  "*" -> Just Multiply
  ('e':rx) -> case words rx of
    [a] | Just t <- readMaybe a -> Just (Evil t)
    _ -> Nothing
  _ -> Nothing

data HungerException = HungerException | FileCorrupted | FileIOError IOError
  deriving (Show, Eq, Exception)

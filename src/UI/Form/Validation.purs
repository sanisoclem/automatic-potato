module AP.UI.Form.Validation where

import Prelude

import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Data.String as String

data FormError
  = Required
  | TooShort
  | TooLong

errorToString :: FormError -> String
errorToString = case _ of
  Required -> "This field is required."
  TooShort -> "Not enough characters entered"
  TooLong -> "Too many characters entered"

required :: forall a. Eq a => Monoid a => a -> Either FormError a
required = check (_ /= mempty) Required

-- | Ensure that an input string is longer than the provided lower limit.
minLength :: Int -> String -> Either FormError String
minLength n = check (\str -> String.length str > n) TooShort

-- | Ensure that an input string is shorter than the provided upper limit.
maxLength :: Int -> String -> Either FormError String
maxLength n = check (\str -> String.length str <= n) TooLong

-- | A small helper function for writing validation functions that rely on a
-- | true/false predicate.
check :: forall a. (a -> Boolean) -> FormError -> a -> Either FormError a
check f err a
  | f a = Right a
  | otherwise = Left err
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Types 
  ( Text32(unText32)
  , text32
  , Text64(unText64)
  , text64
  , AppValidation
  , VError(..)
  , withArraySequenceA
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Validation
import qualified Data.Text as T
import qualified Data.Traversable as TR
import qualified Data.Vector as V

-- Alias to make types shorter
type AppValidation a = AccValidation [VError] a

data VError = MustNotBeEmpty T.Text
            | MustBeLessThanLength32 T.Text
            | MustBeLessThanLength64 T.Text
            deriving (Eq, Show)

-- Basic validated types with smart constructors
newtype Text32 = Text32 { unText32 :: T.Text } deriving (Eq, Show)
 
text32 :: T.Text -> AppValidation Text32
text32 t
  | T.length t == 0   = _Failure # [MustNotBeEmpty t]
  | T.length t <= 32  = _Success # Text32 t
  | otherwise         = _Failure # [MustBeLessThanLength32 t]

-- The following can be cleaned by extracting common code
newtype Text64 = Text64 { unText64 :: T.Text } deriving (Eq, Show)
 
text64 :: T.Text -> AppValidation Text64
text64 t
  | T.length t == 0   = _Failure # [MustNotBeEmpty t]
  | T.length t <= 64  = _Success # Text64 t
  | otherwise         = _Failure # [MustBeLessThanLength64 t]

-- aeson instances
instance FromJSON (AppValidation Bool) where
  parseJSON = withBool "Bool" $ pure . pure

instance FromJSON (AppValidation Text32) where
  parseJSON = withText "Text32" $ pure . text32

instance FromJSON (AppValidation Text64) where
  parseJSON = withText "Text64" $ pure . text64

-- aeson helper for [f a] -> f [a]
withArraySequenceA :: (FromJSON (f a), Applicative f) => String -> Value -> Parser (f [a])
withArraySequenceA s = withArray s $ fmap TR.sequenceA . mapM parseJSON . V.toList
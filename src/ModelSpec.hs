{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module ModelSpec
    ( ModelSpec (..)
    , fromFile
    ) where

import           Control.Exception          (IOException, catch)
import           Data.Aeson                 (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           GHC.Generics               (Generic)

data ModelSpec = ModelSpec
    { file    :: !FilePath
    , texture :: !(Maybe FilePath)
    , bumpMap :: !(Maybe FilePath)
    } deriving (Show, Generic, FromJSON)

fromFile :: FilePath -> IO (Either String ModelSpec)
fromFile filePath = readIt `catch` handler
    where
        readIt :: IO (Either String ModelSpec)
        readIt = eitherDecode <$> LBS.readFile filePath

        handler :: IOException -> IO (Either String ModelSpec)
        handler = return . Left . show

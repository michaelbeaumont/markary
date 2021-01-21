#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Either
import Data.List (sortOn)
import qualified Data.Map as M
import Data.Maybe
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import System.FilePath.Posix
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Prelude hiding (lines)

data CustomMetadata = CustomMetadata
  { title :: Text,
    date :: Text
  }
  deriving (Generic, Show)

instance FromJSON CustomMetadata

main :: IO ()
main =
  (either error id . eitherDecode' <$> BL.getContents)
    >>= go
    >>= BL.putStr . encode
  where
    go (Pandoc (Meta m) d) = do
      let posts =
            case M.lookup "posts-metadata" m of
              Just (MetaString ps) -> T.unpack <$> T.words ps
              _ -> []
      let doMeta fn = do
            contents <- BL.readFile fn
            let makeCustom cm =
                  M.fromList $
                    [ ("href", MetaString . T.pack $ takeBaseName fn),
                      ("date", MetaString $ date cm),
                      ("title", MetaString $ title cm)
                    ]
                      <> catMaybes
                        [ ("date-meta",) . MetaString <$> normalizeDate (date cm)
                        ]
            return $ makeCustom <$> decode contents
      postsMetadata <- MetaList . fmap MetaMap . sortOn (Down . M.lookup "date-meta") . catMaybes <$> doMeta `mapM` posts
      let m' = M.insert "posts" postsMetadata m
      return $ Pandoc (Meta m') d

#!/usr/bin/env runhaskell

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Control.Arrow
import Control.Monad
import Data.Aeson
import Data.Bool
import qualified Data.ByteString.Lazy as BL
import Data.Either
import Data.IORef
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import System.Exit
import System.Process
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Prelude hiding (lines)

newtype Ref = Ref {unRef :: T.Text} deriving (Show)

data Mark = Add | Del deriving (Show)

data MarkedLine = MarkedLine {line :: Int, mark :: Mark}

data MarkedFile = MarkedFile {fileNo :: Int, markedLines :: [MarkedLine]}

data FileRef = FileRef {uri :: Text, ref :: Ref, lines :: Lines, highlightDiff :: Maybe Ref} deriving (Show)

data Lines = All | From Int | Lines Int Int deriving (Show)

main :: IO ()
main = do
  initialMarked <- newIORef []
  contents <- either error id . eitherDecode' <$> BL.getContents
  output <- go (initialMarked, contents)
  BL.putStr (encode output)
  where
    go (ref, d@(Pandoc m _)) = do
      Pandoc (Meta m) d <- walkM (includeGit ref m) d
      markedFiles <- readIORef ref
      let m' =
            let headerIncludes =
                  MetaBlocks
                    [ RawBlock
                        (Format "html")
                        ("<style>" <> makeDiffClass markedFiles <> "</style>")
                    ]
             in M.insert "header-includes" headerIncludes m
      return $ Pandoc (Meta m') d

showText = T.pack . show

makeDiffClass :: [MarkedFile] -> Text
makeDiffClass mfs =
  T.unlines $ concatMap classes mfs
  where
    classes MarkedFile {fileNo, markedLines} =
      let color Add = "rgba(0, 255, 0, 0.1)"
          color Del = "rgba(255, 0, 0, 0.1)"
          classForLine MarkedLine {mark, line} = "#cb" <> showText fileNo <> "-" <> showText line <> "{" <> "background-color: " <> color mark <> " !important;" <> "}"
       in map classForLine markedLines

git :: [String] -> IO (Maybe T.Text)
git args = do
  (code, out, _) <- readProcessWithExitCode "git" args ""
  return $ bool Nothing (Just $ T.pack out) (code == ExitSuccess)

handleDiff :: Text -> IO (Text, [MarkedLine])
handleDiff t = do
  let trimmed = drop 1 . dropWhile (not . T.isPrefixOf "@@") . T.lines $ t
      go (i, line) (lines, markedLines) =
        case T.uncons line of
          Just (c, rest) ->
            let markedLine =
                  let mark =
                        case c of
                          '+' -> Just Add
                          '-' -> Just Del
                          _ -> Nothing
                      markLine mark = MarkedLine {line = i, mark}
                   in markLine <$> mark
             in (rest : lines, maybe id (:) markedLine markedLines)
          _ -> (line : lines, markedLines)
  return . first T.unlines $ foldr go ([], []) (zip [1 ..] trimmed)

getObj :: FileRef -> IO (Maybe Text, [MarkedLine])
getObj FileRef {uri, ref = Ref ref, lines, highlightDiff} = do
  (out, handle) <-
    case highlightDiff of
      Just (Ref diff) -> do
        out <- git ["diff", "-U10000", T.unpack diff, T.unpack ref, "--", T.unpack uri]
        return (out, handleDiff)
      Nothing -> do
        out <- git ["show", T.unpack $ ref <> ":" <> uri]
        return (out, return . (,[]))
  case out of
    Just t -> first Just . trim <$> handle t
    Nothing -> return (Nothing, [])
  where
    trim (out, marked) =
      let outLines = T.lines out
          (a, b) =
            case lines of
              All -> (1, length outLines)
              From a -> (a, length outLines)
              Lines a b -> (a, b)
          marked' =
            let dropLine m = m {line = line m - a + 1}
             in map dropLine marked
       in (T.unlines $ take (b - a + 1) $ drop (a - 1) outLines, marked')

getUrl :: Text -> FileRef -> Text
getUrl repo FileRef {uri, ref = Ref ref} = T.intercalate "/" [repo, "tree", ref, uri]

linkText :: FileRef -> Inline
linkText = Str . uri

getRepo :: Meta -> Maybe Text
getRepo meta =
  lookupMeta "repo" meta >>= \case
    MetaString repo -> Just repo
    MetaInlines [Str repo] -> Just repo
    _ -> Nothing

fileRefFromKVs :: [(Text, Text)] -> Maybe FileRef
fileRefFromKVs kvs =
  let a = lookup "a" kvs
      b = lookup "b" kvs
      diff = lookup "diff" kvs
      ref = lookup "ref" kvs
      uri = lookup "uri" kvs
   in flip fmap ((,) <$> uri <*> ref) $ \case
        (uri, ref) ->
          let lines =
                case (fmap fst . T.decimal <$> a, fmap fst . T.decimal <$> b) of
                  (Just (Right a), Just (Right b)) -> Lines a b
                  (Just (Right a), _) -> From a
                  _ -> All
           in FileRef {uri, ref = Ref ref, lines, highlightDiff = Ref <$> diff}

includeGit :: IORef [MarkedFile] -> Meta -> Block -> IO Block
includeGit markedAcc meta cb@(CodeBlock (ids, origAttrs, kvs) origContents) = do
  markedFiles <- readIORef markedAcc
  let thisSection = 1 + maybe 0 fileNo (listToMaybe markedFiles)
  (markedLines, block) <-
    case fileRefFromKVs kvs of
      Nothing -> return ([], cb)
      Just fr@FileRef {uri, ref, lines} ->
        do
          let url = do
                repo <- getRepo meta
                return $ getUrl repo fr
          (obj, newMarked) <- getObj fr
          let (contents, attrs) = maybe (origContents, origAttrs) (,[]) obj
              repoLink = case url of
                Just url ->
                  let target = (url, "")
                   in Link nullAttr [linkText fr] target
                Nothing -> linkText fr
              sectionId = "git" <> showText thisSection
              codeSectionLink = Link nullAttr [Str "⮟"] ("#" <> sectionId, "")
              block = Div ("", ["code"], []) [Header 6 (sectionId, [], []) [codeSectionLink, Space, repoLink], CodeBlock (ids, origAttrs, kvs) contents]
          return (newMarked, block)
  writeIORef markedAcc $ MarkedFile {fileNo = thisSection, markedLines = markedLines} : markedFiles
  return block
includeGit _ _ b = return b

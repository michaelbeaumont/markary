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
import Data.List (unfoldr)
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

data MarkedLine = MarkedLine {line :: Int, mark :: Mark} deriving (Show)

data MarkedFile = MarkedFile {fileNo :: Int, markedLines :: [MarkedLine]}

data FileRef = FileRef {uri :: Text, ref :: Ref, lines :: Lines, chunk :: Maybe Int, highlightDiff :: Maybe Ref} deriving (Show)

data Lines = From Int | Lines Int Int deriving (Show)

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

handleDiff :: Int -> Text -> IO (Text, [MarkedLine])
handleDiff chunk t = do
  let takeChunk = break (T.isPrefixOf "@@") . drop 1 . dropWhile (not . T.isPrefixOf "@@")
      diffChunks = unfoldr (fmap takeChunk . mfilter (not . null) . Just) $ T.lines t
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
  return . first T.unlines $ foldr go ([], []) (zip [1 ..] (diffChunks !! chunk))

getObj :: FileRef -> IO (Maybe Text, [MarkedLine])
getObj FileRef {uri, ref = Ref ref, lines, chunk, highlightDiff} = do
  (out, handle) <-
    case highlightDiff of
      Just (Ref diff) -> do
        let revRange =
              case diff of
                "^!" -> ref <> "^!"
                _ -> diff <> ".." <> ref
            context =
              case chunk of
                Nothing -> "-U10000"
                Just _ -> "-U3"
        out <- git ["diff", context, T.unpack revRange, "--", T.unpack uri]
        return (out, handleDiff $ fromMaybe 0 chunk)
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
  let a =
        case fmap fst . T.decimal <$> lookup "a" kvs of
          Just (Right x) -> x
          _ -> 1
      b =
        case fmap fst . T.decimal <$> lookup "b" kvs of
          Just (Right x) -> Just x
          _ -> Nothing
      diff = lookup "diff" kvs
      rawChunk = lookup "chunk" kvs
      ref = lookup "ref" kvs
      uri = lookup "uri" kvs
   in flip fmap ((,) <$> uri <*> ref) $ \case
        (uri, ref) ->
          let lines =
                case (a, b) of
                  (a, Just b) -> Lines a b
                  (a, _) -> From a
              chunk = fst . fromRight (error "couldn't parse chunk") . T.decimal <$> rawChunk
           in FileRef {uri, ref = Ref ref, lines, chunk = chunk, highlightDiff = Ref <$> diff}

includeGit :: IORef [MarkedFile] -> Meta -> Block -> IO Block
includeGit markedAcc meta cb@(CodeBlock (ids, origAttrs, kvs) origContents) = do
  markedFiles <- readIORef markedAcc
  let thisSection = 1 + maybe 0 fileNo (listToMaybe markedFiles)
  (markedLines, block) <-
    case fileRefFromKVs kvs of
      Nothing -> return ([], cb)
      Just fr@FileRef {uri, ref, lines} ->
        do
          (obj, newMarked) <- getObj fr
          let url = do
                repo <- getRepo meta
                return $ getUrl repo fr
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

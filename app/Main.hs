module Main where

import Control.Monad (forM_, when)
import Data.List (isPrefixOf)
import Data.Char (isSpace)
import System.Directory
  ( listDirectory
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , createDirectoryLink
  )
import System.FilePath ((</>), takeExtension)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

-- ---------------------------------------------------------------------------
-- Directive syntax:
--   <!--#layout="_layouts/default.html"-->   (first line of page only)
--   <!--#include="_includes/header.html"-->  (anywhere)
--   <!--#content-->                          (inside layout files only)
-- ---------------------------------------------------------------------------

srcDir  :: FilePath
srcDir  = "src"

distDir :: FilePath
distDir = "dist"

main :: IO ()
main = do
  createDirectoryIfMissing True distDir
  processDir srcDir distDir
  createResourcesLink

-- | Recursively process src/, skipping _ prefixed directories.
processDir :: FilePath -> FilePath -> IO ()
processDir src out = do
  entries <- listDirectory src
  forM_ entries $ \entry -> do
    let srcPath = src </> entry
        outPath = out </> entry
    isDir <- doesDirectoryExist srcPath
    if isDir
      then when (not (isSkippedDir entry)) $ do
             createDirectoryIfMissing True outPath
             processDir srcPath outPath
      else when (takeExtension entry == ".html") $
             processHtmlFile srcPath outPath

isSkippedDir :: FilePath -> Bool
isSkippedDir entry
  =  "_" `isPrefixOf` entry
  || entry == "resources"

-- | Resolve layout + includes for a single HTML file.
-- Include paths are always resolved relative to srcDir.
processHtmlFile :: FilePath -> FilePath -> IO ()
processHtmlFile src out = do
  putStrLn $ "  " <> src <> " -> " <> out
  raw <- readFile src
  let ls = lines raw
  result <- case ls of
    (first:rest) ->
      case parseLayout first of
        Just layoutRel -> do
          let layoutPath = srcDir </> layoutRel
          exists <- doesFileExist layoutPath
          if exists
            then do
              layoutRaw <- readFile layoutPath
              let body   = unlines rest
              let merged = substituteContent layoutRaw body
              resolveIncludes merged
            else do
              hPutStrLn stderr $ "ERROR: layout not found: " <> layoutPath
              exitFailure
        Nothing -> resolveIncludes raw
    [] -> return raw
  writeFile out result

-- | Replace <!--#content--> in a layout with the page body.
substituteContent :: String -> String -> String
substituteContent layout body =
  let ls = lines layout
      replaced = map (\l -> if isContentDirective l then body else l) ls
  in unlines replaced

isContentDirective :: String -> Bool
isContentDirective line = dropWhile isSpace line == "<!--#content-->"

-- | Recursively resolve <!--#include="..."--> directives.
-- Paths are always resolved relative to srcDir.
resolveIncludes :: String -> IO String
resolveIncludes content = do
  let ls = lines content
  resolved <- mapM resolveLine ls
  return $ unlines resolved

resolveLine :: String -> IO String
resolveLine line =
  case parseInclude line of
    Just relPath -> do
      let fullPath = srcDir </> relPath
      exists <- doesFileExist fullPath
      if exists
        then do
          included <- readFile fullPath
          resolveIncludes included
        else do
          hPutStrLn stderr $ "ERROR: include not found: " <> fullPath
          exitFailure
    Nothing -> return line

-- ---------------------------------------------------------------------------
-- Parsers
-- ---------------------------------------------------------------------------

-- | Parse <!--#layout="path"--> from the first line.
parseLayout :: String -> Maybe FilePath
parseLayout = parseDirective "#layout"

-- | Parse <!--#include="path"-->.
parseInclude :: String -> Maybe FilePath
parseInclude = parseDirective "#include"

parseDirective :: String -> String -> Maybe FilePath
parseDirective name line =
  let s = dropWhile isSpace line
  in if ("<!--" <> name <> "=\"") `isPrefixOf` s
       then let rest = drop (length ("<!--" <> name <> "=\"")) s
                path = takeWhile (/= '"') rest
            in if null path then Nothing else Just path
       else Nothing

-- ---------------------------------------------------------------------------
-- Symlink: dist/resources -> ../src/resources
-- ---------------------------------------------------------------------------

createResourcesLink :: IO ()
createResourcesLink = do
  let target   = ".." </> srcDir </> "resources"   -- relative from dist/
      linkPath = distDir </> "resources"
  dirExists <- doesDirectoryExist linkPath
  when (not dirExists) $ do
    putStrLn $ "  symlink: " <> linkPath <> " -> " <> target
    createDirectoryLink target linkPath

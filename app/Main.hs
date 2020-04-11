module Main where

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString (unpack)
import qualified Data.ByteString.Lazy as ByteString.Lazy (hPut)

import Data.Traversable (for)
import Data.Maybe (fromMaybe, catMaybes)

import Control.Monad (when, void, guard)
import Control.Monad.IO.Class (liftIO)

import qualified Network.Wreq as Wreq (get, responseBody)
import qualified Web.Scotty as Scotty (scotty, get, file, setHeader)

import Control.Concurrent (newMVar, readMVar, modifyMVar, modifyMVar_)
import Control.Concurrent.Async (async, wait, forConcurrently)

import Control.Lens ((^.))

import Text.HTML.Scalpel ((//), (@:))
import qualified Text.HTML.Scalpel as Scalpel (Scraper, text, attr, attrs, hasClass, scrapeURL, chroot, anySelector)

import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process (system)

type URL = String
type Book = [URL]

fetchBook :: URL -> IO Book
fetchBook url = do
    pages <- fromMaybe [] <$> Scalpel.scrapeURL url do
      Scalpel.chroot ("div" @: [Scalpel.hasClass "entry-content"]) do
        preface <- link "Предисловие"
        chapters <- Scalpel.attrs "href" ("li" // "a")
        pure (preface : chapters)
    chapters <- forConcurrently pages \page ->
      Scalpel.scrapeURL page (link "Содержимое текущего раздела")
    pure $ catMaybes chapters
  where
    link content = Scalpel.chroot "a" do
      actual <- Scalpel.text Scalpel.anySelector
      guard $ actual == ByteString.unpack (encodeUtf8 content)
      Scalpel.attr "href" Scalpel.anySelector

buildBook :: FilePath -> Book -> IO ()
buildBook target book = go [] [] book where
  go downloads files [] = do
    for downloads wait
    void $ system $ unwords $ ["pdfunite"] <> reverse files <> [target]
  go downloads files (chapter : chapters) =
    withSystemTempFile "chapter.pdf" \file handle -> do
      download <- async do
        pdf <- Wreq.get chapter
        ByteString.Lazy.hPut handle (pdf ^. Wreq.responseBody)
        hClose handle
      go (download : downloads) (file : files) chapters

main :: IO ()
main = do
  cache <- newMVar []
  counter <- newMVar 0
  Scotty.scotty 3000 $ Scotty.get "/rumilewski.pdf" do
    liftIO $ print =<< modifyMVar counter (\x -> pure (x + 1, x + 1))
    actual <- liftIO $ fetchBook base
    cached <- liftIO $ readMVar cache
    when (cached /= actual) do
      liftIO $ modifyMVar_ cache \_ -> do
        actual <- fetchBook base
        buildBook "rumilewski.pdf" actual
        pure actual
    Scotty.setHeader "Content-Type" "application/pdf"
    Scotty.file "rumilewski.pdf"
  where
    base = "https://henrychern.wordpress.com/2017/07/17/httpsbartoszmilewski-com20141028category-theory-for-programmers-the-preface/"

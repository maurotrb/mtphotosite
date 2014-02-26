{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      :  Main
-- Copyright   :  Mauro Taraborelli 2014
-- License     :  BSD3
--
-- Maintainer  :  mauro@maurotaraborelli.com
--
-- Photography portfolio site made with Hakyll

module Main
    (
     main
    )
where

import           Hakyll

mtphotositeConfiguration = defaultConfiguration { destinationDirectory = "site-pub"
                                                , storeDirectory       = ".hakyll-cache"
                                                , tmpDirectory         = ".hakyll-cache/tmp"
                                                , providerDirectory    = "site-src"
                                                }

main :: IO ()
main = hakyllWith mtphotositeConfiguration $ do
  -- compile and compress scss
  match "assets/foundation/mtphotosite.scss" $ do
    route $ gsubRoute "foundation/" (const "css/") `composeRoutes` setExtension "css"
    compile sassCompiler
  match "assets/foundation/bower-foundation/scss/normalize.scss" $ do
    route $ gsubRoute "foundation/bower-foundation/scss/" (const "css/") `composeRoutes` setExtension "css"
    compile sassCompiler
  -- copy javascript
  match ( "assets/foundation/bower-foundation/js/foundation.min.js"
          .||. "assets/foundation/bower-foundation/js/vendor/modernizr.js"
          .||. "assets/foundation/bower-foundation/js/vendor/jquery.js" ) $ do
    route $ gsubRoute "foundation/bower-foundation/" (const "")
    compile copyFileCompiler
  -- copy static text files
  match ( "robots.txt" .||. "humans.txt" ) $ do
    route idRoute
    compile copyFileCompiler


-- | Convert a @*.sass@ file into compressed CSS. Require ruby sass.
sassCompiler :: Compiler (Item String)
sassCompiler = getResourceString
               >>= withItemBody (unixFilter "sass" [ "--stdin"
                                                   , "--scss"
                                                   , "--style"
                                                   , "compressed"
                                                   , "--load-path"
                                                   , "site-src/assets/foundation/"
                                                   ])

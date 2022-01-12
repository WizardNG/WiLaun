--{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE OverloadedStrings #-}

module ReadMenu ( readMenu
                , readSystemComponent
                , readPath
                , readSystemItems
                , readUM
                , parseM
                --, hr, h1
                ) where

import System.Posix.Env
import System.Posix.Files
import System.FilePath.Posix
import System.Directory

import Data.Functor
import Control.Monad

--import Data.Aeson
--import Data.Aeson.KeyMap (toList)
--import Data.Aeson.Types (parse)
--import qualified Data.ByteString.Lazy.Internal.ByteString as B
--import qualified Data.ByteString.Lazy as B
import Data.Text
import qualified Data.Text.IO as TIO

import FileControl
import MenuTypes

menuFile :: FilePath
--menuFile = "~/.config/wilaun/menu"
menuFile = "/home/hask/conf/wilaun/menu"
--menuFile = "~/conf/wilaun/menu"


readMenu :: IO Menu
--readMenu =  Menu <$> ( (++) <$> readSystemComponent  <*> readUM) -- serComponent
readMenu =  Menu <$> ( (++) <$> readSystemComponent  <*> readUM) -- serComponent

readSystemComponent :: IO [MenuElement]
readSystemComponent = do
    p <- readPath
    pp <- filterM fileExist p
    l <- traverse readSystemItems pp 
    pure [SubLevel (Submenu "System" $ Menu l )]

readPath :: IO [FilePath]
readPath = getEnv "PATH" <&> parsePath

parsePath :: Maybe String -> [FilePath]
parsePath Nothing = []
parsePath (Just a) = splitSearchPath a

readSystemItems :: FilePath -> IO MenuElement
readSystemItems p = do 
    ff <- fmap (Prelude.map (p </>)) (listDirectory p) 
    f <- filterM fileExist ff
    me <- filterMenu [Item $ MenuItem (pack x) (pack (p </> x)) | x<-f]
    pure $ SubLevel $ Submenu (pack p) (Menu me)

filterMenu :: [MenuElement] -> IO [MenuElement]
filterMenu (x@(Item (MenuItem _ ex)) : xs) = do
            f <- isExec (unpack ex) 
            if f 
               then  fmap (x :) (filterMenu xs) 
               else filterMenu xs
filterMenu x = pure x

readUM :: IO [MenuElement]
readUM = do
    fm <- fileExist menuFile
    l <- if fm then parseM
               else pure $ Menu [] 
    pure [SubLevel (Submenu "User " l )]

parseM ::  IO Menu
parseM = do 
            f <- TIO.readFile menuFile
            pure $ makeMenu f

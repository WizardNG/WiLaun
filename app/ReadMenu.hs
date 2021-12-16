--{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module ReadMenu ( readMenu
                , readSystemComponent
                , readPath
                , readSystemItems
                , readUM
                , decoUM
                , parseME
                ) where

import System.Posix.Env
import System.Posix.Files
import System.FilePath.Posix
import System.Directory

import Data.Functor
import Control.Monad

import Data.Aeson
import Data.Aeson.KeyMap (toList)
--import qualified Data.ByteString.Lazy.Internal.ByteString as B
import qualified Data.ByteString.Lazy as B

import FileControl
import MenuTypes

menuFile :: FilePath
--menuFile = "~/.config/wilaun/menu"
menuFile = "/home/hask/conf/wilaun/menu"
--menuFile = "~/conf/wilaun/menu"


readMenu :: IO Menu
readMenu =  Menu <$> ( (++) <$> readSystemComponent  <*> readUM) -- serComponent

readSystemComponent :: IO [MenuElement]
readSystemComponent = do
    p <- readPath
    pp <- filterM fileExist p
    l <- traverse readSystemItems pp 
    pure [SubLevel (Submenu "System"  $ Menu l )]

readPath :: IO [FilePath]
readPath = getEnv "PATH" <&> parsePath

parsePath :: Maybe String -> [FilePath]
parsePath Nothing = []
parsePath (Just a) = splitSearchPath a

readSystemItems :: FilePath -> IO MenuElement
readSystemItems p = do 
    ff <- fmap (map (p </>)) (listDirectory p) 
    f <- filterM fileExist ff
    me <- filterMenu [Item $ MenuItem x (p </> x) | x<-f]
    pure $ SubLevel $ Submenu p (Menu me)

filterMenu :: [MenuElement] -> IO [MenuElement]
filterMenu (x@(Item (MenuItem _ ex)) : xs) = do
            f <- isExec ex 
            if f 
               then  fmap (x :) (filterMenu xs) 
               else filterMenu xs
filterMenu x = pure x

readUM :: IO [MenuElement]
readUM = do
    fm <- fileExist menuFile
    if fm then do
            f <- B.readFile menuFile
            pure $ parseMenu (decode f :: Maybe Value)
        else pure [] 
-- Tempore function. Delete after debuging
decoUM :: IO [(Key, Value)]
decoUM = do 
            f <- B.readFile menuFile
            let o = (decode f::Maybe Object)
            let ll = maybe [] toList o
            pure ll

parseMenu ::Maybe Value -> [MenuElement]
parseMenu Nothing = []
parseMenu (Just a) = parseME a

parseME :: Value -> [MenuElement]
parseME (Array a) = if null a then []
                                else [Item $ MenuItem "1" "2"]
--parseME (Array a) = [parseMenu (Just x) | x <- a]                               
parseME _ = []

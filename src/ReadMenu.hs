--{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module ReadMenu (
                  MenuItem (..)
                , Submenu (..)
                , MenuElement (..)
                , Menu (..)
                , readMenu
                , readSystemComponent
                , readPath
                , readSystemItems
                ) where

--import System.IO
import System.Posix.Env
import System.Posix.Files
import System.FilePath.Posix
import System.Directory

--import Data.List 
--import Data.List.Extra
import Data.Functor
--import Util
import Control.Monad

import FileControl

-- | Type of elementary menu item 
data MenuItem = MenuItem { name :: String
                         , action :: String}
                         deriving (Show, Eq)

-- | Type of menu sublevel
data Submenu = Submenu { name :: String
                       , sub :: Menu}
                       deriving (Show, Eq)

-- | Type of base menu element: item or sublevel menu
data MenuElement = Item MenuItem | SubLevel Submenu deriving (Show, Eq)

newtype Menu = Menu [MenuElement] deriving (Show, Eq)

readMenu :: IO Menu
--readMenu = pure $ Menu []
readMenu =  Menu <$> readSystemComponent  -- ++ readUserComponent


readSystemComponent :: IO [MenuElement]
readSystemComponent = do
    p <- readPath
    pp <- filterM fileExist p
    l <- traverse readSystemItems pp 
    pure [SubLevel (Submenu "System"  $ Menu l )]

--readUserComponent :: [MenuElement]
--readUserComponent = [SubLevel  
                            --(Submenu "User" (Menu 
                                        --[Item (MenuItem "Telegram" "Telegram")]
                      --))]

readPath :: IO [FilePath]
readPath = getEnv "PATH" <&> parsePath

parsePath :: Maybe String -> [FilePath]
parsePath Nothing = []
parsePath (Just a) = splitSearchPath a

readSystemItems :: FilePath -> IO MenuElement
readSystemItems p = do 
    ff <- liftM (map (p </>)) (listDirectory p) 
    f <- filterM fileExist ff
    me <- filterMenu [Item $ MenuItem x (p </> x) | x<-f]
    pure $ SubLevel $ Submenu p (Menu me)

--readAppList :: FilePath -> IO [FilePath]
--readAppList path = listDirectory path >>= fltrExec . map (path </>) 

filterMenu :: [MenuElement] -> IO [MenuElement]
--filterMenu [] = pure []
filterMenu (x@(Item (MenuItem _ ex)) : xs) = do
            f <- isExec ex 
            if f 
               then  fmap (x :) (filterMenu xs) 
               else filterMenu xs
filterMenu x = pure x

--fltrExec :: [FilePath] -> IO [FilePath]
--fltrExec = filterM isExec 

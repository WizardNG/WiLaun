{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module ReadMenu (
                  MenuItem (..)
                , Submenu (..)
                , MenuPunkt (..)
                , Menu (..)
                , readMenu
                ) where

import System.IO
import System.Posix.Env
import System.FilePath.Posix
import System.Directory

import Data.List 
import Data.Functor
import Util

import FileControl

data MenuItem = MenuItem { name :: String
                         , action :: String}
                         deriving (Show, Eq)

data Submenu = Submenu { name :: String
                       , sub :: Menu}
                       deriving (Show, Eq)

data MenuPunkt = Item MenuItem | SubLevel Submenu deriving (Show, Eq)

newtype Menu = Menu [MenuPunkt] deriving (Show, Eq)

readMenu :: Menu
readMenu = Menu $ readSystemComponent ++ readUserComponent


readSystemComponent :: [MenuPunkt]
readSystemComponent = [SubLevel (Submenu "System"  (Menu []))]

readUserComponent :: [MenuPunkt]
readUserComponent = [SubLevel  
                            (Submenu "User" (Menu 
                                        [Item (MenuItem "Telegram" "Telegram")]
                      ))]

readPath :: IO [FilePath]
readPath = getEnv "PATH" <&> parsePath

parsePath :: Maybe String -> [FilePath]
parsePath Nothing = []
parsePath (Just a) = splitSearchPath a

readFiles :: FilePath -> [MenuPunkt]
readFiles _ = []

readFileList :: FilePath -> ([FilePath] -> [FilePath]) -> IO [FilePath]
readFileList path fltr = do
    l1 <- getDirectoryContents path
    let l2 = fltr l1
    let l3 = map (path </>) l2
    return l3 



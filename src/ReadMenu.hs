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

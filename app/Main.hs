module Main where

import ReadMenu(readMenu)
--import OpenMenu
--import Execution

main :: IO ()
main = do 
    --let menu = readMenu
    menu <- readMenu
    print menu
    --let comm = openMenu menu
    --executeCommand comm

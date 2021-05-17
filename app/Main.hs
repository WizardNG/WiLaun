module Main where

import ReadMenu
import OpenMenu
import Execution

main :: IO ()
main = do 
    --let menu = readMenu
    menu <- readMenu
    let comm = openMenu menu
    executeCommand comm

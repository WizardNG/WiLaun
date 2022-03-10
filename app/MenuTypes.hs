{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module MenuTypes(
                  MenuItem (..)
                , Submenu (..)
                , MenuElement (..)
                , Menu (..)
                , makeMenu
                )where 

--import qualified    Data.ByteString as B
--import              Control.Applicative((<*>), (<$>))
--import  Control.Monad (mzero)
--import  GHC.List(length)
import  GHC.Generics
import qualified GHC.Base as B
import qualified GHC.List as L
import Data.Text
import qualified Data.Bifunctor as DB 

-- | Type of elementary menu item 
data MenuItem = MenuItem { name :: Text
                         , action :: Text}
                         deriving (Generic, Show, Eq)

-- | Type of menu sublevel
data Submenu = Submenu { name :: Text
                       , sub :: Menu}
                       deriving (Generic, Show, Eq)

-- | Type of base menu element: item or sublevel menu
data MenuElement = Item MenuItem | SubLevel Submenu deriving (Generic, Show, Eq)

-- | Main type of menu
newtype Menu = Menu [MenuElement] deriving (Generic, Show, Eq)

makeMenu :: Text -> Menu
--makeMenu t = Menu $ makeMEList $ Data.Text.lines t 
makeMenu = Menu . makeMEList . Data.Text.lines 

makeMEList :: [Text] -> [MenuElement]
makeMEList = fst . makeHelper
--makeMEList [] = []
--makeMEList (x:xs) = makeME x : makeMEList xs

makeHelper :: [Text] -> ([MenuElement], [Text])
makeHelper [] = ([], [])
makeHelper (τ:τσ) = if | t1=="]" -> ([], τσ)
                       | t2 == "[" -> (DB.first (SubLevel (Submenu t1 (Menu τ')) :) (makeHelper τσ'))
                       | (t1, t2) == ("","") -> (τ', τσ')
                       | otherwise -> (DB.first (makeME τ :) (makeHelper τσ))
    where (t1, t2) = parseStr τ
          (τ', τσ') = makeHelper τσ

parseStr :: Text -> (Text, Text)
parseStr τ = if  | l==2 -> (L.head t, L.last t)
                 | l==1 -> (L.head t, "")
                 | otherwise -> ("","")
                 where t = B.map strip $ splitOn ":" $ L.head $ splitOn "#" τ
                       l = L.length t


makeME :: Text -> MenuElement
--makeME x xs = Item $ MenuItem "1" "2"
makeME σ = Item $ MenuItem φ λ
                where (φ,λ) = parseStr σ 

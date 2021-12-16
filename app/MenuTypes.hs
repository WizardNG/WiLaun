{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module MenuTypes(
                  MenuItem (..)
                , Submenu (..)
                , MenuElement (..)
                , Menu (..)
                )where 

--import qualified    Data.ByteString as B
--import              Control.Applicative((<*>), (<$>))
import  Control.Monad (mzero)
--import  GHC.List(length)
import  GHC.Generics
import  Data.Aeson
import  Data.Aeson.KeyMap ( toList)
import  Data.Aeson.Key( toString
                      , fromString)
--import qualified    Data.Text.Internal as DT

-- | Type of elementary menu item 
data MenuItem = MenuItem { name :: String
                         , action :: String}
                         deriving (Generic, Show, Eq)

-- | Type of menu sublevel
data Submenu = Submenu { name :: String
                       , sub :: Menu}
                       deriving (Generic, Show, Eq)

-- | Type of base menu element: item or sublevel menu
data MenuElement = Item MenuItem | SubLevel Submenu deriving (Generic, Show, Eq)

-- | Main type of menu
newtype Menu = Menu [MenuElement] deriving (Generic, Show, Eq)

-- JSON instances from menu item
instance FromJSON MenuItem where
  parseJSON (Object m) = if length l /= 1 then mzero
                                         else MenuItem n <$> m .: k 
                                           where l = toList m
                                                 k = fst $ head l
                                                 n = toString k
  parseJSON _ = mzero
                                  
instance ToJSON MenuItem where
  toJSON (MenuItem n a) = object [fromString n .= a ]


-- JSON instances from submenu
instance FromJSON Submenu where
  parseJSON (Object m) = if length l /= 1 then mzero
                                         else Submenu n <$> m .: k 
                                           where l = toList m
                                                 k = fst $ head l
                                                 n = toString k
  parseJSON _ = mzero
--
instance ToJSON Submenu where 
  toJSON (Submenu n s) = object [fromString n .= toJSON s]


-- JSON instances from  menu elements
instance FromJSON MenuElement where
  parseJSON _ = mzero

instance ToJSON MenuElement where
  toJSON (Item i) = toJSON i
  toJSON (SubLevel s) = toJSON s

-- JSON instances from menu
instance FromJSON Menu where
  parseJSON (Object m) = if length l /= 1 then mzero
                                         else Menu <$> m .: k 
                                           where l = toList m
                                                 k = fst $ head l
  parseJSON _ = mzero

instance ToJSON Menu where
  toJSON (Menu m) =  toJSON $ fmap toJSON m


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
import              Control.Monad (mzero)
import              GHC.Generics
import              Data.Aeson
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

instance FromJSON MenuItem where
  parseJSON (Object m) = MenuItem <$> m .: "name" 
                                  <*> m .: "action" 
  parseJSON _ = mzero
                                  
--instance FromJSON Submenu 

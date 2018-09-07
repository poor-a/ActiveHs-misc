{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification #-}
module ActiveHs.Base
    ( toDyn
    , Dynamic
    , WrapData (WrapData)
    , wrapData
    , WrapData2 (WrapData2)
    , Data
    ) where

import Data.Dynamic (Dynamic, toDyn)
import Data.Data (Data, Typeable)

-------------------------

data WrapData
    = forall a. Data a 
    => WrapData a
        deriving (Typeable)

data WrapData2
    = forall a. Data a
    => WrapData2 a a
        deriving (Typeable)

wrapData :: Data a => a -> WrapData
wrapData = WrapData

{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, FlexibleInstances, TypeFamilies, GeneralizedNewtypeDeriving #-}
module Angular where

import           Yesod

import           Yesod.Angular.Types 
import           Data.IORef
import           Data.Aeson                    (ToJSON (..), FromJSON (..),
                                                (.:), (.=), object,
                                                    Value (Object, Array), withText)
import           Data.Text                     (Text, pack, unpack)
import           Data.Map                      (Map)
import qualified Data.Map                   as Map
import           Control.Applicative           ((<$>), (<*>), pure)
import qualified Data.Vector as V
import           Language.Haskell.TH           (mkName)
import           Yesod.Angular.Macros          (mkAngViewADT)
import           Yesod.Angular.Dependencies    (NgAPIModule(..))
import           Web.PathPieces                (PathPiece,
                                                toPathPiece,
                                                fromPathPiece)


newtype PersonId = PersonId Text
    deriving (Ord, Eq, Read, Show, ToJSON, FromJSON)

instance PathPiece PersonId where
    toPathPiece (PersonId t) = toPathPiece t
    fromPathPiece t = Just $ PersonId t

data Person = Person Text Int
instance ToJSON Person where
    toJSON (Person name age) = object
        [ "name" .= name
        , "age" .= age
        ]

instance FromJSON Person where
    parseJSON (Object o) = Person
        <$> o .: "name"
        <*> o .: "age"
    parseJSON _ = fail "Expected an object"

newtype Singleton a = Singleton { unSingleton :: a }

instance ToJSON a => ToJSON (Singleton a) where
    toJSON = Array . V.singleton . toJSON . unSingleton

instance FromJSON a => FromJSON (Singleton a) where
    parseJSON (Array a) =
        case V.toList a of
            [x] -> Singleton <$> parseJSON x
            _ -> fail "Not a single-element array"
    parseJSON _ = fail "Not an array"


--------------------------------------------------------------------------------
--                           Angular client Data Types                        --
--------------------------------------------------------------------------------

data PersonClient = PersonClient
                    deriving (Bounded, Enum, Eq, Ord, Show, Read)

type instance YesodBase PersonClient = App

instance NgCompFoundation PersonClient where

    data CompName PersonClient = PersonService
         deriving (Bounded, Enum, Eq, Ord, Show, Read)

instance NgFoundation PersonClient where

    data ViewName PersonClient = AllPersons
                               | SinglePerson
                               | AddPerson
                                 deriving (Bounded, Enum, Eq, Ord, Show, Read)

    data ViewRoute PersonClient = AllPersonsNgR
                                | SinglePersonNgR (NgRtCtxt PersonId)
                                | AddPersonNgR

    data ClientEntity PersonClient = PersonEntity PersonId Person

    -- the server-side route used for partial retrieval
    viewTemplateRoute = PersonTemplateR -- 

    -- which views do the routes correspond to?
    assocView AllPersonsNgR = AllPersons
    assocView (SinglePersonNgR _) = SinglePerson
    assocView (AddPersonNgR) = AddPerson

    routeParamNames AllPersons        = []
    routeParamNames SinglePerson      = ["personId"]
    routeParamNames AddPerson         = []

    routeArgs AllPersonsNgR  = []
    routeArgs (SinglePersonNgR rtCtxt)  = [viewArgs rtCtxt]
    routeArgs (AddPersonNgR)  = []

--------------------------------------------------------------------------------
--                  Some instances for our definitions                        --
--------------------------------------------------------------------------------

instance ToJSON (ClientEntity PersonClient) where
  toJSON (PersonEntity pid p) = object 
    [ "id" .= pid
    , "person" .= p
    ]

instance FromJSON (ClientEntity PersonClient) where
  parseJSON (Object pe) = PersonEntity 
    <$> pe .: "id"
    <*> pe .: "person"
  parseJSON _ = fail "Expected an object"

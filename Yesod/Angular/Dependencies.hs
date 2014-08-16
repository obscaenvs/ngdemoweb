{-# Language OverloadedStrings                 #-}
module Yesod.Angular.Dependencies where

import           Data.Text                     (Text)
import           Yesod.Core                    (Route, Yesod)

-- | For use with e.g. `Yesod.Core.addScriptEither`.
class HasSource t where
  getSource :: (Yesod s) => t -> Either (Route s) Text
  getSource = undefined

data NgAPIModule = NgBase
                 | NgResource
                 | NgRoute
                   deriving (Enum, Bounded, Eq, Ord)

instance Show NgAPIModule where
    show NgBase     = "ngBase"
    show NgResource = "ngResource"
    show NgRoute    = "ngRoute"

instance Read NgAPIModule where
    readsPrec _ "ngBase" = [(NgBase, "")]
    readsPrec _ "ngResource" = [(NgResource, "")]
    readsPrec _ "ngRoute" = [(NgRoute, "")]
    readsPrec _ _            = []

data NgAPIService = QSrv 
                  | ResourceSrv -- needs NgResource
                  | RouteSrv 
                  | RouteParamsSrv 
                    deriving (Enum, Bounded, Eq, Ord)

instance Show NgAPIService where
    show QSrv        = "$q"
    show ResourceSrv = "$resource"
    show RouteSrv    = "$route"

instance Read NgAPIService where
    readsPrec _ "$q" = [(QSrv, "")]
    readsPrec _ "$resource" = [(ResourceSrv, "")]
    readsPrec _ "$route" = [(RouteSrv, "")]
    readsPrec _ _            = []

getModuleAPIDeps :: NgAPIService -> [NgAPIModule]
getModuleAPIDeps QSrv = []
getModuleAPIDeps ResourceSrv = [NgResource]
getModuleAPIDeps RouteSrv = []
getModuleAPIDeps RouteParamsSrv = [NgRoute]

-- instance HasSource NgAPIModule where
--   getSource NgBase = Right "//ajax.googleapis.com/ajax/libs/angularjs/1.2.1/angular.min.js"
--   getSource NgResource = Right "//ajax.googleapis.com/ajax/libs/angularjs/1.2.1/angular-resource.min.js"
--   getSource NgRoute = Right "//ajax.googleapis.com/ajax/libs/angularjs/1.2.1/angular-route.min.js" 
--    Right "//ajax.googleapis.com/ajax/libs/angularjs/1.0.7/angular-resource.min.js"

instance HasSource NgAPIModule where
  getSource NgBase = Right "//ajax.googleapis.com/ajax/libs/angularjs/1.2.1/angular.js"
  getSource NgResource = Right "//ajax.googleapis.com/ajax/libs/angularjs/1.2.1/angular-resource.js"
  getSource NgRoute = Right "//ajax.googleapis.com/ajax/libs/angularjs/1.2.1/angular-route.js" 


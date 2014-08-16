{-# LANGUAGE OverloadedStrings                 #-}
{-# LANGUAGE QuasiQuotes                       #-}
{-# LANGUAGE RecordWildCards                   #-}
{-# LANGUAGE TemplateHaskell                   #-}
{-# LANGUAGE ConstraintKinds                   #-}
{-# LANGUAGE FlexibleContexts                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving        #-}
{-# LANGUAGE GADTs                             #-}
{-# LANGUAGE ScopedTypeVariables               #-}

module Yesod.Angular
    ( NgView
    , mkView
    , addAPIDep
    , addView
    , addComponent
    , addService
    , addServiceDeps
    , mkNgViewQ
    , mkNgServiceQ
    , mkNgService
    , withDefaultView
    ) where

import           Prelude                       hiding (head, init,
                                                       last, readFile,
                                                       tail, writeFile)
import           Data.List                     (intersperse, foldl')
import           Data.Either
import           Data.Monoid                   
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Text.Hamlet                   (HtmlUrl, hamlet, hamletFile)
import           Text.Julius                   (juliusFile, julius,
                                                JavascriptUrl, julius, rawJS)
import           Control.Monad                 (forM_)
import           Yesod.Core                    (WidgetT, Route, HandlerSite, Yesod,
                                                newIdent, toWidget, hamlet,
                                                getUrlRenderParams,
                                                toHtml)
import           Data.Text.Lazy.Builder        (Builder)
import           Text.Blaze.Html               (Html)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Yesod.Angular.Types     
import           Yesod.Angular.Dependencies    (NgAPIModule(..))

import           Language.Haskell.TH           (ExpQ, Exp(ConE),mkName)
--------------------------------------------------------------------------------
--                           Function definitions                             --
--------------------------------------------------------------------------------
-- | This function fairly obviously constructs an `NgView` from
-- constituent parts.
mkView :: (NgFoundation c) =>
                 ViewName c -- ^ A name for the view.
              -> JavascriptUrl (Route (YesodBase c)) -- ^ Controller.
              -> Either (HtmlUrl (Route (YesodBase c))) (HtmlUrl (Route (YesodBase c))) -- ^ Template.
              -> NgView c
mkView = NgView 


withDefaultView :: (NgFoundation c) =>
                   c
                -> NgView c
                -> NgMain c
withDefaultView modName ngView =
    NgMain modName ngView mempty mempty [NgBase]

addAPIDep :: (NgFoundation c) =>
             NgMain c
          -> NgAPIModule
          -> NgMain c
addAPIDep ngmain@NgMain{..} apiMod =
  ngmain { ngMainApiDeps = ngMainApiDeps <> [apiMod] }    

addView :: (NgFoundation c) =>
              NgMain c -- ^ The module to be expanded upon.
           -> NgView c -- ^ View to be added.
           -> NgMain c 
addView ngmain@NgMain{..} view =
  ngmain { ngMainAuxViews = ngMainAuxViews <> [view] }

addComponent :: (NgFoundation c) =>
                NgMain c -- ^ The module to add the component to.
             -> NgComponents c -- ^ The component to be added.
             -> NgMain c 
addComponent mod@NgMain{..} xcomps =
    mod { ngMainComps = ngMainComps `mappend` xcomps } 

addService :: (NgFoundation c, NgCompFoundation c) =>
              NgMain c -> NgService c -> NgMain c
addService ngmain@NgMain{..} srv = ngmain `addComponent` (serviceToComponent srv)
    
serviceToComponent :: (NgCompFoundation c) => NgService c -> NgComponents c
serviceToComponent srv = NgComponents ([srv],[],[])

directiveToComponent :: (NgCompFoundation c) => NgDirective c -> NgComponents c
directiveToComponent dct = NgComponents ([],[dct],[])

filterToComponent :: (NgCompFoundation c) => NgFilter c -> NgComponents c
filterToComponent flt = NgComponents ([],[],[flt])

addServiceDeps :: NgService c -> [NgAPIService] -> NgService c
addServiceDeps srv@NgService{..} deps = 
    srv {ngServiceAPIDeps = ngServiceAPIDeps <> deps}

-- | All that is needed to use this is to replace a view definition
-- with 
-- @
-- $(mkAngularViewQ SomeViewName),
-- @
-- and the components of the view are fetched from disk according to
-- the settings in the class methods `clientHomePath` and
-- 'viewFilesPath'.  The functions 'viewControllerPath' and
-- 'viewTemplatePath' assembles the full path to the controllers and
-- template partials, respectively.

mkNgViewQ :: (NgFoundation c) => ViewName c -> ExpQ
mkNgViewQ v = do
  let
      viewConE = return $ ConE $ mkName $ show v 
  [|mkView $(viewConE) $( juliusFile $ controllerFile v)
                                 (Right $(hamletFile $ templateFile v))|]

mkNgService :: (NgCompFoundation c) =>
               CompName c
            -> JavascriptUrl (Route (YesodBase c))
            -> [NgAPIService]
            -> [CompName c]
            -> NgService c
mkNgService = NgService

mkNgServiceQ :: (NgCompFoundation c) => CompName c -> ExpQ
mkNgServiceQ cname = do
  let compConE = return $ ConE $ mkName $ show cname
  [| mkNgService $( compConE ) $( juliusFile $ componentFile cname ) [] [] |]




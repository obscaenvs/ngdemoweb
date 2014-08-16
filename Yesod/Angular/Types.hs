{-# Language OverloadedStrings                 #-}
{-# LANGUAGE TemplateHaskell                   #-}
{-# LANGUAGE QuasiQuotes                       #-}
{-# LANGUAGE ConstraintKinds                   #-}
{-# LANGUAGE ExistentialQuantification         #-}
{-# LANGUAGE RecordWildCards                   #-}
{-# LANGUAGE TypeFamilies                      #-}
{-# LANGUAGE FlexibleContexts                  #-}
{-# LANGUAGE FlexibleInstances                 #-}
{-# LANGUAGE MultiParamTypeClasses             #-}
{-# LANGUAGE ScopedTypeVariables               #-}
{-# LANGUAGE UndecidableInstances              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving        #-}
{-# LANGUAGE GADTs                             #-}
{-# LANGUAGE IncoherentInstances               #-}
{-# LANGUAGE EmptyDataDecls                    #-}

module Yesod.Angular.Types
    ( NameType
    , YesodBase
    , NgMain(..)
    , NgFoundation(..)
    , NgCompFoundation(..)
    , NgView(..)
    , NgService(..)
    , NgFilter(..)
    , NgDirective(..)
    , NgComponents(..)
    , NgAPIService -- re-export
    , NgRtCtxt(..)
    , TemplateMap 
    , viewArgs
    , mkTemplateMap
    , lookupTemplate
    , dispatchTemplate
    , controllerFile
    , componentFile
    , templateFile
    , viewRouteJSArgs
    , viewRouteDef
    , ngLayoutSimple
    ) where



import           Yesod                         (JavascriptUrl, HtmlUrl)  
import           Data.Text                     (Text, pack, unpack, null)
import           Data.Monoid                   (mappend, mempty, mconcat,
                                                Monoid, (<>))
import           Data.Maybe                    (fromMaybe)
import           Yesod.Core                    (HandlerT, WidgetT, Route,
                                                Html, Html, Yesod,
                                                defaultLayout, whamlet, hamlet,
                                                getUrlRenderParams,
                                                sendResponse,
                                                toContent,
                                                toTypedContent, MonadWidget,
                                                HandlerSite, getYesod)
import           Yesod.Core.Widget             (ToWidget, toWidget,
                                                addScriptEither)
import           Language.Haskell.TH           (mkName)
import qualified Data.Map                   as Map
import           Data.Map                      (Map(..),lookup, insert)
import           Data.List                     (foldl', intersperse, (\\), nub)
import           Text.Julius                   (JavascriptUrl, julius, rawJS)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Control.Monad                 (forM_)
import           Web.PathPieces                (PathPiece, toPathPiece, fromPathPiece)
import           Data.Aeson                    (ToJSON (..), FromJSON (..))
import           System.FilePath               (pathSeparator)

import           Yesod.Angular.Dependencies    
import           Yesod.Angular.Util

--------------------------------------------------------------------------------
--                             Class definitions                              --
--------------------------------------------------------------------------------

-- | This is the way we name our modules and views among other things.
-- The 'Ord' instance is needed since we sometimes use a 'NameType' as
-- a key in a map. Uses -XConstraintKinds.
type NameType t = (Bounded t, Enum t, Eq t, Ord t, Show t, Read t)

-- | The type that specifies a client route argument.
data NgRtCtxt t = NgRtVal t -- statique: une valeur définitive fournie par le serveur web
                | NgRtVar Text  -- dynamique: nom d'une variable fournie à AngularJS
                                   -- (pour interprétation dans "{{}}",
                                   -- ou si on change de route par un appel fonction JavaScript)                                  

-- | This type family is needed to keep track of what instance of Yesod
-- is running server-side operations. 
type family YesodBase t

data NgMain c = (NgFoundation c) =>
    NgMain
    { ngMainName :: c -- ^ A name for the client
    , ngMainDefaultView :: (NgView c) -- ^ The default view
    , ngMainAuxViews :: [NgView c] -- ^ Other, auxiliary views
    , ngMainComps :: NgComponents c -- ^ Modules on which the main module is dependent.
    , ngMainApiDeps :: [NgAPIModule] 
    }

data NgView c = (NgFoundation c) =>
    NgView
    { ngViewName       :: ViewName c
    , ngViewController :: JavascriptUrl (Route (YesodBase c))
    , ngViewTemplate   :: Either (HtmlUrl (Route (YesodBase c))) 
                                 (HtmlUrl (Route (YesodBase c))) -- ^ Left htmlUrl: embed directly in source. Right htmlUrl: Fetch asynchronously when need arises.
    }

data NgService c = (NgCompFoundation c) =>
    NgService
    { ngServiceName :: CompName c
    , ngServiceDef :: JavascriptUrl (Route (YesodBase c))
    , ngServiceAPIDeps :: [NgAPIService]
    , ngServiceCompDeps :: [CompName c]
    }

data NgDirective c = (NgCompFoundation c) =>
    NgDirective
    { ngDirectiveName :: CompName c
    , ngDirectiveDef :: JavascriptUrl (Route (YesodBase c))
    , ngDirectiveAPIDeps :: [NgAPIService]
    , ngDirectiveCompDeps :: [CompName c]
    }

data NgFilter c = (NgCompFoundation c) =>
    NgFilter
    { ngFilterName :: CompName c
    , ngFilterDef :: JavascriptUrl (Route (YesodBase c))
    , ngFilterAPIDeps :: [NgAPIService]
    , ngFilterCompDeps :: [CompName c]
    }

type NgServiceModule c   = [NgService c]
type NgDirectiveModule c = [NgDirective c]
type NgFilterModule c    = [NgFilter c]

data NgComponents c = (NameType c) =>
    NgComponents
    {
      unNgComp :: (NgServiceModule c, NgDirectiveModule c, NgFilterModule c)
    }

instance (NameType c) => Monoid (NgComponents c) where
    mempty = NgComponents mempty
    c `mappend` c' = NgComponents (unNgComp c `mappend` unNgComp c')

-- | `NgRtVal t` is interpreted as just being a plain JavaScript value .
--   `NgRtVar t` is interpreted as an AngularJS handlebar variable.
instance (Show t) => IsViewArg (NgRtCtxt t) where
  viewArgs (NgRtVal t) = pack . show $ t
  viewArgs (NgRtVar t) = "{{" <> t <> "}}"

addNgService ::  (NgCompFoundation c) => NgService c -> NgComponents c -> NgComponents c 
addNgService srv comps = 
    let (ss,ds,fs) = unNgComp comps
    in NgComponents (srv:ss,ds,fs)

-- | Extract the services.
getServiceModule :: NgComponents c -> NgServiceModule c
getServiceModule c = (\(x,_,_) -> x) $ unNgComp c

-- | Extract the directives.
getDirectiveModule :: NgComponents c -> NgDirectiveModule c
getDirectiveModule c = (\(_,x,_) -> x) $ unNgComp c

-- | Extract the filters.
getFilterModule :: NgComponents c -> NgFilterModule c
getFilterModule c = (\(_,_,x) -> x) $ unNgComp c

class ( NameType c
      , NameType (ViewName c)
      , ToJSON (ClientEntity c)
      , FromJSON (ClientEntity c)
      , Yesod (YesodBase c)
      ) =>
    NgFoundation c where 

    -- | Named views. 
    data ViewName c

    -- | Type-safe routes.
    data ViewRoute c

    -- | The model entities; could be used and misused for all sorts
    -- of things, but the intented usage is something like that of
    -- 'PersistentEntity': an id, or key, coupled with a data
    -- payload. This is - hopefully - in line with the design decisions in the rest of the Yesod framework (cf. <http://www.yesodweb.com/book/persistent#_insert the Yesod book, Persistent chapter>).
    data ClientEntity c

    -- | How to include a client. For a simpler way without the pre-
    -- and post widgets, see @ngLayoutSimple@.
    ngLayout :: (NgFoundation c) =>
                WidgetT (YesodBase c) IO () 
             -- ^ Widget that comes before AngularJS app.
             -> NgMain c
             -- ^ The AngularJS app to put on the wire.
             -- Will be converted by means of `toWidget` and included.
             -> WidgetT (YesodBase c) IO ()
             -- ^ Widget that comes after AngularJS app.
             -> HandlerT (YesodBase c) IO Html
    ngLayout preW ngmain@NgMain{..} postW = do
      let ngMainW = toWidget ngmain :: WidgetT (YesodBase c) IO ()
      defaultLayout $ do
        [whamlet| $newline always
         ^{preW}
         <div data-ng-app=#{ngRef ngmain}>
           <div data-ng-view>
         ^{ngMainW}
         ^{postW}
       |]

    -- | The user-provided route that is intended to deliver the template partials
    -- asynchronously.
    viewTemplateRoute :: ViewName c -> Route (YesodBase c)

    -- | A function to get the name of the view that the input route points to.
    assocView :: ViewRoute c -> ViewName c

    -- | (CORRECT THIS ENTRY!)Formatting of a route to a path expression suitable for using
    -- in a deep link within the client. Two types of formatting need
    -- to be distinguished:
    --
    -- * Interpreted by browser: must include a '#' preceding the route.
    --
    -- * Interpreted by AngularJS : must *not* include the aforementioned '#'.
    --
    -- Hence the 'Bool': 'True' appends the '#', while a value of
    -- 'False' does not.

    -- <<obscaenvs>> Should not need to be redefined, and so might be better to not
    -- have in the class.
    viewRoute :: ViewRoute c -> Text -- default most used
    viewRoute r = "#/" <> viewName 
          <> (mconcat $ zipWith zipper argNames argValues)
      where
        viewName = (pack . show) av
        zipper = nameParamConcat isForRouteDef
        argNames = routeParamNames av
        argValues = routeArgs r 
        isForRouteDef = False
        av = assocView r 

    viewRoute' :: Bool -> ViewRoute c -> Text -- default most used
    viewRoute' forBrowser r =
        if forBrowser then "#/" else "/"
          <> viewName 
          <> (mconcat $ zipWith zipper argNames argValues)
      where
        viewName = pack . show . assocView $ r 
        zipper = nameParamConcat isForRouteDef
        argNames = routeParamNames . assocView $ r
        argValues = routeArgs r 
        isForRouteDef = False

    -- | The value of the route arguments.
    routeArgs :: ViewRoute c -> [Text]

    -- | The name of the route parameters.
    -- 
    --  This name is used both as an URL path piece just prior to the
    --  argument sent to a view, e.g.
    --
    -- @routeParamNames SomeView = [\"someid\", \"otherid\"]@
    --
    -- gives a route definition string passed to AngularJS looking like:
    --
    -- @\"\/SomeView\/someid\/:someid\/otherid\/:otherid\"@
    --
    -- The values of the arguments can then respectively be retrieved
    -- in a controller via @\"$routeParams.someid\"@ and
    -- @\"$routeParams.otherid\"@.
    --

    routeParamNames :: ViewName c -> [Text]
    
--------------------------------------------------------------------------------

class ( NgFoundation c
      , NameType c
      , NameType (CompName c)
      ) => NgCompFoundation c where 

    data CompName c

--------------------------------------------------------------------------------

moduleHome :: String
moduleHome = "Modules"

-- | Just an internal class. Or it should be.
class HasPath t where
  getPath :: t -> FilePath

-- <<obscaenvs>> To export or not export?
class NgRef t where
  ngRef :: t -> Text

class IsViewArg t where
  viewArgs :: t -> Text

instance (NgFoundation c) => HasPath c where
  getPath c = assemblePath [moduleHome, show c]

instance (NameType (ViewName c)) => HasPath (ViewName c) where
  getPath = show

instance (NameType (CompName c)) => HasPath (CompName c) where
  getPath cname = assemblePath [moduleHome, show cname]

instance (NgFoundation c) => HasPath (NgService c) where
  getPath (NgService name _ _ _) = assemblePath [moduleHome, getPath name]

instance (NgFoundation c) => HasPath (NgFilter c) where
  getPath (NgFilter name _ _ _) = assemblePath [moduleHome, getPath name]

instance (NgFoundation c) => HasPath (NgDirective c) where
  getPath (NgDirective name _ _ _) = assemblePath [moduleHome, getPath name]

instance (NameType c) => NgRef (NgMain c) where
  ngRef NgMain{..} = ((pack . show) ngMainName) <> "Main"

instance (NameType c) => NgRef (NgView c) where
  ngRef NgView{..} = (pack . show) ngViewName

instance (NameType c) => NgRef (NgServiceModule c) where
  ngRef _ = ((pack . show) (minBound :: c)) <> "Srv"

instance (NameType c) => NgRef (NgDirectiveModule c) where
  ngRef _ = ((pack . show) (minBound :: c)) <> "Dct"

instance (NameType c) => NgRef (NgFilterModule c) where
  ngRef _ = ((pack . show) (minBound :: c)) <> "Flt"

instance NgRef NgAPIModule where
  ngRef = (pack . show)

instance (NameType (CompName c)) => NgRef (CompName c) where
  ngRef = (pack . show)

-- can be made more general.
instance (NameType (ViewName c)) => PathPiece (ViewName c) where
    toPathPiece v = pack . show $ v
    fromPathPiece t = case (reads $ unpack t ) of
                      (v, _):_ -> Just v
                      _         -> Nothing

instance (YesodBase c ~ site, NgFoundation c) =>
    ToWidget site (NgMain c) where
    toWidget = mainToWidget

instance (YesodBase c ~ site, NgFoundation c) =>
    ToWidget site (NgService c) where
    toWidget = serviceToWidget

instance (YesodBase c ~ site, NgFoundation c) =>
    ToWidget site (NgDirective c) where
    toWidget = directiveToWidget

instance (YesodBase c ~ site, NgFoundation c) =>
    ToWidget site (NgFilter c) where
    toWidget = filterToWidget

instance (YesodBase c ~ site, NgFoundation c) =>
    ToWidget site (NgServiceModule c) where
    toWidget srvs = mapM_ toWidget srvs

-- <<obscaenvs>> must include the defs for dct:s and flt:s also.
instance (YesodBase c ~ site, NgFoundation c) =>
    ToWidget site (NgComponents c) where
    toWidget comps = srvModDecl `inCommentBlock` "Services"
      where
        (srvMod,ds,fs) = unNgComp comps
        srvModDeps = map ngRef $ nub . mconcat $
                         (map getModuleAPIDeps $
                              nub . mconcat $ fmap ngServiceAPIDeps srvMod )
        srvModDecl = moduleDeclarationW (ngRef srvMod) srvModDeps $ toWidget srvMod

-- <<obscaenvs>> Clearly a Util thingy.
beginNgBlock :: (MonadWidget m) => Text -> m ()
beginNgBlock t = jsComment $ "BEGIN" <> " " <> t

endNgBlock :: (MonadWidget m) => Text -> m ()
endNgBlock t = jsComment $ "END" <> " " <> t

inCommentBlock :: (MonadWidget m) => WidgetT (HandlerSite m) IO () -> Text -> m ()
w `inCommentBlock` t = do
  beginNgBlock t
  toWidget w  
  endNgBlock t

jsComment :: (MonadWidget m) => Text -> m ()
jsComment t = toWidget [julius|
// #{rawJS t}
|]
                         
--  stubs --
directiveToWidget :: forall c m. ( HandlerSite m ~ YesodBase c
                                 , MonadWidget m ) =>
                     NgDirective c -> m ()
directiveToWidget dct@NgDirective{..} = do undefined

filterToWidget :: forall c m. ( HandlerSite m ~ YesodBase c
                              , MonadWidget m ) =>
                  NgFilter c -> m ()
filterToWidget flt@NgFilter{..} = do undefined
-- /stubs --

    ----------------------------------------------------------------------------

mainToWidget :: forall c m. ( HandlerSite m ~ YesodBase c
                              , MonadWidget m
                              , NgFoundation c ) =>
                  NgMain c -> m ()
mainToWidget ngmain@NgMain{..} = do
  ur <- getUrlRenderParams
  mapM_ addScriptEither $ 
        map getSource ngMainApiDeps
  toWidget ngMainComps 
  declareControllers ngmain
  declareMainModule ur `inCommentBlock` "Main"
  where
    allCompDeps = filter (not . Data.Text.null) $
                    getCompNgRefs ngMainComps <>
                    map ngRef (ngMainApiDeps \\ [NgBase]) -- 'NgBase' never needed in main
    routeConfW ur = declareRouteConf ur ngmain
    declareMainModule ur = moduleDeclarationW (ngRef ngmain) allCompDeps $ routeConfW ur

ctrlSuff :: Text
ctrlSuff = nameSep <> "Controller"

routeConfSuff :: Text
routeConfSuff = nameSep <> "RouteConf"

nameSep :: Text
nameSep = ""

-- <<obscaenvs>> Refactor? eliminate Maybe:s?
getCompNgRefs :: NgComponents c -> [Text]
getCompNgRefs (NgComponents (srvs,dcts,flts)) = [hlp srvs, hlp dcts, hlp flts]
  where 
    hlp :: (NgRef [t]) => [t] -> Text
    hlp cs = case cs of
               [] -> ""
               _  -> ngRef cs

declareControllers :: ( HandlerSite m ~ YesodBase c,
                        MonadWidget m ) =>
                      NgMain c -> m ()
declareControllers ngmain@NgMain{..} =
    forM_ (ngMainDefaultView:ngMainAuxViews) $
          (\v -> do declareSingleController v (ngRef v))
    where 
      declareSingleController v name = do
        toWidget [julius|
var #{rawJS $ name <> ctrlSuff} = |]
        toWidget $ ngViewController v

declareRoutes :: forall c m. (
                               HandlerSite m ~ YesodBase c
                             , MonadWidget m
                             , NgFoundation c
                             ) =>
                 UrlRender (YesodBase c) -> NgMain c -> m ()
declareRoutes urlRender NgMain{..} = do
  toWidget [julius| function($routeProvider)
         {
           $routeProvider |]
  forM_ (ngMainDefaultView:ngMainAuxViews) routeWidget
  toWidget [julius|         
              .otherwise({redirectTo: '#{rawJS $ defViewRt}'});
         }|] 
  where

    defViewRt :: Text
    defViewRt = viewRouteDef ngMainDefaultView

    routeWidget :: NgView c -> m ()
    routeWidget view@NgView{..} = do 
      let ngCtrlName = (ngRef view) <> ctrlSuff
      toWidget [julius|
              .when('#{rawJS $ viewRouteDef view}',
                    { controller: '#{rawJS ngCtrlName}'
                    |]
      toWidget $ case ngViewTemplate of
                   Left htmlUrl -> [julius|, template: '#{ rawJS $ renderHtml $ htmlUrl urlRender }'})|]
                   Right route -> [julius|, templateUrl: '@{ viewTemplateRoute ngViewName }'})|]

declareRouteConf :: forall c m. ( HandlerSite m ~ YesodBase c
                                , MonadWidget m
                                , NgFoundation c ) =>
                    UrlRender (YesodBase c) -> NgMain c -> m ()
declareRouteConf urlRender ngmain@NgMain{..} = do
      let ngModuleName = ngRef ngmain
          ngClientName = (pack . show) ngMainName
      toWidget [julius|
       .config(['$routeProvider',  |]
      declareRoutes urlRender ngmain
      toWidget [julius| ])|]

serviceToWidget :: forall c m. ( HandlerSite m ~ YesodBase c, MonadWidget m) =>
                   NgService c -> m ()
serviceToWidget NgService{..} = do 
  let ngSrvName = (pack . show) ngServiceName
      apideps = ngServiceAPIDeps
      srvDepsOutQuoted = quoteConcat (squote . pack . show)  ngServiceAPIDeps
      srvDepsOut = quoteConcat (pack . show)  ngServiceAPIDeps
  toWidget
    [julius|
       .factory(#{ rawJS $ squote $ ngSrvName },
                [#{rawJS srvDepsOutQuoted},
                  function (#{rawJS srvDepsOut})
                  {
                    |]
  toWidget ngServiceDef
  toWidget [julius|
                  }])|]

    ----------------------------------------------------------------------------

-- | A shorthand for where the templates are stored for lookup at runtime.
type TemplateMap c = Map (ViewName c) (HtmlUrl (Route (YesodBase c)))

mkTemplateMap :: (NgFoundation c) => NgMain c -> TemplateMap c 
mkTemplateMap NgMain{..} =
    foldl' iter mempty (ngMainDefaultView:ngMainAuxViews)
  where 
    iter m v = m <> extractRightRoutes v
    {- discard Left:s, make singleton map of Enum to Right:s -}
    extractRightRoutes NgView{..} =
        either (\_ -> mempty) (\h -> Map.insert ngViewName h mempty) ngViewTemplate


lookupTemplate :: (NgFoundation c) =>
                  TemplateMap c
               -> ViewName c
               -> Maybe (HtmlUrl (Route (YesodBase c)))
lookupTemplate  = flip Map.lookup 

dispatchTemplate :: (Yesod site) => (HtmlUrl (Route site)) -> HandlerT site IO Html
dispatchTemplate p = do
  getUrlRenderParams >>= sendResponse . toTypedContent . p 

--------------------------------------------------------------------------------
--                             helper functions                               --
--------------------------------------------------------------------------------
moduleDeclarationW :: (MonadWidget m) =>
                      Text -> [Text] -> WidgetT (HandlerSite m) IO () -> m ()
moduleDeclarationW modname deps innerW = do
    let depsQuoted = mconcat $ intersperse (", " :: Text) $ map squote deps
    toWidget [julius|angular.module('#{ rawJS modname }', [#{rawJS depsQuoted}])|] 
    toWidget innerW
    toWidget [julius|;|] 

-- <<obscaenvs>> Weak name, refactor.
nameParamConcat :: Bool -> Text -> Text -> Text
nameParamConcat isForRouteDef pname pval = "/" <>
    (if pname == "" then "" else pname <> "/") <>
    (if isForRouteDef then ":" else "") <> pval

paramNamesVals :: Bool -> [Text] -> [Text] -> [Text]
paramNamesVals isForRouteDef pnames vnames =
    zipWith (nameParamConcat isForRouteDef) pnames vnames

-- | Prepare a list of identifiers for output given a quoting or
-- formatting function `qw`.
quoteConcat :: (a -> Text) -> [a] -> Text
quoteConcat qw xs = mconcat $ intersperse (", " :: Text) $ map qw xs

-- | Retrieve the location for the Julius defining the controllers.
-- Configured by the type class 'HasPath'.
templateFile :: (NgFoundation c) => ViewName c -> FilePath
templateFile v = assemblePath [viewFilesLocation v, "template.hamlet"]

controllerFile :: (NgFoundation c) => ViewName c -> FilePath
controllerFile v = viewFilesLocation v <> (pathSeparator : "controller.julius")

componentFile :: (NgCompFoundation c) => CompName c -> FilePath
componentFile comp = assemblePath [getPath comp, "component.julius"]

viewFilesLocation :: forall c. (NgFoundation c) => ViewName c -> FilePath
viewFilesLocation v = assemblePath [clientDir, viewDir]
  where
    clientDir = getPath (minBound :: c)
    viewDir = getPath v

assemblePath :: [FilePath] -> FilePath
assemblePath fs = concat $ intersperse (pathSeparator:"") fs

-- | The simplest way to include an AngularJS client: nothing but the
-- widget output from an `NgMain`. Implementation in terms of 'ngLayout'.
ngLayoutSimple :: (NgFoundation c) => NgMain c -> HandlerT (YesodBase c) IO Html
ngLayoutSimple ngmain@NgMain{..} = ngLayout mempty ngmain mempty

-- | Formatting of a route suitable for use in an AngularJS route definition.
-- Internal to this module?
viewRouteDef :: (NgFoundation c) => NgView c -> Text 
viewRouteDef v@NgView{..} = "/" <> ngRef v <> (mconcat $ 
                            paramNamesVals isForRouteDef paramNames paramNames)
 where isForRouteDef = True
       paramNames    = routeParamNames ngViewName



-- | This function is used with scripted view switching; in a button
-- is probably the canonical example. Let's say the function
-- @go@ is used to switch views on some $scope, like so:
--
-- @
-- $scope.go = function ( p ) { $location.path( p ); };
-- @
--
-- Then we can write code like this inside Hamlet:
--
-- @
-- \<button data-ng-click=\"go(\#{ viewRouteJSArgs $ SinglePersonNgR \"p.id\" })\">Edit\<\/button\>
-- @
--
-- ...to be able to use scripted view switching within e.g. @ng-repeat@  like so:
--
-- @
-- \<ul\>
--   \<li ng-repeat=\"p in persons\"\>
--    Name: \<b\>{{p.person.name}}\<\/b\>, Age: \<b\>{{p.person.age}} \<\/b\> (id: {{p.id}})
--   \<button data-ng-click=\"go('/SinglePerson/' + 'pid/' + p.id)\">Edit\<\/button\>
--   \<\/li\>
-- \<ul\>
-- @
--
-- To see how to use client-side interpreted links that do not use
-- scripted view switching, see 'viewRoute'.
-- 
-- MOVE to viewRoute! The same
-- thing that does not use scripted view switching, looks like this
-- (just replace the whole @button@ element above):
--
-- @
-- \<a data-ng-href=\#{ viewRoute (SinglePersonNgR \"{{p.id}}\") }>Edit\<\/a\>
-- becomes
-- \<a data-ng-href=\"\#\/SinglePerson\/pid\/2\" href=\"\#\/SinglePerson\/pid\/2\"\>Edit\</a\>
-- if for a particular iteration 'p.id' has the value '2'.
-- @
--
-- For more information on different route considerations in AngularJS
-- and yesod-ng, see ...
--
viewRouteJSArgs :: (NgFoundation c) => ViewRoute c -> Text -- default most used
viewRouteJSArgs r = mconcat $
    ["'/", viewName, "/'"] <> (zipWith hlp argNames argValues)
  where
    viewName = pack . show . assocView $ r 
    argNames = routeParamNames . assocView $ r
    argValues = routeArgs r 
    hlp name value = " + " <> (squote $ name <> "/") <> " + " <> value

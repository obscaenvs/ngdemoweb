{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, ScopedTypeVariables, GeneralizedNewtypeDeriving, RecordWildCards, FlexibleInstances#-}
module Main  where

import           Prelude
import           Angular
import           Yesod.Angular 
import           Yesod.Angular.Types 
import           Yesod.Angular.Dependencies
import           Yesod.Angular.Macros          (mkAngViewADT)
import           Yesod.Angular.Util

import           Yesod
import           Yesod.Core.Widget             (addScriptEither)
import           Control.Applicative           ((<$>),(<*>))
import           Control.Arrow                 ((&&&))
import           Data.IORef
import           Data.Text                     (Text, pack)
import           Data.Map                      (Map)
import qualified Data.Map                   as Map
import           Data.Monoid                   ((<>), mempty)
import           Language.Haskell.TH           (mkName)
import           Text.Julius                   (rawJS, juliusFile)
import           Text.Hamlet                   (hamlet, hamletFile)

import Data.Aeson (ToJSON (..), FromJSON (..), (.:), (.=), object, Value (Object))

import Data.Time.LocalTime (getZonedTime)

mkYesodDispatch "App" resourcesApp 

getPersonTemplateR :: ViewName PersonClient -> HandlerT (YesodBase PersonClient) IO Html
getPersonTemplateR vn = do
    let tmpl = lookupTemplate viewMap vn -- viewMap from global scope
    maybe (return mempty) dispatchTemplate tmpl

--------------------------------------------------------------------------------
--                       AngularJS client definition                          --
--------------------------------------------------------------------------------

-- Defining a client in AngularJS With TH, and something of an DSL-ish
-- way of defining and adding the components of a client.

-- <<obscaenvs>> This is global; put in an env for a Reader instead?
angClient :: NgMain PersonClient
angClient = PersonClient
              `withDefaultView` $(mkNgViewQ AllPersons)
              `addAPIDep`       NgRoute
              `addAPIDep`       NgResource
              `addView`         $(mkNgViewQ AddPerson)
              `addView`         $(mkNgViewQ SinglePerson)
              `addService`      (
                                  $(mkNgServiceQ PersonService)
                                    `addServiceDeps` [read "$resource"]
                                )
                                

--------------------------------------------------------------------------------

-- <<obscaenvs>> Left from proof of concept stage. Refactor.
viewMap :: TemplateMap PersonClient 
viewMap = mkTemplateMap angClient

-- <<obscaenvs>> Modify to use env + ReaderT instead?
-- something like: `ReaderT NgEnv (HandlerT App IO) Html`
-- then: `runReaderT ngapp ngenv :: HandlerT App IO Html`
-- `runNgApp` better?
handleHomeR :: HandlerT App IO Html
handleHomeR = do
  site <- getYesod
  t <- lift getZonedTime
  let personClient = ngClient site
  ngLayout (preW t) personClient mempty
    where preW t = toWidget [hamlet|$newline always
                             <h1>#{show PersonClient}
                             <ul>
                               <li>
                                 <a href=#{viewRoute AllPersonsNgR}>All persons
                               <li>
                                 <a href=#{viewRoute (AddPersonNgR)}>Add person
                             <pre>Time rendered: #{show t} 
                             |] 

-- For the AngularJS resource method `get`.
getNgPersonResR :: PersonId -> HandlerT App IO Value
getNgPersonResR pid =
    jsonHandler_ $ do
        app <- getYesod
        m <- liftIO $ readIORef $ ipeople app
        case Map.lookup pid m of
            Nothing -> notFound
            Just p -> return p

-- For the AngularJS resource method `save` (saving an edit).
postNgPersonResR :: PersonId -> HandlerT App IO Value
postNgPersonResR pid =
    jsonHandler $ \(p :: Person) -> do
        app <- getYesod
        () <- liftIO $ atomicModifyIORef (ipeople app) $
                         \m -> (Map.insert pid p m, ())
        return $ PersonEntity pid p

-- For the AngularJS resource method `remove`.
deleteNgPersonResR :: PersonId -> HandlerT App IO Value
deleteNgPersonResR pid = 
    jsonHandler_ $ do
        app <- getYesod
        mp <- liftIO $ atomicModifyIORef (ipeople app) $
                Map.delete pid &&& Map.lookup pid
--                \m -> (Map.delete pid m, Map.lookup pid m)
        return $ maybe (PersonEntity pid (Person "" 0))
                       (PersonEntity pid)
                       mp

-- For the AngularJS resource method `query`.
-- AngularJS expects a JSON array back.
getNgPersonResNoArgR :: HandlerT App IO Value
getNgPersonResNoArgR =
    jsonHandler_ $ do
      people' <- getYesod >>= liftIO . readIORef . ipeople
      return $ map (uncurry PersonEntity) $
               Map.toList people'

-- For the AngularJS resource method `save`, when saving a new person.
-- Angular expects a JSON object back - hence the return thingy.
postNgPersonResNoArgR :: HandlerT App IO Value
postNgPersonResNoArgR = 
  jsonHandler $ \person -> do
        app <- getYesod
        personId <- fmap (PersonId . pack . show) $ liftIO $ atomicModifyIORef (nextId app) $ \personId -> (personId + 1, personId + 1)
        () <- liftIO $ atomicModifyIORef (ipeople app) $ \m ->
            (Map.insert personId person m, ())
        -- any JSON object can be returned here, but we'll settle for just the
        -- person that was inserted along with its id.
        return $ PersonEntity personId person

-- <<obscaenv>> have to change this.
main :: IO ()
main = do
    p <- newIORef Map.empty
    ni <- newIORef 1
    warp 3000 $ App p ni angClient

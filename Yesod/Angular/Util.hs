{-# Language OverloadedStrings                 #-}

module Yesod.Angular.Util where

import Yesod.Core
import           Yesod.Core.Json                    (jsonToRepJson, parseJsonBody_)
import           Data.Monoid                   ((<>))
import           Data.Text                     (Text)

-- | Just a handy type synonym.
type UrlRender site = Route site -> [(Text, Text)] -> Text

-- | Make a handler to act on the (assumed) JSON in the request body.
jsonHandler :: (FromJSON i, ToJSON o)
            => (i -> HandlerT site IO o) -> HandlerT site IO Value
jsonHandler f = parseJsonBody_ >>= f >>= returnJson

-- | Make a JSON handler, ignore request body.
jsonHandler_ :: (ToJSON o)
            => HandlerT site IO o -> HandlerT site IO Value
jsonHandler_ f = f >>= returnJson

-- | Single-quote the argument; e.g. useful for displaying injected
-- variables before controller and service functions.
squote :: Text -> Text
squote x =  "'" <> x <> "'"
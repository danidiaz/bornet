{-# LANGUAGE OverloadedStrings #-}

-- | Lucid definitions of HTMX attributes.
module Bornet.Api.Server.Htmx where

import Data.Aeson as Aeson
import Data.Aeson.Text qualified as Text
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy (toStrict)
import Lucid
import Lucid.Base (makeAttributes)
import Network.URI (URI, uriToString)

-- https://hackage.haskell.org/package/lucid-htmx-0.1.0.7/docs/Lucid-Htmx.html
-- https://github.com/AjaniBilby/hx-drag/blob/main/app/routes/bucket.tsx
hxDrag_ :: Aeson.Value -> Attributes
hxDrag_ v = makeAttributes "hx-drag" $ encodez v

hxDrop_ :: Aeson.Value -> Attributes
hxDrop_ v = makeAttributes "hx-drop" $ encodez v

hxTarget_ :: Text -> Attributes
hxTarget_ = makeAttributes "hx-target"

hxSwap_ :: Text -> Attributes
hxSwap_ = makeAttributes "hx-swap"

hxDropMethod_ :: Text -> Attributes
hxDropMethod_ = makeAttributes "hx-drop-method"

hxDropAction_ :: URI -> Attributes
hxDropAction_ uri = makeAttributes "hx-drop-action" $ Text.pack $ uriToString id uri ""

hxExt_ :: Text -> Attributes
hxExt_ = makeAttributes "hx-ext"

hxSwapOOB_ :: Text -> Attributes
hxSwapOOB_ = makeAttributes "hx-swap-oob"

hxDelete_ :: URI -> Attributes
hxDelete_ uri = makeAttributes "hx-delete" $ Text.pack $ uriToString id uri ""

hxConfirm_ :: Text -> Attributes
hxConfirm_ = makeAttributes "hx-confirm"

encodez :: Aeson.Value -> Text
encodez v = v & Text.encodeToLazyText & toStrict

-- | Variant of href_ that takes directly an URI.
uriHref_ :: URI -> Attributes
uriHref_ uri = href_ $ uriText uri

uriText :: URI -> Text
uriText uri = Text.pack $ uriToString id uri ""


{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

import Yesod

data App = App

mkYesod "App" [parseRoutes|
  / HomeR GET
  |]

instance Yesod App

getHomeR = return $ object ["msg" .= "Hello, World!"]

main = warp 3000 App

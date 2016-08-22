{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

import Yesod

data App = App

mkYesod "App" [parseRoutes|
  / HomeR GET
  /set-message SetMessageR POST
  |]

instance Yesod App where
  defaultLayout widget = do
    pc <- widgetToPageContent widget
    mmsg <- getMessage
    withUrlRenderer
      [hamlet|
        $doctype 5
        <html>
          <head>
            <title>#{pageTitle pc}
            ^{pageHead pc}
          <body>
            $maybe msg <- mmsg
              <p>Your message was: #{msg}
            ^{pageBody pc}
      |]

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = defaultLayout
  [whamlet|
    <form method=post action=@{SetMessageR}>
      My message is : #
      <input Type=text name=message>
      <button>Go
  |]

postSetMessageR :: Handler ()
postSetMessageR = do
  msg <- runInputPost $ ireq textField "message"
  oldMsg <- getMessage
  setMessage $ toHtml $ msg
  redirect HomeR

main :: IO ()
main = warp 3000 App

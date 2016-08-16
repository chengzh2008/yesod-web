{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)
import Data.Time (Day)
import Yesod
import Yesod.Form.Jquery

data App = App

mkYesod "App" [parseRoutes|
  / HomeR GET
  /person PersonR POST
  |]

instance Yesod App

instance RenderMessage App FormMessage where
  -- can provide the translation funciton if i18n needed
  renderMessage _ _ = defaultFormMessage

-- default jquery library "Google CDN"
instance YesodJquery App

-- the data type from the form
data Person = Person
    { personName :: Text
    , personBirthday :: Day
    , personFavoriteColor :: Maybe Text
    , personEmail :: Text
    , personWebsite :: Maybe Text
    }
  deriving Show

personForm :: Html -> MForm Handler (FormResult Person, Widget)
personForm = renderDivs $ Person
  <$> areq textField "Name" Nothing
  <*> areq (jqueryDayField def
      { jdsChangeYear = True -- give a year drop-down
      , jdsYearRange = "1900:-5" -- 1900 to five years ago
      }) "Birthday" Nothing

  <*> aopt textField "Favorite color" Nothing
  <*> areq emailField "Email address" Nothing
  <*> aopt urlField "Website" Nothing

-- GET handler to display the form
getHomeR :: Handler Html
getHomeR = do
  -- generate the form
  (widget, enctype) <- generateFormPost personForm
  defaultLayout
    [whamlet|
      <p>
        The widget generated contains only the contents of the form, not the form tag itself. So...
      <form method=post action=@{PersonR} enctype=#{enctype}>
        ^{widget}
        <p>It also doesn't include the submit button.
        <button>Submit
    |]

postPersonR :: Handler Html
postPersonR = do
  ((result, widget), enctype) <- runFormPost personForm
  case result of
    FormSuccess person -> defaultLayout [whamlet|<p>#{show person}|]
    _ -> defaultLayout
        [whamlet|
          <p>Invalid input, let's try again.
          <form method=post action=@{PersonR} enctype=#{enctype}>
            ^{widget}
            <button>Submit
        |]


main :: IO ()
main = warp 3000 App


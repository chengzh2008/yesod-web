{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, ViewPatterns #-}

import Data.Text (Text)
import qualified Data.Text as T
import Yesod

data App = App
instance Yesod App

mkYesod "App" [parseRoutes|
  /person/#Text PersonR GET
  /year/#Integer/month/#Text/day/#Int DateR
  /wiki/*Texts WikiR GET
  |]

getPersonR :: Text -> Handler Html
getPersonR name = defaultLayout [whamlet|
  <h1>
    Hello #{name}!|]


handleDateR :: Integer -> Text -> Int -> Handler Text
handleDateR year month day =
  return $
    T.concat [month, " ", T.pack $ show day, ", ", T.pack $ show year]

getWikiR :: [Text] -> Handler Text
getWikiR = return . T.unwords

main :: IO ()
main = warp 3000 App

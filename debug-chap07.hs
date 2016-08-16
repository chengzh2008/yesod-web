{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

import Control.Exception (IOException, try)
import Control.Monad (when)
import Yesod

data App = App
instance Yesod App where
  -- this function controls which messages are logged
  shouldLog App src level =
    True
    -- level == LevelWarn || level == LevelError

mkYesod "App" [parseRoutes|
  / HomeR GET
  |]

getHomeR :: Handler Html
getHomeR = do
  $logDebug "Trying to read data file"
  edata <- liftIO $ try $ readFile "datafile.txt"
  case edata :: Either IOException String of
    Left e -> do
      $logError $ "Could not read datafile.txt"
      defaultLayout [whamlet|An XXXXXerror occurred|]
    Right str -> do
      $logInfo "Reading of data file succeeded"
      let ls = lines str
      when (length ls < 5) $ $logWarn "Less than 5 lines of data"
      when (length ls > 5) $ $logWarn "More than 5 lines of data"
      defaultLayout
        [whamlet|
          <ol>
            $forall l <- ls
              <li>#{l}
        |]

main :: IO ()
main = warp 3000 App

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Team where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))


type PageYear = Natural

getTeamR :: PageYear -> Handler Html
getTeamR year = do
    -- $(widgetFile "homepage") -- this should only find the .julius and .lucius files. we want this. we want these to apply
                             -- to the page we request from the Database
    year <- runDB $ getContent year

    case year of
        Just (Year html _) ->
           defaultLayout $ (do
               setTitle "Java Like its Hot #16553"
               aDomId <- newIdent
               toWidget html)
        _ -> defaultLayout [whamlet|404 bad argument|]


getContent :: PageYear -> DB (Maybe Year)
getContent year = do
    content <- getBy $ Date (coerce year) --selectList [YearDate ==. (coerce year)] [] --[OffsetBy $ coerce year]
    case content of
        Just (Entity _ record) -> pure $ Just record
        _ -> pure Nothing

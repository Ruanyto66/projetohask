{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Filme where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

postFilmeR :: Handler Value
postFilmeR = do
    filme <- requireJsonBody :: Handler Filme
    filmeid <- runDB $ insert filme
    sendStatusJSON created201 (object ["resp" .= (fromSqlKey filmeid)])


    
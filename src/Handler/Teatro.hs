{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Teatro where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql



postTeatroR :: Handler Value
postTeatroR = do
    teatro <- requireJsonBody :: Handler Teatro
    teatroid <- runDB $ insert teatro
    sendStatusJSON created201 (object ["resp" .= (fromSqlKey teatroid)])
    
getConsultaTeatroR :: TeatroId -> Handler Value
getConsultaTeatroR teatroid = do
    teatro <- runDB $ get404 teatroid
    sendStatusJSON ok200 (object ["resp" .= toJSON teatro])

getTodosTeatrosR :: Handler Value
getTodosTeatrosR = do
    teatros <- runDB $ selectList [] [Asc TeatroNome]
    sendStatusJSON ok200 (object ["resp" .= toJSON teatros])
   
deleteApagarTeatroR :: TeatroId -> Handler Value
deleteApagarTeatroR teatroid = do
    _ <- runDB $ get404 teatroid
    runDB $ delete teatroid
    sendStatusJSON noContent204 (object ["resp" .= toJSON teatroid])
   

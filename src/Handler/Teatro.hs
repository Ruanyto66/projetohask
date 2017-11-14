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
    
putAlterarTeatroR :: TeatroId -> Handler Value
putAlterarTeatroR teatroid = do
    _ <- runDB $ get404 teatroid
    novoTeatro <- requireJsonBody :: Handler Teatro
    runDB $ replace teatroid novoTeatro
    sendStatusJSON noContent204 (object ["resp" .= toJSON teatroid])
    
patchAlterarTeNomeR :: TeatroId -> Text -> Handler Value
patchAlterarTeNomeR teatroid nome = do
    _ <- runDB $ update teatroid [TeatroNome =. nome]
    sendStatusJSON noContent204 (object ["resp" .= toJSON teatroid])

patchAlterarTeHorarioR :: TeatroId -> UTCTime -> Handler Value
patchAlterarTeHorarioR teatroid horario = do
    _ <- runDB $ update teatroid [TeatroHorario =. horario]
    sendStatusJSON noContent204 (object ["resp" .= toJSON teatroid])

patchAlterarTeDescricaoR :: TeatroId -> Text -> Handler Value
patchAlterarTeDescricaoR teatroid descricao = do
    _ <-runDB $ update teatroid [TeatroDescricao =. descricao]
    sendStatusJSON noContent204 (object ["resp" .= toJSON teatroid])
    
patchAlterarTeTelR :: TeatroId -> Int -> Handler Value
patchAlterarTeTelR teatroid tel = do
    _ <- runDB $ update teatroid [TeatroTel =. tel]
    sendStatusJSON noContent204 (object ["resp" .= toJSON teatroid])
    
patchAlterarTeEnderecoR :: TeatroId -> Text -> Handler Value
patchAlterarTeEnderecoR teatroid endereco = do
    _ <- runDB $ update teatroid [TeatroEndereco =. endereco]
    sendStatusJSON noContent204 (object ["resp" .= toJSON teatroid])
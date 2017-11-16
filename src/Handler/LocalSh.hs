{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.LocalSh where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

postLocalShR :: Handler Value
postLocalShR = do
    localSh <- requireJsonBody :: Handler LocalSh
    localShid <- runDB $ insert localSh
    sendStatusJSON created201 (object ["resp" .= (fromSqlKey localShid)])

getConsultaLocalShR :: LocalShId -> Handler Value
getConsultaLocalShR localShid = do
    localSh <- runDB $ get404 localShid
    sendStatusJSON ok200 (object ["resp" .= toJSON localSh])
 
getTodosLocaisShR :: Handler Value
getTodosLocaisShR = do
    locaisSh <- runDB $ selectList [] [Asc LocalShNome]
    sendStatusJSON ok200 (object ["resp" .= toJSON locaisSh])

deleteApagarLocalShR :: LocalShId -> Handler Value
deleteApagarLocalShR localShid = do
    _ <- runDB $ get404 localShid
    runDB $ delete localShid
    sendStatusJSON noContent204 (object ["resp" .= toJSON localShid])

putAlterarLocalShR :: LocalShId -> Handler Value
putAlterarLocalShR localShid = do
    _ <- runDB $ get404 localShid
    novoLocalSh <- requireJsonBody :: Handler LocalSh
    runDB $ replace localShid novoLocalSh
    sendStatusJSON noContent204 (object ["resp" .= toJSON localShid])

patchAlterarShNomeR :: LocalShId -> Text -> Handler Value
patchAlterarShNomeR localShid nome = do
    _ <- runDB $ update localShid [LocalShNome =. nome]
    sendStatusJSON noContent204 (object ["resp" .= toJSON localShid])

patchAlterarShHorarioR :: LocalShId -> UTCTime -> Handler Value
patchAlterarShHorarioR localShid horario = do
    _ <- runDB $ update localShid [LocalShHorario =. horario]
    sendStatusJSON noContent204 (object ["resp" .= toJSON localShid])

patchAlterarShDescricaoR :: LocalShId -> Text -> Handler Value
patchAlterarShDescricaoR localShid descricao = do
    _ <- runDB $ update localShid [LocalShDescricao =. descricao]
    sendStatusJSON noContent204 (object ["resp" .= toJSON localShid])

patchAlterarShTelR :: LocalShId -> Int -> Handler Value
patchAlterarShTelR localShid tel = do
    _ <- runDB $ update localShid [LocalShTel =. tel]
    sendStatusJSON noContent204 (object ["resp" .= toJSON localShid])

patchAlterarShEnderecoR :: LocalShId -> Text -> Handler Value
patchAlterarShEnderecoR localShid endereco = do
    _ <- runDB $ update localShid [LocalShEndereco =. endereco]
    sendStatusJSON noContent204 (object ["resp" .= toJSON localShid])


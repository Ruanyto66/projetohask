{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.LocalExpo where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql


postLocalExpoR :: Handler Value
postLocalExpoR = do
    localExpo <- requireJsonBody :: Handler Teatro
    localExpoid <-runDB $ insert localExpo
    sendStatusJSON created201 (object ["resp" .= (fromSqlKey localExpoid)])
    
    
getConsultaLocalExpoR :: LocalExpoId -> Handler Value
getConsultaLocalExpoR localExpoid = do
    localExpo <- runDB $ get404 localExpoid
    sendStatusJSON ok200 (object ["resp" .= toJSON localExpo])
    
getTodosLocaisExpoR :: Handler Value
getTodosLocaisExpoR = do
    locaisExpo <- runDB $ selectList [] [Asc LocalExpoNome]
    sendStatusJSON ok200 (object ["resp" .= toJSON locaisExpo])

deleteApagarLocalExpoR :: LocalExpoId -> Handler Value
deleteApagarLocalExpoR localExpoid = do
    _ <- runDB $ get404 localExpoid
    runDB $ delete localExpoid
    sendStatusJSON noContent204 (object ["resp" .= toJSON localExpoid])

putAlterarLocalExpoR :: LocalExpoId -> Handler Value
putAlterarLocalExpoR localExpoid = do
    _ <- runDB $ get404 localExpoid
    novoLocalExpo <- requireJsonBody :: Handler LocalExpo
    runDB $ replace localExpoid novoLocalExpo
    sendStatusJSON noContent204 (object ["resp" .= toJSON localExpoid])
    
patchAlterarLsNomeR :: LocalExpoId -> Text -> Handler Value
patchAlterarLsNomeR localExpoid nome = do
    _ <- runDB $ update localExpoid [LocalExpoNome =. nome]
    sendStatusJSON noContent204 (object ["resp" .= toJSON localExpoid])

patchAlterarLsHorarioR :: LocalExpoId -> UTCTime -> Handler Value
patchAlterarLsHorarioR localExpoid horario = do
    _ <- runDB $ update localExpoid [LocalExpoHorario =. horario]
    sendStatusJSON noContent204 (object ["resp" .= toJSON localExpoid])
    
patchAlterarLsTelR :: LocalExpoId -> Int -> Handler Value
    _ <- runDB $ update localExpoid [LocalExpoTel =. tel]
    sendStatusJSON noContent204 (object ["resp" .= toJSON localExpoid])
    
patchAlterarLsEnderecoR :: LocalExpoId -> Text -> Handler Value
    _ <- runDB $ update localExpoid [LocalExpoEndereco =. endereco]
    sendStatusJSON noContent204 (object ["resp" .= toJSON localExpoid])
    
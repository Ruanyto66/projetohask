{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Cinema where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql



postCinemaR :: Handler Value
postCinemaR = do
    cinema <-requireJsonBody :: Handler Cinema
    cinemaid <-runDB $ insert cinema
    sendStatusJSON created201 (object ["resp" .= (fromSqlKey cinemaid)])
    

getConsultaCinemaR :: CinemaId -> Handler Value
getConsultaCinemaR cinemaid = do
   cinema <- runDB $ get404 cinemaid
   sendStatusJSON ok200 (object ["resp" .=  toJSON cinema])
   
   
getTodosCinemasR :: Handler Value
getTodosCinemasR = do
    cinemas <- runDB $ selectList [] [Asc CinemaNome]
    sendStatusJSON ok200 (object ["resp" .= toJSON cinemas])
    
    
deleteApagarCinemaR :: CinemaId -> Handler Value
deleteApagarCinemaR cinemaid = do
    _ <-runDB $ get404 cinemaid
    runDB $ delete cinemaid
    sendStatusJSON noContent204 (object ["resp" .= toJSON cinemaid])

putAlterarCinemaR :: CinemaId -> Handler Value
putAlterarCinemaR cinemaid = do
    _ <- runDB $ get404 cinemaid
    novoCinema <- requireJsonBody :: Handler Cinema
    runDB $ replace cinemaid novoCinema
    sendStatusJSON noContent204 (object ["resp" .= toJSON cinemaid])
    
patchAlterarNomeR :: CinemaId -> Text -> Handler Value
patchAlterarNomeR cinemaid nome = do
    _ <- runDB $ update cinemaid [CinemaNome =. nome]
    sendStatusJSON noContent204 (object ["resp" .= toJSON cinemaid])

patchAlterarHorarioR :: CinemaId ->  UTCTime -> Handler Value
patchAlterarHorarioR cinemaid horario = do
    _ <- runDB $ update cinemaid [CinemaHorario =. horario]
    sendStatusJSON noContent204 (object ["resp" .= toJSON cinemaid])

patchAlterarDescricaoR :: CinemaId -> Text -> Handler Value
patchAlterarDescricaoR cinemaid descricao = do
    _ <- runDB $ update cinemaid [CinemaDescricao =. descricao]
    sendStatusJSON noContent204 (object ["resp" .= toJSON cinemaid])

patchAlterarTelR :: CinemaId -> Int -> Handler Value
patchAlterarTelR cinemaid tel = do
    _ <- runDB $ update cinemaid [CinemaTel =. tel]
    sendStatusJSON noContent204 (object ["resp" .= toJSON cinemaid])

patchAlterarEnderecoR :: CinemaId -> Text -> Handler Value
patchAlterarEnderecoR cinemaid endereco = do
    _ <- runDB $ update cinemaid [CinemaEndereco =. endereco]
    sendStatusJSON noContent204 (object ["resp" .= toJSON cinemaid])

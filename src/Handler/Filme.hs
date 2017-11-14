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

getConsultaFilmeR :: FilmeId -> Handler Value
getConsultaFilmeR filmeid = do
    filme <- runDB $ get404 filmeid
    sendStatusJSON ok200 (object ["resp" .= toJSON filme])

getTodosFilmesR :: Handler Value
getTodosFilmesR = do
    filmes <- runDB $ selectList [] [Asc FilmeNome]
    sendStatusJSON ok200 (object ["resp" .= toJSON filmes])

deleteApagarFilmeR :: FilmeId -> Handler Value
deleteApagarFilmeR filmeid = do
    _ <- runDB $ get404 filmeid
    sendStatusJSON noContent204 (object ["resp" .= toJSON filmeid])

putAlterarFilmeR :: FilmeId -> Handler Value
putAlterarFilmeR filmeid = do
    _ <- runDB $ get404 filmeid
    novoFilme <- requireJsonBody :: Handler Filme
    runDB $ replace filmeid novoFilme
    sendStatusJSON noContent204 (object ["resp" .= toJSON filmeid])

patchAlterarFiNomeR :: FilmeId -> Text -> Handler Value
patchAlterarFiNomeR filmeid nome = do
    _ <- runDB $ update filmeid [FilmeNome =. nome]
    sendStatusJSON noContent204 (object ["resp" .= toJSON filmeid])


patchAlterarFiSinopseR :: FilmeId -> Text -> Handler Value
patchAlterarFiSinopseR filmeid sinopse = do
    _ <- runDB $ update filmeid [FilmeSinopse =. sinopse]
    sendStatusJSON noContent204 (object ["resp" .= toJSON filmeid])
    
patchAlterarFiHorarioR :: FilmeId -> UTCTime -> Handler Value
patchAlterarFiHorarioR filmeid horario = do
    _ <- runDB $ update filmeid [FilmeHorario =. horario]
    sendStatusJSON noContent204 (object ["resp" .= toJSON filmeid])

patchAlterarFiPrecoR :: FilmeId -> Double -> Handler Value
patchAlterarFiPrecoR filmeid preco = do
    _ <-runDB $ update filmeid [FilmePreco =. preco]
    sendStatusJSON noContent204 (object ["resp" .= toJSON filmeid])

patchAlterarTipoFilmeR :: FilmeId -> Text -> Handler Value
patchAlterarTipoFilmeR filmeid tipoFilme = do
    _ <- runDB $ update filmeid [FilmeTipoFilme =. tipoFilme]
    sendStatusJSON noContent204 (object ["resp" .= toJSON filmeid])
    
patchAlterarAudioFilmeR :: FilmeId -> Text -> Handler Value
patchAlterarAudioFilmeR filmeid audioFilme = do
    _ <- runDB $ update filmeid [FilmeAudioFilme =. audioFilme]
    sendStatusJSON noContent204 (object ["resp" .= toJSON filmeid])

patchAlterarFiGeneroR :: FilmeId -> Text -> Handler Value
patchAlterarFiGeneroR filmeid genero = do
    _ <- runDB $ update filmeid [FilmeGenero =. genero]
    sendStatusJSON noContent204 (object ["resp" .= toJSON filmeid])

patchAlterarFiClassificacaoR :: FilmeId -> Int -> Handler Value
patchAlterarFiClassificacaoR filmeid classificacao = do
    _ <- runDB $ update filmeid [FilmeClassificacao =. classificacao]
    sendStatusJSON noContent204 (object ["resp" .= toJSON filmeid])





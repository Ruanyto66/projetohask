{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Sho where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql


postShoR :: Handler Value
postShoR = do 
    sho <- requireJsonBody :: Handler Sho
    shoid <- runDB $ insert sho
    sendStatusJSON created201 (object ["resp" .= (fromSqlKey shoid)])

getConsultaShoR :: ShoId -> Handler Value
getConsultaShoR shoid = do
    sho <- runDB $ get404 shoid
    sendStatusJSON ok200 (object ["resp" .= toJSON sho])

getTodosShosR :: Handler Value
getTodosShosR = do
    shos <- runDB $ selectList [] [Asc ShoNome]
    sendStatusJSON ok200 (object ["resp" .= toJSON shos])
 
deleteApagarShoR :: ShoId -> Handler Value
deleteApagarShoR shoid = do
    _ <- runDB $ get404 shoid
    sendStatusJSON noContent204 (object ["resp" .= toJSON shoid])
    
putAlterarShoR :: ShoId -> Handler Value
putAlterarShoR shoid = do
   _ <- runDB $ get404 shoid
   novoSho <- requireJsonBody :: Handler Sho
   runDB $ replace shoid novoSho
   sendStatusJSON noContent204 (object ["resp" .= toJSON shoid])
   
patchAlterarDtShoR :: ShoId -> UTCTime -> Handler Value
patchAlterarDtShoR shoid horario = do
    _ <- runDB $ update shoid [ShoHorario =. horario]
    sendStatusJSON noContent204 (object ["resp" .= toJSON shoid])
   
   
patchAlterarShoNomeR :: ShoId -> Text -> Handler Value
patchAlterarShoNomeR shoid nome = do
    _ <- runDB $ update shoid [ShoNome =. nome]
    sendStatusJSON noContent204 (object ["resp" .= toJSON shoid])

patchAlterarShoHorarioR :: ShoId -> UTCTime -> Handler Value
patchAlterarShoHorarioR shoid horario = do
    _ <- runDB $ update shoid [ShoHorario =. horario]
    sendStatusJSON noContent204 (object ["resp" .= toJSON shoid])
    
patchAlterarShoDescricaoR :: ShoId -> Text -> Handler Value
patchAlterarShoDescricaoR shoid descricao = do
    _ <- runDB $ update shoid [ShoDescricao =. descricao]
    sendStatusJSON noContent204 (object ["resp" .= toJSON shoid])
    
patchAlterarShoPrecoR :: ShoId -> Double -> Handler Value
patchAlterarShoPrecoR shoid preco = do
    _ <- runDB $ update shoid [ShoPreco =. preco]
    sendStatusJSON noContent204 (object ["resp" .= toJSON shoid])

patchAlterarShoNomeArtistaR :: ShoId -> Text -> Handler Value
patchAlterarShoNomeArtistaR shoid nmArtista = do
    _ <- runDB $ update shoid [ShoNmArtista =. nmArtista]
    sendStatusJSON noContent204 (object ["resp" .= toJSON shoid])

patchAlterarShoNomeBandaR :: ShoId -> Text -> Handler Value
patchAlterarShoNomeBandaR shoid nmBanda = do
   _ <- runDB $ update shoid [ShoNmBanda =. nmBanda]
   sendStatusJSON noContent204 (object ["resp" .= toJSON shoid])
   
patchAlterarShoEnderecoR :: ShoId -> Text -> Handler Value
patchAlterarShoEnderecoR shoid endereco = do
    _ <- runDB $ update shoid [ShoEndereco =. endereco]
    sendStatusJSON noContent204 (object ["resp" .= toJSON shoid])
    
patchAlterarShoClassificacaoR :: ShoId -> Int -> Handler Value
patchAlterarShoClassificacaoR shoid classificao = do
   _ <- runDB $ update shoid [ShoClassificacao =. classificao]
   sendStatusJSON noContent204 (object ["resp" .= toJSON shoid])
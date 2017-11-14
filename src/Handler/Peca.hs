{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Peca where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

postPecaR :: Handler Value
postPecaR = do
    peca <- requireJsonBody :: Handler Peca
    pecaid <- runDB $ insert peca 
    sendStatusJSON created201 (object ["resp" .= (fromSqlKey pecaid)])

getConsultaPecaR :: PecaId -> Handler Value
getConsultaPecaR pecaid = do
    peca <- runDB $ get404 pecaid
    sendStatusJSON ok200 (object ["resp" .= toJSON peca])

getTodasPecasR :: Handler Value
getTodasPecasR = do
    pecas <- runDB $ selectList [][Asc TeatroNome]
    sendStatusJSON ok200 (object ["resp" .= toJSON pecas])

deleteApagarPecaR :: PecaId -> Handler Value
deleteApagarPecaR pecaid = do
   _ <- runDB $ get404 pecaid
   runDB $ delete pecaid
   sendStatusJSON noContent204 (object ["resp" .= toJSON pecaid])
    
    
putAlterarPecaR :: PecaId -> Handler Value
putAlterarPecaR pecaid = do
    _ <- runDB $ get404 pecaid
    novaPeca <- requireJsonBody :: Handler Peca
    runDB $ replace pecaid novaPeca
    sendStatusJSON noContent204 (object ["resp" .= toJSON pecaid])
    

patchAlterarDtPecaR :: PecaId -> UTCTime -> Handler Value
patchAlterarDtPecaR pecaid dtPeca = do
    _ <- runDB $ update pecaid [PecaDtPeca =. dtPeca]
    sendStatusJSON noContent204 (object ["resp" .= toJSON pecaid])


patchAlterarPeNomeR :: PecaId -> Text -> Handler Value
patchAlterarPeNomeR pecaid nome = do
    _ <- runDB $ update pecaid [PecaNome =. nome]
    sendStatusJSON noContent204 (object ["resp" .= toJSON pecaid])
    
patchAlterarPeSinopseR :: PecaId -> Text -> Handler Value
patchAlterarPeSinopseR pecaid sinopse = do
    _ <- runDB $ update pecaid [PecaSinopse =. sinopse]
    sendStatusJSON noContent204 (object ["resp" .= toJSON pecaid])

patchAlterarPeHorarioR :: PecaId -> UTCTime -> Handler Value
patchAlterarPeHorarioR pecaid horario = do
    _ <- runDB $ update pecaid [PecaHorario =. horario]
    sendStatusJSON noContent204 (object ["resp" .= toJSON pecaid])

patchAlterarPePrecoR :: PecaId -> Double -> Handler Value
patchAlterarPePrecoR pecaid preco = do
    _ <- runDB $ update pecaid [PecaPreco =. preco]
    sendStatusJSON noContent204 (object ["resp" .= toJSON pecaid])
    
patchAlterarPeTipoPecaR :: PecaId -> Text -> Handler Value
patchAlterarPeTipoPecaR pecaid tipoPeca = do
   _ <- runDB $ update pecaid [PecaTipoPeca =. tipoPeca]
   sendStatusJSON noContent204 (object ["resp" .= toJSON pecaid])

patchAlterarPeGeneroR :: PecaId -> Text -> Handler Value
patchAlterarPeGeneroR pecaid genero = do
    _ <- runDB $ update pecaid [PecaGenero =. genero]
    sendStatusJSON noContent204 (object ["resp" .= toJSON pecaid])

patcAlterarPeClassificacao :: PecaId -> Int -> Handler Value
patcAlterarPeClassificacao pecaid classificacao = do
    _ <- runDB $ update pecaid [PecaClassificacao =. classificacao]
    sendStatusJSON noContent204 (object ["resp" =. pecaid])
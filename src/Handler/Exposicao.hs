{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Exposicao where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql


postExposicaoR :: Handler Value
postExposicaoR = do
    exposicao <- requireJsonBody :: Handler Exposicao
    exposicaoid <- runDB $ insert exposicao
    sendStatusJSON created201 (object ["resp" .= (fromSqlKey exposicaoid)])

getConsultaExposicaoR :: ExposicaoId -> Handler Value
getConsultaExposicaoR exposicaoid = do
    exposicao <- runDB $ get404 exposicaoid
    sendStatusJSON ok200 (object ["resp" .= toJSON exposicao])

getTodasExposicoesR :: Handler Value
getTodasExposicoesR = do
    exposicoes <- runDB $ selectList [] [Asc ExposicaoNome]
    sendStatusJSON ok200 (object ["resp" .= toJSON exposicoes])
    
deleteApagarExposicaoR :: ExposicaoId -> Handler Value
deleteApagarExposicaoR exposicaoid = do
    _ <- runDB $ get404 exposicaoid
    runDB $ delete exposicaoid
    sendStatusJSON noContent204 (object ["resp" .= toJSON exposicaoid])

putAlterarExposicaoR :: ExposicaoId -> Handler Value
putAlterarExposicaoR exposicaoid = do
    _ <- runDB $ get404 exposicaoid
    novaExposicao <- requireJsonBody :: Handler Exposicao
    runDB $ replace exposicaoid novaExposicao
    sendStatusJSON noContent204 (object ["resp" .= toJSON exposicaoid])
    
patchAlterarDtExposicaoR :: ExposicaoId -> UTCTime -> Handler Value
patchAlterarDtExposicaoR exposicaoid dtExposicao = do
    _ <- runDB $ update exposicaoid [ExposicaoDtExposicao =. dtExposicao]
    sendStatusJSON noContent204 (object ["resp" .= toJSON exposicaoid])
    
patchAlterarExNomeR :: ExposicaoId -> Text -> Handler Value
patchAlterarExNomeR exposicaoid nome = do
    _ <- runDB $ update exposicaoid [ExposicaoNome =. nome]
    sendStatusJSON noContent204 (object ["resp" .= toJSON exposicaoid])
    
patchAlterarExHorarioR :: ExposicaoId -> UTCTime -> Handler Value
patchAlterarExHorarioR exposicaoid horario = do
   _ <- runDB $ update exposicaoid [ExposicaoHorario =. horario]
   sendStatusJSON noContent204 (object ["resp" .= toJSON exposicaoid])

patchAlterarExDescricaoR :: ExposicaoId -> Text -> Handler Value
patchAlterarExDescricaoR exposicaoid descricao = do
    _ <- runDB $ update exposicaoid [ExposicaoDescricao =. descricao]
    sendStatusJSON noContent204 (object ["resp" .= toJSON exposicaoid])
    
patchAlterarExPrecoR :: ExposicaoId -> Double -> Handler Value
patchAlterarExPrecoR exposicaoid preco = do
    _ <- runDB $ update exposicaoid [ExposicaoPreco =. preco]
    sendStatusJSON noContent204 (object ["resp" .= toJSON exposicaoid])

patchAlterarExEnderecoR :: ExposicaoId -> Text -> Handler Value
patchAlterarExEnderecoR exposicaoid endereco = do
    _ <- runDB $ update exposicaoid [ExposicaoEndereco =. endereco]
    sendStatusJSON noContent204 (object ["resp" .= toJSON exposicaoid])

patchAlterarExTelR :: ExposicaoId -> Int -> Handler Value
patchAlterarExTelR exposicaoid tel = do
    _ <- runDB $ update exposicaoid [ExposicaoTel =. tel]
    sendStatusJSON noContent204 (object ["resp" .= toJSON exposicaoid])
    
patchAlterarExClassificacaoR :: ExposicaoId -> Int -> Handler Value
patchAlterarExClassificacaoR exposicaoid classificacao = do
    _ <- runDB $ update exposicaoid [ExposicaoClassificacao =. classificacao]
    sendStatusJSON noContent204 (object ["resp" .= toJSON exposicaoid])
    

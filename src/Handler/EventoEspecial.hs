{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.EventoEspecial where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql


postEventoEspecialR :: Handler Value
postEventoEspecialR = do
    eventoEspecial <- requireJsonBody :: Handler EventoEspecial
    eventoEspecialid <- runDB $ insert eventoEspecial
    sendStatusJSON created201 (object ["resp" .= (fromSqlKey eventoEspecialid)])

getConsultaEventoEspecialR :: EventoEspecialId -> Handler Value
getConsultaEventoEspecialR eventoEspecialid = do
    eventoEspecial <- runDB $ get404 eventoEspecialid
    sendStatusJSON ok200 (object ["resp" .= toJSON eventoEspecial])
    

getTodosEventosEspeciaisR :: Handler Value
getTodosEventosEspeciaisR = do
    eventosEspeciais <- runDB $ selectList [] [Asc EventoEspecialNmEvento]
    sendStatusJSON ok200 (object ["resp" .= toJSON eventosEspeciais])

deleteApagarEventoEspecialR :: EventoEspecialId -> Handler Value
deleteApagarEventoEspecialR eventoEspecialid = do
    _ <- runDB $ get404 eventoEspecialid
    runDB $ delete eventoEspecialid
    sendStatusJSON noContent204 (object ["resp" .= toJSON eventoEspecialid])
    

putAlterarEventoEspecialR :: EventoEspecialId -> Handler Value
putAlterarEventoEspecialR eventoEspecialid = do
    _ <- runDB $ get404 eventoEspecialid
    novoEventoEspecial <- requireJsonBody :: Handler EventoEspecial
    runDB $ replace eventoEspecialid novoEventoEspecial
    sendStatusJSON noContent204 (object ["resp" .= toJSON eventoEspecialid])

patchAlterarDtEventoEspecialR :: EventoEspecialId -> UTCTime -> Handler Value
patchAlterarDtEventoEspecialR eventoEspecialid dtEventoEspecial = do
    _ <- runDB $ update eventoEspecialid [EventoEspecialDtEventoEspecial =. dtEventoEspecial]
    sendStatusJSON noContent204 (object ["resp" .= toJSON eventoEspecialid])


patchAlterarNomeEventoR :: EventoEspecialId -> Text -> Handler Value
patchAlterarNomeEventoR eventoEspecialid nmEvento = do
    _ <- runDB $ update eventoEspecialid [EventoEspecialNmEvento =. nmEvento]
    sendStatusJSON noContent204 (object ["resp" .= toJSON eventoEspecialid])
   
   
patchAlterarNomeArtistaR :: EventoEspecialId -> Text -> Handler Value
patchAlterarNomeArtistaR eventoEspecialid nmArtista = do
    _ <- runDB $ update eventoEspecialid [EventoEspecialNmArtista =. nmArtista]
    sendStatusJSON noContent204 (object ["resp" .= toJSON eventoEspecialid])


patchAlterarNomeGrupoR :: EventoEspecialId -> Text -> Handler Value
patchAlterarNomeGrupoR eventoEspecialid nmGrupo = do
    _ <- runDB $ update eventoEspecialid [EventoEspecialNmGrupo =. nmGrupo]
    sendStatusJSON noContent204 (object ["resp" .= toJSON eventoEspecialid])
    
    
patchAlterarNomeLocalR :: EventoEspecialId -> Text -> Handler Value
patchAlterarNomeLocalR eventoEspecialid nmLocal = do
    _ <- runDB $ update eventoEspecialid [EventoEspecialNmLocal =. nmLocal]
    sendStatusJSON noContent204 (object ["res" .= toJSON eventoEspecialid])

patchAlterarEeHorarioR :: EventoEspecialId -> UTCTime -> Handler Value
patchAlterarEeHorarioR eventoEspecialid horario = do
    _ <- runDB $ update eventoEspecialid [EventoEspecialHorario =. horario]
    sendStatusJSON noContent204 (object ["resp" .= toJSON eventoEspecialid])
    
patchAlterarEeDescricaoR :: EventoEspecialId -> Text -> Handler Value
patchAlterarEeDescricaoR eventoEspecialid descricao = do
    _ <- runDB $ update eventoEspecialid [EventoEspecialDescricao =. descricao]
    sendStatusJSON noContent204 (object ["resp" .= toJSON eventoEspecialid])

patchAlterarEePrecoR :: EventoEspecialId -> Double -> Handler Value
patchAlterarEePrecoR eventoEspecialid preco = do
    _ <- runDB $ update eventoEspecialid [EventoEspecialPreco =. preco]
    sendStatusJSON noContent204 (object ["resp" .= toJSON eventoEspecialid])

patchAlterarEeEnderecoR :: EventoEspecialId -> Text -> Handler Value
patchAlterarEeEnderecoR eventoEspecialid endereco = do
    _ <- runDB $ update eventoEspecialid [EventoEspecialEndereco =. endereco]
    sendStatusJSON noContent204 (object ["resp" .= toJSON eventoEspecialid])

patchAlterarEeTelR :: EventoEspecialId -> Int -> Handler Value
patchAlterarEeTelR eventoEspecialid tel = do
    _ <- runDB $ update eventoEspecialid [EventoEspecialTel =. tel]
    sendStatusJSON noContent204 (object ["resp" .= toJSON eventoEspecialid])
    
patchAlterarEeClassificacaoR :: EventoEspecialId -> Int -> Handler Value
patchAlterarEeClassificacaoR eventoEspecialid classificacao = do
    _ <- runDB $ update eventoEspecialid [EventoEspecialClassificacao =. classificacao]
    sendStatusJSON noContent204 (object ["resp" .= toJSON eventoEspecialid]) 
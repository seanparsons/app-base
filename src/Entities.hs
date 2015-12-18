{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Entities (
    FieldType(..)
  , Field(..)
  , EntityReference(..)
  , Entity(..)
  , AllEntityInfo
  , FromRow(..)
  , ToRow(..)
  , ToField(..)
  , RowParser
  , MonadReader(..)
  , field
  , makeEntities
  , makeEntitiesLenses
  , makeEntitiesFromRow
  , makeEntitiesToRow
  , makeEntitiesPostgresClasses
) where

import Data.Text hiding (toUpper, toLower, intersperse, foldl', length)
import Data.List (intersperse, foldl')
import Language.Haskell.TH
import Control.Monad
import Control.Lens.TH
import Data.Char
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField hiding (Field)
import Servant.Server
import Control.Monad.Reader.Class
import Control.Monad.Reader.Class.Wiring

data FieldType = TextFieldType
               | IntFieldType
               | MaybeFieldType FieldType

data Field = Field
           { fieldName :: Text
           , fieldType :: FieldType
           }

data EntityReference = EntityReference
                     { originEntity       :: Text
                     , originFields       :: [Text]
                     , referencedEntity   :: Text
                     , referencedFields   :: [Text]
                     }

data EntityMetadata = IndexEntityMetadata [Text]
                    | PrimaryKeyEntityMetadata [Text]

data Entity = Entity
            { entityName      :: Text
            , entityFields    :: [Field]
            , entityMetadata  :: [EntityMetadata]
            }

type AllEntityInfo = ([Entity], [EntityReference])

-- Build database entities that reference the elements rather than containing them.
-- Build "normal" entities that contain the full structure?

mkTextName :: Text -> Name
mkTextName = mkName . unpack

mkEntityName :: Entity -> Name
mkEntityName = mkTextName . entityName

mkEntityType :: Entity -> TypeQ
mkEntityType = conT . mkEntityName

fieldTypeToType :: FieldType -> TypeQ
fieldTypeToType TextFieldType                         = conT $ mkName "Text"
fieldTypeToType IntFieldType                          = conT $ mkName "Int"
fieldTypeToType (MaybeFieldType innerFieldType)       = do
  case innerFieldType of
                        (MaybeFieldType _)  -> reportError "Nested Maybes makes no sense."
                        _                   -> return ()
  let inner     = fieldTypeToType innerFieldType
  let maybeName = conT $ mkName "Maybe"
  appT maybeName inner

fieldToStrictType :: Field -> VarStrictTypeQ
fieldToStrictType (Field name fieldType) = do
  fieldType <- fieldTypeToType fieldType
  return (mkTextName name, IsStrict, fieldType)

defaultDeriving :: [Name]
defaultDeriving = [mkName "Eq", mkName "Ord", mkName "Show"]

upperFirstLetter :: String -> String
upperFirstLetter []         = []
upperFirstLetter (x : xs)   = toUpper x : xs

lowerFirstLetter :: String -> String
lowerFirstLetter []         = []
lowerFirstLetter (x : xs)   = toLower x : xs

makeEntities :: AllEntityInfo -> Q [Dec]
makeEntities (entities, references) = (flip traverse) entities $ \entity -> do
  let declName  = mkTextName $ entityName entity
  let fields    = fmap fieldToStrictType (entityFields entity)
  dataD (return []) declName [] [recC declName fields] defaultDeriving

makeEntitiesLenses :: AllEntityInfo -> Q [Dec]
makeEntitiesLenses (entities, references) = fmap join $ (flip traverse) entities $ \entity -> do
  let nameOfEntity                = entityName entity
  let declName                    = mkTextName nameOfEntity
  let makeLensPair (Field name _) = (unpack name, (lowerFirstLetter $ unpack nameOfEntity) ++ (upperFirstLetter $ unpack name))
  let lensDetails                 = fmap makeLensPair $ entityFields entity
  makeLensesFor lensDetails declName

fieldFnExp :: ExpQ
fieldFnExp = varE $ mkName "field"

makeFromRow :: Text -> [Field] -> ExpQ
makeFromRow entityName []       = conE $ mkTextName entityName
makeFromRow entityName (_ : fs) =
  let apFn        = varE $ mkName "<*>"
      fmapFn      = varE $ mkName "<$>"
      initial     = infixE (Just $ conE $ mkTextName entityName) fmapFn (Just $ fieldFnExp)
  in  foldl' (\w -> \e -> infixE (Just w) apFn (Just fieldFnExp)) initial fs

makeEntitiesFromRow :: AllEntityInfo -> Q [Dec]
makeEntitiesFromRow (entities, references) = (flip traverse) entities $ \entity -> do
  let instanceType  = appT (conT $ mkName "FromRow") (mkEntityType entity)
  let fromRowClause = clause [] (normalB $ makeFromRow (entityName entity) (entityFields entity)) []
  let fromRowDecl   = funD (mkName "fromRow") [fromRowClause]
  instanceD (return []) instanceType [fromRowDecl]

makeToRow :: Text -> [Field] -> ClauseQ
makeToRow entityName fields =
  let fieldNames  = fmap (\n -> mkName ("field" ++ show n)) [1..(length fields)]
      pattern     = conP (mkTextName entityName) $ fmap varP fieldNames
      body        = normalB $ listE $ fmap (\f -> appE (varE $ mkName "toField") (varE f)) fieldNames
  in  clause [pattern] body []

makeEntitiesToRow :: AllEntityInfo -> Q [Dec]
makeEntitiesToRow (entities, references) = (flip traverse) entities $ \entity -> do
  let instanceType  = appT (conT $ mkName "ToRow") (mkEntityType entity)
  let toRowDecl     = funD (mkName "toRow") [makeToRow (entityName entity) (entityFields entity)]
  instanceD (return []) instanceType [toRowDecl]

makeEntitiesPostgresClasses :: AllEntityInfo -> Q [Dec]
makeEntitiesPostgresClasses allInfo = (++) <$> makeEntitiesToRow allInfo <*> makeEntitiesFromRow allInfo

queryReader :: (ToRow q, FromRow a, Functor m, MonadReader r m, MonadIO m, Wirable r Connection) => Query -> q -> m [a]
queryReader query queryParams = do
  connection <- wiredAsk
  liftIO $ query connection query queryParams

makePrimaryKeyedEntityMethods :: Text -> [Field] -> DecQ
makePrimaryKeyedEntityMethods entityName primaryKeyFields = do
  --let methodType = 
  -- Needs a version of wiredAsk that operates on the typeclass.
  let keyClauses  = concat $ intersperse ", " $ fmap (\f -> (unpack $ toLower $ fieldName f) ++ " = ?") primaryKeyFields
  let query       = litE $ stringL ("select * from " ++ (toLower entityName) ++ "s where " ++ keyClauses)
  let clause      = 
  funD (mkName )

makeEntityMethods :: AllEntityInfo -> Q [Dec]
makeEntityMethods (entities, _) = (flip traverse) entities $ \entity -> do
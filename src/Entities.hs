{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}

module Entities (
    FieldType(..)
  , Field(..)
  , EntityReference(..)
  , EntityMetadata(..)
  , Entity(..)
  , AllEntityInfo
  , FromRow(..)
  , ToRow(..)
  , ToField(..)
  , RowParser
  , MonadReader(..)
  , field
  , queryReader
  , makeEntities
  , makeEntitiesLenses
  , makeEntitiesFromRow
  , makeEntitiesToRow
  , makeEntitiesPostgresClasses
  , makeEntityMethods
) where

import Data.Text hiding (toUpper, toLower, intersperse, foldl', length, concat, filter)
import qualified Data.Text as T
import Data.List (intersperse, foldl')
import Language.Haskell.TH
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Wiring
import Control.Lens.TH
import Control.Lens
import Control.Lens.Traversal
import Data.Char
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField hiding (Field)
import Control.Monad.Reader.Class
import Control.Monad.Reader.Class.Wiring
import qualified Data.HashSet as S

data FieldType = TextFieldType
               | IntFieldType
               | MaybeFieldType { _maybeInnerType :: FieldType }

makeLenses ''FieldType

data Field = Field
           { _fieldName :: Text
           , _fieldType :: FieldType
           }

makeLenses ''Field

data EntityReference = EntityReference
                     { _originEntity       :: Text
                     , _originFields       :: [Text]
                     , _referencedEntity   :: Text
                     , _referencedFields   :: [Text]
                     }

makeLenses ''EntityReference

data EntityMetadata = IndexEntityMetadata       { _indexFieldNames      :: [Text] }
                    | PrimaryKeyEntityMetadata  { _primaryKeyFieldNames :: [Text] }

makeLenses ''EntityMetadata

data Entity = Entity
            { _entityName      :: Text
            , _entityFields    :: [Field]
            , _entityMetadata  :: [EntityMetadata]
            }

makeLenses ''Entity

type AllEntityInfo = ([Entity], [EntityReference])

-- Build database entities that reference the elements rather than containing them.
-- Build "normal" entities that contain the full structure?

mkTextName :: Text -> Name
mkTextName = mkName . unpack

mkEntityName :: Entity -> Name
mkEntityName = mkTextName . _entityName

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

fieldToType :: Field -> TypeQ
fieldToType (Field _ fieldType) = fieldTypeToType fieldType

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
  let declName  = mkTextName $ _entityName entity
  let fields    = fmap fieldToStrictType (_entityFields entity)
  dataD (return []) declName [] [recC declName fields] defaultDeriving

makeEntitiesLenses :: AllEntityInfo -> Q [Dec]
makeEntitiesLenses (entities, references) = fmap join $ (flip traverse) entities $ \entity -> do
  let nameOfEntity                = _entityName entity
  let declName                    = mkTextName nameOfEntity
  let makeLensPair (Field name _) = (unpack name, (lowerFirstLetter $ unpack nameOfEntity) ++ (upperFirstLetter $ unpack name))
  let lensDetails                 = fmap makeLensPair $ _entityFields entity
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
  let fromRowClause = clause [] (normalB $ makeFromRow (_entityName entity) (_entityFields entity)) []
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
  let toRowDecl     = funD (mkName "toRow") [makeToRow (_entityName entity) (_entityFields entity)]
  instanceD (return []) instanceType [toRowDecl]

makeEntitiesPostgresClasses :: AllEntityInfo -> Q [Dec]
makeEntitiesPostgresClasses allInfo = (++) <$> makeEntitiesToRow allInfo <*> makeEntitiesFromRow allInfo

queryReader :: (ToRow q, FromRow a, Functor m, MonadReader r m, MonadIO m, Wirable r Connection) => Query -> q -> m [a]
queryReader dbQuery queryParams = do
  connection <- wiredAsk
  liftIO $ query connection dbQuery queryParams

makePrimaryKeyedEntityMethods :: Entity -> Q [Dec]
makePrimaryKeyedEntityMethods entity = case (toListOf (entityMetadata . each . primaryKeyFieldNames) entity) of
  []            -> return []
  [_ : _ : _]   -> reportError "Too many primary keys." >> return []
  [fieldNames]  -> do
    let pkFieldNames = S.fromList fieldNames
    let remainingFieldNames = S.difference pkFieldNames (S.fromList $ toListOf (entityFields . each . fieldName) entity)
    if S.null remainingFieldNames then return () else reportError ("Fields listed in primary key not present in entity: " ++ show remainingFieldNames)
    let keyClauses    = concat $ intersperse ", " $ fmap (\f -> (unpack $ T.toLower f) ++ " = ?") fieldNames
    let query         = litE $ stringL ("select * from " ++ (unpack $ T.toLower $ _entityName entity) ++ "s where " ++ keyClauses)
    patternNames      <- traverse (\f -> newName $ unpack f) fieldNames
    let callParams    = [query, (tupE (fmap varE patternNames))]
    let lookupClause  = clause (fmap varP patternNames) (normalB $ foldl' appE (varE $ mkName "queryReader") callParams) []
    do
      let functionName = mkName ("get" ++ (unpack $ _entityName entity) ++ "ByPK")
      let idParameters = foldl' appT (tupleT $ S.size pkFieldNames) $ fmap fieldToType $ filter (\f -> S.member (_fieldName f) pkFieldNames) $ _entityFields entity
      let entityType   = mkEntityType entity
      signatureDef  <- sigD functionName $ [t|(ToRow $idParameters, FromRow $entityType, MonadIO m, MonadReader r m, Wirable r Connection) => $idParameters -> m [$entityType]|]
      functionDef   <- funD functionName [lookupClause]
      return [signatureDef, functionDef]

makeInsertEntityMethods :: Entity -> Q [Dec]
makeInsertEntityMethods entity = do
  let functionName        = mkName ("insert" ++ (unpack $ _entityName entity))
  let fields              = _entityFields entity
  let fieldParameterTypes = foldl' appT (tupleT $ length fields) $ fmap fieldToType $ fields
  let validFieldNames     = fmap (mkName . unpack . _fieldName) $ fields
  let columnNames         = join $ intersperse "," $ fmap (unpack . T.toLower . _fieldName) fields
  let columnWildcards     = join $ intersperse "," $ fmap (\_ -> "?") fields
  let fieldParameters     = tupP $ fmap varP validFieldNames
  let fieldsTuple         = tupE $ fmap varE validFieldNames
  let query               = litE $ stringL ("insert into " ++ (unpack $ T.toLower $ _entityName entity) ++ "(" ++ columnNames ++ ") VALUES (" ++ columnWildcards ++ ")")
  signatureDef  <- sigD functionName $ [t|(ToRow $fieldParameterTypes, MonadIO m, MonadReader r m, Wirable r Connection) => $fieldParameterTypes -> m ()|]
  let lookupClause        = clause [[p|$fieldParameters|]] (normalB $ [|wiredAsk >>= (\c -> execute c $query $fieldsTuple) >> return ()|]) []
  functionDef   <- funD functionName [lookupClause]
  return [signatureDef, functionDef]

makeEntityMethods :: AllEntityInfo -> Q [Dec]
makeEntityMethods (entities, _) = do
  primaryKeyMethods <- (flip traverse) entities $ \entity -> do
    makePrimaryKeyedEntityMethods entity
  return $ join primaryKeyMethods

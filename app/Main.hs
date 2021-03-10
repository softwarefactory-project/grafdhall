{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Dhall
import Grafana

tenants :: FilePath -> IO [Text]
tenants = inputFile auto

data InfluxTag = InfluxTag
  {key :: Text, operator :: Text, value :: Text}
  deriving stock (Eq, Show, Generic)

instance ToJSON InfluxTag

data InfluxGroupBy = InfluxGroupBy
  { _params :: [Text],
    _type :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON InfluxGroupBy where
  toJSON InfluxGroupBy {..} =
    object
      [ "params" .= _params,
        "type" .= _type
      ]

data InfluxQuery = InfluxQuery
  { groupBy :: [InfluxGroupBy],
    select :: [[InfluxGroupBy]],
    measurement :: Text,
    orderByTime :: Text,
    policy :: Text,
    resultFormat :: Text,
    refId :: Text,
    query :: Text,
    rawQuery :: Bool,
    tags :: [InfluxTag]
  }
  deriving stock (Eq, Show, Generic)

gaugeQuery :: Text -> InfluxQuery
gaugeQuery measurement = InfluxQuery {..}
  where
    groupBy = [InfluxGroupBy ["$__interval"] "time", InfluxGroupBy ["0"] "fill"]
    select = [[InfluxGroupBy ["value"] "field", InfluxGroupBy [] "first"]]
    orderByTime = "DESC"
    rawQuery = True
    query = "SELECT last(\"value\") FROM \"" <> measurement <> "\" WHERE $timeFilter GROUP BY time($__interval) fill(0) ORDER BY time DESC\n"
    refId = "A"
    tags = [InfluxTag "metric_type" "=" "gauge"]
    policy = "value"
    resultFormat = "time_series"

instance ToJSON InfluxQuery

addInfluxQuery :: InfluxQuery -> Value -> Value
addInfluxQuery iq = go
  where
    go :: Value -> Value
    go = \case
      Object ks
        | Just xs <- HM.lookup "panels" ks -> Object (HM.insert "panels" (go xs) ks)
        | Just _ <- HM.lookup "targets" ks -> Object (HM.insert "targets" iqs ks)
      Array xs -> Array (fmap go xs)
      x -> x
    iqs = Array $ V.fromList [toJSON iq]

tenantDashboard :: Text -> Dashboard
tenantDashboard name = defaultDashboard {dashboardTitle = name <> " dashboard", dashboardPanels = panels}
  where
    resourcesGraph =
      defaultGraph
        { graphTitle = "Resources",
          graphUnit = Just SecondsFormat,
          graphHasBars = True
        }
    panels =
      [ graphPanel resourcesGraph (GridPos 12 24 0 0)
      ]

main :: IO ()
main = do
  tenantsDashboards <- fmap tenantDashboard <$> tenants "conf.dhall"
  let iq = gaugeQuery "zuul.nodepool.resources.tenant.fedora.cores"
  let iqDashboards = fmap (addInfluxQuery iq . toJSON) (take 1 tenantsDashboards)
  mapM_ (T.putStrLn . T.decodeUtf8 . LBS.toStrict . encode) iqDashboards

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (Value (Array, Bool, Number, Object, String), encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Dhall (inputExpr)
import Dhall.JSON (Conversion (..), convertToHomogeneousMaps, dhallToJSON, omitNull)
import Network.HTTP.Client
  ( RequestBody (RequestBodyLBS),
    Response (responseStatus),
    applyBasicAuth,
    httpNoBody,
    method,
    parseRequest,
    requestBody,
    requestHeaders,
  )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (Status (statusCode))
import System.Environment (getArgs, getEnv, lookupEnv)

defaultConversion :: Conversion
defaultConversion =
  Conversion
    { mapKey = "mapKey",
      mapValue = "mapValue"
    }

postDashboard :: Value -> IO ()
postDashboard dashboard = do
  grafanaUrl <- getEnv "GRAFANA_URL"
  grafanaPass <- BS.pack <$> getEnv "GRAFANA_PASS"
  grafanaUser <- BS.pack . fromMaybe "admin" <$> lookupEnv "GRAFANA_USER"
  initReq <- parseRequest $ grafanaUrl <> "/api/dashboards/db"
  let req =
        applyBasicAuth grafanaUser grafanaPass $
          initReq
            { method = "POST",
              requestHeaders = [("Content-Type", "application/json")],
              requestBody = RequestBodyLBS (encode body)
            }
  manager <- newTlsManager
  resp <- httpNoBody req manager
  case statusCode $ responseStatus resp of
    200 -> putStrLn $ "Dashboard updated: " <> dashboardName
    _ -> error $ "Got " <> show resp
  where
    getString :: Value -> String
    getString = \case
      String txt -> T.unpack txt
      _ -> error "Not a string"
    body =
      Object
        [ ("dashboard", dashboard),
          ("folderId", Number 0),
          ("overwrite", Bool True)
        ]
    dashboardName = getString $ case dashboard of
      Object attrs -> fromMaybe (error "title missing") $ HM.lookup "title" attrs
      _ -> error "Dashboard is not an object"

postDashboards :: Value -> IO ()
postDashboards = \case
  Array xs -> mapM_ postDashboard xs
  x -> postDashboard x

load :: FilePath -> IO ()
load fp = do
  confExpr <- inputExpr (T.pack fp)
  case dhallToJSON (convertToHomogeneousMaps defaultConversion confExpr) of
    Right v -> postDashboards (omitNull v)
    Left err -> error ("Could not convert configuration to JSON: " <> show err)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> putStrLn "usage: grafdhall ./conf.dhall"
    [fp] -> load fp
    _ -> putStrLn "usage: grafdhall ./conf.dhall"

let Grafana = ~/src/github.com/weeezes/dhall-grafana/package.dhall

let panels =
      [ Grafana.Panels.mkTextPanel
          Grafana.TextPanel::{
          , title = "Generated with grafdhall"
          , gridPos = { x = 0, y = 0, w = 24, h = 1 }
          , content =
              ''
              # This dashboard is generated with grafdhall
              ''
          }
      , Grafana.Panels.mkGraphPanel
          Grafana.GraphPanel::{
          , title = "Resources"
          , gridPos = { x = 0, y = 1, w = 24, h = 8 }
          , targets =
            [ Grafana.MetricsTargets.InfluxTarget
                { groupBy =
                  [ { params = [ "\$__interval" ], type = "time" }
                  , { params = [ "0" ], type = "fill" }
                  ]
                , measurement = "zuul.nodepool.resources.tenant.fedora.cores"
                , orderByTime = "DESC"
                , policy = "value"
                , query =
                    ''
                    SELECT last("value") FROM "zuul.nodepool.resources.tenant.fedora.cores" WHERE $timeFilter GROUP BY time($__interval) fill(0) ORDER BY time DESC
                    ''
                , rawQuery = True
                , refId = "A"
                , resultFormat = "time_series"
                , select =
                  [ [ { params = [ "value" ], type = "field" }
                    , { params = [] : List Text, type = "first" }
                    ]
                  ]
                , tags =
                  [ { key = "metric_type", operator = "=", value = "gauge" } ]
                }
            ]
          }
      ]

in  Grafana.Dashboard::{
    , title = "Fedora tenant resources"
    , panels = Grafana.Utils.generateIds panels
    , uid = Some "fedora-tenant-resources"
    , editable = True
    }

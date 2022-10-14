let Grafana =
      https://raw.githubusercontent.com/weeezes/dhall-grafana/f78d2887939dcb555a47a4b85a91a3d6b38ec2ea/package.dhall
        sha256:a0e1b5432090944fa671efce0085c6049019ae0d00ca289c268b4528d1cd39af

let Prelude =
      https://prelude.dhall-lang.org/v17.0.0/package.dhall
        sha256:10db3c919c25e9046833df897a8ffe2701dc390fa0893d958c3430524be5a43e

let Tenant = { Type = { name : Text, id : Text } }

let tenants =
      [ { name = "Fedora", id = "fedora" }
      , { name = "RDO", id = "rdoproject_org" }
      , { name = "Packit", id = "packit-service" }
      , { name = "Local", id = "local" }
      ]

let gauge =
      \(measurement : Text) ->
        Grafana.MetricsTargets.InfluxTarget
          { groupBy =
            [ { type = "time", params = [ "\$__interval" ] }
            , { type = "fill", params = [ "previous" ] }
            ]
          , measurement
          , orderByTime = "ASC"
          , policy = "default"
          , refId = "A"
          , rawQuery = False
          , query = ""
          , resultFormat = "time_series"
          , select =
            [ [ { type = "field", params = [ "value" ] }
              , { type = "distinct", params = [] : List Text }
              ]
            ]
          , tags = [ { key = "metric_type", operator = "=", value = "gauge" } ]
          }

let resource-panel =
      \(title : Text) ->
      \(measurement : Text) ->
        Grafana.Panels.mkGraphPanel
          Grafana.GraphPanel::{
          , title
          , gridPos = { x = 0, y = 1, w = 24, h = 8 }
          , targets = [ gauge measurement ]
          }

let panels =
      \(tenant : Text) ->
        [ Grafana.Panels.mkTextPanel
            Grafana.TextPanel::{
            , title = "Generated with grafdhall"
            , gridPos = { x = 0, y = 0, w = 24, h = 1 }
            , content =
                ''
                # This dashboard is generated with grafdhall
                ''
            }
        , resource-panel
            "Instances"
            "zuul.nodepool.resources.tenant.${tenant}.instances"
        , resource-panel
            "Cores"
            "zuul.nodepool.resources.tenant.${tenant}.cores"
        , resource-panel "Memory" "zuul.nodepool.resources.tenant.${tenant}.ram"
        ]

let all-panel =
      \(measurement : Text) ->
        Grafana.Panels.mkGraphPanel
          Grafana.GraphPanel::{
          , title = "Resources usage: ${measurement}"
          , gridPos = { x = 0, y = 0, w = 24, h = 8 }
          , targets =
              Prelude.List.map
                Tenant.Type
                Grafana.MetricsTargets
                ( \(tenant : Tenant.Type) ->
                    gauge
                      "zuul.nodepool.resources.tenant.${tenant.id}.${measurement}"
                )
                tenants
          }

let tenant-dashboard =
      \(tenant : Tenant.Type) ->
        Grafana.Dashboard::{
        , title = "Tenant ${tenant.name} resources usage"
        , panels = Grafana.Utils.generateIds (panels tenant.id)
        , uid = Some "${tenant.name}-tenant-resources"
        , editable = True
        }

let tenant-dashboards =
      Prelude.List.map
        Tenant.Type
        Grafana.Dashboard.Type
        tenant-dashboard
        tenants

let all-tenant-dashboard =
      Grafana.Dashboard::{
      , title = "Tenants resource usage"
      , uid = Some "all-tenants-resources"
      , panels =
          Grafana.Utils.generateIds
            ( Prelude.List.map
                Text
                (Natural -> Grafana.Panels.Panels)
                all-panel
                [ "instances", "cores", "ram" ]
            )
      , editable = True
      }

in  [ all-tenant-dashboard ] # tenant-dashboards

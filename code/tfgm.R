Sys.getenv("TFGM") # tfgm api key
library(jsonlite)
library(sf)
library(httr)
library(tmap)
tmap_mode("view")
# http://api.bt-hypercat.com/sensors/feeds/794006a6-1a12-4fdf-badd-d4c704e7a9e5
feed_tfgm = "af6ad0cb-ac12-40d2-b140-b2aaee8233a5/features"
u_bt = "http://api.bt-hypercat.com/geo/feeds"
k = Sys.getenv("TFGM")
GET(url = u_bt, config = list(key = k))
msg = 'curl -H"Accept:application/json" -u4fe23d41-f932-4f92-8ade-2e64f58b1ed1: "http://api.bt-hypercat.com/geo/feeds/af6ad0cb-ac12-40d2-b140-b2aaee8233a5/features?limit=10000" > data/tfgm-cycle-infra.geojson'
system(msg)
cycle_infra = st_read("data/tfgm-cycle-infra.geojson")
qtm(cycle_infra)
# d = read_json("fbbdata/2018-1-15.json") # strange format
# st_sfc("POINT(-2.234636 53.470932)")

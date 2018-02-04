Sys.getenv("TFGM") # tfgm api key
# install.packages("wellknown")
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
d = read_json("fbbdata/2018-1-15.json") # strange format
wkt2latlon = function(d, f) {
  d2 = gsub("POINT\\(|\\)", "", d)
  d3 = gsub(" ", ",", d2)
  write(d3, f, append = T)
}
writeLines(d1, "data/")
wkt2latlon(d1, "data/d1.csv")
purrr::map(d, ~ wkt2latlon(.$wkt, "data/d.csv"))
seesense = readr::read_csv("data/d.csv", col_names = c("x", "y"))
plot(seesense)
library(leaflet.extras)
leaflet() %>% 
  addTiles() %>% 
  addHeatmap(lng = seesense$x, lat = seesense$y, intensity = 1, radius = 5)
st_read("data/d1.wkt")
# st_sfc("POINT(-2.234636 53.470932)")
# journey time data
msg = 'curl -H "Accept:application/json" -u4fe23d41-f932-4f92-8ade-2e64f58b1ed1: "http://api.bt-hypercat.com/geo/feeds/243af936-bb94-4031-a066-79ad0ab9e94f/features" > data/jtimes.json'
system(msg)
# http://api.bt-hypercat.com/tfgm/feeds/243af936-bb94-4031-a066-79ad0ab9e94f
Format: JSON | XML

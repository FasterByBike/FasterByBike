# set-up
library(tidyverse)
library(osmdata)
library(sf)
library(stplanr)
library(tmap)
tmap_mode("view")
# gmapsdistance(origin = "Chorlton, UK", destination = "Bright Building, Manchester", mode = "driving", key = Sys.getenv("GOOGLEDIST") )

manchester = getbb("Manchester", format_out = "sf_polygon")
# get data on transport:
u_l = "https://github.com/npct/pct-outputs-regional-notR/raw/master/commute/msoa/greater-manchester/l.geojson"
download.file(u_l, "l.geojson")
l_all = read_sf("l.geojson")
l_manchester = l_all[manchester, ]
names(l_all)
l = l_manchester %>% 
  filter(rf_dist_km > 1, rf_dist_km < 8) %>% 
  top_n(n = 50, wt = all)
qtm(l) +
  qtm(manchester) +
  qtm(l_route, lines.col = "blue")
od = line2df(l_top)
l_route = line2route(l = l_top, route_fun = route_graphhopper, vehicle = "car")
l$geometry_car = l_route$geometry
l$time = NA
i = 1
for(i in 1:nrow(od)) {
  t = dist_google(from = od[i, c("fx","fy")], to = od[i, c("tx", "ty")], mode = "driving")
  l$time[i] = t$duration
}
l$time = l$time / 60
summary(l$time / l$rf_time_min)

# test code...
# d = as.numeric(ymd_hm("2018-02-05 01:30"))
# 
# results = gmapsdistance(origin = "Washington+DC", destination = "seattle",
#                          mode = "walking", 
#                          departure = d,
#                          traffic_model = "pessimistic", 
#                          shape = "long",
#                          key=Sys.getenv("GOOGLEDIST"))
# 
# 
# "https://api.mapbox.com/directions/v5/mapbox/driving/13.4301,52.5109;13.4265,52.5080;13.4194,52.5072?radiuses=40;;100&geometries=polyline&access_token=46c9e633-3a28-4199-a4e7-0c6c2b355288"
# 
# r = route_graphhopper("Chorlton", "Manchester", silent = F, vehicle = "car")
# 
# mapview::mapview(r)
# r = route_osrm()
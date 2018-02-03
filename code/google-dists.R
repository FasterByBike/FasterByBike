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
u_rf = "https://github.com/npct/pct-outputs-regional-notR/raw/master/commute/msoa/greater-manchester/rf.geojson"
if(!file.exists("rf.geojson")) {
  download.file(u_rf, "rf.geojson")
}

rf_all = read_sf("rf.geojson")
l_all = read_sf("l.geojson")
l_all$geometry_cycle = rf_all$geometry
l_manchester = l_all[manchester, ]
names(l_all)

l = l_manchester %>% 
  filter(rf_dist_km > 1, rf_dist_km < 8) %>% 
  top_n(n = 50, wt = all)

od = line2df(l)
l_route = line2route(l = l, route_fun = route_graphhopper, vehicle = "car")
l$geometry_car = l_route$geometry
l$time_car = NA
i = 1
for(i in 1:nrow(od)) {
  t = dist_google(from = od[i, c("fx","fy")], to = od[i, c("tx", "ty")], mode = "driving")
  l$time_car[i] = t$duration / 60
}


summary(l$time_car / l$rf_time_min)
plot(l$rf_time_min, l$time, cex = l$all / 200, ylab = "Time by car (minutes)", xlab = "Time by bike (minutes)")
abline(c(0, 1))
qtm(manchester) +
  qtm(l, lines.col = "grey") +
  qtm(l$geometry_cycle, lines.col = "blue") +
  qtm(l$geometry_car, lines.col = "red")

routes = select(l, all, car_driver, bicycle, foot, bus, car_time = time, time_car = rf_time_min)

# dir.create("data")
write_sf(routes, "data/routes.geojson")
routes$geometry = l$geometry_cycle
write_sf(routes, "data/routes_cycle.geojson")
routes$geometry = l$geometry_car
write_sf(routes, "data/routes_car.geojson")

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
# test_time = ymd_hm("2018-02-02 08:30")
# D = as.numeric(test_time)
# dist_google(from = "Chorlton, UK", to = "Bright Building, Manchester", mode = "driving", arriva
# Route_Google
install.packages("stplanr")
library(stplanr)
library(sf)
library(jsonlite)
library(tmap)
library(dplyr)
tmap_mode("view")

maps_api <- "AIzaSyB5i09ip-4z0diX4le-9YcFmAFVmzZkKvc"

o <- matrix( c(53.476539, -2.247968, 53.482070, -2.252612), nrow = 2, byrow = T)
d <- matrix( c(53.480377, -2.230122, 53.480377, -2.230122), nrow = 2, byrow = T)

from = c(53.476539, -2.247968)
to = c(53.480377, -2.230122)

res <- dist_google(from = from, to = to, google_api = maps_api, mode = "driving")

dir_google <- function(from = NA, to = NA, api = NA, mode = "transit" ){
  url1 = "https://maps.googleapis.com/maps/api/directions/json?origin="
  url2 = "&destination="
  url3 = "&key="
  url4 = "&mode="
  #from = paste(from, collapse = ",")
  #to = paste(to, collapse = ",")
  request = paste0(url1,from,url2,to,url3,api,url4,mode)
  #print(request)
  result = fromJSON(request)
  result <- result$routes
  
  atrib <- result$legs[[1]]
  atrib <- atrib[,c("distance","duration","end_address","start_address")]
  atrib$distance <- atrib$distance$value
  atrib$duration <- atrib$duration$value
  
  result.line <-  decode_gl(result$overview_polyline)
  result.line$lat <- result.line$lat * 10
  result.line$lng <- result.line$lng * 10
  result.sf.line <- st_linestring(as.matrix(result.line[,c(2,1)]))
  
  result.geom <- st_geometry(result.sf.line)
  atrib$geometry <- result.geom
  
  #result.sf.line <- st_linestring(as.matrix(result.line[,c(2,1)])) 
  #df <- data.frame(id = 1, )
  atrib <- st_as_sf(atrib)
  st_crs(atrib) = 4326
  qtm(atrib)

  return(atrib)
}


lines <- readRDS("../FasterByBike - Copy/data/l.Rds")
lines <- st_as_sf(lines)
names(lines)
lines <- lines[,c("id","geo_code1","geo_code2","geo_name1",
                  "geo_name2","all","bicycle","foot",
                  "car_driver","car_passenger","train","light_rail",            
                  "bus","taxi","motorbike","other",
                  "e_dist_km","rf_dist_km","rq_dist_km","dist_rf_e",            
                  "dist_rq_rf","rf_avslope_perc","rq_avslope_perc","rf_time_min",        
                  "rq_time_min","geometry")]

lines$origin <- NA
lines$dest <- NA
for(i in 1:nrow(lines)){
  orign <- lines$geometry[i][[1]][1,]
  orign <- paste(orign[c(2,1)], collapse = ",")
  dest <- lines$geometry[i][[1]][2,]
  dest <- paste(dest[c(2,1)], collapse = ",")
  
  lines$origin[i] <- orign
  lines$dest[i] <- dest
}

class(lines$origin)

res_list <- list()
lines <- lines[order(lines$all, decreasing = T),]
for(j in 1:500){
  id = as.character(lines$id[j])
  from = lines$origin[j]
  to = lines$dest[j]
  route.tmp <- dir_google(from = from, to = to, api = maps_api, mode = "driving")
  route.tmp$id <- id
  res_list[[j]] <- route.tmp
  message(paste0("done ",j))
}

res_list2 <- bind_rows(res_list)
#rebuild the sf object
res_list2 <- as.data.frame(res_list2)
res_list2$geometry <- st_sfc(res_list2$geometry)
res_list2 <- st_sf(res_list2)
st_crs(res_list2) <- 4326

saveRDS(res_list2,"../FasterByBike/GoogleDrive.Rds")

res_transit <- list()
for(j in 1:500){
  id = as.character(lines$id[j])
  from = lines$origin[j]
  to = lines$dest[j]
  route.tmp <- dir_google(from = from, to = to, api = maps_api, mode = "transit")
  route.tmp$id <- id
  res_transit[[j]] <- route.tmp
  message(paste0("done ",j))
}

res_transit <- bind_rows(res_transit)
#rebuild the sf object
res_transit <- as.data.frame(res_transit)
res_transit$geometry <- st_sfc(res_transit$geometry)
res_transit <- st_sf(res_transit)
st_crs(res_transit) <- 4326


lines.sub <- as.data.frame(lines[1:500,])
lines.sub <- lines.sub[,c("id","all","bicycle","car_driver","rf_time_min","geometry")]
names(lines.sub) <- c("id","all","bicycle","car_driver","biketime","geometry")

time.car <- as.data.frame(res_list2)
time.car <- time.car[,c("distance","duration")]
names(time.car) <- c("cardist","cartime")

time.transit <- as.data.frame(res_transit)
time.transit <- time.transit[,c("distance","duration")]
names(time.transit) <- c("transitdist","transittime")

lines.sub <- cbind.data.frame(lines.sub,time.car)
lines.sub <- cbind.data.frame(lines.sub,time.transit)
lines.sub <- st_as_sf(lines.sub)

lines.sub$cartimepark <- lines.sub$cartime + (5 * 60)

lines.sub$fastest <- NA
for(i in 1:nrow(lines.sub)){
  bike <- lines.sub$biketime[i]
  car <- lines.sub$cartimepark[i]
  transit <- lines.sub$transittime[i]
  
  if(bike <= car & bike <= transit){
    lines.sub$fastest[i] <- "bike"
  }else if(transit <= car & transit <= bike){
    lines.sub$fastest[i] <- "transit"
  }else{
    lines.sub$fastest[i] <- "car"
  }
  
}

lines.sub$transitCarratio <- lines.sub$transittime / lines.sub$cartime



tmap_style("col_blind")
qtm(lines.sub, lines.col = "fastest", lines.lwd = 3)

st_write(lines.sub,"../FasterByBike/lines.geojson")
st_write(res_transit,"../FasterByBike/transit.geojson")
st_write(res_list2,"../FasterByBike/driving.geojson")

rf <- readRDS("../FasterByBike/data/rf.Rds")
rf <- st_as_sf(rf)
st_crs(rf)
rf <- rf[rf$id %in% lines.sub$id,]
st_write(rf,"../FasterByBike/cycle.geojson")

lines.sub$timesaved <- ifelse(lines.sub$fastest == "bike", (lines.sub$all * (lines.sub$cartimepark - lines.sub$biketime) * 2) / (60 * 60) ,0)
summary(lines.sub$timesaved)

sum(lines.sub$timesaved) / 24 /365 # total time saved if everybody cycled in years per year
sum(lines.sub$all[lines.sub$fastest == "bike"])

zones <- readRDS("../FasterByBike/data/z.Rds")

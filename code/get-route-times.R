library(stplanr)
library(tidyverse)
l = sf::st_read("data/routes.geojson")
od = line2df(l)
timestamp = Sys.time()
new_times = od %>% 
  mutate(travel_time = as.numeric(NA), timestamp = timestamp) %>% 
  select(id = L1, travel_time, timestamp)
  
# readr::write_csv(new_times, "data/manchester_route_times.csv")
old_times = readr::read_csv("data/manchester_route_times.csv") %>% 
  mutate(travel_time = as.numeric(travel_time))

i = 1
for(i in 1:nrow(od)) {
	  res = dist_google(from = od[i, c("fx","fy")], to = od[i, c("tx", "ty")], mode = "driving")
  new_times$travel_time[i] = res$duration / 60
}

new_times = bind_rows(old_times, new_times)
readr::write_csv(new_times, "data/manchester_route_times.csv")

# run this from the command-line
# R CMD BATCH "code/get-route-times.R"

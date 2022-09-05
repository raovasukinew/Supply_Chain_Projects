library(clipr)
library(geosphere)

base <- read.csv("fac_loc_temp_sample.csv")

customer_locations <- base[,c('id','lat','lng')]
names(customer_locations) <- c('id','x','y')

warehouse_locations <- base[,c('id','lat','lng')]
names(warehouse_locations) <- c('id','x','y')

n <- nrow(customer_locations)
m <- nrow(warehouse_locations)

dem<- as.numeric(base[,'adl'])

capacity <- rep.int(200,m)

transportcost <- function(i, j) {
  customer <- customer_locations[i, ]
  warehouse <- warehouse_locations[j, ]
  (distHaversine(cbind(customer$y,customer$x),cbind(warehouse$y,warehouse$x))*1.35)/1000
}

library(ompr)
library(magrittr)

model <- MIPModel() %>%
  # 1 iff i gets assigned to warehouse j
  add_variable(x[i, j], i = 1:n, j = 1:m, type = "binary") %>%
  
  # 1 iff warehouse j is built
  add_variable(y[j], j = 1:m, type = "binary") %>%
  
  # maximize the preferences
  set_objective(sum_expr(transportcost(i, j) * x[i, j], i = 1:n, j = 1:m), "min") %>%
  
  # every customer needs to be assigned to a warehouse
  add_constraint(sum_expr(x[i, j], j = 1:m) == 1, i = 1:n) %>% 
  
  # if a customer is assigned to a warehouse, then this warehouse must be built
  add_constraint(x[i,j] <= y[j], i = 1:n, j = 1:m)%>%
  
  # number of hubs limitation
  add_constraint(sum_expr(y[j], j = 1:m) <=3)
  # capacity constraint per hub
  #add_constraint(sum_expr(x[i,j]*dem[i],i=1:n)<=capacity[j]*y[j],j=1:m)

model

#library(ROI.plugin.symphony)
#result <- solve_model(model, with_ROI(solver = "symphony",
#                                      verbosity=-1, gap_limit=1.5))

library(ompr.roi)
library(ROI.plugin.glpk)
result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

result$objective_value


suppressPackageStartupMessages(library(dplyr))
matching <- result %>% 
  get_solution(x[i,j]) %>%
  filter(value > .9) %>%  
  select(i, j)

plot_assignment <- matching %>% 
  inner_join(customer_locations, by = c("i" = "id")) %>% 
  inner_join(warehouse_locations, by = c("j" = "id"))%>%
  inner_join(base,by=c('i'='id'))%>%
  mutate(dist = ((distHaversine(cbind(x.x,y.x),cbind(x.y,y.y)))*1.35)/1000)%>%
  mutate(wtage = round(adl*dist,2))

customer_count <- matching %>% group_by(j) %>% summarise(n = n()) %>% rename(id = j)

plot_assignment%>%
  group_by(j)%>%
  summarise(sum(adl),sum(wtage)/sum(adl))



# library(leaflet)
# 
# map <- leaflet()
# 
# map <- leaflet() %>%
#   addTiles()%>%
#   fitBounds(lat1=min(customer_locations$x),
#                lng1 = min(customer_locations$y),
#                lat2 = max(customer_locations$x),
#                lng2 = max(customer_locations$y))%>%
#   addCircleMarkers(data = plot_assignment,
#              lat = plot_assignment$x.x,
#              lng = plot_assignment$y.x,
#              color = 'navy blue',
#              fillOpacity = 0.8,
#              radius = (plot_assignment$adl)/20,
#              popup = paste(plot_assignment$i,plot_assignment$adl,sep = "<br>"))
# 
# map


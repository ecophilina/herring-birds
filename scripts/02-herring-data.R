#### DFO DATA ####
# view dfo herring data and extract data for preliminary analyses
# caution that raw data should be acquired at some point
# install.packages("shiny")
shiny::runGitHub( repo="FIND", username="grinnellm" )

# load packages
library(tidyverse)
# library(readr)

# load bird survey coordinates
coords1 <- readxl::read_excel(
  "raw/Survey_Stations.xlsx", 
  sheet = "Survey_Stations"
) 

coords1[coords1$Name == "Comox", ]$Name <- "Goose Spit"

coords2 <- readxl::read_excel(
  "raw/SurveyStationLatLong.xls", 
  sheet = "SurveyStationLatLong"
) %>% rename(Lon = easting, Lat = northing) %>% select(-OID_)

coords <- left_join(coords1, coords2)
plot(northing~easting, data = coords)
plot(Lat~Lon, data = coords)

# load dfo data
dfodatNN <- read_csv("raw/SpawnData-NN.csv")
dfodatN <- read_csv("raw/SpawnData-142.csv")
dfodatS <- read_csv("raw/SpawnData-143-172.csv") %>% filter(Latitude > 49.13124)

dfodat <- rbind(dfodatN, dfodatS)  
dfodat <- rbind(dfodat, dfodatNN)  


dfodat <- dfodat %>% filter(Year %in% c(1998, 1999, 2015, 2016)) %>% 
  rename(Lat = Latitude, Lon = Longitude)
# plot(Lat~Lon, data = dfodat)


dfodat <- dfodat %>%
  dplyr::mutate(X = Lon, Y = Lat) 

# creates coast lines for area defined in lat lon
coast <- gfplot:::load_coastline(
  range(dfodat$X) + c(-1, 1),
  range(dfodat$Y) + c(-1, 1),
  utm_zone = 9
)

# coast <- gfplot:::utm2ll(coast, utm_zone = 9)
dfodat <- gfplot:::ll2utm(dfodat, utm_zone = 9)

coords <- coords %>%
  dplyr::mutate(X = Lon, Y = Lat) 
coords <- gfplot:::ll2utm(coords, utm_zone = 9)

coords4 <- coords3 <- coords2 <- coords1 <- coords
coords1$Year <- 1998
coords2$Year <- 1999
coords3$Year <- 2015
coords4$Year <- 2016
coords <- rbind(coords1, coords2, coords3, coords4)



dfodat %>% filter( Method == "Dive") %>% 
  ggplot(aes(X, Y)) +
  geom_polygon(
    data = coast, aes_string(x = "X", y = "Y", group = "PID"),
    fill = "grey87", col = "grey70", lwd = 0.2
  ) + 
  geom_point(data = coords, aes(X,Y), colour = "red", shape = 21, size = 3, inherit.aes = F) +
  coord_cartesian(xlim = c(760, 885), ylim = c(5450, 5565), expand = F) +
  # coord_cartesian(xlim = c(-125.3, -123.8), ylim = c(49.1, 50.1), expand = F) + 
  geom_point() + facet_wrap(~Year)


#######################

dfodat$X2 <- 2 * round(dfodat$X/2)
dfodat$Y2 <- 2 * round(dfodat$Y/2)

coords$X2 <- 2 * round(coords$X/2)
coords$Y2 <- 2 * round(coords$Y/2)



dfodat %>% filter( Method == "Dive") %>% 
  ggplot(aes(X2, Y2)) +
  # geom_polygon(
  #   data = coast, aes(X, Y, group = "PID"),
  #   fill = "grey87", col = "grey70", lwd = 0.2, inherit.aes = F
  # ) +
  geom_point(aes(X2, Y2), data = coords, colour = "red", shape = 21, size = 3, inherit.aes = F) +
  # coord_cartesian(xlim = c(760, 885), ylim = c(5450, 5565), expand = F) +
  # coord_cartesian(xlim = c(-125.3, -123.8), ylim = c(49.1, 50.1), expand = F) + 
  geom_point() + facet_wrap(~Year)


dfo <- left_join(coords, dfodat, by= c("Year", "X2", "Y2")) %>% filter(Method == "Dive")

dfo %>% 
  ggplot(aes(X.y, Y.y)) +
  geom_polygon(
    data = coast, aes_string(x = "X", y = "Y", group = "PID"),
    fill = "grey87", col = "grey70", lwd = 0.2
  ) +
  geom_point(aes(X.x, Y.x), colour = "red", shape = 21, size = 3) +
  coord_cartesian(xlim = c(760, 885), ylim = c(5450, 5565), expand = F) +
  # coord_cartesian(xlim = c(-125.3, -123.8), ylim = c(49.1, 50.1), expand = F) + 
  geom_point() + facet_wrap(~Year)


all_dat <- readRDS("data/bird-survey-data.rds")
sort(unique(all_dat$site))
sort(unique(dfo$Name))

dfo[dfo$Name == "Oyster Bay", ]$Name <- "Oyster River"
dfo[dfo$Name == "Parksville", ]$Name <- "Parksville Bay"
dfo[dfo$Name == "Pipers Bay", ]$Name <- "Piper's Bay"

dfo <- dfo  %>% 
  filter(LocationName != "Whalebone Pt") %>% 
  filter(LocationName != "Qualicum Bay") %>% 
  unique()

dfo2 <- left_join(dfo, all_dat, by = c("Name" = "site", "Year" = "year"))

dfo2 <- dfo2 %>% select(
  Name, LocationName, Year, X2, Y2, Lon.x, Lat.x, Lon.y, Lat.y, SpawnNumber, SpawnIndex,
  Fishing, Milt, Eggs, Spawn_near, Active_spawn,
  Date, Start, End, matchID, SPCount
)
#TODO: figure out how ot add group back


# transform data in prep for adding zeros
dfo3 <-dfo2 %>% group_by(Name, Year, Date, matchID) %>%
  mutate(row = row_number()) %>% 
  pivot_wider(names_from = matchID, 
    values_from = SPCount, values_fill = list(SPCount=0)) 

# return to long form with zeros added
dfo2 <- dfo3 %>% group_by(Name, Year, Date) %>%
  pivot_longer(21:51, names_to = "matchID", values_to = "SPCount")

# exploratory plots
# which sites surveyed in which years
dfo2 %>% ggplot(aes(as.factor(Year), (SpawnIndex))) + 
  geom_point() + facet_wrap(~Name)

dfo2 %>% ggplot(aes((SpawnIndex), (SPCount+1))) + 
  geom_point() + scale_x_log10() + scale_y_log10() + 
  facet_wrap(~matchID, scales = "free_y")


### WRANGLE DATES
### work out dates of spawn relative to survey dates...
str(dfo2$Start)
dfo2 <- mutate(dfo2, start_str = paste(Year, Start), end_str = paste(Year, End))

dfo2$start_date <- strptime(dfo2$start_str, format="%Y %b %d")
dfo2$end_date <- strptime(dfo2$end_str, format="%Y %b %d")
str(dfo2$start_date)

dfo2$start_doy <- as.numeric(strftime(dfo2$start_date, format = "%j"))
dfo2$end_doy <- as.numeric(strftime(dfo2$end_date, format = "%j"))
str(dfo2$start_doy)

str(dfo2$Date)
dfo2$sample_doy <- as.numeric(strftime(dfo2$Date, format = "%j"))
str(dfo2$sample_doy)

dfo4 <- dfo2 %>% mutate( 
  start_date = as.character(start_date), 
  end_date = as.character(end_date),
  start_diff = sample_doy - start_doy,
  end_diff = sample_doy - end_doy,
  spawn_diff = if_else(start_diff > 0, if_else(end_diff > 0, end_diff, 0), start_diff)
)

saveRDS(dfo4, file = "data/bird-dfo-combined.rds")

# check if these numbers make sense
# time_check <- select(dfo4, 
#   Name, SpawnNumber, Year, Start, End, Date, 
#   sample_doy, start_doy, end_doy, start_diff, end_diff, spawn_diff
#   ) %>% unique()

hist(dfo4$SpawnIndex2)
dfo4$SpawnIndex2 <- gfranges::collapse_outliers(dfo4$SpawnIndex, c(0.05, 0.95))


dfo4 %>% #as_tibble() %>% 
  # filter(spawn_diff == 0) %>%
  filter(spawn_diff > -20 & spawn_diff < -3) %>%
  ggplot(aes((SpawnIndex2), (SPCount + 1)#, colour = (spawn_diff)
  )) + 
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm") +
  scale_x_sqrt() +
  scale_y_log10() + 
  scale_colour_viridis_c(option = "C",
    trans = gfranges::fourth_root_power, direction = 1) + 
  # facet_wrap(~group, scales = "free_y")
  facet_wrap(~matchID, scales = "free_y")

dfo4 %>% #as_tibble() %>% 
  filter(spawn_diff == 0) %>%
  # filter(spawn_diff >=0 & spawn_diff <= 2) %>%
  ggplot(aes((SpawnIndex2), (SPCount + 1)#, colour = (spawn_diff)
    )) + 
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm") +
  scale_x_sqrt() +
  scale_y_log10() + 
  scale_colour_viridis_c(option = "C",
    trans = gfranges::fourth_root_power, direction = 1) + 
  # facet_wrap(~group, scales = "free_y")
  facet_wrap(~matchID, scales = "free_y")

dfo4 %>% #as_tibble() %>% 
  # filter(spawn_diff == 0) %>%
  filter(spawn_diff > 0 & spawn_diff < 7) %>%
  ggplot(aes((SpawnIndex2), (SPCount + 1)#, colour = (spawn_diff)
  )) + 
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm") +
  scale_x_sqrt() +
  scale_y_log10() + 
  scale_colour_viridis_c(option = "C",
    trans = gfranges::fourth_root_power, direction = 1) + 
  # facet_wrap(~group, scales = "free_y")
  facet_wrap(~matchID, scales = "free_y")

dfo4 %>% #as_tibble() %>% 
  # filter(spawn_diff == 0) %>%
  filter(spawn_diff > 7 & spawn_diff < 21) %>%
  ggplot(aes((SpawnIndex2), (SPCount + 1)#, colour = (spawn_diff)
  )) + 
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm") +
  scale_x_sqrt() +
  scale_y_log10() + 
  scale_colour_viridis_c(option = "C",
    trans = gfranges::fourth_root_power, 
    direction = 1) + 
  # facet_wrap(~group, scales = "free_y")
  facet_wrap(~matchID, scales = "free_y")


# dfo4 %>% 
#   ggplot(aes(spawn_diff, (SPCount+1), 
#     colour = log(SpawnIndex))) + 
#   geom_point(alpha = 0.25) + 
#   # geom_smooth(method = "lm") +
#   scale_x_log10() + scale_y_log10() + 
#   facet_wrap(~group, scales = "free_y") + 
#   scale_colour_viridis_c(option = "C")


# coords %>%
#   ggplot(aes(X, Y)) +
#   geom_polygon(
#     data = coast, aes_string(x = "X", y = "Y", group = "PID"),
#     fill = "grey87", col = "grey70", lwd = 0.2
#   ) + 
#   coord_cartesian(xlim = c(750, 900), ylim = c(5440, 5570), expand = F) +
#   # coord_cartesian(xlim = c(-125.3,-123.8), ylim = c(49.1, 50.1), expand = F) + 
#   geom_point()

# View(spawn)
# norman <- filter(spawn, LocationCode == 1713)



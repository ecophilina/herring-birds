#### GET HERRING BIRD SURVEY DATA ###
## 
## combines data from survey in late 1990s 
## with repeat survey in 2010s
## 
## Notes ##
#
# Wind speed: recorded using Beaufort scale
# Sea state: Beaufort scale
# 
## Tide ##
# H = High Slack
# ME = Mid-ebb
# MF = Mid-flood
# L = Low
# 
## Herring activity ##
# E = eggs
# M = milt
# Y = yes (evidence of spawn occuring)
# N = no spawn
#
## load libraries ##

library(dplyr)
library(lubridate)
library(ggplot2)
###

new_surv <- readxl::read_excel(
  "raw/Coastal_Waterbird_Herring_Counts_2015&2016_5Feb2017.xlsx", 
  sheet = "Survey_Observations"
  )

old_surv <- readxl::read_excel(
  "raw/Database_TS98-99_JCworkingfile_9Feb2017.xlsx", 
  sheet = "Pelagic Observes"
  )

old_meta <- readxl::read_excel(
  "raw/Database_TS98-99_JCworkingfile_9Feb2017.xlsx", 
  sheet = "Pelagic Obsarea"
  )

coords <- readxl::read_excel(
  "raw/Survey_Stations.xlsx", 
  sheet = "Survey_Stations"
)

old_surv_all <- left_join(old_surv, old_meta, by = c("ObsareaID" = "ObsareaID"))
sort(unique(old_surv_all$Location))
sort(unique(coords$Name))

old_surv_all[old_surv_all$Location == "1565", ]$Location <- "1565 East Bay Rd"

# old_surv_all$Location <- trimws(old_surv_all$Location)
# coords$Name <- trimws(coords$Name)


old_surv_all <- left_join(old_surv_all, coords, by = c("Location" = "Name"))

glimpse(old_surv_all)
View(old_surv_all)

old_surv_all$Survey <- old_surv_all$`Survey#`

# adjust old names to match new ones
# old_surv_all[old_surv_all$Location == "1565", ]$Location <- "1565 East Bay Rd"
old_surv_all[old_surv_all$Location == "Campbell R", ]$Location <- "Campbell River 50th Parallel"
old_surv_all[old_surv_all$Location == "Nanaimo", ]$Location <- "Nanaimo Harbour"
old_surv_all[old_surv_all$Location == "Nanoose Bay", ]$Location <- "Nanoose"
old_surv_all[old_surv_all$Location == "Oar Rd", ]$Location <- "Oar Road"
old_surv_all[old_surv_all$Location == "Oyster Bay", ]$Location <- "Oyster River"
old_surv_all[old_surv_all$Location == "Parksville", ]$Location <- "Parksville Bay"
old_surv_all[old_surv_all$Location == "Pipers Bay", ]$Location <- "Piper's Bay"
old_surv_all[old_surv_all$Location == "Buckley Bay", ]$Location <- "Buckley Bay FT"
old_surv_all[old_surv_all$Location == "Willow Pt", ]$Location <- "Willow Point"
old_surv_all[old_surv_all$Location == "Comox", ]$Location <- "Goose Spit"

# rename variables and select relevant variables
old_dat <- old_surv_all %>% 
  mutate(period = 1, Date = paste(Year, Month, Day, sep = "-")) %>% 
  rename(year = Year, month = Month, day = Day, 
    site = Location, survey = Survey) %>%
  select(period, year, month, day, 
    SpeciesID, SPCount, 
    site, easting, northing, #Mid_lat, Mid_lon, 
    survey,
    Date,
    LengthM, SWidthM, AreaM2)

old_dat$Date <- ymd(old_dat$Date)

best_coords <- old_dat %>% select(site, easting, northing)


# rename variables and select relevant variables
new_dat <- new_surv %>% mutate(period = 2,
  year = year(ymd(Date)), month = month(ymd(Date)), day = day(ymd(Date)), 
  hour = hour(ymd_hms(`Start time`))
  ) %>% rename(site = Site, survey = Survey) %>% select(
    period, year, month, day, hour,
    SpeciesID, SPCount, 
    site, survey,
    Date, Behaviour, Comments, SPAWN_CATEGORY) 

new_dat$Date <- ymd(new_dat$Date)

new_dat[new_dat$SPCount == "500+", ]$Comments <- "500+"
new_dat[new_dat$SPCount == "500+", ]$SPCount <- 500
new_dat$SPCount <- as.numeric(new_dat$SPCount)

new_dat <- left_join(new_dat, best_coords)

# View(old_dat)
# View(new_dat)
# 
# sort(unique(old_dat$site))
# sort(unique(new_dat$site))

dat <- bind_rows(old_dat, new_dat)

View(dat)

### add in herring observations from 
new_herring <- readxl::read_excel(
  "raw/Coastal_Waterbird_Herring_Counts_2015&2016_5Feb2017.xlsx",
  sheet = "Survey_Summary"
)
# unique(new_herring$Surveyor)
new_herring <- new_herring %>% select(Location, Event, Year, Month, Day, StartHR, StartMIN, ENDHR, ENDMIN, `General visibility`, Vis_comment, Precip, WindSpeed, Tide, Disturbance, Boats, `Herring comments`, Comments )


# View(new_herring)
# glimpse(new_herring)

new_herring[new_herring$Location == "Campbell River (50th Parallel)", ]$Location <- "Campbell River 50th Parallel"
new_herring[new_herring$Location == "Comox", ]$Location <- "Goose Spit"
new_herring[new_herring$Location == "Nanaimo Harbor", ]$Location <- "Nanaimo Harbour"
new_herring[new_herring$Location == "Oyster Bay", ]$Location <- "Oyster River"
new_herring[new_herring$Location == "Parksville", ]$Location <- "Parksville Bay"
new_herring[new_herring$Location == "Piper's Lagoon", ]$Location <- "Piper's Bay"
new_herring[new_herring$Location == "Snaw-naw-as Campground", ]$Location <- "Snaw-naw-as"

sort(unique(new_herring$Location))
sort(unique(dat$site))


new_herring <- new_herring %>% rename(
    year = Year, month = Month, day = Day, 
    site = Location
  )
new_herring$Date <- ymd(new_herring$Date)

# Need to read methods doc... if it doesn't provide, ask...
# 
# why so little duration data? Fixed length except when noted?
# can we convert herring comments into categorical variable of somekind?

all_dat <- left_join(dat, new_herring, by = c("year", "month", "day", "site"))
 
saveRDS(all_dat, "data/bird-survey-data.rds")

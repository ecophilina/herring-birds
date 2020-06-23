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
# View(old_surv_all)

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
  rename(
    year = Year, month = Month, day = Day,
    site = Location, survey = Survey
  ) %>%
  select(
    SpeciesID, SPCount,
    period, survey, year, month, day,
    Date,
    site, easting, northing, # Mid_lat, Mid_lon,
    AreaM2, LengthM, SWidthM
  )

old_dat$Date <- ymd(old_dat$Date)

saveRDS(old_dat, "data/bird-survey-data-90s.rds")

best_coords <- old_dat %>%
  select(site, easting, northing, LengthM, SWidthM, AreaM2) %>%
  unique()


# rename variables and select relevant variables
new_dat <- new_surv %>%
  mutate(
    period = 2,
    year = year(ymd(Date)), month = month(ymd(Date)), day = day(ymd(Date)),
    hour = hour(ymd_hms(`Start time`))
  ) %>%
  rename(site = Site, survey = Survey, spawn_stage = SPAWN_CATEGORY)

new_dat$Date <- ymd(new_dat$Date)

new_dat[new_dat$SPCount == "500+", ]$Comments <- "500+"
new_dat[new_dat$SPCount == "500+", ]$SPCount <- 500
new_dat$SPCount <- as.numeric(new_dat$SPCount)

new_dat_coords <- left_join(new_dat, best_coords) %>%
  unique() %>%
  select(
    SpeciesID, SPCount,
    period, survey, year, month, day, hour, Date,
    site, easting, northing, AreaM2, LengthM, SWidthM,
    spawn_stage, Behaviour, Comments
  )

# View(old_dat)
# View(new_dat)
#
# sort(unique(old_dat$site))
# sort(unique(new_dat$site))

old_dat <- readRDS("data/bird-survey-data-90s.rds")

dat <- bind_rows(old_dat, new_dat_coords)

View(dat)

### add in herring observations from
new_meta <- readxl::read_excel(
  "raw/Coastal_Waterbird_Herring_Counts_2015&2016_1Jun2020.xlsx",
  sheet = "Survey_Summary"
)
# unique(new_meta$Surveyor)
new_meta <- new_meta %>% select(
  Location, Event, Year, Month, Day, StartHR, StartMIN, ENDHR, ENDMIN,
  `General visibility`, Vis_comment, Precip, WindSpeed, Tide,
  Disturbance, Boats,
  Herring, Fishing, Milt, Eggs, Spawn_near, Active_spawn, Other_baitballs,
  `Herring comments`, Comments
)


# View(new_meta)
# glimpse(new_meta)

new_meta[new_meta$Location == "Campbell River (50th Parallel)", ]$Location <- "Campbell River 50th Parallel"
new_meta[new_meta$Location == "Comox", ]$Location <- "Goose Spit"
new_meta[new_meta$Location == "Nanaimo Harbor", ]$Location <- "Nanaimo Harbour"
new_meta[new_meta$Location == "Oyster Bay", ]$Location <- "Oyster River"
new_meta[new_meta$Location == "Parksville", ]$Location <- "Parksville Bay"
new_meta[new_meta$Location == "Piper's Lagoon", ]$Location <- "Piper's Bay"
new_meta[new_meta$Location == "Snaw-naw-as Campground", ]$Location <- "Snaw-naw-as"

sort(unique(new_meta$Location))
sort(unique(dat$site))


new_meta2 <- new_meta %>% rename(
  year = Year, month = Month, day = Day,
  site = Location, bird_notes = Comments
)

# Need to read methods doc... if it doesn't provide, ask...
#
# why so little duration data? Fixed length except when noted?
# can we convert herring comments into categorical variable of somekind?

all_dat <- left_join(dat, new_meta2, by = c("year", "month", "day", "site")) # %>% unique()

# View(all_dat)

### make list of species with total records for each period
# species_list <- all_dat %>%
#   group_by(SpeciesID, period) %>%
#   mutate(period_count = sum (SPCount)) %>%
#   select(SpeciesID, period, period_count) %>% unique() %>%
#   pivot_wider(names_prefix = "period", names_from = period, values_from = period_count)
#
# write.csv(species_list, "data/new-species-list.csv")

### after groupings added manually, reimport
spp_list <- read_csv("species-list.csv")
all_dat <- left_join(all_dat, spp_list)

saveRDS(all_dat, "data/bird-survey-data.rds")

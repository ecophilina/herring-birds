#### BEGIN PLOTTING HERRING SURVEY DATA ###
## 

all_dat <- readRDS("data/bird-survey-data.rds")

### calculate density per km2
all_dat <- all_dat %>% mutate(SPDensity = signif(SPCount/(AreaM2/1000000), 2))
all_dat$SPDensity

### remove unmatched site
all_dat <- all_dat %>% filter(site != "Snaw-naw-as") %>% filter(site != "Eby Rd")


### total birds recorded by site and year
total_count <- all_dat %>% group_by(year, site) %>% 
  mutate(total_birds = sum(SPCount)) %>% 
  select(year, site, total_birds) %>% unique()

### max counts at any one site in each year
max_counts_site <- all_dat %>% group_by(year, SpeciesID) %>%
  mutate(max_SPCount = max(SPCount)) %>%
  select(year, SpeciesID, max_SPCount) %>% unique()

filter(max_counts_site, max_SPCount > 50) %>%
  ggplot(aes(year, max_SPCount)) + geom_line() + 
  facet_wrap(~as.factor(SpeciesID), scales = "free_y")

### max counts on any given survey in each year
max_counts_surv <- all_dat %>% group_by(year, survey, SpeciesID) %>%
  mutate(surv_sums = sum(SPCount)) %>% ungroup() %>%
  group_by(year, SpeciesID) %>%
  mutate(max_SPCount = max(surv_sums)) %>%
  select(year, SpeciesID, max_SPCount, spawn_stage) %>% unique()

filter(max_counts_surv, max_SPCount > 50) %>%
  ggplot(aes(year, max_SPCount)) + geom_point() + 
  geom_line() + 
  facet_wrap(~as.factor(SpeciesID), scales = "free_y")



### maybe these should be densities?
# ### max counts at any one site in each year
# max_dens_site <- all_dat %>% group_by(year, SpeciesID) %>% 
#   mutate(max_SPDensity = max(SPDensity)) %>% 
#   select(year, SpeciesID, max_SPDensity) %>% unique()
# 
# filter(max_dens_site, max_SPDensity > 100) %>% 
#   ggplot(aes(year, max_SPDensity)) + geom_line() + 
#   facet_wrap(~as.factor(SpeciesID), scales = "free_y")
# 
# ### max Densitys on any given day in each year
# max_dens_surv <- all_dat %>% group_by(year, month, day, SpeciesID) %>% 
#   mutate(daily_sums = sum(SPDensity), daily_area = sum((AreaM2/1000000))) %>% ungroup() %>%
#   group_by(year, SpeciesID) %>% 
#   mutate(max_SPDensity = max(daily_sums)) %>% 
#   select(year, SpeciesID, max_SPDensity) %>% unique()
# 
# filter(max_dens_day, max_SPDensity > 100) %>% 
#   ggplot(aes(year, max_SPDensity)) + geom_line() + 
#   facet_wrap(~as.factor(SpeciesID), scales = "free_y")



### SPAWN STAGE
filter(all_dat, spawn_stage != "NA") %>% mutate(spawn_stage = factor(spawn_stage, levels = c("Pre-spawn", "Spawn", "Post-spawn"))
  
) %>%
  ggplot(aes(spawn_stage, log(SPCount))) + geom_boxplot() +
  facet_wrap(~as.factor(SpeciesID), scales = "free_y")


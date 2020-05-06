#### BEGIN PLOTTING HERRING SURVEY DATA ###
## 

all_dat <- readRDS("data/bird-survey-data.rds")

### total birds recorded by site and year
total_count <- all_dat %>% group_by(year, site) %>% 
  mutate(total_birds = sum(SPCount)) %>% 
  select(year, site, total_birds) %>% unique()

### max counts at any one site in each year
max_counts_site <- all_dat %>% group_by(year, SpeciesID) %>% 
  mutate(max_SPCount = max(SPCount)) %>% 
  select(year, SpeciesID, max_SPCount) %>% unique()

filter(max_counts_site, max_SPCount > 100) %>% 
  ggplot(aes(year, max_SPCount)) + geom_line() + facet_wrap(~as.factor(SpeciesID), scales = "free_y")

### max counts on any given day in each year
max_counts_day <- all_dat %>% group_by(year, month, day, SpeciesID) %>% 
  mutate(daily_sums = sum(SPCount)) %>% ungroup() %>%
  group_by(year, SpeciesID) %>% 
  mutate(max_SPCount = max(daily_sums)) %>% 
  select(year, SpeciesID, max_SPCount) %>% unique()

filter(max_counts_day, max_SPCount > 100) %>% 
  ggplot(aes(year, max_SPCount)) + geom_line() + facet_wrap(~as.factor(SpeciesID), scales = "free_y")


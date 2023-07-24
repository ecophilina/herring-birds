#### BEGIN PLOTTING HERRING SURVEY DATA ###
##
library(tidyverse)

all_dat <- readRDS("data/bird-survey-data.rds")

### calculate density per km2
all_dat <- all_dat %>% mutate(
  SPDensity = signif(SPCount / (AreaM2 / 1000000), 2),
  Fishing = as.factor(case_when(Fishing == "N" ~ 0, Fishing == "Y" ~ 1)),
  Milt = as.factor(case_when(Milt == "N" ~ 0, Milt == "M" ~ 1)),
  Eggs = as.factor(case_when(Eggs == "N" ~ 0, Eggs == "E" ~ 1)),
  Spawn_near = as.factor(case_when(Spawn_near == "N" ~ 0, Spawn_near == "Y" ~ 1)),
  Other_baitballs = as.factor(case_when(Other_baitballs == "N" ~ 0, Other_baitballs == "Y" ~ 1))
)

# all_dat$SPDensity

### remove unmatched site
all_dat <- all_dat %>%
  filter(site != "Snaw-naw-as") %>%
  filter(site != "Eby Rd")


### total birds recorded by site and year
total_count <- all_dat %>%
  group_by(year, site) %>%
  mutate(total_birds = sum(SPCount)) %>%
  select(year, site, total_birds) %>%
  unique()


### current dataframes
# write.csv(all_dat, "data/bird-survey-data.csv")
#
# new_spp_list <- all_dat %>% group_by(matchID) %>% mutate(group = paste(unlist(unique(group)), collapse=''), code_list = paste(unlist(unique(SpeciesID)), collapse='/'), year_list = paste(unlist(unique(year)), collapse='/')) %>% select(matchID, group, code_list, year_list) %>% unique()
#
# write.csv(new_spp_list, "data/new-spp-list.csv")

### violin plots ####

# filter(all_dat, group != "Rapter") %>%
#   ggplot(aes(as.factor(year), SPCount, group = year)) +
#   # ggplot(aes(as.factor(year), log(SPCount+1), group = year)) +
#   # geom_point(size =2) +
#   scale_y_continuous(trans = "log1p") +
#   geom_violin() +
#   facet_wrap(~as.factor(group), scales = "free_y")

p <- filter(all_dat, group != "Rapter") %>%
  ggplot(aes(as.factor(year), SPCount, group = year)) +
  # ggplot(aes(as.factor(year), log(SPCount+1), group = period)) +
  geom_violin() +
  geom_jitter(aes(
    colour = year, alpha = SPCount,
    size = SPCount
  ), # size =1,
  # alpha = 0.1,
  width = 0.25
  ) +
  scale_colour_viridis_c(end = 0.5) +
  facet_wrap(~ as.factor(group), scales = "free_y") +
  gfplot::theme_pbs()
# ggsave("plots/group-violin-all-raw.pdf")


(p <- filter(all_dat, group != "Rapter") %>%
  ggplot(aes(as.factor(spawn_stage), SPCount, group = spawn_stage)) +
  # ggplot(aes(as.factor(year), log(SPCount+1), group = period)) +
  geom_violin() +
  geom_jitter(aes(
    colour = year, alpha = SPCount,
    size = SPCount
  ), # size =1,
  # alpha = 0.1,
  width = 0.25
  ) +
  scale_colour_viridis_c(end = 0.5) +
  facet_wrap(~ as.factor(group), scales = "free_y") +
  gfplot::theme_pbs())

p
# ggsave("plots/group-violin-all-raw.pdf")





# (p + scale_y_continuous(trans = "log1p")) # log plus 1?
# ggsave("plots/group-violin-all-log.pdf")

(p + scale_y_continuous(trans = gfranges:::fourth_root_power))
# ggsave("plots/group-violin-all.pdf")

p <- filter(all_dat, group != "Rapter") %>%
  filter(!is.na(matchID)) %>%
  ggplot(aes(as.factor(year), SPCount, group = year)) +
  # ggplot(aes(as.factor(year), log(SPCount+1), group = period)) +
  geom_violin() +
  geom_jitter(aes(
    colour = year, alpha = SPCount,
    size = SPCount
  ), # size =1,
  # alpha = 0.1,
  width = 0.25
  ) +
  scale_colour_viridis_c(end = 0.5) +
  facet_wrap(~ as.factor(matchID), scales = "free_y") +
  gfplot::theme_pbs()
# ggsave("plots/matchID-violin-all-raw.pdf")


# (p + scale_y_continuous(trans = "log1p"))
# ggsave("plots/matchID-violin-all-log.pdf")

(p + scale_y_continuous(trans = gfranges:::fourth_root_power))
# ggsave("plots/matchID-violin-all.pdf")



# think about filtering out low records
# all_dat %>% group_by()
#
# filter(all_dat, group != "Rapter") %>%
#   ggplot(aes(as.factor(year), SPCount, group = year)) +
#   scale_y_continuous(trans = "log1p") +
#   # ggplot(aes(as.factor(year), log(SPCount+1), group = period)) +
#   geom_violin() +
#   geom_jitter(aes(colour = year, size = SPCount),  # size =1,
#     alpha = 0.2, width = 0.25
#   ) +
#   scale_colour_viridis_c(end = 0.5) +
#   facet_wrap(~as.factor(group), scales = "free_y") + gfplot::theme_pbs()


# ### max counts at any one site in each year ####
max_counts_site <- all_dat %>%
  group_by(year, SpeciesID) %>%
  mutate(max_SPCount = max(SPCount)) %>%
  select(year, SpeciesID, max_SPCount) %>%
  unique()

# filter(max_counts_site, max_SPCount > 50) %>%
#   ggplot(aes(year, max_SPCount)) + geom_line() +
#   facet_wrap(~as.factor(SpeciesID), scales = "free_y")

### max counts on any given survey in each year ####
max_counts_surv <- all_dat %>%
  group_by(year, survey, SpeciesID) %>%
  mutate(surv_sums = sum(SPCount)) %>%
  ungroup() %>%
  group_by(year, SpeciesID) %>%
  mutate(max_SPCount = max(surv_sums)) %>%
  select(year, SpeciesID, max_SPCount, spawn_stage) %>%
  unique()

# filter(max_counts_surv, max_SPCount > 0) %>%
#   ggplot(aes(year, max_SPCount)) + geom_point() +
#   geom_line() +
#   facet_wrap(~as.factor(SpeciesID), scales = "free_y")

### redo limited to matched IDs max counts on any given survey in each year ####
max_counts_surv <- all_dat %>%
  group_by(year, survey, matchID) %>%
  mutate(surv_sums = sum(SPCount)) %>%
  ungroup() %>%
  group_by(year, matchID) %>%
  mutate(max_SPCount = max(surv_sums)) %>%
  select(year, matchID, group, max_SPCount, spawn_stage) %>%
  unique()

filter(max_counts_surv, max_SPCount > 0) %>%
  ggplot(aes(year, max_SPCount)) + geom_point(size = 2) +
  geom_smooth(method = "lm") +
  facet_wrap(~ as.factor(matchID), scales = "free_y") +
  gfplot::theme_pbs()

max_group_surv <- all_dat %>%
  group_by(year, survey, group) %>%
  mutate(surv_sums = sum(SPCount)) %>%
  ungroup() %>%
  group_by(year, group) %>%
  mutate(max_GRCount = max(surv_sums)) %>%
  select(year, group, max_GRCount, spawn_stage) %>%
  unique()

filter(max_group_surv, max_GRCount > 25) %>%
  ggplot(aes(year, max_GRCount)) + geom_point(size = 2) +
  geom_smooth(method = "lm") +
  facet_wrap(~ as.factor(group), scales = "free_y") +
  gfplot::theme_pbs()

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


### HERRING VARIABLES IN 2010s

### SPAWN STAGE
filter(all_dat, spawn_stage != "NA") %>%
  mutate(spawn_stage = factor(spawn_stage,
    levels = c("Pre-spawn", "Spawn", "Post-spawn")
  )) %>%
  ggplot(aes(spawn_stage, log(SPCount + 1))) + geom_boxplot() +
  facet_wrap(~ as.factor(SpeciesID), scales = "free_y")

### ACTIVE SPAWN in wider area
filter(all_dat, Active_spawn != "NA") %>%
  ggplot(aes(Active_spawn, log(SPCount + 1))) + geom_boxplot() +
  facet_wrap(~ as.factor(SpeciesID), scales = "free_y")

### ACTIVE OR RECENT SPAWN in wider area
filter(all_dat, Spawn_near != "NA") %>%
  ggplot(aes(Spawn_near, log(SPCount + 1))) + geom_boxplot() +
  facet_wrap(~ as.factor(SpeciesID), scales = "free_y")

### FISHING within survey area
filter(all_dat, Fishing != "NA") %>%
  ggplot(aes(Fishing, log(SPCount + 1))) + geom_boxplot() +
  facet_wrap(~ as.factor(SpeciesID), scales = "free_y")

### MILT within survey area
filter(all_dat, Milt != "NA") %>%
  ggplot(aes(Milt, log(SPCount + 1))) + geom_boxplot() +
  facet_wrap(~ as.factor(SpeciesID), scales = "free_y")

### EGGS within survey area
filter(all_dat, Egg != "NA") %>%
  ggplot(aes(Egg, log(SPCount + 1))) + geom_boxplot() +
  facet_wrap(~ as.factor(SpeciesID), scales = "free_y")

#### DFO data
dfodat <- read_csv("raw/SpawnRaw-142.csv")


allsurvyears <- dfodat[dfodat$Year %in% c(1998, 1999, 2015, 2016), ]

totals <- dfodat %>%
  group_by(Year) %>%
  summarise(
    tot_SI_under = sum(UnderSI, na.rm = T), n_under = sum(!is.na(UnderSI)),
    tot_SI_surf = sum(SurfSI, na.rm = T), n_surf = sum(!is.na(SurfSI)),
    total = tot_SI_under + tot_SI_surf
  ) %>%
  filter(Year > 1996)

survyeartots <- totals[totals$Year %in% c(1998, 1999, 2015, 2016), ]
plot(total ~ Year, data = survyeartots)

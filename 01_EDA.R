
# Proctor Data Analysis Assessment: EDA

## Load Data and Packages ------------------------------------------------------

library(tidyverse)

# data
mbita_schisto <- read_rds("provided_files_data/mbita_schisto.rds")
mbita_spatial <- read_rds("provided_files_data/mbita_spatial.rds")




## Visualize -------------------------------------------------------------------

### schisto --------

View(mbita_schisto)


#### skim ----
# `sm_epg` `kk_pos` both missing 237
skimr::skim_without_charts(mbita_schisto)



#### `year` ----

# numeric
# age
ggplot(mbita_schisto) +
  geom_histogram(aes(year))

# unique obsv
unique(mbita_schisto$year)

# n 
# increase observations each year
mbita_schisto %>% 
  select(year) %>% 
  group_by(year) %>% 
  count()

# explore increasing numbers
# no repeat ids
mbita_schisto %>% 
  #filter(year == 2012) #%>% 
  group_by(vid) %>% 
  count() %>%
  print(n = 30)
  #filter(n > 1)



mbita_schisto$vid



#### `agey` ----

# numeric
# age
ggplot(mbita_schisto) +
  geom_histogram(aes(agey))


#### `sea` ----

# numeric
# Luminex reponse in MFI-bg to S. mansoni soluble egg antigen (SEA)
ggplot(mbita_schisto) +
  geom_histogram(aes(sea))


#### `sm25` ----

# numeric
# Luminex reponse in MFI-bg to S. mansoni Sm25 recombinant antigen
ggplot(mbita_schisto) +
  geom_histogram(aes(sm25))


#### `sm_epg` ----

# Kato-Katz dual slide S. mansoni eggs per gram of stool
# data type: int

# mean 42, max 5004 (sd 188)
# possible outlier at 5000
ggplot(mbita_schisto) +
  geom_histogram(aes(sm_epg))


#### `sea_pos` ----

# probably should be a factor

# numeric
# seropositive by SEA 965 MFI-bg 
ggplot(mbita_schisto) +
  geom_histogram(aes(sea_pos))


#### `sm25_pos` ----

# probably should be a factor

# numeric
# seropositive by SEA 38 MFI-bg
ggplot(mbita_schisto) +
  geom_histogram(aes(sm25_pos))


#### `kk_pos` ----

# probably should be a factor
# missing 237

# numeric
# Kato-Katz positive for S. mansoni, equal to 1 if sm_epg > 0
ggplot(mbita_schisto) +
  geom_histogram(aes(kk_pos))


# other numeric var: year, vid (village id)

# factor sea_pos, sm25_pos, kk_pos
# already factored arm, sex



#### `arm` ------

# factor
# study arm for the original randomized trial. 
# CWT: community-wide treatment; SBT: school-based treatment

mbita_schisto %>% 
  select(arm) %>% 
  group_by(arm) %>% 
  count()


#### `sex` ------

# factor
# sex (male; female) 


#### `pid` ------

# character
# individual child ID 



#### missingness ------

# `sm_epg` `kk_pos` both missing 237
# missing for the same individuals

mbita_schisto %>% 
  select(pid, vid, year, sm_epg, kk_pos) %>% 
  filter(is.na(sm_epg), is.na(kk_pos)) %>% 
  group_by(year) %>% 
  count()

# look at NAs
# 237 
mbita_schisto %>% 
  select(pid, vid, sm_epg, kk_pos) %>% 
  filter(is.na(sm_epg), is.na(kk_pos)) %>% 
  head(n = 10)





### spatial ----------------------------------------------------------------

# 30 obs, 5 var
mbita_spatial
 
skimr::skim_without_charts(mbita_spatial)

# `vid` village ID

#### `elev`------

# `elev` village elevation in meters from SRTM mission
# numeric
ggplot(mbita_spatial) +
  geom_histogram(aes(elev))


#### `tmin`------

# `tmin` village average minimum temperature from WorldClim
# numeric
ggplot(mbita_spatial) +
  geom_histogram(aes(tmin))

# literally what could the units possibly be 
# is it celcius recorded wrong??? like should there be a decimal there?


#### `prec`------

# `prec` village average precipitation from WorldClim
# numeric
ggplot(mbita_spatial) +
  geom_histogram(aes(prec))



#### `dist_victoria` -------


# `dist_victoria` : village distance to lake Victoria calculated from the global surface water layer
ggplot(mbita_spatial) +
  geom_histogram(aes(dist_victoria))


mbita_spatial %>% 
  select(vid, elev, dist_victoria) %>% 
  arrange(desc(elev))


#### large values? ----

# 23, 17, 30
mbita_spatial %>% 
  select(vid, elev) %>% 
  arrange(desc(elev)) %>% 
  head(n = 3)


# 23, 24, 29
mbita_spatial %>% 
  select(vid, dist_victoria) %>% 
  filter(dist_victoria[,1] > 3000)




# key variable: village id (vid)




## Factor/Clean Data -----------------------------------------------------------------

# factor sea_pos, sm25_pos, kk_pos, year in schisto data
# already factored arm, sex

# mbita_schisto$kk_pos

mbita_schisto_factor <- mbita_schisto %>% 
  mutate(sea_pos = factor(sea_pos, levels = c(0, 1)),
         sm25_pos = factor(sm25_pos, levels = c(0, 1)),
         kk_pos = factor(kk_pos, levels = c(0, 1)),
         year = factor(year, levels = c(2012, 2013, 2014)),
         vid = factor(vid, levels = 1:30),
         # relevel so reference group is SBT
         arm = relevel(arm, "SBT")
         )


mbita_spatial_factor <- mbita_spatial %>% 
  mutate(vid = factor(vid, levels = 1:30),
         # making assumption that tmin is recorded wrong bc 150-160 doesn't make sense on F, C, or K
         # divide by 10 to make more sense in deg Celcius
         tmin = tmin/10
         )

# mbita_schisto_factor$kk_pos

# ggplot(mbita_schisto_factor) +
#   geom_bar(aes(sea_pos))
# 
# ggplot(mbita_schisto_factor) +
#   geom_bar(aes(sm25_pos))
# 
# ggplot(mbita_schisto_factor) +
#   geom_bar(aes(kk_pos))



## Join Data -------------------------------------------------------------------

joined_data <- left_join(mbita_schisto_factor, mbita_spatial_factor, by = "vid")

# keep sea_pos, sm25_pos and kk_pos numeric for potential ease later on
joined_numeric_seroprev <- left_join(mbita_schisto, mbita_spatial, by = "vid") %>% 
  mutate(year = factor(year, levels = c(2012, 2013, 2014)),
         vid = factor(vid, levels = 1:30),
         tmin = tmin/10,
         arm = relevel(arm, "SBT")
  )

## Save Data --------------------------------------------------------------------


save(mbita_schisto_factor, mbita_spatial_factor, joined_data, joined_numeric_seroprev,
     file = "processed_data/schisto_spatial_processed.rda")







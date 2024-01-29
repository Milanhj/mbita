
# Compare S. mansoni seroprevalence between groups


## Load Data and Packages ------------------------------------------------------

library(tidyverse)
library(stats)
library(sjPlot)
library(sjmisc)
library(sjlabelled)

load("processed_data/schisto_spatial_processed.rda")



## instructions (given) ----

# Estimate the effect of CWT versus SBT on S. mansoni seroprevalence
  # measured by IgG seropositivity to the Soluble Egg Antigen (SEA) and/or the recombinant Sm25 antigen
  # treat SBT that as comparison group and CWT as intervention group

# Compare groups based on diff in prevalence averaged over entire post-treatment period 
  # (combining measurements over 2013 and 2014).
# absolute measure of effect

# Provide estimates of effect, 95% confidence intervals, and a P-value for the difference 
# Summarize your results in a formatted table. 
# Provide a brief interpretation of the results.




## Create outcome Variable ---------------------------------------------------

### test1 ----

# This section is scratch work 
# group by vid so the sum of variable is taken for each village separately


# if_else to separate baseline values from 2012 from 2013/14
# need just values from 13/14 to calculate the proportion of pos tests
  # avg_sea_pos = if_else(year != 2012, sum(sea_pos), NA) 


test1 <- joined_data %>%
  group_by(vid) %>%
  # seperate values from 2012 and 13+14
  mutate(sea_2012 = if_else(year == 2012, sea_pos, NA), 
         sm25_2012 = if_else(year == 2012, sm25_pos, NA),
         # seperate test values from 2013
         sea_2013 = if_else(year == 2013, sea_pos, NA),
         sm25_2013 = if_else(year == 2013, sm25_pos, NA),
         # seperate test values from 2014
         sea_2014 = if_else(year == 2014, sea_pos, NA),
         sm25_2014 = if_else(year == 2014, sm25_pos, NA),
         # combined 13/14 results
         sea_2013_14 = if_else(year != 2012, sea_pos, NA),
         sm25_2013_14 = if_else(year != 2012, sm25_pos, NA),
         
         # want the ratio positive/total in 2013 and 2014 combined
         # need to find total obs for each year bc its different
         # counts sea
         sea_2012_count = sum(sea_2012 == "1" | sea_2012 == "0", na.rm=T),
         sea_2013_count = sum(sea_2013 == "1" | sea_2013 == "0", na.rm=T),
         sea_2014_count = sum(sea_2014 == "1" | sea_2014 == "0", na.rm=T),
         # counts sm24
         sm25_2012_count = sum(sm25_2012 == "1" | sm25_2012 == "0", na.rm=T),
         sm25_2013_count = sum(sm25_2013 == "1" | sm25_2013 == "0", na.rm=T),
         sm25_2014_count = sum(sm25_2014 == "1" | sm25_2014 == "0", na.rm=T),
         # total count obs in village
         # no NA in either sea or sm25 so will have the same total counts 
         total_obs_2013_14 = sea_2013_count + sea_2014_count,
         
         # ratio positive/total in 2012
         sea_prop_2012 = sum(sea_2012 == "1", na.rm=T) / sea_2012_count,
         sm25_prop_2012 = sum(sm25_2012 == "1", na.rm=T) / sm25_2012_count,
         
         # combined proportion positives in 2013+14
         sea_prop_2013_14 = sum(sea_2013_14 == "1", na.rm=T) / total_obs_2013_14,
         sm25_prop_2013_14 = sum(sm25_2013_14 == "1", na.rm=T) / total_obs_2013_14,
         
         # compute avg village age
         avg_age = mean(agey),
         # m:f sex ratio of village
         sex_perc = sum(sex == "male", na.rm=T) / n(),
  )%>%
  # remove all the extra variables I wont use in models
  # filter out all extraneous rows (all row w/in each village have same values)
  # now can remove temp id too
  select(-sea, -sm25, -sm_epg, -sm25_pos, -kk_pos, 
         -pid, -agey, -sex) %>%
  # ungroup
  ungroup(vid)


# check that I get the counts right for each year for calc prop
t1 <- joined_data %>% 
  filter(year == 2012) %>% 
  group_by(vid) %>%
  count() %>% 
  rename(n_12 = n)

t2 <- joined_data %>% 
  filter(year == 2013) %>% 
  group_by(vid) %>% 
  count() %>% 
  rename(n_13 = n) %>% 
  left_join(t1, by = "vid")
  

t3 <- joined_data %>% 
  filter(year == 2014) %>% 
  group_by(vid) %>% 
  count() %>%
  rename(n_14 = n) %>% 
  left_join(t2, by = "vid")

t3

### test2 ----
# check
# this works for both var


test2 <- joined_data %>%
  group_by(vid) %>% 
  filter(vid == 1) %>%
  # separate columns for values from each 3 years
  mutate(sea_2012 = if_else(year == 2012, sea_pos, NA), 
         sm25_2012 = if_else(year == 2012, sm25_pos, NA),
         sea_2013 = if_else(year == 2013, sea_pos, NA),
         sm25_2013 = if_else(year == 2013, sm25_pos, NA),
         sea_2014 = if_else(year == 2014, sea_pos, NA),
         sm25_2014 = if_else(year == 2014, sm25_pos, NA),
         
         # create variables with values only from 2013 and 2014
         # sea_2013_14 = if_else(year != 2012, sea_pos, NA),
         # sm25_2013_14 = if_else(year != 2012, sm25_pos, NA),
         # ratio positive/total in 2013 and 2014 combined
         # need to find total obs for each year bc its different
         
         # counts sea
         sea_2012_count = sum(sea_2012 == "1" | sea_2012 == "0", na.rm=T),
         sea_2013_count = sum(sea_2013 == "1" | sea_2013 == "0", na.rm=T),
         sea_2014_count = sum(sea_2014 == "1" | sea_2014 == "0", na.rm=T), #)%>%
         
         # counts sm24
         sm25_2012_count = sum(sm25_2012 == "1" | sm25_2012 == "0", na.rm=T),
         sm25_2013_count = sum(sm25_2013 == "1" | sm25_2013 == "0", na.rm=T),
         sm25_2014_count = sum(sm25_2014 == "1" | sm25_2014 == "0", na.rm=T),
    
         # ratio positive/total in 2012
         sea_prop_2012 = sum(sea_2012 == "1", na.rm=T) / sea_2012_count,
         sm25_prop_2012 = sum(sm25_2012 == "1", na.rm=T) / sm25_2012_count,

        # calculate proportion positives
        sea_prop_2013 = sum(sea_2013 == "1", na.rm=T) / sea_2013_count,
        sm25_prop_2013 = sum(sm25_2013 == "1", na.rm=T) / sm25_2013_count,
        sea_prop_2014 = sum(sea_2014 == "1", na.rm=T) / sea_2014_count,
        sm25_prop_2014 = sum(sm25_2014 == "1", na.rm=T) / sm25_2014_count,
  
        
        # combined prop for 2013 and 14
        sea_prop_2013_14 = sea_prop_2013 + sea_prop_2014,
        sm25_prop_2013_14 = sm25_prop_2013 + sm25_prop_2014,
         
         
         # prop
         # sea_prop_2012 = sum(sea_2012 == "1", na.rm=T) / n(),
         # sm25_prop_2012 = sum(sm25_2012 == "1", na.rm=T) / n(),
         # sea_prop_2013 = sum(sea_2013 == "1", na.rm=T) / n(),
         # sm25_prop_2013 = sum(sm25_2013 == "1", na.rm=T) / n(),
         # sea_prop_2014 = sum(sea_2014 == "1", na.rm=T) / n(),
         # sm25_prop_2014 = sum(sm25_2014 == "1", na.rm=T) / n(),
  
  # total counts from 2012
  
  # # positive counts from 2012
  # sea_pos_count_2012 = sum(sea_2012 == "1", na.rm=T),
  # sm25_pos_count_2012 = sum(sm25_2012 == "1", na.rm = TRUE),
  # 
  # # neg counts from 2012
  # sea_neg_count_2012 = sum(sea_2012 == "0", na.rm = TRUE),
  # sm25_neg_count_2012 =  sum(sm25_2012 == "0", na.rm = TRUE),
         
         # average prop 2012/2013
         # sea_avg_prop_13_14 = (sea_prop_2013 + sea_prop_2014)/2,
         # sm25_avg_prop_13_14 = (sm25_prop_2013 + sm25_prop_2014)/2,
         
         # # prop
         # sea_prop_2013_14 = sum(sea_2013_14 == "1", na.rm=T) / n(),
         # sm25_prop_2013_14 = sum(sm25_2013_14 == "1", na.rm=T) / n(),
         
         # # positive counts
         # sea_pos_count_2013_14 = sum(sea_2013_14 == "1", na.rm = TRUE),
         # sm25_pos_count_2013_14 = sum(sm25_2013_14 == "1", na.rm = TRUE),
         # 
         # # neg counts from 2012
         # sea_neg_count_2013_14 = sum(sea_2013_14 == "0", na.rm = TRUE),
         # sm25_neg_count_2013_14 =  sum(sm25_2013_14 == "0", na.rm = TRUE),
         
         # compute avg village age
         # avg_age = mean(agey),
         # 
         # # m:f sex ratio of village
         # sex_perc = sum(sex == "male", na.rm=T) / n()
         
         # create a new temporary id variable for obs within each village
         temp_id = row_number()
         
  ) #%>%      
         # this for computing arithmetic avg
         # can do this after I join w just village data
         # sea_2013 = if_else(year == 2013, sea_pos, NA),
         # sm25_2013 = if_else(year == 2013, sm25_pos, NA),
         # 
         # sea_2014 = if_else(year == 2014, sea_pos, NA),
         # sm25_2014 = if_else(year == 2014, sm25_pos, NA),
         # avg_sea_2013 = sum(sea_2013, na.rm = TRUE),
         # avg_sm25_2013 = sum(sm25_2013, na.rm = TRUE),
         # 
         # avg_sea_2014 = sum(sea_2014, na.rm = TRUE),
         # avg_sm25_2014 = sum(sm25_2014, na.rm = TRUE)

  # group_by(sm25_no_2012) %>%
  # count()
  #select(vid, sea_pos_2013_14, sm25_pos_2013_14)

# prop.table(table(test1$vid, test$sea_pos_2013_14), margin = 1)
# prop.table(table(test1$vid, test$sex), margin = 1)


### inspect test data ----


# most of positivity ratios decrease after treatment
# groups 1 and 13 have large decreases in sea positivity (especially 1 ~0.9->0.5)
# g1 CWT; g13 SBT

# in test section I made test1 dataset
# is same dataset but includes all calculated var and all observations
# filter for only vid 1 and 13
inspect <- test1  %>% 
  filter(vid == 1 | vid == 13) %>% 
  select(vid, year, sea_pos, sea_2013_14, sea_2012, sea_2013, sea_2014)

# vid1: sea_pos for 2012, 13, and 14 are same as in joined_data
# vid13: sea_pos for 2012, 13, and 14 same as in joined_data

# Need to calculate P different 



### final model data ---------------------------------------



model_data <- joined_data %>%
  group_by(vid) %>%
  # seperate values from 2012 and 13+14
  mutate(sea_2012 = if_else(year == 2012, sea_pos, NA), 
         sm25_2012 = if_else(year == 2012, sm25_pos, NA),
         # seperate test values from 2013
         sea_2013 = if_else(year == 2013, sea_pos, NA),
         sm25_2013 = if_else(year == 2013, sm25_pos, NA),
         # seperate test values from 2014
         sea_2014 = if_else(year == 2014, sea_pos, NA),
         sm25_2014 = if_else(year == 2014, sm25_pos, NA),
         # combined 13/14 results
         sea_2013_14 = if_else(year != 2012, sea_pos, NA),
         sm25_2013_14 = if_else(year != 2012, sm25_pos, NA),
         
         # want the ratio positive/total in 2013 and 2014 combined
         # need to find total observations for each year bc they're different
         # counts sea
         sea_2012_count = sum(sea_2012 == "1" | sea_2012 == "0", na.rm=T),
         sea_2013_count = sum(sea_2013 == "1" | sea_2013 == "0", na.rm=T),
         sea_2014_count = sum(sea_2014 == "1" | sea_2014 == "0", na.rm=T),
         # counts sm24
         sm25_2012_count = sum(sm25_2012 == "1" | sm25_2012 == "0", na.rm=T),
         sm25_2013_count = sum(sm25_2013 == "1" | sm25_2013 == "0", na.rm=T),
         sm25_2014_count = sum(sm25_2014 == "1" | sm25_2014 == "0", na.rm=T),
         # total count obs in village
         # no NA in either sea or sm25 so will have the same total counts 
         total_obs_2013_14 = sea_2013_count + sea_2014_count,
         
         # ratio positive/total in 2012
         sea_prop_2012 = sum(sea_2012 == "1", na.rm=T) / sea_2012_count,
         sm25_prop_2012 = sum(sm25_2012 == "1", na.rm=T) / sm25_2012_count,
         
         # combined proportion positives in 2013+14
         sea_avg_2013_14 = sum(sea_2013_14 == "1", na.rm=T) / total_obs_2013_14,
         sm25_avg_2013_14 = sum(sm25_2013_14 == "1", na.rm=T) / total_obs_2013_14,

         # compute avg village age
         avg_age = mean(agey),
         # m:f sex ratio of village
         sex_perc = sum(sex == "male", na.rm=T) / n(),
         # create a new temporary id variable for obs within each village
         temp_id = row_number()
  )%>%
  # remove all the extra variables I wont use in models
  select(-sea_2012, -sm25_2012, -sea_2013, -sm25_2013, -sea_2014, -sm25_2014,
         -sea_2013_14, -sm25_2013_14, -sea_2012_count, -sea_2013_count, -sea_2014_count,
         -sm25_2012_count, -sm25_2013_count, -sm25_2014_count,
         -sea, -sm25, -sm_epg, -sea_pos, -sm25_pos, -kk_pos, 
         -pid, -agey, -sex, -year
         ) %>%
  # filter out all extraneous rows (all row w/in each village have same values)
  filter(temp_id == 1) %>%
  # now can remove temp id too
  select(-temp_id) %>%
  # ungroup
  ungroup(vid)





## Explore covariates ----

# visualize some relationships!

# make empty list
var_list <- list()

# fil list with variables
for(var in colnames(model_data)){
  var_list[var] <- model_data[[var]]
}

var_tbl <- enframe(unlist(var_list))

# get the names out to iterate over with map function
var_names <- var_tbl %>%
  pull(name)

# !!sym() removes quotes (un-strings)
plot_fun_sea <- function(var = NULL, y = NULL){
  ggplot(model_data, aes(x = !!sym(var), y = sea_avg_2013_14)) +
    geom_point()
}

plot_fun_sm25 <- function(var = NULL, y = NULL){
  ggplot(model_data, aes(x = !!sym(var), y = sm25_avg_2013_14)) +
    geom_point()
}


# plot scatter plots for all variables in the dataset
map(var_names, plot_fun_sea)
map(var_names, plot_fun_sm25)


# corrplot
model_data %>% 
  select_if(is.numeric) %>%
  cor(use = "complete.obs") %>% 
  corrplot::corrplot(type = "upper", method = "number")


# sea_prop_2012, prec, dist_victoria, tmin look more important
# play around removing sex, age, elev





## SEA ------------------------------------------------------------------------

colnames(model_data)

### base linear model ----

# intercept is armCWT bc dummy var
sea_base_model <- glm(sea_avg_2013_14 ~ arm, 
                      data = model_data, 
                      family = binomial,
                      weights = total_obs_2013_14) # provide prior weights

summary(sea_base_model)



### kitchen sink ----

sea_ks_model <- glm(sea_avg_2013_14 ~ arm + sea_prop_2012 + tmin + prec + dist_victoria + 
                     avg_age + elev + sex_perc,
                   data = model_data, 
                   family = binomial,
                   weights = total_obs_2013_14
                   )

                   
summary(sea_ks_model)



### select recipe 1 ----

# sea_prop_2012, prec, dist_victoria, tmin 

# remove sex ratio
sea_s1_model <- glm(sea_avg_2013_14 ~ arm + sea_prop_2012 + tmin + prec + dist_victoria +
                      avg_age + elev,
                    data = model_data, 
                    family = binomial,
                    weights = total_obs_2013_14
)


summary(sea_s1_model)
# ks intercept 81.45
# sbt -0.315


### select recipe 2 ----

# take out elev
sea_s2_model <- glm(sea_avg_2013_14 ~ arm + sea_prop_2012 + tmin + prec + dist_victoria + avg_age,
                          data = model_data, 
                          family = binomial,
                          weights = total_obs_2013_14
)

summary(sea_s2_model)


### select recipe 3 ----

# take out avg age
sea_s3_model <- glm(sea_avg_2013_14 ~ arm + sea_prop_2012 + tmin + prec + dist_victoria,
                    data = model_data, 
                    family = binomial,
                    weights = total_obs_2013_14
)

summary(sea_s3_model)


### select recipe 4 ----

# take out age
# add in elev

sea_s4_model <- glm(sea_avg_2013_14 ~ arm + sea_prop_2012 + tmin + prec + dist_victoria + elev,
                    data = model_data, 
                    family = binomial,
                    weights = total_obs_2013_14
)

summary(sea_s4_model)




### compare ----

# want smaller AIC
AIC(sea_base_model, sea_ks_model, sea_s1_model, sea_s2_model, sea_s3_model, sea_s4_model)

# s1 model is best

### final sea ----
sea_final_model <- glm(sea_avg_2013_14 ~ arm + sea_prop_2012 + tmin + prec + dist_victoria + 
                         avg_age + elev + sex_perc,
                       data = model_data, 
                       family = binomial,
                       weights = total_obs_2013_14
)


summary(sea_final_model)

exp(2.974e-02) # 1.03 increase by 28.03%



## Sm25 ------------------------------------------------------------------------

colnames(model_data)

### base glm model ----

# intercept is armCWT bc dummy var
sm25_base_model <- glm(sm25_avg_2013_14 ~ arm, 
                      data = model_data, 
                      family = binomial,
                      weights = total_obs_2013_14) # provide prior weights

summary(sm25_base_model)



### kitchen sink ----

sm25_ks_model <- glm(sm25_avg_2013_14 ~ arm + sm25_prop_2012 + tmin + prec + dist_victoria + 
                      avg_age + elev + sex_perc,
                    data = model_data, 
                    family = binomial,
                    weights = total_obs_2013_14
)


summary(sm25_ks_model)



### select recipe 1 ----

# sm25_prop_2012, prec, dist_victoria, tmin 

# remove sex ratio
sm25_s1_model <- glm(sm25_avg_2013_14 ~ arm + sm25_prop_2012 + tmin + prec + dist_victoria +
                      avg_age + elev,
                    data = model_data, 
                    family = binomial,
                    weights = total_obs_2013_14
)


summary(sm25_s1_model)
# ks intercept 81.45
# sbt -0.315


### select recipe 2 ----

# take out elev
sm25_s2_model <- glm(sm25_avg_2013_14 ~ arm + sm25_prop_2012 + tmin + prec + dist_victoria + avg_age,
                    data = model_data, 
                    family = binomial,
                    weights = total_obs_2013_14
)

summary(sm25_s2_model)


### select recipe 3 ----

# take out avg age
sm25_s3_model <- glm(sm25_avg_2013_14 ~ arm + sm25_prop_2012 + tmin + prec + dist_victoria,
                    data = model_data, 
                    family = binomial,
                    weights = total_obs_2013_14
)

summary(sm25_s3_model)


### select recipe 4 ----

# take out age
# add in elev

sm25_s4_model <- glm(sm25_avg_2013_14 ~ arm + sm25_prop_2012 + tmin + prec + dist_victoria + elev,
                    data = model_data, 
                    family = binomial,
                    weights = total_obs_2013_14
)

summary(sm25_s4_model)




### compare ----

# want smaller AIC
AIC(sm25_base_model, sm25_ks_model, sm25_s1_model, sm25_s2_model, sm25_s3_model, sm25_s4_model)

# ks model is best

### final sm25 ----
sm25_final_model <-  glm(sm25_avg_2013_14 ~ arm + sm25_prop_2012 + tmin + prec + dist_victoria + 
                           avg_age + elev + sex_perc,
                         data = model_data, 
                         family = binomial,
                         weights = total_obs_2013_14
)



summary(sm25_final_model)

exp(0.1056648) # 1.111449 increase by 16.5%%



## Results -------------------------------------------------

### CI ----

# 95 % CI for sea
confint(sea_final_model, level = 0.95)

# 95 % CI for sea
confint(sm25_final_model, level = 0.95)


### Table ----

pred_labels <- c("arm CWT", "Proportion seropositive by SEA in 2012", 
                 "Average minimum temperature", "Average precipitation", 
                 "Distance to lake Victoria", "Average age", "Elevation",
                 "Sex ratio (male:female)", "Proportion seropositive by Sm25 in 2012")

dv_labels <- c("Proportion seropositive by SEA", "Proportion seropositive by Sm25" )


# build table
tab_model(sea_final_model, sm25_final_model,
          title = "Regression Results",
          show.intercept = FALSE,
          pred.labels = pred_labels,
          dv.labels = dv_labels,
          string.est = "Odds Ratio",
          string.ci = "95% CI")




## Save Data -----------------------------------------------------------------

write_rds(model_data, file = "processed_data/model_data.rds")






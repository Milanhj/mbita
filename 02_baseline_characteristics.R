
# Summarize Baseline Characteristics:


## Load Data and Packages ----------------------------------------------------------------

# packages
library(tidyverse)
library(table1)
library(stringr)

# data
load("processed_data/schisto_spatial_processed.rda")



## Instructions ----


# compare groups based on measurable baseline characteristics (table 1)

# independent units for analysis are the community

# Create a table that summarizes individual-level and cluster-level characteristics by randomized group (cwt and sbt): 
  # row = a variable or level of that variable 
  # separate column for each group

# For measures of S. mansoni infection or antibody response, limit summary to the categorical measures
# For other quantitative variables, summarize the mean and standard deviation (and/or median and interquartile range). 
# For categorical variables, report the N and percent


## Build table ---------------------------------------------------------

# all variable names
colnames(joined_data)

# Labels
label(joined_data$year) <- "Year"
label(joined_data$agey) <- "Age"
label(joined_data$sex) <- "Sex"
label(joined_data$sea_pos) <- "SEA positive"
label(joined_data$sm25_pos) <- "Sm25 positive"
label(joined_data$kk_pos) <- "Kato-Katz positive"
label(joined_data$elev) <- "Elevation"
label(joined_data$tmin) <- "Average minimum temperature"
label(joined_data$prec) <- "Average precipitation"
label(joined_data$dist_victoria[,1]) <- "Distance to lake Victoria"

# table
table1(~ year + agey + sex + sea_pos + sm25_pos + kk_pos +
         elev + tmin + prec + dist_victoria | 
         arm, data = joined_data, overall = F)



# elev + tmin + prec + dist_victoria 
# sea + sm25 + sm_epg +
# label(joined_data$sea) <- "SEA"
# label(joined_data$sm25) <- "Sm25"
# label(joined_data$sm_epg) <- "Sm EPG"


ggplot(joined_data, aes(sea)) +
  geom_histogram() +
  facet_wrap(~arm)



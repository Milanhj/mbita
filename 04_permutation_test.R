
# Permutation Test:


## Load Data and Packages ------------------------------------------------------

library(tidyverse)
library(stats)
library(coin)

load("processed_data/schisto_spatial_processed.rda")
model_data <- read_rds("processed_data/model_data.rds")


## Instructions ----------------------------------------------------------------

# arm assignment re-randomized across permutations
# test statistic is computed in each

# estimate the permutation P-value for differences between groups, 
# assuming that only random variation in the trial is the community-level arm assignment
# any test statistic you want. 
# How does your inference compare with your results from the previous section?


## Model -----------------------------------------------------------------------

# 2 groups: CWT vs SBT

# skim data again
View(
  model_data %>% 
  select(-elev, -prec, -tmin, -avg_age, -sex_perc, 
         -dist_victoria, -vid, -total_obs_2013_14
         ) %>% 
  group_by(arm) %>% 
  skimr::skim()
)


# ttest 

t.test(sea_avg_2013_14 ~ arm, model_data)
t.test(sm25_avg_2013_14 ~ arm, model_data)




### SEA --------------------------------------------------------------



### example 1 ---------------------------------------

# code adapted from https://towardsdatascience.com/permutation-test-in-r-77d551a9f891


# Simulate 10k shuffles without replacement: how often observe this or more extreme diff in group means 
# Null hypothesis = no diff


# redefine variables as an object to make code more concise
sea_avg_2013_14 <- model_data$sea_avg_2013_14
sm25_avg_2013_14 <- model_data$sm25_avg_2013_14
arm <- model_data$arm

# Difference in means
# orig_diff <- diff(tapply(sea_avg_2013_14, arm, mean)) # -0.08048025 

orig_diff <- mean(model_data$sea_avg_2013_14[model_data$arm == "CWT"]) -
  mean(model_data$sea_avg_2013_14[model_data$arm == "SBT"])   # 0.08048025

# Permutation test function
# n is # permutations
# trial and outcome are variables of interest
perm_test <- function(trial, outcome, n){
  # empty vector to store values
  distribution <- c()
  result <- 0
  # iterate through each
  for(i in 1:n){
    # store diff in means for each perm
    # sample without replacement
    # by(data, sample trial 30x, apply mean function 
    # find difference test stats (means)
    # don't apply abs() yet bc want to be able to plot
    distribution[i] <- diff(by(outcome, sample(trial, length(trial), FALSE), mean))
  }
  # p value 
  result <- sum(abs(distribution) >= orig_diff)/(n)
  # returns a list w/ first item as p value, second as the distribution of means
  return(list(result, distribution))
}

# set a seed when testing that it works 
set.seed(301)
test <- perm_test(arm, sea_avg_2013_14, 10000)

# plot a histogram of the distribution of test statistics
# extract second item from the list (vector of diffs of means)
hist(test[[2]], breaks = 30, col='grey', las = 1, main = "Mean Difference Distribution", xlab = '')
abline(v = orig_diff, col="red")

# p = 0.3999
test[[1]]


#Compare to t-test
t.test(sea_avg_2013_14 ~ arm)

# independence test 
independence_test(sea_avg_2013_14 ~ arm, model_data)






### example 2 --------------------

# taking above code and redoing it w my own approach/ more manually

orig_diff <- mean(model_data$sea_avg_2013_14[model_data$arm == "CWT"]) - 
  mean(model_data$sea_avg_2013_14[model_data$arm == "SBT"]) 




# number of observations
n <- length(model_data$arm)
# number of permutation samples
p <- 10000
# outcome variable of interest
outcome <- model_data$sea_avg_2013_14


# create matrix to hold permutation sets
# fill with 0s: nrows = number of observations, ncols = same # cols as # permutations
p_samples  <- matrix(0, nrow = n, ncol = p)


# for loop to iterate through and fill matrix 
set.seed(3013)
for (i in 1:p) {
  # replace 0s in column i w/ samples
  p_samples[,i] <- sample(outcome, size = n, replace = FALSE)
}

p_samples[,1:5]

# vector to store test stats (diff means) for each permutation
# 10k 0s
test_stats <- rep(0, 10000)

# calc test stats for each and store
for(i in 1:p) {
  # abs value of diff btwn SBT and CWT means for each permutation 
  test_stats[i] <-
    mean(p_samples[model_data$arm =="SBT", i]) - mean(p_samples[model_data$arm=="CWT", i])
}

# H0: abs(mean_sbt - mean_cwt) = 0
# calc p-value
# evaluate a 0.05
# (num perm test stats > observed ts) / total num perm test stats

mean(abs(test_stats) >= abs(orig_diff)) # 0.3882


# plot a histogram of the distribution of test statistics
# extract second item from the list (vector of diffs of means)
hist(test_stats, breaks = 30, col='grey', las = 1, main = "Mean Difference Distribution", xlab = '')
abline(v = orig_diff, col="red")



### example 3 --------------------

# independence test from `coin`

independence_test(sea_avg_2013_14 ~ arm, model_data) # p-value = 0.3825

# all 3 approaches reach essentially the same p value 
# coin package and manual approach most similar





## Sm25 ----------------------------------------------------------------------------------------


### example 1 ----------------

orig_diff_sm25 <- mean(model_data$sm25_avg_2013_14[model_data$arm == "CWT"]) - 
  mean(model_data$sm25_avg_2013_14[model_data$arm == "SBT"]) 



# number of observations
n <- length(model_data$arm)
# number of permutation samples
p <- 10000
# outcome variable of interest
outcome_sm25 <- model_data$sm25_avg_2013_14


# create matrix to hold permutation sets
# fill with 0s: nrows = number of observations, ncols = same # cols as # permutations
p_samples_sm25  <- matrix(0, nrow = n, ncol = p)


# for loop to iterate through and fill matrix 
set.seed(3013)
for (i in 1:p) {
  # replace 0s in column i w/ samples
  p_samples_sm25[,i] <- sample(outcome_sm25, size = n, replace = FALSE)
}

p_samples_sm25[,1:5]

# vector to store test stats (diff means) for each permutation
# 10k 0s
test_stats_sm25 <- rep(0, 10000)

# calc test stats for each and store
for(i in 1:p) {
  # abs value of diff btwn SBT and CWT means for each permutation 
  test_stats_sm25[i] <-
    mean(p_samples_sm25[model_data$arm =="SBT", i]) - 
    mean(p_samples_sm25[model_data$arm=="CWT", i])
}

# H0: abs(mean_sbt - mean_cwt) = 0
# calc p-value
# evaluate a 0.05
# (num perm test stats > observed ts) / total num perm test stats

mean(abs(test_stats_sm25) >= abs(orig_diff_sm25)) # 0.8214


# plot a histogram of the distribution of test statistics
# extract second item from the list (vector of diffs of means)
hist(test_stats_sm25, breaks = 30, col='grey', las = 1, main = "Mean Difference Distribution", xlab = '')
abline(v = orig_diff_sm25, col="red")



### example 2 --------------------

# independence test from `coin`

independence_test(sm25_avg_2013_14 ~ arm, model_data) # p-value = 0.8178

# all 3 approaches reach essentially the same p value 
# coin package and manual approach most similar







#############################################################
# Load packages, create data set and submission file
#############################################################

# install required packages, if required.
if(!require(tidyverse))   install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret))       install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggplot2))     install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(randomForest))install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(rpart))       install.packages("rpart", repos = "http://cran.us.r-project.org")

# download the data set and load into a variable
ginf <- tempfile()
download.file("https://github.com/Henkeur10/edx-datascience/raw/master/ginf.csv", ginf)
ginf <- read.csv(ginf)


#############################################################
# Data analysis
#############################################################

# show basic info of the "ginf" data set
nrow(ginf) # number of rows
names(ginf) # column names
unique(ginf$season) # unique seasons in "ginf"
unique(ginf$country)# unique countries in "ginf"
unique(ginf$league) # unique leagues in "ginf"

# add new columns to the "ginf" data set and load the result into a new variable "df".
#
# the new column "result" shows the actual result of the match, based on the columns
# "fthg" (full time home goals) and "ftag" (full time away goals).
# If fthg > ftag, then the home team wins (= 1),
# if fthg < ftag, then the away team wins (= 2),
# if fthg = ftag, then it's a draw (= 3).
#
# the new column "gd" shows the goal difference per match.
# a positive integer means the home team wins.
# a negative integer means the away team wins,
# zero means it's a draw.
df <- ginf %>%
  mutate(result = factor(ifelse(.$fthg > .$ftag, 1, ifelse(.$fthg < .$ftag, 2, 3))),
         gd = .$fthg - .$ftag)

#############################################################
# Create train and test set
#############################################################

nrow(df) # number of rows
train <- df[df$season != 2017,] # create train set: season 2012-2016
test <- df[df$season == 2017,] # create test set: season 2017
nrow(train) + nrow(test) # check if no rows got lost in the process

# define home teams that exist in the test set, but not in the train set.
# those are teams that were promoted in 2017 and weren't playing on the highest level
# in earlier seasons. These teams need to be removed from the test set,
# both when they are play home or away.

diff <- setdiff(test$ht, train$ht) # determine which teams are in the test set, but not in the train.

length(diff)

test <- test[!(test$ht %in% diff | test$at %in% diff),] # remove the teams from the test set.

setdiff(test$ht, train$ht) # double check
setdiff(test$at, train$at) # double check


#############################################################
# Data analysis, continued
#############################################################

# show the mean of home wins, away wins and draws per season
train %>%
  group_by(season) %>%
  summarise(mean_per_season_1 = mean(result == 1),
            mean_per_season_2 = mean(result == 2),
            mean_per_season_3 = mean(result == 3),
            sum_means = mean_per_season_1 + mean_per_season_2 + mean_per_season_3) %>%
  ggplot(aes(season)) +
  geom_line(aes(y = mean_per_season_1, colour = "Mean of home wins")) +
  geom_line(aes(y = mean_per_season_2, colour = "Mean of away wins")) +
  geom_line(aes(y = mean_per_season_3, colour = "Mean of draws")) +
  xlab("Season") +
  ylab("Mean")

# show the mean of home wins, away wins and draws per season, per country
train %>% 
  group_by(season, country) %>%
  summarise(mean_per_season_1 = mean(result == 1),
            mean_per_season_2 = mean(result == 2),
            mean_per_season_3 = mean(result == 3),
            sum_means = mean_per_season_1 + mean_per_season_2 + mean_per_season_3) %>%
  ggplot(aes(season)) +
  geom_line(aes(y = mean_per_season_1, colour = "Mean of home wins")) +
  geom_line(aes(y = mean_per_season_2, colour = "Mean of away wins")) +
  geom_line(aes(y = mean_per_season_3, colour = "Mean of draws")) +
  facet_grid(cols = vars(country)) +
  xlab("Season") +
  ylab("Mean") +
  theme(axis.text.x  = element_text(angle=90))

# show a histogram of the goal difference column per match
train %>%
  ggplot(aes(gd, stat(density))) +
  geom_histogram(binwidth = 1) +
  xlab("Goal difference (gd) per match") +
  ylab("Density")

# show a histogram of the goal difference column per match per season
train %>%
  group_by(season) %>%
  ggplot(aes(gd, stat(density))) +
  geom_histogram(binwidth = 1) +
  facet_grid(cols = vars(season)) +
  xlab("Goal difference (gd) per match per season") +
  ylab("Density")

# show a histogram of the goal difference column per match per country
train %>%
  group_by(country) %>%
  ggplot(aes(gd, stat(density))) +
  geom_histogram(binwidth = 1) +
  facet_grid(cols = vars(country)) +
  xlab("Goal difference (gd) per match per country") +
  ylab("Density")


#############################################################
# Building the models
# Step 1
# Educated guess
#############################################################

y_hat_ht <- factor(replicate(881, 1)) # guessing on a win for every home team
#confusionMatrix(y_hat_ht, y)

y_hat_at <- factor(replicate(881, 2)) # guessing on a win for every away team
#confusionMatrix(y_hat_at, y)

y_hat_dr <- factor(replicate(881, 3)) # guessing on a draw all the time
#confusionMatrix(y_hat_dr, y)


#############################################################
# Building the models
# Step 2
# Lineair regression
#############################################################

set.seed(1) # set seed to reproduce the same results

fit <- lm(result ~ ht + at, train) # create a fit which can predict the result, using the ht (home team) and at (away team) as predictors

y_hat_lm_1 <- predict(fit, test) # predic the result, using the fit on the test set

y_hat_lm_1 <- factor(round(y_hat_lm_1)) # round to nearest integer, since we only use 1,2,3. Convert to factor

mean(y_hat_lm_1 == "2")

#confusionMatrix(y_hat_lm, y) # bad result ... Note there are a lot of 2, while we know 1 should be more present

set.seed(1) # set seed to reproduce the same results

y_hat_lm_2 <- predict(fit, test) # create a fit which can predict the result, using the ht (home team) and at (away team) as predictors

y_hat_lm_2 <- factor(as.integer(y_hat_lm_2)) # Insteand of rounding, just grab the integer value.

y_hat_lm_2[y_hat_lm_2 == "0"] <- "1" # change the 0 to 1

y_hat_lm_2 <- droplevels(y_hat_lm_2) # drop levels that are not used

mean(y_hat_lm_2 == "1")

#confusionMatrix(y_hat_lm_2, y) # A better result than just guessing!.


#############################################################
# Building the models
# Step 3
# K nearest neighbours (KNN)
#############################################################
set.seed(1)

countries <- unique(train$country) # grab all the unique countries

# use the knn alogrithm per country
accuracy_knn <- map_df(countries, function(c){
  train_knn <- train %>% # create a train set per country
    filter(country == c)
  
  test_knn <- test %>% # create a test set per country
    filter(country == c)
  
  # drop all the unused levels
  train_knn$ht <- droplevels(train_knn$ht)
  train_knn$at <- droplevels(train_knn$at)
  test_knn$ht <- droplevels(test_knn$ht)
  test_knn$at <- droplevels(test_knn$at)
  
  #trick to make sure all levels in train are also in the test set. Otherwise the predict function will give an error
  test_knn <- rbind(train_knn[1, ] , test_knn)
  test_knn <- test_knn[-1,] 
  
  ks <- seq(2, 20, 2) # define an array with the different number of K's that will be used
  
  map_df(ks, function(k){ # per country, use the knn3 algorithm
    # create a fit which can predict the result, using the ht (home team) and at (away team) as predictors
    fit_knn <- knn3(result ~ ht + at, train, k = k)
    # predic the result, using the fit on the test set
    y_hat_knn <- predict(fit_knn, newdata = test, type = "class")
    # determine the accuracy by comparing the predicted value with the actual value
    cM <- confusionMatrix(data = y_hat_knn, reference = test$result)$overall["Accuracy"]
    
    # list the results
    list(country=c, accuracy=cM, k = k)
  })
})


#############################################################
# Building the models
# Step 4
# Random Forests
#############################################################

set.seed(1)

countries <- unique(train$country) # grab all the unique countries

# use the knn alogrithm per country
accuracy_rf <- map_df(countries, function(c){
  train_rf <- train %>% # create a train set per country
  filter(country == c)
  
  test_rf <- test %>% # create a test set per country
  filter(country == c)
  
  # drop all the unused levels
  train_rf$ht <- droplevels(train_rf$ht)
  train_rf$at <- droplevels(train_rf$at)
  test_rf$ht <- droplevels(test_rf$ht)
  test_rf$at <- droplevels(test_rf$at)
  
  #trick to make sure all levels in train are also in the test set. Otherwise the predict function will give an error
  test_rf <- rbind(train_rf[1, ] , test_rf)
  test_rf <- test_rf[-1,]
  
  number_trees <- seq(10, 100, 10) # define an array with the different number of trees that will be used
  
  map_df(number_trees, function(n){
    # create a fit which can predict the result, using the ht (home team) and at (away team) as predictors
    fit_rf <- randomForest(result ~ ht + at, train_rf, ntree = n)
    # predic the result, using the fit on the test set
    y_hat_rf <- predict(fit_rf, test_rf)
    # determine the accuracy by comparing the predicted value with the actual value
    cM <- confusionMatrix(data = y_hat_rf, reference = test_rf$result)$overall["Accuracy"]
    
    # list the results
    list(country=c, accuracy=cM, n = n)
  })
})


#############################################################
# Results
#############################################################

y <- test$result # the actual results of the match in the 2017 season

# Educated guess
confusionMatrix(y_hat_ht, y)$overall["Accuracy"] # all home teams win
confusionMatrix(y_hat_at, y)$overall["Accuracy"] # all away teams win
confusionMatrix(y_hat_dr, y)$overall["Accuracy"] # only draws

# Lineair regression
confusionMatrix(y_hat_lm_1, y)$overall["Accuracy"] # with rounding
confusionMatrix(y_hat_lm_2, y)$overall["Accuracy"] # with integer value

# K nearest neighbours
accuracy_knn %>% ggplot(aes(k, accuracy, color = country)) + 
  geom_line()

accuracy_knn %>%
  group_by(k) %>%
  summarise(mean = mean(accuracy)) %>%
  filter(mean == max(mean))

# Random Forests
accuracy_rf %>% ggplot(aes(n, accuracy, color = country)) + 
  geom_line()

accuracy_rf %>%
  group_by(n) %>%
  summarise(mean = mean(accuracy)) %>%
  filter(mean == max(mean))

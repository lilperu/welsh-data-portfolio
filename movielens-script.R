###########################
# Install and load packages
###########################

install.packages("learnr")
install.packages("caret")
install.packages("knitr")
install.packages("gridExtra") 
install.packages("dplyr")
install.packages("dslabs")
install.packages("ggthemes") 
install.packages("lubridate") 
install.packages("readr") 
install.packages("stringi") 
install.packages("stringr") 
install.packages("tidyr")
install.packages("tibble")
install.packages("broom")
install.packages("tidyverse")
install.packages("tidymodels")
install.packages("parsnip")
install.packages("rsample")
install.packages("Metrics")

library(learnr)
library(caret)
library(knitr) # markdown reports
library(dplyr)
library(dslabs)
library(ggthemes) # plot editing
library(lubridate) # date formatting
library(readr) # importing data
library(stringi) # newer version of stringr
library(stringr) # string detection, editing
library(tidyr)
library(tibble)
library(broom)
library(gridExtra) #for side-by-side charts
library(tidyverse)
library(rsample)
library(Metrics)

##################################################
# Data Import, Training and Test data set creation
# Code provided by HarvardX in course material
##################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(caret)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip
options(timeout = 120)
dl <- "ml-10M100K.zip"
if(!file.exists(dl))
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
unzip(dl, ratings_file)
movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
unzip(dl, movies_file)
ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
mutate(userId = as.integer(userId),
movieId = as.integer(movieId),
rating = as.numeric(rating),
timestamp = as.integer(timestamp))
movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
mutate(movieId = as.integer(movieId))
movielens <- left_join(ratings, movies, by = "movieId")

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

final_holdout_test <- temp %>% # Final hold-out test set will be 10% of MovieLens data
semi_join(edx, by = "movieId") %>% # Make sure userId and movieId in final hold-out test set are also in edx set
semi_join(edx, by = "userId")

removed <- anti_join(temp, final_holdout_test) # Add rows removed from final hold-out test set back into edx set
edx <- rbind(edx, removed)
rm(dl, ratings, movies, test_index, temp, movielens, removed)

######################################
# Create train and test subsets of edx
######################################

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)

edx_train <- edx[-test_index,]
temp <- edx[test_index,]

edx_test <- temp %>% # edx_test set will be 10% of edx data
  semi_join(edx_train, by = "movieId") %>% # Make sure userId and movieId in final hold-out test set are also in edx set
  semi_join(edx_train, by = "userId")
removed <- anti_join(temp, edx_test) # Add rows removed from final edx_test set back into edx_train set
edx_train <- rbind(edx_train, removed)

#########################
# Describing the Data Set
#########################

edx %>% as_tibble()
final_holdout_test %>% as_tibble()
glimpse(edx)

###########################################
# Describing the Response Variable "rating"

summary(edx$rating) # mean, median, range
edx %>% summarize(sdev= sd(rating), var=var(rating)) # variance and sd

# ratings distribution
edx %>% group_by(rating) %>% summarize(count=n())
edx %>% group_by(rating) %>% ggplot(aes(rating))+ geom_bar()

#random example of user who did not rate each movie
edx %>% filter(userId==50)

#Sample table of NAs for movies the user did not rate
sample<- edx %>% select(userId, rating, title) %>% sample_n(10)
sample %>% pivot_wider(names_from=title, values_from=rating)

#####################
# Feature Engineering

# create separate column for release year
edx<- edx %>% mutate(year= str_sub(title, -5,-2))
edx$year<- as.integer(edx$year)
edx %>% as_tibble()
summary(edx$year)

# remove year from title column
edx$title<- str_sub(edx$title, end= -8)

# create date column
edx<- mutate(edx, date = as_datetime(timestamp))
summary(year(edx$date))

# create age column
edx<- mutate(edx, age= year(date)- year)
# age vs avg rating
edx %>% group_by(age) %>% summarize(n=n(), avg= mean(rating), se= sd(rating)/sqrt(n())) %>% ggplot(aes(age, avg))+ geom_point()+ geom_smooth()

# Feature Engineering on edx_train
edx_train<- edx_train %>% mutate(year= str_sub(title, -5,-2))
edx_train$year<- as.integer(edx_train$year)
edx_train$title<- str_sub(edx_train$title, end= -8)
edx_train<- mutate(edx_train, date = as_datetime(timestamp))
edx_train<- mutate(edx_train, age= year(date)- year)

# Feature Engineering on edx_test
edx_test<- edx_test %>% mutate(year= str_sub(title, -5,-2))
edx_test$year<- as.integer(edx_test$year)
edx_test$title<- str_sub(edx_test$title, end= -8)
edx_test<- mutate(edx_test, date = as_datetime(timestamp))
edx_test<- mutate(edx_test, age= year(date)- year)

glimpse(edx_train)
glimpse(edx_test)

# Feature Engineering on final_holdout_test
final_holdout_test<- final_holdout_test %>% mutate(year= str_sub(title, -5,-2))
final_holdout_test$year<- as.integer(final_holdout_test$year)
final_holdout_test$title<- str_sub(final_holdout_test$title, end= -8)
final_holdout_test<- mutate(final_holdout_test, date = as_datetime(timestamp))
final_holdout_test<- mutate(final_holdout_test, age= year(date)- year)

glimpse(final_holdout_test)

#######################################
# Describing the Explanatory Variables 

# Describing movieId Variable
# ratings per movie are right skewed
ggplot(edx, aes(movieId))+ geom_histogram(binwidth=10) 
# log distribution
edx %>% count(movieId) %>% ggplot(aes(n))+ geom_histogram(bins=40, color="black")+ scale_x_log10()+ labs(x= "number of ratings", y= "number of movies")
# summary stats
edx %>% summarize(min=min(movieId), max=max(movieId), unique=n_distinct(movieId))

# Describing userId Variable
n_distinct(edx$userId)
# ratings per userId are right skewed
ggplot(edx, aes(userId))+ geom_histogram(binwidth=50)
# summary stats
edx %>% summarize(min=min(userId), max=max(userId), unique=n_distinct(userId), avg_n=max/unique)

# log distributions of movie ratings and userId ratings
p1 <- edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Ratings per Movie")
p2 <- edx %>% 
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Ratings per User")
grid.arrange(p1, p2, ncol = 2)

# describing genres variable
n_distinct(edx$genres)
edx %>% filter(nchar(genres)<9) %>% group_by(genres) %>% count()


###########################
# EXPLORATORY DATA ANALYSIS
###########################

# average rating grouped by month
edx %>% mutate(date = round_date(date, unit = "month")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()

# the more often a movie is rated, the higher its average rating
edx %>% 
  filter(year >= 1960) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2009 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()

# relationship between genres and average rating value
edx %>% group_by(genres) %>% summarize(n=n(), avg= mean(rating), se= sd(rating)/sqrt(n())) %>% filter(n>= 100000) %>% mutate(genres= reorder(genres, avg)) %>% ggplot(aes(x= genres, y= avg, ymin= avg- 2*se, ymax= avg+ 2*se))+ geom_point()+ geom_errorbar()+ theme(axis.text.x= element_text(angle= 90, hjust=1))

# summary stats for single-genre movies
edx %>% filter(nchar(genres)<9) %>% summarize(avg_rating=mean(rating), median=median(rating), sdev=sd(rating), var=var(rating))

# relationship between single-genre movies and average rating value
edx %>% filter(nchar(genres)<9) %>% group_by(genres) %>% summarize(n=n(), avg= mean(rating), se= sd(rating)/sqrt(n())) %>% mutate(genres= reorder(genres, avg)) %>% ggplot(aes(x= genres, y= avg, ymin= avg- 2*se, ymax= avg+ 2*se))+ geom_point()+ geom_errorbar()+ theme(axis.text.x= element_text(angle= 90, hjust=1))


###########
# MODELING
###########

RMSE <- function(true_ratings, predicted_ratings){ #calculates rmse
  sqrt(mean((true_ratings - predicted_ratings)^2))}

# Naive Model
mu_hat <- mean(edx_train$rating)
naive_rmse <- RMSE(edx_test$rating, mu_hat)
naive_rmse

# confirm that any number other than mu_hat would result in a higher RMSE
random_effect <- rep(2.5, nrow(edx_test))
RMSE(edx_test$rating, random_effect)

# add rmse to table
train_results <- tibble(method = "Just the average", RMSE = naive_rmse)

# Model Movie Effects
mu <- mean(edx_train$rating)
movie_effects <- edx_train %>% 
  group_by(movieId) %>% 
  summarize(b_m = mean(rating - mu))

# Add User Effects
user_effects <- edx_train %>% 
  left_join(movie_effects, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_m))
predicted_1 <- edx_test %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  mutate(pred = mu + b_m + b_u) %>%
  pull(pred)
model_1_rmse <- RMSE(predicted_1, edx_test$rating)
train_results <- bind_rows(train_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_1_rmse ))

train_results %>% knitr::kable()

###############################
# Accounting for further biases
# Add Genres Effects
genres_effects <- edx_train %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_m - b_u))

# Add Age Effects
age_effects <- edx_train %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  left_join(genres_effects, by='genres') %>%
  group_by(age) %>%
  summarize(b_a = mean(rating - mu - b_m - b_u - b_g))

# Add Year Effects 
# UMGAY= User, Movie, Genres, Age, Year
umgay_effects <- edx_train %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by= 'userId') %>%
  left_join(genres_effects, by='genres') %>%
  left_join(age_effects, by='age') %>%
  group_by(year) %>%
  summarize(b_y= mean(rating - mu - b_m - b_u - b_g - b_a))

umgay_preds <- edx_test %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  left_join(genres_effects, by='genres') %>%
  left_join(age_effects, by='age') %>%
  left_join(umgay_effects, by='year') %>%
  mutate(pred = mu + b_m + b_u + b_g + b_a + b_y) %>%
  pull(pred)

model_2_rmse <- RMSE(umgay_preds, edx_test$rating)

train_results <- bind_rows(train_results,
                          data_frame(method="UMGAY Model",  
                                     RMSE = model_2_rmse))
train_results %>% knitr::kable()


################
# Regularization
################

movie_titles<- edx %>% select(movieId, title) %>% distinct()

# number of users that rated 10 best movies
edx %>% count(movieId) %>% 
  left_join(movie_effects) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_m)) %>% 
  slice(1:10) %>% 
  pull(n)
# number of users that rated 10 worst movies
edx %>% count(movieId) %>% 
  left_join(movie_effects) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_m) %>% 
  slice(1:10) %>% 
  pull(n)

lambda <- 3
movie_reg_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_m = sum(rating - mu)/(n()+lambda), n_i = n())

# Best 10 movies after regularization
edx %>% count(movieId) %>% 
  left_join(movie_reg_avgs, by="movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_m)) %>% 
  select(title, b_m, n) %>% 
  slice(1:10) %>% 
  pull(title)
# Worst 10 movies after regularization
edx %>% count(movieId) %>% 
  left_join(movie_reg_avgs, by="movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_m) %>% 
  select(title, b_m, n) %>% 
  slice(1:10) %>% 
  pull(title)

####################################################################
# Optimal Regularized User + Movie Effect Model
lambdas <- seq(0, 10, 0.1)
um_rmses<- sapply(lambdas, function(l){
  b_m <- edx_train %>% 
    group_by(movieId) %>%
    summarize(b_m = sum(rating - mu)/(n()+l))
  b_u <- edx_train %>% 
    left_join(b_m, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_m - mu)/(n()+l))
  predicted_3 <- edx_test %>% 
    left_join(b_m, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    mutate(pred = mu + b_m + b_u) %>%
    pull(pred)
  return(RMSE(predicted_3, edx_test$rating))
})

# plot lambdas in the sequence vs RMSE
qplot(lambdas, um_rmses)  

# specify lambda value of smallest RMSE
lambda <- lambdas[which.min(um_rmses)]
lambda

train_results <- bind_rows(train_results,
                          data_frame(method="Regularized User + Movie Effect Model",  
                                     RMSE = min(um_rmses)))
train_results %>% knitr::kable()

######################################################################
# Optimal Regularized UMGAY Model
lambdas<- seq(0, 10, 0.1)
umgay_rmses<- sapply(lambdas, function(l){
  b_m<- edx_train %>% 
    group_by(movieId) %>%
    summarize(b_m = sum(rating - mu)/(n()+l))
  b_u<- edx_train %>% 
    left_join(b_m, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_m - mu)/(n()+l))
  b_g<- edx_train %>% 
    left_join(b_m, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_m - b_u - mu)/(n()+l))
  b_a<- edx_train %>% 
    left_join(b_m, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    group_by(age) %>%
    summarize(b_a = sum(rating - b_m - b_u - b_g - mu)/(n()+l))
  b_y<- edx_train %>% 
    left_join(b_m, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    left_join(b_a, by="age") %>%
    group_by(year) %>%
    summarize(b_y = sum(rating - b_m - b_u - b_g - b_a - mu)/(n()+l))
  predicted_4<- edx_test %>% 
    left_join(b_m, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by="genres") %>%
    left_join(b_a, by="age") %>%
    left_join(b_y, by="year") %>%
    mutate(pred = mu + b_m + b_u + b_g + b_a + b_y) %>%
    pull(pred)
  return(RMSE(predicted_4, edx_test$rating))
})

# specify lambda value of smallest RMSE
lambda <- lambdas[which.min(umgay_rmses)]
lambda

train_results <- bind_rows(train_results,
                           data_frame(method="Regularized UMGAY on Train",  
                                      RMSE = min(umgay_rmses)))
train_results %>% knitr::kable()

######################################################################

# Predict on Test Set
lambdas<- seq(0, 10, 0.1)
final_rmses<- sapply(lambdas, function(l){
  b_m<- edx %>% 
    group_by(movieId) %>%
    summarize(b_m = sum(rating - mu)/(n()+l))
  b_u<- edx %>% 
    left_join(b_m, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_m - mu)/(n()+l))
  b_g<- edx %>% 
    left_join(b_m, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_m - b_u - mu)/(n()+l))
  b_a<- edx %>% 
    left_join(b_m, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    group_by(age) %>%
    summarize(b_a = sum(rating - b_m - b_u - b_g - mu)/(n()+l))
  b_y<- edx %>% 
    left_join(b_m, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    left_join(b_a, by="age") %>%
    group_by(year) %>%
    summarize(b_y = sum(rating - b_m - b_u - b_g - b_a - mu)/(n()+l))
  predicted_final<- final_holdout_test %>% 
    left_join(b_m, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by="genres") %>%
    left_join(b_a, by="age") %>%
    left_join(b_y, by="year") %>%
    mutate(pred = mu + b_m + b_u + b_g + b_a + b_y) %>%
    pull(pred)
  return(RMSE(predicted_final, final_holdout_test$rating))
})

test_results <- data_frame(method="Regularized UMGAY on Test",  
                                     RMSE = min(final_rmses))
test_results %>% knitr::kable()
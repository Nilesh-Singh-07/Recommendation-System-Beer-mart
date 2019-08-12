library(recommenderlab)
library(dplyr)
library(ggplot2)
beer_data <-read.csv(file.choose(), stringsAsFactors = F)

# Data cleaning
# General Data
View(beer_data)
dim(beer_data)
str(beer_data)
summary(beer_data)
# Checking missing value
sapply(beer_data, function(x) sum(is.na(x)))      # 0 NA values


# Checking for the duplicated values

# Remove duplicated rows based on 
# beer_beerid and review_profilename
beer_distinct <- beer_data %>% distinct(beer_beerid, review_profilename, .keep_all = TRUE)
# 474560 obs.

#######################################################################################################

# Data preparation

# 1.Choose only those beers that have at least N number of reviews

# count based on beer-id and profile name

beer_revw_count <- beer_data %>%
  group_by(review_overall) %>%
  summarise(revw_count=n()) %>% arrange(desc(revw_count))

# count based on distinct beer

beer_distinct_cnt <- beer_data %>%
  group_by(beer_beerid) %>%
  summarise(beer_count=n()) %>% arrange(desc(beer_count))

# total 40308 distinct beer are present

# count based on user

beer_count_user <- beer_data %>%
  group_by(review_profilename) %>%
  summarise(count=n()) %>% arrange(desc(count))

# total 22498 distict user are present.

# northyorksammy provided maximum reviews:1846


# ploting overall review count

ggplot(beer_revw_count,aes(x=beer_revw_count$review_overall,y=beer_revw_count$revw_count)) + geom_histogram(stat = "identity")

# beer with 4 reviews is maximum. 

# The median is 4. This is very important because it means the ratings are skewed toward high values.

summary(beer_count_user)

#review_profilename     count        
#Length:22498       Min.   :   1.00  
#Class :character   1st Qu.:   1.00  
#Mode  :character   Median :   3.00  
#Mean   :  21.16  
#3rd Qu.:  11.00  
#Max.   :1846.00  

# With 50 percent of people having done no more than 3 reviews
# with one person going crazy with more than 1800 reviews

summary(beer_revw_count)

#review_overall    revw_count    
#Min.   :0.000   Min.   :     6  
#1st Qu.:1.625   1st Qu.:  5766  
#Median :2.750   Median : 22389  
#Mean   :2.700   Mean   : 47598  
#3rd Qu.:3.875   3rd Qu.: 80484  
#Max.   :5.000   Max.   :174804


# minimum number of reviews are 6 which is for beer having 0.0 rating. 
# Removing beer with such a low rating 
# 25% of beer have close to 1.7 rating and 5766 reviews 
 

# so, choosing those beer which have more than 50 reviews as 6 would be too low 

beer_cnt_data <-subset(beer_distinct_cnt,beer_distinct_cnt$beer_count > 50)
beer_user_data <-subset(beer_count_user,beer_count_user$count > 50)

beer_sorted <- merge(beer_data,beer_cnt_data,by.x="beer_beerid",by.y="beer_beerid")
beer_sorted <- merge(beer_sorted,beer_user_data,by.x="review_profilename",by.y="review_profilename")

# Convert this data frame to a "realratingMatrix" before you build your collaborative filtering models

beer_rmatrix <- as(beer_sorted[,c(1,2,3)], "realRatingMatrix")

# getting info
dimnames(beer_rmatrix)  
rowCounts(beer_rmatrix) 
colCounts(beer_rmatrix) 
rowMeans(beer_rmatrix)

# coerce the matrix to a dataframe
beer_df <- as(beer_rmatrix, "data.frame")
str(beer_df)

# Data Exploration

# Determine how similar the first ten users are with each other and visualise it

similar_users <- similarity(beer_rmatrix[1:10,],method = "cosine",which = "users") 
#Similarity matrix

as.matrix(similar_users)

#Visualise similarity matrix

image(as.matrix(similar_users), main = "User similarity")

# Compute and visualise the similarity between the first 10 beers

similar_beer <- similarity(beer_rmatrix[,1:10 ],
                            method = "cosine",
                            which = "items")
as.matrix(similar_beer)

image(as.matrix(similar_beer), main = "Item similarity")

# What are the unique values of ratings?
  
beer_df %>% group_by(rating) %>% 
  summarise(revw_count=n()) %>% View()

# there are 26 different ratings

#4.Visualise the rating values and notice:
  
#  The average beer ratings
average_Bratings<-beer_df %>% group_by(item) %>% summarise(avg_beer_rating=mean(rating))
# plot

ggplot(average_Bratings,aes(x=avg_beer_rating)) + geom_histogram(bins=30)+
  ggtitle("Distribution of the average beer rating")

#  The average user ratings
average_Uratings<-beer_df %>% group_by(user) %>% summarise(avg_user_rating=mean(rating))
# plot

ggplot(average_Uratings,aes(x=avg_user_rating)) + geom_histogram(bins=30) +
  ggtitle("Distribution of the average user rating")

#  The average number of ratings given to the beers

beer_avg_rating<-beer_sorted %>% group_by(beer_beerid) %>% summarise(average_reviews=mean(review_overall))
# plot

ggplot(beer_avg_rating,aes(x=average_reviews)) + geom_histogram(bins=30) +
  ggtitle("Distribution of the average rating per beer")

#  The average number of ratings given by the users

user_avg_rating <-beer_sorted %>% group_by(review_profilename) %>% summarise(average_reviews=mean(review_overall))
# plot

ggplot(user_avg_rating,aes(x=average_reviews)) + geom_histogram(bins=30) +
  ggtitle("Distribution of the average rating given by user ")

####################################################################################################################################
#3. Recommendation Models

# Divide your data into training and testing datasets

# Experiment with 'split' and 'cross-validation' evaluation schemes
#?evaluationScheme
# split

scheme <- evaluationScheme(beer_rmatrix, method = "split", train = .75,
                           k = 1, given = 1, goodRating = 4)
scheme
# cross-validation with 10 fold 

scheme1 <- evaluationScheme(beer_rmatrix, method = "cross-validation",k = 10, given = 1, goodRating = 4)
scheme1

# Build IBCF and UBCF models

algorithms <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=30, minRating=3)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
  ))
)

# run algorithms, predict next beer rating 
results <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10, 15, 20))
class(results)

results1 <- evaluate(scheme1, algorithms, n=c(1, 3, 5, 10, 15, 20))
class(results1)

# Draw ROC curve
plot(results, annotate = 1:4, legend="topleft")
plot(results1, annotate = 1:4, legend="topleft")

#Give the names of the top 5 beers that you would recommend to the users "cokes", "genog" & "giblet"
recc_model <- Recommender(data = beer_rmatrix, method = "UBCF")
recc_model
## Recommender of type 'UBCF' for 'realRatingMatrix'
## learned using 2147 users
#Let's extract some details about the model using getModel:
model_details <- getModel(recc_model)
#Let's take a look at the components of the model:
  names(model_details)


#Apart from the description and parameters of model, model_details contains a
#data slot:
  model_details$data
## 2147 x 2034 rating matrix of class 'realRatingMatrix' with 43846
#ratings.
## Normalized using center on rows.
#The model_details$data object contains the rating matrix. The reason is that UBCF
#is a lazy-learning technique, which means that it needs to access all the data to
#perform a prediction

  n_recommended <- 5
  recc_predicted_cokes <- predict(object = recc_model,
                            newdata = beer_rmatrix['cokes'], n = n_recommended)

as(recc_predicted_cokes, "list")
#recommendation for cokes: "19426" "1153"  "198"   "47658" "7971" 

recc_predicted_genog <- predict(object = recc_model, beer_rmatrix['genog'], n=n_recommended)
as(recc_predicted_genog, "list")
#recommendation for geong: "2093" "6075" "571"  "782"  "1010"  

recc_predicted_giblet <- predict(object = recc_model, beer_rmatrix['giblet'], n=n_recommended)
as(recc_predicted_giblet, "list")
#recommendation for giblet: "10325" "977"   "7971"  "19960" "1545" 
## Assignment  No -1 DANA 4830 
## Topic -Clustering Analysis  And Clustering Distances
## Clustering can be defined as group of similar observations within a dataset .When we cluster the dataset we want the observations within the dataset to be similar and observations in different groups to be dissimilar, And when there is no response variable this is an unspervised method of learning .Clustering helps in categoirising the observations into one and categorises them And in this assignment we will be using k means clustering method which is the simplest one
##Before Clustering there is a need to prepare the data as well so we need to take few things into consideration one we have to have a clean dataset and the other thing missing values should be treated and the values should be standardised meaning the eman should be zero and the standard deviation should be one
## Question 1 (Data cleaning and validation) (10 Bonus marks)
## Clean the provided dataset (do not remove any column), store it as "df".
## loading the dataset 
 bank.market_final <- read.csv("C:/Users/16043/Downloads/bank-market_final.csv")
 View(bank.market_final)
 str(bank.market_final)## Str function clearly demonstrates the internal structure of the dataset
 ## checking the missing values
 library(visdat)
 vis_dat(bank.market_final)
 vis_miss(bank.market_final) 
 ## Deduction there are less missing values present in the dataset 
 sum(is.na(bank.market_final))
 ## So this indicates that there is one missing record in the dataset 
 df <- read.csv("C:/Users/16043/Downloads/bank-market_final.csv")
 View(df)
str(df) 
dim(df)
unique(df)
# check NA values
sum(is.na(df)) 

# remove NA
df <-df[complete.cases(df),] 
sum(is.na(df))
#check if any duplicate rows
sum(duplicated(df)) 
# remove duplicates
distinct(df)
 
# We checked by making the boxplot for detecting the outliers 
windows()
library(ggplot2)
hist(df$age)
boxplot(df$age)
# Remove duplicate rows of the dataframe using NAME variable
df = distinct(df, customerid, .keep_all= TRUE)
dim(df)
library(dplyr)
df %>% filter(age>18)
dim(df)
df %>% filter(duration>0)
dim(df)
# Variable 'Balance'
hist(df$balance)
boxplot(df$balance)

# selecting negative balance records

neg_bal <- df[df$balance <0,]
neg_bal

# 85 rows customers have negative balance

# variable 'duration'
hist(df$duration)
boxplot(df$duration)

# variable 'campaign'
hist(df$campaign)
boxplot(df$campaign)

# variable 'pdays'
hist(df$pdays)
boxplot(df$pdays)

# variable 'previous'
hist(df$previous)
boxplot(df$previous)


######################################################

install.packages('radiant')
library(radiant)
install.packages('timetk')
library(timetk)
data(df)

radiant()
##Question 2 (Clustering Comparison 1) (10 marks)
##(a) Remove the column of "customerid" from the "df". Use daisy() to calculate the Gower
##Distance of "df" and store the result as "gd1".
df1<-df[,-1] ## Deleting the customerid column from dataset
View(df1)
str(df1)
##we converted all the charector variables into factors because the k means clustering is a method to find the centroid points and every points inThe idea behind the k-Means clustering algorithm is to find k-centroid points and every point in the dataset will belong to either of the k-sets having minimum Euclidean distance. The k-Means algorithm is not applicable to categorical data, as categorical variables are discrete and do not have any natural origin
df1$marital=as.factor(df1$marital)
df1$education=as.factor(df1$education)
df1$housing=as.factor(df1$housing)
df1$loan=as.factor(df1$loan)
df1$deposit=as.factor(df1$deposit)
install.packages('cluster')
library(cluster)
##The Gower distance is a metric that measures the dissimilarity of two items with mixed numeric and non-numeric data. 
gd1 <-daisy(df1, metric = "gower")
gd1
library(factoextra)
library(ggplot2)
fviz_dist(gd1)
##(b) Use the "gd1" to perform k-means with k = 2 with seed = 123. Store the result as "km1".
set.seed(123) # set seed 
km1<-kmeans((gd1),2) # perform k-means with k=2 
# Cluster number for each of the observations
km1$cluster
## Cluster size
km1$size ##which the no of observations in each cluster
### Cluster means
km1$centers
##(c) Remove the column "current" and store the new dataset as "df2". Use daisy () to
##calculate the Gower Distance of "df2" and store the result as "gd2".
library(dplyr)
df2 <-select(df1, -11)
str(df2)
gd2 <-daisy(df2, metric = "gower")
gd2
library(factoextra)
library(ggplot2)
fviz_dist(gd2)
##(d) Use the "gd2" to perform k-means with k = 2 with seed = 123. Store the result as "km2".
set.seed(123) # set seed 
km2<-kmeans((gd2),2) 
# Cluster number for each of the observations
km2$cluster
## Cluster size
km2$size ##which the no of observations in each cluster
### Cluster means
km2$centers
##(e) Compare the clustering result of "km1" and "km2" (no need to see the clusters'characteristics). What do you observe and why? (Word limit: 50)
table(km1 = km1$cluster, km2 = km2$cluster)
##There is no misclassification as far as looking at the table because the removal of that variable is not causing any effect to that cluster and then further we checked the sw values were also same so which meant that the objects were similar to thier clusters a
# create function to calculate sw (shihoutte) and CH score (sw() & CH())
set.seed(123)
library(cluster)

sw<-function(km, d) 
{
  s<-silhouette(km$cluster, d)
  s<-mean(s[,3])
  return(s)
}

CH<-function(km)
{
  penalty<-(length(km$cluster)-length(km$size))/(length(km$size)-1) # penalty for increasing k
  ch<-penalty*km$betweenss/sum(km$withinss) # calculate CH Score
  return (ch)
}
# comparison using shihoutte index -> 
sw1 = sw(km1, gd1)
sw1
sw2 = sw(km2, gd2)
sw2

# comparison using CH index -> 
ch1 = CH(km1) 
ch1
ch2 = CH(km2) 
ch2


##################################################################################


##Question 3 (Clustering Comparison 2) (10 marks)
##(a) Select all numeric columns from "df"1 and store them as a new dataset "df_num". Use daisy() to calculate the Gower Distance of "df_sum" and store the result as "gd_num".
num_cols <- unlist(lapply(df1, is.numeric))         
num_cols
df_num <- df1[ , num_cols]                        
df_num
gd_num <-daisy(df_num, metric = "gower")
gd_num
##(b) Use the "gd_num" to perform k-means with k = 2 with seed = 123. Store the result as"km_num"
set.seed(123) # set seed 
km_num<-kmeans(scale(gd_num),2) # perform k-means with k=2
# Cluster number for each of the observations
km_num$cluster
## Cluster size
km_num$size ##which the no of observations in each cluster
### Cluster means
km_num$centers
##(c) Use scale() on "df_num" and store the result as "df_scale". Use daisy() to calculate the Gower Distance of "df_scale" and store the result as "gd_scale".
df_scale<- scale(df_num)
gd_scale<-daisy(df_scale,metric='gower')
gd_scale
##(d) Use the "gd_scale" to perform k-means with k = 2 with seed = 123. Store the result as"km_scale".
set.seed(123) # set seed 
km_scale<-kmeans(scale(gd_scale),2)
# Cluster number for each of the observations
km_scale$cluster
## Cluster size
km_scale$size ##which the no of observations in each cluster
### Cluster means
km_scale$centers
##(e) Compare the clustering result of "km_num" and "km_scale" (no need to see the clusters'characteristics). What do you observe and why? (Word limit: 50)
table(km_num = km_num$cluster, km_scale = km_scale$cluster)
##So looking at the table I figured out that there was no missclassification as the clusters in both km_num and km_scale were similar  and the values of sw were also the same 
# comparison using shihoutte index ->
sw3 = sw(km_scale, gd_scale)
sw3
sw4 = sw(km_num, gd_num)
sw4

# comparison using CH index -> 
ch3 = CH(km_scale) 
ch3
ch4 = CH(km_num) 
ch4
##############################################################################################


##Question 4 (Clustering Comparison 3) (6 marks)
##(a) Use dist() to calculate the Euclidean Distance of "df_num" and "df_scale", and store the results as "eu" and "eu_scale" respectively.
## calculate the euclidean distance
eu<-dist(df_num)
eu
distance <- get_dist(df_num)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
eu_scale<-dist(df_scale)
eu_scale
distance <- get_dist(df_scale)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
#(b) Use "eu" and "eu_scale" to perform k-means with k = 2 with seed = 123 respectively.Store the results as "km_eu" and "km_eu_scale".
set.seed(123) 
km_eu<-kmeans(scale(eu),2)
# Cluster number for each of the observations
km_eu$cluster
## Cluster size
km_eu$size ##which the no of observations in each cluster
### Cluster means
km_eu$centers
set.seed(123)
fviz_nbclust(df_num, kmeans, method = "wss")
set.seed(123) 
km_eu_scale<-kmeans(scale(eu_scale),2)
# Cluster number for each of the observations
km_eu_scale$cluster
## Cluster size
km_eu_scale$size ##which the no of observations in each cluster
### Cluster means
km_eu_scale$centers
##(c) Compare the clustering result of "km_eu " and "km_eu_scale" (no need to see the clusters' characteristics). What do you observe and why? (Word limit: 50)
table(km_eu = km_eu$cluster, km_eu_scale = km_eu_scale$cluster)

## There is a misclassification in the table as in one technique the data points are differently classified and in the other the data points are scaled so which marks the missclassification
 ###############################################################################


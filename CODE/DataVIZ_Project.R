#Nessecarp packages to be installed
install.packages("plotly")
install.packages("ggplot2")
install.packages("readr")
install.packages("dplyr")
install.packages("corrplot")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("Hmisc")
install.packages("rpart.plot")
install.packages("e1071")
install.packages("randomForest")
install.packages("forecast")
install.packages("C50")

#import their libraries
# ggplot2 for doing the data visualization
library(ggplot2)
#Reading the csv file
library(readr)
library(dplyr)
#for plotting purposes
library(plotly)
library(tidyverse)
library(lubridate)
plot_ly

#Taking input of dataset in train_data
train_data = read.csv("/Users/prakhardungarwal/Desktop/malitious_dataset.csv",header = TRUE)

#viewing the dataset
glimpse(train_data)

## AN OVERVIEW OF THE DATASET

#head returns the first 6 entries of each attributes
head(train_data)

#dimmension of the dataset
# It gives dimmension as rows * columns
dim(train_data)

# Structure of the dataset
str(train_data)

#summary of the dataset
summary(train_data)

#no. of missing values in the dataset
# returns sum of columns having missing value
colSums(sapply(train_data, is.na))

## EXPLORING THE DATASET

#all variables
variable.names(train_data)

#Categorising variables as numeric
#selects if var is numeric
numeric_var = select(train_data, is.numeric)
colSums(sapply(numeric_var, is.na))
numeric_var

#Categorising variables as categorical
#selects if var is categorical
categorical_var = select(train_data, is.character)
colSums(sapply(categorical_var, is.na))
categorical_var

#Explore the type attribute
# no. of 0's and 1's in Type
table(train_data$Type)

#categorising charset with tyoe
#getting charset info for type 0 and type 1
with(train_data, table(Type, CHARSET))

#categorising country with tyoe
#getting country info for type 0 and type 1
with(train_data, table(Type, WHOIS_COUNTRY))

# PLOTTING THE TARGET FEATURE
#resize plot with width = 5 and height = 5
options(repr.plot.width = 5,repr.plot.height=5)
#bar plot of Type attribute
ggplot(train_data, aes(x = Type)) + geom_bar(color='red',fill="#1dde91")

# PLOT TARGET FEATURES WITH NUMERIC FEATURES

#plots a histogram for url_length and count of these found
ggplot(train_data, aes(x = URL_LENGTH)) + geom_histogram(binwidth = 5, fill = "#0571f5")+ scale_fill_hue(l=40, c=35)

library(purrr)
library(tidyr)

# Removing non numeric data
train_data %>%
  #keep only numeric columns
  keep(is.numeric)%>%
  # Convert to key-value pairs
  gather() %>%
  # Plot the values
  ggplot(aes(value)) +
  # In separate panels
    facet_wrap(~key, scales = "free") +
  # as histograms
    geom_histogram()


#PLOTTING THE NUMERIC AND CAGEORICAL FEATURES
#ploting REMOTE_IPS with URL_LENGTH
ggplot(train_data, aes(x=REMOTE_IPS,y=URL_LENGTH)) + geom_point(fill="black")

#REMOTE_APP_PACKETS vs SOURCE_APP_PACKETS
ggplot(train_data, aes(REMOTE_APP_PACKETS, y=SOURCE_APP_PACKETS)) + 
  geom_boxplot(color = 'blue')

#WEBSITE TYPE vs TCP CONVERSATION
boxplot(TCP_CONVERSATION_EXCHANGE~Type,data=train_data, main="Tcp Conversation exchange vs Type",
        xlab="Website Type", ylab="TCP conversation", col=(c("yellow","red")))

# CHARSET vs REMOTE TCP
boxplot(DIST_REMOTE_TCP_PORT~CHARSET,data=train_data, main="Dist Remote Tcp Port vs Type",
       xlab="Charset", ylab="Remote TCP Port", col=(c("red","darkgreen")))

# DATA PREPERATION

#getting first 6 missing rows
missing_row = train_data[!complete.cases(train_data),]
head(missing_row)

#no. of missing row
nrow(missing_row)

#ratio of missing rows
sum(is.na(train_data))/(nrow(train_data)*ncol(train_data))

#ratio of missing values
pMiss = function(x){sum(is.na(x))/length(x)*100}
apply(train_data,2,pMiss)

#impute the missing values
# for QUERY_TIMES we replace with 0
# for SERVER we replace with 'ngnix'
# for CONTENT_LENGTH we replace with mean length
# impute allows filling up of missing feature values through various techniques
library(Hmisc)
train_data$DNS_QUERY_TIMES=impute(train_data$DNS_QUERY_TIMES, 0)
train_data$SERVER=impute(train_data$SERVER, 'nginx')
train_data$CONTENT_LENGTH=impute(train_data$CONTENT_LENGTH, mean)

#DEAL WITH OUTLIERS
# Extract numeric attributes
train_cap = train_data[,c("REMOTE_APP_PACKETS", "URL_LENGTH", "SOURCE_APP_PACKETS","REMOTE_APP_BYTES","SOURCE_APP_BYTES",
                     "APP_BYTES", "REMOTE_IPS", "DIST_REMOTE_TCP_PORT","TCP_CONVERSATION_EXCHANGE")]
# seperate attr which do not need capping
train_other = train_data[,-c(2,11,12,13,14,15,16,17,18)]
head(train_other)

#outlier capping fucntion
#For missing values that lie outside the limits,
# we could cap it by
# replacing those observations outside the lower limit with the value of 5th %ile
# and those that lie above the upper limit, with the value of 95th %ile. 
# only for the numeric values
pcap <- function(x){
  for (i in which(sapply(x, is.numeric))) {
    quantiles <- quantile( x[,i], c(.05, .95 ), na.rm =TRUE)
    x[,i] = ifelse(x[,i] < quantiles[1] , quantiles[1], x[,i])
    x[,i] = ifelse(x[,i] > quantiles[2] , quantiles[2], x[,i])}
  x}

# Replacing extreme values with percentiles
train_capped = pcap(train_cap)

#join back with rest of data;
train_combine = cbind(train_other, train_capped)
summary(train_combine)

#MODELING THE DATA
# sample takes a sample of the specified size from the elements of train_combine
indexes = sample(1:nrow(train_combine), size=0.2*nrow(train_combine))

# Split data
# Create a test data
x_test = train_combine[indexes,]
dim(x_test) 
# Create a train data from remaining rows
x_train = train_combine[-indexes,]
# HERE WE HAVE CREATED TRAIN AND TEST DATASET
dim(x_train)

#Load Train and Test datasets
#Identify feature and response variable(s) and numeric values
# y_train selects Type attribute in combined dataset
# 1 is for malitious and 0 is for amiable
y_train <- train_combine$Type

# Train the model using the training sets and check score
# lm(), or “linear model,” function
# is used to create a simple regression model.
linear <- lm(y_train ~ ., data = train_cap)
summary(linear)
#predicted output
# By comparing trained and test dataset
# predicts the url as type 0 or type 1 probability on basis of test dataset
# y_train selects Type attribute in combined dataset
# 1 is for malitious and 0 is for amiable
predicted= predict(linear,x_test)
predicted

# Train the model using the training sets and check score
# glm is used to fit generalized linear models,
# according to type 0 or 1 in y_train
# 1 is for malitious and 0 is for amiable
# family is a description of the error distribution and link function to be used in the model. 
logistic <- glm(y_train ~ ., data = train_cap,family='binomial')
summary(logistic)
#Predict Output
predicted= predict(logistic,x_test)
predicted

## IMPLEMENTING ML ALGORITHMS

###################Decision Tree#######################

## Decision tree is a graph to represent choices and their results in form of a tree.
## The nodes in the graph represent an event or choice and
## the edges of the graph represent the decision rules or conditions.
library(rpart)
library(rpart.plot)
# grow tree 
# rpart used for Recursive Partitioning And Regression Trees
# rpart.control is for control for Control For Rpart Fits
# cp is complexity parameter.
# Any split that does not decrease the overall lack of fit
# # by a factor of cp is not attempted.
fit <- rpart(y_train ~ ., data = train_cap ,method="class",control = rpart.control(cp = 0.01))
#try with train_other, train_combine
summary(fit)
#Predict Output 
predicted= predict(fit,x_test)
plotcp(fit)
printcp(fit)
rpart.plot(fit, 
           box.palette="RdYlGn",
           branch.lty=3, shadow.col="gray", nn=TRUE)

## DECISION TREE MODEL SHOWS HOW A WEBSITE IS CLASSIFIED AS MALITIOUS OR AMIABLE
## BASED ON THE NETWORK AND APPLICATION LAYER CHARACTERISTICS IN DATASET
## IT CLASSIFIES INTO TYPE 0-AMIABLE AND TYPE 1-MALITIOUS BASED ON 
## CHARACTERISTICS LIKE TCP PORT URL LENGTH ETC AND FORMS A DECISION TREE FOR CLASSIFICATION


######################### Random Forest ###########################

## Random forest builds multiple decision trees (called the forest) 
## and glues them together to get a more accurate and stable prediction.
##  The forest it builds is a collection of Decision Trees, which are trained 
## can also be used in unsupervised mode for
## assessing proximities among data points.
library(randomForest)
library(forecast)
# Fitting model
# na.action is A function to specify the action to be taken if NAs are found. 
fit <- randomForest(y_train ~ ., train_cap,importance =TRUE,ntree=500,nodesize=7, na.action=na.roughfix)
summary(fit)
# plotting the graph
## varImp is A generic method for calculating variable importance
## for objects produced by train and method specific methods
varImpPlot(fit, type=1) 
#Predict Output 
predicted= predict(fit,x_test)

## The plot shows us that the most important
## Property used to classify an URL as malitious
## or amiable is URL_LENGTH as its %IncMSE is highest


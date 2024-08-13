# Read the File
data <- read.csv("kc_house_data.csv",header = T,sep = ",")

summary(data)
# Change the Datatype into Date type
data$date <- as.Date(data$date,format = '%Y%m%dT000000')
str(data$date)

library(lubridate)
# Calculate the House Age
data$age <- year(data$date)-data$yr_built
# Covert Waterfront and yr_renovated columns into Factor
data$waterfront <- factor(data$waterfront)
data$yr_renovated <- factor(ifelse(data$yr_renovated == 0,0,1))

# Drop the unnecessary Columns
my_data <- data[,-c(1:2,6:7,15,17:19)]

# Remove the Null Values
summary(my_data)
my_data <- na.omit(my_data)

# Segregate data into Train and Test
set.seed(1234)
index <- sample(1:nrow(my_data), .80*nrow(my_data))
train_data <- my_data[index,]
test_data <- my_data[-index,]

# Build the Model
options(scipen = 10) # remove the scientific notation 
model <- lm(price ~ .,data = train_data)
summary(model)

# Explain the Model
# R-Square : it explains the contribution of Independent Variables on Direct Variables.
#            it means that all the IVs explains the contribution on DVs is 0.6493 i.e 64% rest is error i.e 36%

# Age : 3368.04 that means increase in age of house (for every one year) the price increases by 3368.
#       positive sign explains increase in Age increases the Price


### Check some assumptions
### check multicolinearity in the model whether IVs are highly correlated or not 

install.packages("car")
library(car)
## VIF : variance inflaction Factor
vif(model)

## Thumb Rule : if any of these having Vif >5, Then its multicolinearity Problem
## since, here none of these having vif >5, so no multicolinearity problem exists.

train_data$residual <- model$residuals

Q3 <- quantile(train_data$residual, p=.75)
Q3
Q1 <- quantile(train_data$residual, p=.25)
Q1
iqr <- IQR(train_data$residual)
iqr

library(dplyr)
train_no_out <- train_data %>% filter(residual < Q3+(1.5*iqr) & residual > Q1-(1.5*iqr))
train_out <- train_data %>% filter(residual >= Q3+(1.5*iqr) | residual <= Q1-(1.5*iqr))  

boxplot(train_data$residual)
boxplot(train_out$residual)
boxplot(train_no_out$residual)

# Run the revised model using no-outliers

train_no_out <- train_no_out %>% select(-residual)
model_no_out <- lm(price ~ ., data = train_no_out)
summary(model_no_out)
 ### errors or outliers should be normally distributed

hist(model$residuals)
hist(model_no_out$residuals)


test_data$predicted <- predict(model_no_out,newdata = test_data)

install.packages("Metrics")
library(Metrics)

rmse <- rmse(test_data$price,test_data$predicted)
rmse
mape <- mape(test_data$price,test_data$predicted)
mape
accuracy <- 1-mape
accuracy

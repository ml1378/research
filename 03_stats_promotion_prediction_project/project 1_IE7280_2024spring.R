
library(dplyr)  # For data manipulation
library(tidyr)  # For data tidying
library(glmnet) # For logistic regression
library(caret)  # For model training and evaluation
library(Metrics) 

project1 <- read.table("C:\\Users\\mingl\\Desktop\\iE7280 spring 2024\\Project 1 data.csv", header=TRUE, sep= ",")

project1a <- project1[, 1:17]


#dim(project1a %>% filter(slstyr == 0 & ordtyr != 0))[1]
#dim(project1a %>% filter(slstyr != 0 & ordtyr == 0))[1]

#dim(project1a %>% filter(slslyr == 0 & ordlyr != 0))[1]
#dim(project1a %>% filter(slslyr != 0 & ordlyr == 0))[1]

#dim(project1a %>% filter(sls2ago == 0 & ord2ago != 0))[1]
#dim(project1a %>% filter(sls2ago != 0 & ord2ago == 0))[1]

#dim(project1a %>% filter(sls3ago == 0 & ord3ago != 0))[1]
# dim(project1a %>% filter(sls3ago != 0 & ord3ago == 0))[1]

#hist(project1a[project1a$targdol>0, ]$targdol, main = "Histogram of response customer", xlab = "purchase value", ylab = "counts",  col = "blue" )
#hist(project1a[project1a$targdol>0, ]$datelp6, main = "Histogram of response customer", xlab = "purchase value", ylab = "counts",  col = "blue" )

#barplot(table(project1a[project1a$targdol>0, ]$datelp6))


summary(project1a)


remaining_data_1 <- project1a %>% filter(!(slstyr == 0 & ordtyr != 0))

remaining_data_2 <- remaining_data_1 %>% filter(!(slslyr == 0 & ordlyr != 0))

remaining_data_3 <- remaining_data_2  %>% filter(!(sls2ago == 0 & ord2ago != 0))

remaining_data_4 <- remaining_data_3 %>% filter(!(sls3ago == 0 & ord3ago != 0))


#dim(remaining_data_4 %>% filter(slstyr == 0 & ordtyr != 0))[1]
#dim(remaining_data_4 %>% filter(slstyr != 0 & ordtyr == 0))[1]

#dim(remaining_data_4 %>% filter(sls3ago == 0 & ord3ago != 0))[1]
#dim(remaining_data_4 %>% filter(sls3ago != 0 & ord3ago == 0))[1]

remaining_data_4 <- remaining_data_4 %>%
  mutate(avgstyr= ifelse(ordtyr != 0, slstyr/ordtyr, 0))

remaining_data_4 <- remaining_data_4 %>%
  mutate(avgslyr = ifelse(ordlyr != 0, slslyr/ordlyr, 0))

remaining_data_4 <- remaining_data_4 %>%
  mutate(avgs2ago = ifelse(ord2ago != 0, sls2ago/ord2ago, 0))

remaining_data_4 <- remaining_data_4 %>%
  mutate(avgs3ago = ifelse(ord3ago != 0, sls3ago/ord3ago, 0))

remaining_data_4 <- remaining_data_4 %>%
  mutate(tyr= ifelse(ordtyr != 0, 0.4, 0))

remaining_data_4 <- remaining_data_4 %>%
  mutate(lyr = ifelse(ordlyr != 0, 0.3, 0))

remaining_data_4 <- remaining_data_4 %>%
  mutate(y2ago = ifelse(ord2ago != 0, 0.2, 0))


remaining_data_4 <- remaining_data_4 %>%
  mutate(y3ago = ifelse(ord3ago != 0, 0.1, 0))


remaining_data_4 <- remaining_data_4 %>%
  mutate(consis = tyr + lyr + y2ago + y3ago)


remaining_data_4 <- remaining_data_4 %>%
  mutate(avgstyr= ifelse(ordtyr != 0, 0.4, 0))

remaining_data_4 <- remaining_data_4 %>%
  mutate(avgslyr = ifelse(ordlyr != 0, 0.3, 0))

remaining_data_4 <- remaining_data_4 %>%
  mutate(avgs2ago = ifelse(ord2ago != 0, 0.2, 0))

remaining_data_4 <- remaining_data_4 %>%
  mutate(avgs3ago = ifelse(ord3ago != 0, 0.1, 0))


#remaining_data_4 <- select(remaining_data_4, -ordtyr, -slstyr, -slslyr, -ordlyr, -sls2ago,-ord2ago, -sls3ago, -ord3ago)
  
remaining_data_4 <- mutate(remaining_data_4, weight_avg_s = 0.4*avgstyr + 0.3*avgslyr + 0.2*avgs2ago + 0.1*avgs3ago)   

remaining_data_4 <- select(remaining_data_4, -avgstyr, -avgslyr, -avgs2ago, -avgs3ago)  

remaining_data_4 <- remaining_data_4 %>%
  mutate(fal = ifelse(falord != 0, 0.5, 0))

remaining_data_4 <- remaining_data_4 %>%
  mutate(spr = ifelse(sprord != 0, 0.5, 0))

remaining_data_4 <- mutate(remaining_data_4, f_S = fal + spr)

#remaining_data_4 <- select(remaining_data_4, -falord, -sprord, -fal, -spr)


remaining_data_4$Date_add_file <- as.Date(remaining_data_4$datead6, , format = "%m/%d/%Y")

remaining_data_4$Date_last_pur <- as.Date(remaining_data_4$datelp6, format = "%m/%d/%Y")

remaining_data_4 <- select(remaining_data_4, -datead6, -datelp6, -lpuryear)

remaining_data_4$Year_add_file <- as.integer(format(remaining_data_4$Date_add_file, "%Y"))
remaining_data_4$Year_last_pur <- as.integer(format(remaining_data_4$Date_last_pur, "%Y"))

remaining_data_4$Year_gap_add_file <- 2013 - remaining_data_4$Year_add_file
remaining_data_4$Year_gap_last_pur <- 2013 - remaining_data_4$Year_last_pur
#remaining_data_4 <- select(remaining_data_4, -Date_add_file , -Date_last_pur, -Year_add_file, -Year_last_pur)
remaining_data_4 <- mutate(remaining_data_4, LTD_S_por = slshist  + ordhist)
remaining_data_4 <- mutate(remaining_data_4, LTD_avg_s = slshist/Year_gap_add_file)
remaining_data_4 <- mutate(remaining_data_4, LTD_avg_o = ordhist/Year_gap_add_file)

#str(project1a)

# hist(project1a$train, main = "Histogram of data splitting", xlab = "Train/Test dataset", ylab = "counts",  col = "blue")


remaining_data_4$log_purchase <- log(remaining_data_4$targdol + 1)
#table(project1a[project1a$targdol>0, ]$datelp6)

#project1a$datelp6
#project1a$lpuryear
#dim(subset(project1a, fal_spr != ordhist))[1]


train <- remaining_data_4[remaining_data_4$train == 0, ]
#train <- select(train, -train)

#missing_values <- sum(is.na(remaining_data_4))
#missing_values


test <-remaining_data_4[remaining_data_4$train == 1, ]
#test <- select(test, -test)



train$tar <- ifelse(train$log_purchase > 0, 1, 0)

train_1 <- train[train$tar==1, ]
# dim(train_1)[1]/dim(train)[1]

train_0 <- train[train$tar==0, ]

oversampled_data_1 <- train_1[rep(1:nrow(train_1), each = 10), ]
oversampled_data <- rbind(oversampled_data_1, train_0)


  
predictor <- c( "ordhist",'ordtyr', 'slstyr', 'slslyr', 'ordlyr', 'sls2ago','ord2ago', 'sls3ago', 'ord3ago')  

#  , 'falord', 'sprord'"slshist",
predictor_string <- paste("tar ~", paste(predictor, collapse = "+"))
logit_model <- glm(predictor_string, data = oversampled_data, family = binomial)

mq <- 5.4562*(1-0.0925)/(1-(5.4562*0.0925))

k <- (predict(logit_model, test,  type = "response"))/((1-predict(logit_model, test,  type = "response"))*mq)

test$test_y_1_prob <- k/(1+k)



#predictor_string <- paste("log_purchase ~", paste(predictor_1, collapse = "+"))
#install.packages("car")
library(car)


predictor <- c("slshist", "ordhist",  'ordtyr', 'slstyr', 'slslyr', 'ordlyr', 'sls2ago','ord2ago', 'sls3ago', 'ord3ago')  

custom_scale <- function(x) {
  ((x - min(x)) / (max(x) - min(x))) +1
}

oversampled_data_2<- as.data.frame(lapply(oversampled_data_1[predictor], custom_scale))
oversampled_data_2$log_purchase <- oversampled_data_1$log_purchase


oversampled_data_2$ordhist <- log(oversampled_data_2$ordhist)
oversampled_data_2$ordtyr <- log(oversampled_data_2$ordtyr)
oversampled_data_2$slstyr <- log(oversampled_data_2$slstyr)
oversampled_data_2$slslyr <- log(oversampled_data_2$slslyr)
oversampled_data_2$ordlyr <- log(oversampled_data_2$ordlyr)
oversampled_data_2$sls2ago <- log(oversampled_data_2$sls2ago)
oversampled_data_2$ord2ago <- log(oversampled_data_2$ord2ago)
oversampled_data_2$sls3ago <- log(oversampled_data_2$sls3ago)
oversampled_data_2$ord3ago <- log(oversampled_data_2$ord3ago) 
#oversampled_data_2$falord <- log(oversampled_data_2$falord)
#oversampled_data_2$sprord <- log(oversampled_data_2$sprord)


full_model <- lm(log_purchase~( ordhist   + ordtyr + slstyr+slslyr+ ordlyr+sls2ago+ord2ago+sls3ago+ord3ago)^2-1 , data = oversampled_data_2)
# +falord+sprordslshist+

summary(full_model)


new_model <- step(full_model)
summary(new_model)

#  

predictor <- c( "ordhist", 'ordtyr', 'slstyr', 'slslyr', 'ordlyr', 'sls2ago','ord2ago', 'sls3ago', 'ord3ago')  
#, "slshist",'falord', 'sprord'

custom_scale <- function(x) {
  ((x - min(x)) / (max(x) - min(x))) +1
}
test_1 <- as.data.frame(lapply(test[predictor], custom_scale))
#test_1 <- as.data.frame(test[predictor])

#test_1$slshist <- log(test_1$slshist)
test_1$ordhist <- log(test_1$ordhist)
test_1$ordtyr <- log(test_1$ordtyr)
test_1$slstyr <- log(test_1$slstyr)
test_1$slslyr <- log(test_1$slslyr)
test_1$ordlyr <- log(test_1$ordlyr)
test_1$sls2ago <- log(test_1$sls2ago)
test_1$ord2ago <- log(test_1$ord2ago)
test_1$sls3ago <- log(test_1$sls3ago)
test_1$ord3ago <- log(test_1$ord3ago) 
#test_1$falord <- log(test_1$falord)
#test_1$sprord <- log(test_1$sprord)

#predict(new_model, newdata = test)



test_1$test_y_1_prob <- test$test_y_1_prob

#test_1$predict_y <- ifelse(test_1$test_y_1_prob < 0.2 , 0, exp(predict(full_model, newdata = test_1) * test$test_y_1_prob) -1)

test_1$predict_y <-  exp(predict(new_model, newdata = test_1) * test$test_y_1_prob) -1

test_1$predict_y <- ifelse(test_1$predict_y < 0 , 0, test_1$predict_y)

test_1$predict_y <- ifelse(test_1$predict_y > max(train$targdol) , max(train$targdol), test_1$predict_y)

#test_1$predict_y <- ifelse(test$test_y_1_prob < 0.5 , 0, exp(predict(full_model, newdata = test_1) -1))

#test_1$predict_y <- exp(predict(full_model, newdata = test_1) * test$test_y_1_prob)-1


#test_1$predict_y <- predict(new_model, newdata = test_1)
test_1$y <- test$targdol

test_1$diff <- (test_1$predict_y - test_1$y)


mean(test_1$diff)
plot(test_1$predict_y, test_1$diff)

#qqnorm(test_1$diff)
#qqline(test_1$diff)




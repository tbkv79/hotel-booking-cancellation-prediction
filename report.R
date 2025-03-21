hotels <- read.csv("\\\\homeblue01\\tbkv79\\DUDE\\Desktop\\hotels.csv")

str(hotels)
summary(hotels)
head(hotels)

num_vars <- c("lead_time", "adr", "adults", "children", "babies", 
              "stays_in_week_nights", "stays_in_weekend_nights")
hotels[num_vars] <- lapply(hotels[num_vars], as.numeric)

cat_vars <- c("hotel", "country", "agent", "company", "deposit_type", 
              "customer_type", "reservation_status", "market_segment", 
              "distribution_channel", "meal")
hotels[cat_vars] <- lapply(hotels[cat_vars], as.factor)

hotels$agent[hotels$agent == "NULL"] <- NA
hotels$agent[is.na(hotels$agent)] <- "unknown"
hotels$company[hotels$company == "NULL"] <- NA
hotels$company[hotels$country == "NULL"] <- NA
mean(is.na(hotels$agent))
mean(is.na(hotels$company))
mean(is.na(hotels$country))

hotels$company <- NULL

colSums(is.na(hotels))
hotels <- na.omit(hotels)

# outliers
hotels <- hotels[hotels$adr > 0 & hotels$adr < 1000, ]
hotels <- hotels[hotels$adults > 0, ]
hotels <- hotels[hotels$lead_time <= 365, ]
hotels <- hotels[hotels$stays_in_weekend_nights <= 8, ]
hotels <- hotels[hotels$stays_in_week_nights <= 20, ]
hotels <- hotels[hotels$children <= 5, ]
hotels <- hotels[hotels$babies <= 3, ]
hotels <- hotels[hotels$previous_cancellations <= 10, ]
hotels <- hotels[hotels$previous_bookings_not_canceled <= 30, ]
hotels <- hotels[hotels$booking_changes <= 5, ]
hotels <- hotels[hotels$days_in_waiting_list <= 180, ]
hotels <- hotels[hotels$required_car_parking_spaces <= 3, ]

hotels$agent <- droplevels(hotels$agent)
levels(hotels$agent)
any(hotels$agent == "NULL")

set.seed(123)
library(caret)

trainIndex <- createDataPartition(hotels$is_canceled, p=0.8, list=FALSE)
trainData <- hotels[trainIndex, ]
testData <- hotels[-trainIndex, ]

levels(hotels$country)
levels(hotels$agent)

install.packages("ranger")
library(ranger)

ranger_model <- ranger(
  is_canceled ~ ., 
  data = trainData, 
  probability = TRUE, 
  importance = "impurity"
)
pred <- predict(ranger_model, testData)$predictions[,2]
pred_class <- ifelse(pred > 0.5, 1, 0)

levels(as.factor(pred_class))
levels(testData$is_canceled)

testData$is_canceled <- factor(testData$is_canceled, levels = c(0, 1))
confusionMatrix(as.factor(pred_class), testData$is_canceled)

ranger_model$variable.importance

# model improvement
trainData_clean <- trainData[, !(names(trainData) %in% c("reservation_status", "reservation_status_date"))]
testData_clean <- testData[, !(names(testData) %in% c("reservation_status", "reservation_status_date"))]

ranger_model2 <- ranger(
  is_canceled ~ ., 
  data = trainData_clean, 
  probability = TRUE, 
  importance = "impurity"
)

pred2 <- predict(ranger_model2, testData_clean)$predictions[, 2]
pred_class2 <- ifelse(pred2 > 0.5, 1, 0)

testData_clean$is_canceled <- factor(testData_clean$is_canceled, levels = c(0, 1))
confusionMatrix(factor(pred_class2, levels = c(0, 1)), testData_clean$is_canceled)

# performance evaluation
confusionMatrix(factor(pred_class2, levels = c(0, 1)), testData_clean$is_canceled)

install.packages('pROC')
library(pROC)
roc_obj <- roc(testData_clean$is_canceled, pred2)
plot(roc_obj, main="ROC Curve", col="blue", lwd=2)
auc(roc_obj)



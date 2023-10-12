library(mstrio)
install.packages("randomForest")
library(randomForest)
install.packages("dply")
install.packages("caTools")
install.packages("caret")
library(caret)
library(caTools)


base_url <- "http://nslp-440.nicesoftwaresolutions.com:8080/MicroStrategyLibrary/api"
username <- "Administrator"
password <- ""
project_name <- "MicroStrategy Tutorial"

conn <- Connection$new(base_url=base_url, username=username, password=password, project_name=project_name)

my_report <- Report$new(connection=conn, report_id="C2FF03354D1D1457070B2C94649032D0")

df <- my_report$to_dataframe()
head(df,10)
tail(df,5)
print(class(df$Month))
print(class(df$Gross Revenue))
str(df)
df -> my_dataframe

my_dataframe$Month <- gsub("2014|2015|2016", "", df$Month)
tail(my_dataframe,10)

print(df)
data <- subset(my_dataframe, select = -Country)
data
str(data)
new_column_names <- c("Year", "Month" ,"Region","Call_centre","Gross_Revenue")
names(data)<- new_column_names
str(data)

# Now we will convert categorical data into factors 

data$Year <- as.factor(data$Year)
data$Month <- as.factor(data$Month)
data$Region <- as.factor(data$Region)
data$Call_centre <- as.factor(data$Call_centre)

# Splitting data into features and target

#features <- data[, c("Year", "Month", "Region", "Call_centre")]
#target <- data$Gross_Revenue
sample <- sample.split(data,SplitRatio = 0.70)
train <- subset(data,sample=TRUE)
test  <-subset(data,sample=FALSE)
#fitting the data into model

rf_model <- randomForest(target ~ ., data = features, ntree = 100)
model_new <- randomForest(Gross_Revenue ~ ., data =   train)
model_new


# Print model summary

#print(rf_model)
#predictions <- predict(rf_model, features)
#cat("Predicted Gross Revenue:\n", predictions)


#testing the model
test$predictions <- predict(model_new,test)
confusionMatrix(test$predictions,test$Gross_Revenue)


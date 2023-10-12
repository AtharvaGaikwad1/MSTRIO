#C2FF03354D1D1457070B2C94649032D0 - Report ID 
library(caret)
library(caTools)
library(randomForest)
library(mstrio)

# Connecting to microstrategy

base_url <- "http://nslp-440.nicesoftwaresolutions.com:8080/MicroStrategyLibrary/api"
username <- "Administrator"
password <- ""
project_name <- "MicroStrategy Tutorial"

conn <- Connection$new(base_url=base_url, username=username, password=password, project_name=project_name)

#Loading the Report- (Monthly Region wise gross revenue)created in MSTR

my_first_report <- Report$new(connection=conn, report_id="C2FF03354D1D1457070B2C94649032D0")
df <- my_first_report$to_dataframe()
str(df)
df$Month <- gsub("2014|2015|2016", "", df$Month)
new_column_names <- c("Year", "Month" ,"Region","Call_centre","Cost","Units","Revenue")
names(df)<- new_column_names
df

#Exploratory Analysis

# plot(df$Units, df$Revenue, 
#      main = "Scatter Plot of Units_sold vs Revenue",  # Title of the plot
#      xlab = "Units sold",                             # Label for the X-axis
#      ylab = "Revenue",                                # Label for the Y-axis
#      pch = 16,                                        # Point character (16 is a filled circle)
#      col = "blue", 
#      xlim = c(0, 6),                          # X-axis limits
#      ylim = c(0, 6) # Point color
#                                 
#                            # Y-axis limits
# )

#Converting data types according to R 

Cols_factor <- c("Year","Month","Region","Call_centre")
df[Cols_factor] <-lapply(df[Cols_factor],factor)

Cols_numeric <- c("Units","Revenue") 
df[Cols_numeric] <- lapply(df[Cols_numeric],as.numeric)

#Splitting the data into 80% training and 20% testing metrics

sample  <- sample.split(df,SplitRatio = 0.80)
training_data <- subset(df,sample ==TRUE)
testing_data <- subset(df,sample==FALSE)

print("-----------------------")
training_data

print("----------------------- printing testing data")
testing_data


#Training the model 
# Y ~ X means Y is dependant variable and X is independant variable
model <- lm( Revenue ~ Units, data = training_data)
model

summary(model)

 


# plot(training_data$Units, training_data$Revenue, 
#      main = "Scatter Plot with Best-Fit Line (Subset)", 
#      xlab = "X", 
#      ylab = "Y",
#           )
# abline(model, col = "red")



predicted_values <- predict(model, dumm_df = data.frame(X = testing_data))
predicted_values

predicted_dataFrfnl <- data.frame(orignal_val = training_data$Revenue,predcited_Rev = predicted_values)




write.csv(predicted_dataFrfnl, file = "Run_task_scheduler-3.csv", row.names = FALSE)


#Exporting the data back to MSTR 

ds_mstr = Dataset$new(connection=conn, name="Monthly_Region_wise Analysis")
ds_mstr$add_table(name ="Monthly_Region_wise Analysis", data_frame=predicted_dataFrfnl, update_policy="add")
ds_mstr$create()
ds_mstr$certify()



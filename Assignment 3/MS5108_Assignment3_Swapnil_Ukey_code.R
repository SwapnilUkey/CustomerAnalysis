# Name: Swapnil Ukey
# Module Code: 2223-MS5108 Applied Customer Analytics
# Student Id: 22220959

# get the current working directory
my_location <- getwd()

# set working directory
setwd(my_location)

# Setting csv name in an variable 
data_file_name <- "Crossfit-Ridgeline-DataSet.csv"

#load the data set.
survey_data <- read.csv(data_file_name, header=TRUE)

# Remove duplicate rows
survey_data <- unique(survey_data)

# remove rows with missing values
crossfit <- na.omit(survey_data)

# create a new column "Age Group"
crossfit$AgeGroup <- cut(crossfit$Age, breaks = c(0, 12, 20, 35, 50, Inf), labels = c("Child", "Teenager", "Young Adult", "Adult", "Old"))
crossfit$Gender_new <- ifelse(crossfit$Gender == 1, "female", "male")

# convert all variables to numeric
crossfit[, 2:24] <- sapply(crossfit[, 2:24], as.numeric)

# remove any rows with values less than 1 or greater than 5
crossfit <- crossfit[rowSums(crossfit[, 4:24] < 1 | crossfit[, 4:24] > 5) == 0,]

# create new columns for CUSSAT  groups
crossfit$CUSSAT_total <- rowSums(crossfit[,c("CUSSAT1", "CUSSAT2", "CUSSAT3", "CUSSAT4")])
# create new column for COASAT group
crossfit$COASAT_total <- rowSums(crossfit[,c("COASAT1", "COASAT2", "COASAT3", "COASAT4", "COASAT5")])
# create new column for CLIM group
crossfit$CLIM_total <- rowSums(crossfit[,c("CLIM1", "CLIM2", "CLIM3", "CLIM4", "CLIM5", "CLIM6")])
# create new column for COND group
crossfit$COND_total <- rowSums(crossfit[,c("COND1", "COND2", "COND3", "COND4", "COND5", "COND6")])

# Converting Genders in binary for logistic regression 
crossfit$Gender <- ifelse(crossfit$Gender == 1, 0, 1)

# print the updated dataframe
# write.csv(crossfit, "data.csv") 

library(ggplot2)

#################################################################################################
#1. One boxplot analysis. Provide a short paragraph (max 200 words) describing what the analysis you 
# provide represents.

ggplot(crossfit, aes(x = factor(Gender_new), y = Membership.Tenure, fill = Gender_new)) +
  geom_boxplot() +
  labs(title = "Overall Customer Satisfaction Scores by Gender", x = "Gender", y = "Membership Tenure") +
  scale_fill_manual(values = c("pink", "lightblue"), name = "Gender") +
  theme_minimal()

#################################################################################################
#. One other visualization of your choosing. Provide a short paragraph (max 200 words) describing 
# what the analysis you provide represents.

ggplot(crossfit, aes(fill=Gender_new)) +
  geom_density(aes(x=CUSSAT_total, linetype="Customer"),
               alpha=0.5, color="black", show.legend = TRUE) +
  geom_density(aes(x=COASAT_total, linetype="Coach"),
               alpha=0.5, color="black", show.legend = TRUE) +
  geom_density(aes(x=CLIM_total, linetype="Climate"),
               alpha=0.5, color="black", show.legend = TRUE) +
  geom_density(aes(x=COND_total, linetype="Conditions"),
               alpha=0.5, color="black", show.legend = TRUE) +
  xlab("Satisfaction Scores") +
  ylab("Density") +
  ggtitle("Density Plots of Satisfaction Scores by Gender") +
  facet_wrap(~ Gender_new, nrow=2) +
  theme_minimal() +
  scale_fill_manual(values=c("pink", "lightblue"), name="Gender") +
  scale_linetype_manual(name="Satisfaction Type", values=c("Customer"="solid", "Coach"="dashed",
                                                           "Climate"="dotted", "Conditions"="dotdash"))

###################################################################################################

#3. One regression model, with one dependent variable and at least three independent variables. The 
#model should make logical sense i.e. do not pick variables at random to see if they are related. 
#Write the code in R to test if there are significant relationships in your model. Run the analysis and 
#record the findings. Provide a short paragraph (max 300 words) describing what the analysis you 
#provide represents
set.seed(123) # Set seed for reproducibility
train_index <- sample(seq_len(nrow(crossfit)), size = round(0.95*nrow(crossfit)), replace = FALSE)
training <- crossfit[train_index, ]
testing <- crossfit[-train_index, ]

# Model For Membership.Tenure
lm_model <- lm(Membership.Tenure ~ CUSSAT_total + Age + Gender, data = training)

# Summary
summary(lm_model)

# Test for significant relationships
anova(lm_model, test = "Chisq")

#95% CI
confint(lm_model,parm = "Membership.Tenure")

predicted_values <- predict(lm_model, newdata = testing)
accuracy <- 1 - mean((testing$Membership.Tenure - predicted_values)^2) / var(testing$Membership.Tenure)
print(paste("Model efficacy:", round(accuracy, 2)))

mse <- mean((testing$Membership.Tenure - predicted_values)^2)
rmse <- sqrt(mse)
print(paste("MSE:", round(mse, 2)))
print(paste("RMSE:", round(rmse, 2)))

ggplot(testing, aes(x = predicted_values, y = Membership.Tenure)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  xlab("Predicted Values") +
  ylab("Actual Values") +
  ggtitle("Model Performance")

#install.packages("corrplot")

# Load the corrplot package
library(corrplot)

correlation_matrix <- cor(training[c("Membership.Tenure", "Age", "Gender", "CUSSAT_total")])
corrplot::corrplot(correlation_matrix, method = "circle")


#############################################################################################
# 4. A second regression model, with one dependent variable, one independent variable, and one 
# interacting variable. Write the code in R to test if the independent variables interact to predict the 
# dependent variable. Run the analysis and record the findings. Provide a short paragraph (max 300 
# words) describing what the analysis you provide represents.
# Model with interaction term
logit_model <- glm(Gender ~ Membership.Tenure + CUSSAT_total + Membership.Tenure * 
                     CUSSAT_total, data = training, family = binomial)

# Summary
summary(logit_model)

# Perform an ANOVA test on the model
anova(logit_model, test = "Chisq")

testing$predicted_gender <- predict(logit_model, newdata = testing, type = "response")
testing

ggplot(data = testing, aes(x = predicted_gender, y = CUSSAT_total, color = Gender)) + 
  geom_point() +
  ggtitle("Predicted Gender vs. Actual Gender") +
  xlab("Predicted Gender") +
  ylab("CUSSAT_total")














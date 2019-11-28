#1.Import the dataset data_marketing_budget_mo12 and do the exploratory
#data analysis .
a=read.csv("C:/Users/admin/Desktop/data.csv",header=TRUE)
summary(a)
#Inferences: 
#  The minimum value of spend variable is 1000 and the maximum value is 15000, whereas for sales, it is 9914 and 158484 respectively. 
#The average values for spend and sales is 6542 and 70870 respectively.

#2. Use Scatter Plot To Visualise The Relationship
plot(a$Month,a$Spend,main="Relationship bw Month and Expenditure",xlab="Month",ylab="Expenditure",pch=19)
#Inferences:
#We can see that the expenditure in the month of September is the highest and January the lowest. The expenditure is showing 
#an incresing trend from January till September with a decrease in the month of May and has decreased linearly after the month of September.
plot(a$Spend,a$Sales,main="Relationship bw Sales and Expenditure",xlab="Expenditure",ylab="Sales",pch=19)
#Inferences:
#We infer that the Sales and Expenditure is linearly correlated and it follows a positive increasing trend.

#3.Using BoxPlot To Check For Outliers
boxplot(a$Sales,main="Boxplot of Sales",xlab="Sales",col="orange",border="brown")
boxplot(a$Spend,main="Boxplot of Expenditure",xlab="Expenditure",col="orange",border="brown")
#Inferences:
#   We can understand that there are no outliers in both Spend and Sales such that the data is affected.

#4. Using Density Plot To Check If Response Variable Is Close To Normal
plot(density(a$Sales), main="Density Plot: Sales", ylab="Frequency",sub=paste("Skewness:",round(e1071::skewness(a$Sales),2)))
#Inferences:
#      We can see that the Sales follows a positively skewed normal distribution with a skewness of 0.56. It follows a decreasing trend.

#5. Check the Correlation Analysis
cor(a$Spend,a$Sales)
#Inferences:
#   There is an almost perfect correlation between the Expenditure and Sales.

#6. Build the Linear Regression Model
model=lm(a$Spend~a$Sales)
print(model)
#Inferences:
#We can see that the value of the intercept of the linear model is -114.67027 and the slope of the model is 0.09392. So, we get the complete 
#formula of the linear model as Sales = 0.09392 * (Spend) - 114.67027.

#7. Using P Value Check For Statistical Significance
t.test(a$Spend,a$Sales,paired=TRUE,alternative = "two.sided")
#Inferences:
#   Here, the p value is 0.000221, which is less than the level of significance of 0.05. Hence the null hypothesis is rejected and 
#hence both the variables are significantly different and hence is statistically significant.

#8.Capture the summary of the linear model
summary(model)
#Inferences:
#a) The distribution of the RESIDUALS are symmetrical between -293.22 and 312.02, so the model predicted the response variable accurately. 
#b) The COEFFICIENTS shows the intercept and slope of the linear regression model and it is given by 'Sales = 0.09392 * (Spend) - 114.67027'. The estimate of sales, when the spend is 0, is 9.392e-02. The standard error coefficient is 1.437e-03, which gives the approximate variations that the sales variable can have with respect to the spend variable. The t-value is 65.378, which is significantly far from 0, which shows that the variables are statistically significant.
#c) Residual Standard Error is measure of the quality of a linear regression fit and here it is calculated as 217.5 on 10 degrees of freedom.
#d) The R-squared (R^2) statistic provides a measure of how well the model is fitting the actual data.R^2  is a measure of the linear relationship between our predictor variable and our response/target variable. The value obtained is 0.9977 which shows a 99.77% of fitting of the variable to the linear model.
#e) The F value of 4274 on 1, which is relatively larger than 1 hence, we see that there is a good relationship between sales and spend variables.

#9.Also perform the Linear Diagnostics for the given data set
plot(model)
# Inferences:
# a) Residual vs Fitted Graph: We can see that the equally spread residuals around a horizontal line without distinct patterns. Hence, a linear regression model is well fit for the dataset.
# b) Normal Q-Q Graph: Here, we can infer that the residuals are normally distributed, since the residuals are lined well on the straight dashed line.
# c) Scale - Location Graph: The residuals are equally distributed and the red line is horizontal. Hence, the variance is equal for the data or we can say that the data is homoscedastic. 
#d) Residual vs Leverage Graph: We can see that the red dotted line shows the Cook's distance and there is one datapoint lying after the 
#dashed line. So, there is one influential points in the data. Hence, the results can be affected by that influential point.

#9. Create the training and test data (60:40)
rows=sample(nrow(a))
# Randomly order data:
data=a[rows, ]
# Identify row to split on: split
split = round(nrow(data) * .60)
# Create train
train=data[1:split,]
# Create test
test=data[(split+1):nrow(data),]
train
test

#10. Fit the model on training data and predict sales on test data
linmod = lm(Spend ~ Sales, data=train)  # build the model
Pred = predict(linmod, test) 
linmod
Pred

#INFERENCES:
#The model that is fitting the data is given by 'Sales = (Spend)*0.09144 + 6.51842'. The predicted values are also given.

#11.Review the diagnostic measures
summary(linmod)
AIC(linmod)
#INFERENCES:
# The distribution of the RESIDUALS are symmetrical between -379.55 and 233.49, so the model predicted the response variable accurately. 
#b) The COEFFICIENTS shows the intercept and slope of the linear regression model and it is given by 'Sales = (Spend)*0.09207 + 39.01667'. The estimate of sales, when the spend is 0, is 9.207e-02. The standard error coefficient is 2.328e-03, which gives the approximate variations that the sales variable can have with respect to the spend variable. The t-value is 39.55, which is significantly far from 0, which shows that the variables are statistically significant.
#c) Residual Standard Error is measure of the quality of a linear regression fit and here it is calculated as 233.7 on 5 degrees of freedom.
#d) The R-squared (R^2) statistic provides a measure of how well the model is fitting the actual data.R^2  is a measure of the linear relationship between our predictor variable and our response/target variable. The value obtained is 0.9968 which shows a 99.68% of fitting of the variable to the linear model.
#e) The F value of 1564 on 1 and 5 DF, which is relatively larger than 1 hence, we see that there is a good relationship between sales and spend variables.

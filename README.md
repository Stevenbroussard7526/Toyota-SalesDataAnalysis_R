 Load "Toyota5.csv" and save it as carsData.
Load any packages that you may use for this assignment

Toyota5=read.csv("Toyota5.csv")
library(ggplot2)
library(tidyverse)

# Q1:Check all the columns/variable to find the variable that has missing values. 
  anyNA(Toyota5)
  apply(Toyota5,2,anyNA) #only Mileage column has True missing values 
# Locate and replace missing values with the average
  is.na(Toyota5$Mileage)
  sum(is.na(Toyota5$Mileage)) #Counts number of missing values for Mileage column
  mileage_missingrow=which(is.na(Toyota5$Mileage)) # Obtains the row number of missing values
  Toyota5$Mileage[mileage_missingrow]=mean(Toyota5$Mileage,na.rm=TRUE)
  
# of the non-missing values of the corresponding variable.
  sum(is.na(Toyota5$Mileage))



# Q2:Summarize the fuel type variable using either a frequency table or a plot.
  fuel_type_freq= table(Toyota5$Fuel_Type)
  fuel_type_freq
  barplot(sort(fuel_type_freq))
  
# Write a code that returns the name of the most common fuel type category.
 which.max(fuel_type_freq)
 
# Q3:Create a boxplot that compares the distribution of price across different fuel types.
 boxplot(Price~Fuel_Type,Toyota5)
 
# Also, write a code that calculates the average price for each of these fuel type categories.
 Avg_fuel_price=aggregate(Price~Fuel_Type,data=Toyota5,FUN=mean)
 Avg_fuel_price
 
 
# Which fuel category has the highest average price? 
# (write a code that returns both the name and the average price of this category)
Avg_fuel_price[which.max(Avg_fuel_price$Price),]

 
# Q4:Create two scatterplots: Price (y-axis) vs. Age (x-axis), Price (y-axis) vs. Mileage (x-axis)
   plot(Price~Age,data=Toyota5)
   plot(Price~Mileage,data=Toyota5)
 
# Which of the variables is more correlated with price? Calculate the appropriate measure that can help you answer this question?
   Age_Price_correlation = cor(Toyota5$Age,Toyota5$Price)
   Age_Price_correlation #Age is more correlated with price as it's closer to -1 
   Mileage_Price_correlation = cor(Toyota5$Mileage,Toyota5$Price) 
   Mileage_Price_correlation

# Q5: Referring to the plots of Q4, identify and remove the three cars that are outliers with higher price.
   plot(Price~Mileage,data=Toyota5)
   Toyota5[(Toyota5$Price>26000),]
# Save the results as a new dataset, called carsUpdated.
   carsUpdated= Toyota5[(Toyota5$Price<26000),]
   carsUpdated
   plot(carsUpdated$Mileage,carsUpdated$Price)
   
   
# Q6: Run a regression of price using age as the only predictor. (use the updated dataset)Save the results as regMileage. Write a code that returns the summary of the results.
   regAge = lm(Price~Age,carsUpdated)
   summary(regAge)
   

# Q7: Run a regression of price using fuel type as the main predictor. (use the updated dataset)Save the results as regMileage. Write a code that returns the summary of the results.
  regMileage = lm(Price~Mileage,carsUpdated)
   summary(regMileage)

# Q8:Create a table that compares the two regression models in terms of residual standard error and r-squares. (Note: you can do so by building a dataframe of the desired outputs or using stargazer function. In either case, make sure reach row/column is labeled accordingly.)

  model_name=c("regAge","regMileage")
  res.se_value=c(summary(regAge)$sigma,summary(regMileage)$sigma)
  rsq_value=c(summary(regAge)$r.squared,summary(regMileage)$r.squared)
  a.rsq_value=c(summary(regAge)$adj.r.squared,summary(regMileage)$adj.r.squared)
  comb_lm_table=data.frame(model_name,res.se_value,rsq_value,a.rsq_value)
  comb_lm_table

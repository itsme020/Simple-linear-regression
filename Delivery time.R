View(delivery_time)
#to plot box
boxplot(delivery_time)

#plot scatter plot
plot(delivery_time$Sorting.Time,delivery_time$Delivery.Time)

#plot histogram
hist(delivery_time$Sorting.Time)
hist(delivery_time$Delivery.Time)


####summary of data######################

summary(delivery_time)

# to find correalation

deltime<- delivery_time$Delivery.Time
sorttime <- delivery_time$Sorting.Time

cor(deltime,sorttime)## corr value is 0.825

##########Model################

reg<-lm(deltime~sorttime)
summary(reg)
plot(reg)
p <- predict(reg,interval  = "prediction")
# Adjusted R-squared value for the above model is 0.6655 

# Applying transformations
# Logarthmic transformation
reg1<-lm(deltime~log(sorttime))  # Regression using logarthmic transformation
summary(reg1)
# Adjusted R-squared:  0.6794 
plot(reg1)
p1 <- predict(reg1,interval = "prediction")

# Exponential model 
reg2<-lm(log(deltime)~sorttime)
# regression using Exponential model
summary(reg2)
plot(reg2)
p2 <- predict(reg2,interval = "prediction")
# R-squared value is 0.7109
# Adjusted R SQuare Value is 0.6957

# Cubic model
reg3 <- lm(deltime~sorttime+I(sorttime^2)+I(sorttime^3),data=delivery_time)
summary(reg3) 
plot(reg3)
p3 <- predict(reg3,interval = "prediction")
rmse<-sqrt(mean((p3-dt)^2))
rmse



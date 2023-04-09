## Michael Torres
mydata <- read.delim("ArmStrength_DynamicLift_data.txt", header = TRUE, sep = ",")

#Quick summary on the data
summary(mydata)

#Creating the linear model with data
model.lm <- lm(DynamicLift ~ ArmStrength, data = mydata)
model.lm

summary(model.lm)

# Formatting Fitted Equation
# Extracting coefficients
b0 <- coef(model.lm)[1]
b1 <- coef(model.lm)[2]


# Construct fitted equation
fitted_equation <- paste0("y = ", b0, " + ", b1, "x")
print(fitted_equation)


#Calculating SUM xi
sum(mydata$ArmStrength, na.rm = TRUE)

#Calculating SUM xi^2
sum(mydata$ArmStrength^2, na.rm = TRUE)

#Calculating SUM yi
sum(mydata$DynamicLift, na.rm = TRUE)

#Calculating SUM yi^2
sum(mydata$DynamicLift^2, na.rm = TRUE)

#Calculating SUM xi * yi
sum(mydata$ArmStrength*mydata$DynamicLift, na.rm = TRUE)


#Plotting the Oxygen Demand vs Solid Reduction
plot(mydata, xlab= "Arm Strength" , ylab= "Dynamic Lift", ylim = c(-20, 110), xlim = c(0, 80))
abline(coef(model.lm), h=0)


#Diagnostic Plots
par(mfrow=c(2,2))
plot(model.lm)
abline(coef(model.lm))

plot(mydata,resid(model.lm), ylim = c(-20, 110), xlim = c(0, 80))
hist(resid(model.lm))



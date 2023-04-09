## Michael Torres
mydata <- read.delim("Solid_Reduction_Oxygen_Demand.txt", header = TRUE, sep = ",")

#Quick summary on the data
summary(mydata)

#Creating the linear model with data
model.lm <- lm(OxygenDemand ~ SolidReduction, data = mydata)
model.lm

summary(model)

# Formatting Fitted Equation
# Extracting coefficients
b0 <- coef(model.lm)[1]
b1 <- coef(model.lm)[2]

# Construct fitted equation
fitted_equation <- paste0("y = ", b0, " + ", b1, "x")
print(fitted_equation)


#Plotting the Oxygen Demand vs Solid Reduction
plot(mydata, xlab= "Solid Reduction (%)" , ylab= "Oxygen Demand (%)")
abline(coef(model.lm))

#Diagnostic Plots
par(mfrow=c(2,2))
plot(model.lm)
abline(coef(model.lm))

#Residual and Fitted Values
resid(model.lm)
fitted(model.lm)

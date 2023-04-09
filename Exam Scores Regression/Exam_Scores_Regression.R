## Michael Torres

#Question 1
mydata <- read.delim("Exam_Scores.txt", header = TRUE, sep = ",")

#Creating the linear model with data
ExamScores.lm <- lm(Final ~ Midterm, data = mydata)
ExamScores.lm

# Formatting Fitted Equation
# Extracting coefficients
b0 <- coef(ExamScores.lm)[1]
b1 <- coef(ExamScores.lm)[2]

# Construct fitted equation
fitted_equation <- paste0("y = ", b0, " + ", b1, "x")
print(fitted_equation)

x85 <- data.frame(Midterm = 85)
predicted_x85 <- predict(ExamScores.lm, newdata = x85)

predicted_x85

#Question 2
predict(ExamScores.lm, newdata = x85, interval = "confidence", level = 0.85)

#Question 3
hist(resid(ExamScores.lm))

plot(Midterm, resid(ExamScores.lm), main="Residuals versus the x values")
abline(h=0)

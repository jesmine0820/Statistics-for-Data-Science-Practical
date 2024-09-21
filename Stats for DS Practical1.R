#Simple Linear Regression

#Example 1.12
#Step 1 - Define the dataset
X <- c(0,2,4,6,8) #Quiz 1 data
Y <- c(6,5,8,7,9) #Quiz 2 data

#Step 2 - Visualize the data
plot(Y ~ X, col = "blue",
     main = "Scatter Diagram",
     xlab = "Quiz 1",
     ylab = "Quiz 2"
     ) #Plot scatter diagram

#Step 3 - Modeling (SLR)
Fitline <- lm(Y ~ X); 
Fitline #Estimate parameters
Yhat <- predict(Fitline); 
Yhat

#Step 4 - Plot the model
abline(Fitline, col="red")
#Add regression line on the scatter diagram

#Step 5 - Evaluate the model
anova(Fitline) #Test significance of the regression
Fcv <- qf(0.95,1,3);
Fcv

test <- summary(Fitline);
test #Test significance of the individual variable
tcvl <- qt(0.025, 3);
tcvl #LHS
tcvr <- qt(0.975,3);
tcvr #RHS

R = cor(X,Y) #correlation
R2 = 1-(sum((test$residuals)^2)/sum((Y-mean(Y))^2))
mse = mean((test$residuals)^2) #mean square error
mae = mean(abs(test$residuals)) #mean absolute error
rmse = sqrt(mse) #root mean square error
cat(" R:", R,"\n", "R-squared:", R2,"\n", "MAE:", mae,"\n", "MSE:", mse,"\n", "RMSE:", rmse)

confint(Fitline) #confidence interval of the estimators
#Confidence interval of the mean response, E(X)
predict(FitLine, newdadta=data.frame(X=7), interval='confidence', level=0.95)
#Prediction interval
predict(Fitline, newdata=data.frame(X=6), interval='prediction', level=0.95)

#Plot confidence interval of the mean response
library(ggplot2)
quiz <- as.data.frame(cbind(X,Y)) #Set up data frame
ggplot(quiz, aes(x = X, y = Y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, colour = 'blue') +
  ggtitle('Scatter Diagram') + 
  xlab('Quiz 1') +
  ylab('Quiz 2')
  
  
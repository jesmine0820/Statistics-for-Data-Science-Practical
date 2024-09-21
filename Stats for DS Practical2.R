Y <- c(64,73,61,76,72,80,71,83,83,89,86,93,88,95,94,100)
X1 <- c(4,4,4,4,6,6,6,6,8,8,8,8,10,10,10,10)
X2 <- c(2,4,2,4,2,4,2,4,2,4,2,4,2,4,2,4)

Fitline <- lm(Y ~ X1 + X2);
Fitline
Yhat <- predict(Fitline);
Yhat

test <- summary(Fitline); test #t-test
tcvl <- qt(0.005,13); tcvl
tcvr <- qt(0.995,13); tcvr

df <- as.data,frame(cbind(X1,X2,Y)) #Set up data frame
pairs(cbind(X1,X2,Y)) #pairwise comparison scatter plot matrix
cor(df) #find correlation between variables X1,X2,Y

R2 = 1-(sum((test$residuals)^2)/sum((Y-mean(Y))^2))
R = sqrt(R2)
mse = mean((test$residuals)^2)
mae = mean(abs(test$residuals))
rmse = sqrt(mse)
cat(" R:", R,"\n", "R-squared:", R2,"\n",
    "MAE:", mae,"\n", "MSE:", mse,"\n", "RMSE:", rmse)

confint(Fitline, level=0.99) #99% confidence interval of the estimators
#Confidence interval of the mean response
predict(FitLine, newdata=data.frame(X=5), interval='confidence', level=0.99)
#Prediction interval
predict(FitLine, newdata=data.frame(X=5), interval='prediction', level=0.99)

FitLine <- lm(Y ~ poly(X,2)) #Use orthogonal polynomials
anova(FitLine)
Fcv <- qf(0.99, 2, 10); Fcv





























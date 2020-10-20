####### Model1
lm = lm(BODYFAT ~., data = bodyfat.new)
lm0 = lm(BODYFAT ~ 1, data = bodyfat.new)

#AIC both
aic.both = step(lm0, scope = list(upper=lm), direction = "both", k=2)

lm.aic.both = lm(BODYFAT ~ ABDOMEN + WEIGHT + WRIST + FOREARM + NECK, 
             data = bodyfat.new)  ##1
summary(lm.aic.both)

lm.aic.both1 = lm(BODYFAT ~ ABDOMEN + WEIGHT + WRIST + FOREARM, 
              data = bodyfat.new ) ##2
summary(lm.aic.both1)


#AIC forward
aic.forward = step(lm0, scope = list(upper=lm), direction = "forward", k=2)
#same as lm.aic.both1

#AIC back
aic.back = step(lm, scope = list(upper=lm0), direction = "back", k=2)

lm.aic.back = lm(BODYFAT ~ AGE + WEIGHT + NECK + ABDOMEN + THIGH + FOREARM + WRIST, 
                    data = bodyfat.new) ##3
summary(lm.aic.back)
#same as lm.aic.both1

#BIC both
bic.both = step(lm0, scope = list(upper=lm), direction = "both",
                k = log(nrow(bodyfat.new)))
#same as lm.aic.both1

#BIC forward
bic.forward = step(lm0, scope = list(upper=lm), direction = "forward",
                   k = log(nrow(bodyfat.new)))
#same as lm.aic.both1

#BIC back
bic.back = step(lm, scope = list(upper=lm0), direction = "back",  
                k = log(nrow(bodyfat.new)))
#same as lm.aic.both1


#US navy
USC = lm(BODYFAT ~ log(ABDOMEN) + log(HEIGHT) + log(NECK),
         data = bodyfat.new) ##4
summary(USC)


#Cross validation 
CV = function(lm, k=100, data, seed = 1){
  nrow = nrow(data)
  residual = c()
  set.seed(seed)
  for (i in 1:k) {
    p = .9 # ratio btw train vs. valid, which you can decide
    idx = sample.int(n = nrow(data), size = floor(p*nrow(data)), replace = FALSE)
    train = data[idx,]
    test = data[-idx,]
    lm.model = lm(lm, data = train)
    pvalue = predict(newdata = test[,-1], object = lm.model)
    difference = test[,1] - pvalue
    residual[i] = sum((difference)^2)/length(pvalue)
  }
  print(mean(residual))
}

CV(BODYFAT ~ ABDOMEN + WEIGHT + WRIST + FOREARM + NECK, data = bodyfat.new)
CV(BODYFAT ~ ABDOMEN + WEIGHT + WRIST + FOREARM, data = bodyfat.new)
CV(BODYFAT ~ AGE + WEIGHT + NECK + ABDOMEN + THIGH + FOREARM + WRIST, data = bodyfat.new)
CV(BODYFAT ~ log(ABDOMEN) + log(HEIGHT) + log(NECK), data = bodyfat.new)


#diagnostic
final_lm=lm(BODYFAT ~ ABDOMEN + WEIGHT + WRIST + FOREARM,
            data = bodyfat.new)
summary(final_lm)
#Multiple R-squared:  0.736,	Adjusted R-squared:  0.731
#residual plot
plot(predict(final_lm), resid(final_lm), pch=19, cex = 0.5, cex.lab=1.5, cex.main=1.5, col = "blue",
     xlab="Predicted Body Fat %", ylab="Standardized Residuals",main="Standardized Residual Plot")
abline(a=0, b=0, col="red", lwd = 3, lty = 3)
#almost no outliers

#nomality
shapiro.test(residuals(final_lm))
#p=0.08

par(mfrow = c(2,2))
plot(final_lm, pch = 19, cex = 0.5, col = "navy")
par(mfrow = c(1,1))
shapiro.test(residuals(final_lm))
#p=0.08
#remove influential point 39
par(mfrow = c(1,1))
plot(final_lm, pch = 19, cex = 0.5, col = "navy", which=4)
bodyfat.final = bodyfat.new[-39,]

final_lm2 = lm(BODYFAT ~ ABDOMEN + WEIGHT + WRIST + FOREARM , 
             data = bodyfat.final)
summary(final_lm2) 
#forearm is not significant

final_lm3=lm(BODYFAT ~ ABDOMEN + WEIGHT + WRIST, 
             data = bodyfat.final)
summary(final_lm3)


#check final_lm3
shapiro.test(residuals(final_lm3))
par(mfrow = c(2,2))
plot(final_lm3, pch = 19, cex = 0.4, col = "navy", which = 1)
par(mfrow = c(1,1))

##leverage and influential points
pii = hatvalues(final_lm3)
cooki = cooks.distance(final_lm3)
par(mfrow = c(2,1))
n = dim(bodyfat.final)[1]
plot(1:n, pii, type = "p", pch = 19, cex = 0.5, cex.lab = 1.5, cex.main = 1.5,
     xlab = "Index (Each Observation)", ylab = "Pii", main = "Leverage Values (Pii)", 
     col = "navy")
plot(1:n, cooki, type = "p", pch = 19, cex = 0.5, cex.lab = 1.5, cex.main = 1.5,
     xlab = "Index (Each Observation)", ylab = "Cook's Distance",
     main = "Influence Values (Cook's Distance)", col = "navy")
par(mfrow=c(1,1))




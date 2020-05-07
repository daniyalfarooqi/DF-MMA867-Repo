
library(MLmetrics)
library(StatMeasures)
library(dplyr)
library(tidyr)
library(corrgram)
library(MASS)
library(Hmisc)
library(ggplot2)
library(readxl)
library(glmnet)

trainRaw <- read.csv(file.choose(), header=TRUE, sep=",") #load the data into the HousingPrices dataframe
HousingPricesPrediction <- read.csv(file.choose(), header=TRUE, sep=",") #load the data into the HousingPrices dataframe
HousingPricesPrediction <- mutate(HousingPricesPrediction, SalePrice = 0)
trainRaw <- rbind(trainRaw,HousingPricesPrediction)


HousingPricesTesting<-subset(trainRaw, (Id>1100 & Id<=1460)) 
HousingPricesTraining<-subset(trainRaw, Id<=1100) 


# regression with all variables didnt work so we do the following to check if ....
# ....there is any factors that we have to drop due to singular results
c <- sapply(HousingPricesTraining, function(x) is.numeric(x))
numCols <- HousingPricesTraining[, c]

l <- sapply(HousingPricesTraining, function(x) is.factor(x))
m <- HousingPricesTraining[, l]
ifelse(n <- sapply(m, function(x) length(levels(x))) == 1, "DROP", "NODROP")

# I created transforms for the Age and Remod variables to see if a relationship there exists
HouseAge <- 2020 - HousingPricesTesting$YearBuilt
HouseRemod <- 2020 - HousingPricesTesting$YearRemodAdd
HousingPricesTesting <- cbind(HousingPricesTesting,HouseRemod,HouseAge)

HouseAge <- 2020-HousingPricesTraining$YearBuilt
HouseRemod <- 2020-HousingPricesTraining$YearRemodAdd
HousingPricesTraining <- cbind(HousingPricesTraining,HouseRemod,HouseAge)

HouseAge <- 2020 - HousingPricesPrediction$YearBuilt
HouseRemod <- 2020 - HousingPricesPrediction$YearRemodAdd
HousingPricesPrediction <- cbind(HousingPricesPrediction,HouseRemod,HouseAge)

HouseAge <- 2020 - trainRaw$YearBuilt
HouseRemod <- 2020 - trainRaw$YearRemodAdd
trainRaw <- cbind(trainRaw,HouseRemod,HouseAge)

# check correlation
correlationMatrix <- cor(numCols,use="complete.obs")
corrgram(correlationMatrix, order = TRUE, panel=, lower.panel=, upper.panel=, text.panel=, diag.panel=) 
SalePriceCor <- correlationMatrix[,"SalePrice"]
sort(SalePriceCor, decreasing = TRUE)

#check plots
pairs(numCols[c(38,2:6)])
pairs(HousingPricesTraining[c(81,2:13)])
pairs(cbind(m[c(31:43)],HousingPricesTraining[c(81)]))


# Data Cleanup

# Clean up of LotArea
Q <- quantile(HousingPricesTraining$LotArea, probs=c(0,0.99), na.rm = FALSE)
HousingPricesTraining <- mutate(HousingPricesTraining, LotArea = ifelse(as.numeric(HousingPricesTraining$LotArea) <= (Q[1]),NA,
                                 ifelse(as.numeric(HousingPricesTraining$LotArea) >= (Q[2]),NA,HousingPricesTraining$LotArea)))

# Clean up of MasVnrArea
Q <- quantile(HousingPricesTraining$MasVnrArea, probs=c(0,0.99), na.rm = TRUE)
HousingPricesTraining <- mutate(HousingPricesTraining, MasVnrArea = ifelse(as.numeric(HousingPricesTraining$MasVnrArea) <= (Q[1]),NA,
                                                                        ifelse(as.numeric(HousingPricesTraining$MasVnrArea) >= (Q[2]),NA,HousingPricesTraining$MasVnrArea)))

# Clean up of TotalBsmtSF
Q <- quantile(HousingPricesTraining$TotalBsmtSF, probs=c(0,0.99), na.rm = TRUE)
HousingPricesTraining <- mutate(HousingPricesTraining, TotalBsmtSF = ifelse(as.numeric(HousingPricesTraining$TotalBsmtSF) <= (Q[1]),NA,
                                                                           ifelse(as.numeric(HousingPricesTraining$TotalBsmtSF) >= (Q[2]),NA,HousingPricesTraining$TotalBsmtSF)))

# Clean up of X1stFlrSF
Q <- quantile(HousingPricesTraining$X1stFlrSF, probs=c(0,0.99), na.rm = TRUE)
HousingPricesTraining <- mutate(HousingPricesTraining, X1stFlrSF = ifelse(as.numeric(HousingPricesTraining$X1stFlrSF) <= (Q[1]),NA,
                                                                            ifelse(as.numeric(HousingPricesTraining$X1stFlrSF) >= (Q[2]),NA,HousingPricesTraining$X1stFlrSF)))

# Clean up of GrLivArea
Q <- quantile(HousingPricesTraining$GrLivArea, probs=c(0,0.99), na.rm = TRUE)
HousingPricesTraining <- mutate(HousingPricesTraining, GrLivArea = ifelse(as.numeric(HousingPricesTraining$GrLivArea) <= (Q[1]),NA,
                                                                          ifelse(as.numeric(HousingPricesTraining$GrLivArea) >= (Q[2]),NA,HousingPricesTraining$GrLivArea)))

# Clean up of HouseAge
Q <- quantile(HousingPricesTraining$HouseAge, probs=c(0,0.99), na.rm = TRUE)
HousingPricesTraining <- mutate(HousingPricesTraining, HouseAge = ifelse(as.numeric(HousingPricesTraining$HouseAge) <= (Q[1]),NA,
                                                                          ifelse(as.numeric(HousingPricesTraining$HouseAge) >= (Q[2]),NA,HousingPricesTraining$HouseAge)))


# HousingPricesTraining <- drop_na(HousingPricesTraining,SalePrice)


plot(SalePrice ~ TotRmsAbvGrd, data=HousingPricesTraining)
plot(log(SalePrice) ~ log(HouseAge), data=HousingPricesTraining)
plot(log(SalePrice) ~ log(HouseRemod), data=HousingPricesTraining)

# model 1 - HouseAge Transform - Use Log Log
fit <- lm(log(SalePrice) ~ log(HouseAge), data = HousingPricesTraining)
summary(fit)

plot(log(SalePrice) ~ OverallQual, data=HousingPricesTraining)
plot(log(SalePrice) ~ LotArea, data=HousingPricesTraining)
plot(log(SalePrice) ~ LotArea, xlim=c(0,20000),ylim=c(10,14), data=HousingPricesTraining)
plot(log(SalePrice) ~ (MasVnrArea), data=HousingPricesTraining)
plot(log(SalePrice) ~ log(MasVnrArea), data=HousingPricesTraining)
plot(log(SalePrice) ~ (TotalBsmtSF), data=HousingPricesTraining)
plot(log(SalePrice) ~ (GrLivArea), data=HousingPricesTraining)
plot(log(SalePrice) ~ (TotRmsAbvGrd), data=HousingPricesTraining)
plot(log(SalePrice) ~ FullBath, data=HousingPricesTraining)


# MAIN MODEL

fit <- stepAIC(lm(log(SalePrice) ~ log(HouseAge) + TotRmsAbvGrd + log(HouseRemod) + OverallQual + GrLivArea
          + GarageCars*GarageArea + TotalBsmtSF + log(X1stFlrSF) + LotArea + FullBath*BsmtFullBath
          + FullBath + BsmtFullBath + TotRmsAbvGrd*BedroomAbvGr
          + TotalBsmtSF * log(X1stFlrSF), data = HousingPricesTraining),direction="both")
summary(fit)


fit2 <- stepAIC(lm(log(SalePrice) ~ log(HouseAge) + TotRmsAbvGrd + log(HouseRemod) + OverallQual + GrLivArea
                   + GarageCars*GarageArea + TotalBsmtSF + log(X1stFlrSF) + LotArea + FullBath*BsmtFullBath
                   + MSSubClass + MSZoning + Neighborhood
                   + FullBath + BsmtFullBath + TotRmsAbvGrd * BedroomAbvGr + TotalBsmtSF * log(X1stFlrSF) 
                   + BldgType+ OverallCond + Fireplaces+ RoofStyle + KitchenQual+GarageCars+GarageArea, data = HousingPricesTraining),direction="both")
summary(fit)

predicted.prices <- exp(predict(fit, HousingPricesTesting))
predicted.prices2 <- exp(predict(fit, HousingPricesPrediction))
predicted.prices3 <- exp(predict(fit2, HousingPricesPrediction))


# RealPrices <- cbind(RealPrices,predicted.prices2)
# RealPrices <- na.omit(RealPrices) 

percent.errors.log.i <- abs((HousingPricesTesting$SalePrice - predicted.prices)/predicted.prices)*100
mean(percent.errors.log.i) 

hist(predicted.prices2)
mean(na.omit(predicted.prices2))

#write to excel
write.csv(predicted.prices3,"C:\\Users\\daniy\\Desktop\\predict_LASTModel.csv")


################### LASSO MODEL #######################

Y<-log(HousingPricesTraining$SalePrice)
X<-model.matrix(Id ~ log(HouseAge) + TotRmsAbvGrd + log(HouseRemod) + OverallQual + GrLivArea
                     + GarageCars*GarageArea + TotalBsmtSF + log(X1stFlrSF) + LotArea + FullBath*BsmtFullBath
                     + MSSubClass + MSZoning + Neighborhood
                     + FullBath + BsmtFullBath + TotRmsAbvGrd * BedroomAbvGr + TotalBsmtSF * log(X1stFlrSF) 
                     + BldgType+ OverallCond + Fireplaces+ RoofStyle + KitchenQual+GarageCars+GarageArea
                     , trainRaw)[,-1]

# X<-model.matrix(Id ~ log(HouseAge) + TotRmsAbvGrd + log(HouseRemod) + OverallQual + GrLivArea
#                 + GarageCars*GarageArea + TotalBsmtSF + log(X1stFlrSF) + LotArea + FullBath*BsmtFullBath
#                 + FullBath + BsmtFullBath + TotRmsAbvGrd * BedroomAbvGr
#                 + TotalBsmtSF * log(X1stFlrSF), trainRaw)[,-1]

X<-cbind(trainRaw$Id,X)


X.training<-subset(X,X[,1]<=1100)
X.testing<-subset(X, (X[,1]>1100 & X[,1]<=1460))
X.prediction<-subset(X,X[,1]>1460)

#LASSO (alpha=1)
#selecting the best penalty lambda
lasso.fit<-glmnet(x = X.training, y = Y, alpha = 1)
plot(lasso.fit, xvar = "lambda")

#create cross-validation data
crossval <-  cv.glmnet(x = X.training, y = Y, alpha = 1) 
plot(crossval)

#determine optimal penalty parameter, lambda
penalty.lasso <- crossval$lambda.min 

#see where it was on the graph
log(penalty.lasso)

# lets zoom-in
plot(crossval,xlim=c(),ylim=c())

#estimate the model with the optimal penalty
lasso.opt.fit <- glmnet(x = X.training, y = Y, alpha = 1, lambda = penalty.lasso) 

#resultant model coefficients
coef(lasso.opt.fit)

# predicting the performance on the testing set
lasso.testing_final <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.testing))

#calculate and display MAPE
mean(abs(lasso.testing_final- HousingPricesTesting$SalePrice)/HousingPricesTesting$SalePrice*100)

#predict
predicted.prices.log.i.lasso <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.prediction))

#write to excel
write.csv(predicted.prices.log.i.lasso,"C:\\Users\\daniy\\Desktop\\predict_Final2.csv")

##################################



# keep variable names handy to use
         # SalePrice,
         # X1stFlrSF,
         # GrLivArea,
         # TotRmsAbvGrd,
         # LotArea,
         # MSSubClass,
         # MSZoning,
         # Alley,
         # Neighborhood,
         # BldgType,
         # OverallQual,
         # OverallCond,
         # HouseAge,
         # HouseRemod,
         # RoofStyle
         # RoofMatl,
         # BsmtCond,
         # BsmtFinType1,
         # TotalBsmtSF,
         # HeatingQC,
         # CentralAir,
         # FullBath
         # GarageFinish,
         # BedroomAbvGr,
         # KitchenQual,
         # GarageCars




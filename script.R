library(data.table)
library(caTools)
library(car)
library(rpart)
library(rpart.plot)
library("Hmisc")

# data preparation and cleaning
setwd("/Users/malthus/Desktop/NTU/02 Y2S1/BC2406/AY21 Team Assignment and Project/Final Data and Script")
dt <- fread("data.csv")

# data exploration
nrow(dt) # total number of rows
sapply(dt, function(x) sum(is.na(x))) # number of NAs for each indicator
sapply(dt, function(x) sum(is.na(x)))/nrow(dt)*100 # percentage of NAs

# removing indicators with high percentage of NAs
dt[, cpia_policies:=NULL][, cpia_public:=NULL][, literacy_rate:=NULL][, poverty_ratio:=NULL]

# first dataset (raw) -> removing all rows with at least one NA value
raw.dt <- dt[complete.cases(dt),]

# second dataset (cleaned) -> replacing all NA values with mean value within country and year
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
dt[, infant_mortality:=impute.mean(infant_mortality), by = country]
dt[, unemployment:=impute.mean(unemployment), by = country]
dt[, homicides:=impute.mean(homicides), by = country]
dt[, suicide_mortality:=impute.mean(suicide_mortality), by = country]
dt[, rural:=impute.mean(rural), by = country]
dt[, education_exp:=impute.mean(education_exp), by = country]
dt[, refugees:=impute.mean(refugees), by = country] # ignore warnings, only slight loss in precision
dt[, fsi:=impute.mean(fsi), by = country]
cleaned.dt <- dt[complete.cases(dt),]

# dropping country and year for both datasets
raw.dt[, country:=NULL][, year:=NULL]
cleaned.dt[, country:=NULL][, year:=NULL]

# train-test split for raw dataset
set.seed(2406)
traintest.raw <- sample.split(Y = raw.dt$fsi, SplitRatio=0.7)
train.raw <- subset(raw.dt, traintest.raw == T)
test.raw <- subset(raw.dt, traintest.raw == F)

# train-test split for cleaned dataset
set.seed(2406)
traintest.cleaned <- sample.split(Y = cleaned.dt$fsi, SplitRatio=0.7)
train.cleaned <- subset(cleaned.dt, traintest.cleaned == T)
test.cleaned <- subset(cleaned.dt, traintest.cleaned == F)

# linear regression - raw dataset
raw.lm <- lm(fsi ~., data=train.raw)
summary(raw.lm) # linear regression model without stepwise
raw.lm.step <- step(raw.lm)
summary(raw.lm.step) # stepwise model
raw.pred <- predict(raw.lm.step, newdata=test.raw)
raw.rmse <- sqrt(sum((test.raw$fsi - raw.pred)**2)/length(raw.pred)) # calculating RMSE to measure model accuracy
print(raw.rmse)
# plot(raw.lm.step) -> uncomment and run to get diagnostic checks (commenting out for smooth running of code)
vif(raw.lm.step) # checking for multi-collinearity

# linear regression - cleaned dataset
cleaned.lm <- lm(fsi ~., data=train.cleaned)
summary(cleaned.lm)
cleaned.lm.step <- step(cleaned.lm)
summary(cleaned.lm.step)
cleaned.pred <- predict(cleaned.lm.step, newdata=test.cleaned)
cleaned.rmse <- sqrt(sum((test.cleaned$fsi - cleaned.pred)**2)/length(cleaned.pred))
print(cleaned.rmse)
# plot(cleaned.lm.step) -> uncomment and run to get diagnostic checks (commenting out for smooth running of code)
vif(cleaned.lm.step) # checking for multi-collinearity

# continuous optimal CART - raw dataset
raw.cart <- rpart(fsi ~ ., data = train.raw, method = 'anova', control = rpart.control(minsplit = 2, cp=0))
CVerror.cap <- raw.cart$cptable[which.min(raw.cart$cptable[,"xerror"]), "xerror"] + raw.cart$cptable[which.min(raw.cart$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (raw.cart$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
raw.cp = ifelse(i > 1, sqrt(raw.cart$cptable[i,1] * raw.cart$cptable[i-1,1]), 1) # cp to get optimal tree
raw.cart.opti = prune(raw.cart, cp = raw.cp) # optimal CART model
raw.cart.opti$variable.importance # importance of each variable in determining CART splits
plotcp(raw.cart.opti) # checking cv error against complexity parameter
printcp(raw.cart.opti)
# rpart.plot(raw.cart.opti, nn = T) -> uncomment to see plot of CART model (commenting out for smooth running of code)
raw.cart.pred = predict(raw.cart.opti, newdata = test.raw)
raw.cart.rmse <- sqrt(sum((test.raw$fsi - raw.cart.pred)**2)/length(raw.cart.pred)) # calculating RMSE to measure model accuracy
print(raw.cart.rmse)

# continuous optimal CART - cleaned dataset
cleaned.cart <- rpart(fsi ~ ., data = train.cleaned, method = 'anova', control = rpart.control(minsplit = 2, cp=0))
CVerror.cap <- cleaned.cart$cptable[which.min(cleaned.cart$cptable[,"xerror"]), "xerror"] + cleaned.cart$cptable[which.min(cleaned.cart$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4

while (cleaned.cart$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
cleaned.cp = ifelse(i > 1, sqrt(cleaned.cart$cptable[i,1] * cleaned.cart$cptable[i-1,1]), 1)
cleaned.cart.opti = prune(cleaned.cart, cp = cleaned.cp)
cleaned.cart.opti$variable.importance
plotcp(cleaned.cart.opti)
printcp(cleaned.cart.opti)
# rpart.plot(cleaned.cart.opti, nn = T) -> uncomment to see plot of CART model (commenting out for smooth running of code)
cleaned.cart.pred = predict(cleaned.cart.opti, newdata = test.cleaned)
cleaned.cart.rmse <- sqrt(sum((test.cleaned$fsi - cleaned.cart.pred)**2)/length(cleaned.cart.pred))
print(cleaned.cart.rmse)

# summary of model results
cat("Linear Regression (Raw Dataset) RMSE:", raw.rmse)
cat("Linear Regression (Cleaned Dataset) RMSE:", cleaned.rmse)
cat("Continuous CART (Raw Dataset) RMSE:", raw.cart.rmse)
cat("Continuous CART (Cleaned Dataset) RMSE:", cleaned.cart.rmse)

# testing predictive models using Argentina 2015 data
argentina.2015 <- data.frame(infant_mortality=c(10.2), unemployment=c(7.61), homicides=c(6.5861233), suicide_mortality=c(8.3), rural=c(8.497), education_exp=c(5.2609218), refugees=c(186))
arg.2015.raw.lm = predict(raw.lm.step, newdata = argentina.2015) # using linear regression model trained on raw dataset
cat("Linear Regression (Raw Dataset) Prediction for Argentina in 2015:", arg.2015.raw.lm)
arg.2015.cleaned.lm = predict(cleaned.lm.step, newdata = argentina.2015) # using linear regression model trained on cleaned dataset
cat("Linear Regression (Cleaned Dataset) Prediction for Argentina in 2015:", arg.2015.cleaned.lm)
arg.2015.raw.cart = predict(raw.cart.opti, newdata = argentina.2015) # using CART model trained on raw dataset
cat("Continuous CART (Raw Dataset) Prediction for Argentina in 2015:", arg.2015.raw.cart)
arg.2015.cleaned.cart = predict(cleaned.cart.opti, newdata = argentina.2015) # using CART model trained on cleaned dataset
cat("Continuous CART (Cleaned Dataset) Prediction for Argentina in 2015:", arg.2015.cleaned.cart)


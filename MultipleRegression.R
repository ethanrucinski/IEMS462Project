#library(glmnet)

library(leaps)
#library(bestglm)

data <- read.csv("Huazhen_Joel_Ethan_Updated.csv")

# Generate regression data set
regDat = data[data$targdol > 0,]

# remove extraneous variables (Per Joel)
df<-within(regDat,rm(dm_ad,dm_lp,datelp6,X.1,X,ordtyr,ordlyr,datead6))

# create new interaction variables for consistency (Per Joel)
df$consistent1<-df$ordtyr2*df$ordlyr2
df$consistent2<-df$consistent1*df$ord2ago
df$consistent3<-df$consistent2*df$ord3ago

# binary variables for orders
df$ordtb<-ifelse(df$ordtyr2>0,1,0)
df$ordlb<-ifelse(df$ordlyr2>0,1,0)
df$ord2b<-ifelse(df$ord2ago>0,1,0)
df$ord3b<-ifelse(df$ord3ago>0,1,0)

# binary consistency variables for orders
df$cons1b<-df$ordtb*df$ordlb
df$cons2b<-df$cons1b*df$ord2b
df$cons3b<-df$cons2b*df$ord3b

# Define data sets removing sprord and train because sprord is correlated with falord
train<-df[which(df$train==1),]
train <- data.frame(train[,c(-11,-12)])
test<-df[which(df$train!=1),]
test <- data.frame(test[,c(-11,-12)])


fit1 = lm(targdol~., data = train)
summary(fit1)
#step(fit1,direction="backward")
stepReg <- step(fit1,direction="both")
#fit2 = lm(targdol~1, train)
#summary(fit2)
#biggest=formula(lm(targdol~.,data=train))
#step(fit2,direction="forward",scope=biggest)
#step(fit2,direction="both",scope=biggest)
summary(stepReg)

# Let's remove outliers 37839, 38283, and 90895
train <- train[which(rownames(train) != 37839),]
train <- train[which(rownames(train) != 38283),]
train <- train[which(rownames(train) != 90895),]
fit2 = lm(targdol~., data = train)
summary(fit2)
stepReg2 <- step(fit2,direction="both")
summary(stepReg2)

fit3 = lm(log(targdol)~., data = train)
summary(fit3)
stepReg3 <- step(fit3,direction="both")
summary(stepReg3)
plot(stepReg3)


library(glmnet)
x<-train[,-1]
y<-train[,1]

test<-df[which(df$train!=1),]
x_test<-test[,-1]
y_test<-test[,1]
x_test<-sapply(x_test,as.numeric)
x<-sapply(x,as.numeric)

# str(x)

# fit the model using LASSO regression (this takes a minute or two)
cvfit<-cv.glmnet(x,y)

plot(cvfit)

coef(cvfit, s= cvfit$lambda.min)


newFit = lm(targdol ~ slstyr + slslyr + sls2ago + slshist + ordhist + falord + consistent3 + ordlb + ord3b + cons2b + cons3b, data = train)

newData = polym(df$slstyr, df$slslyr, df$sls2ago, df$sls3ago, df$slshist, degree = 5, raw = TRUE)
tryInteractions = data.frame(df, newData)

summary(tryInteractions)
tryInteractions = sapply(tryInteractions,as.numeric)
tryNewCvfit<-cv.glmnet(tryInteractions[,-1],tryInteractions[,1])
coef(tryNewCvfit, s = tryNewCvfit$lambda.min)

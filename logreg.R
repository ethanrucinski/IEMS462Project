library(glmnet)
library(MASS)

data<-read.csv("Huazhen_Joel_Ethan_Updated.csv")

# remove extraneous variables
df<-within(data,rm(dm_ad,dm_lp,datelp6,X.1,X,ordtyr,ordlyr,datead6))

# create new interaction variables for consistency
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

# gzro is the response variable
df$gzro<-ifelse(df$targdol>0,1,0)

df$targdol<-NULL

# split into training and test
train<-df[which(df$train==1),]

N<-ncol(train)

x<-train[,-N]
y<-train[,N]

test<-df[which(df$train!=1),]
x_test<-test[,-N]
y_test<-test[,N]
x_test<-sapply(x_test,as.numeric)
x<-sapply(x,as.numeric)

# str(x)

# fit the model using LASSO regression (this takes a minute or two)
cvfit<-cv.glmnet(x,y,family="binomial",type.measure = "class")

plot(cvfit)

# most coefficients should be zero
coef(cvfit,s="lambda.min")
# summary(cvfit)

# make predictions on the test data, calculate correct classification rate (CCR), 2x2 table
pred<-predict(cvfit,newx=x_test,s="lambda.min",type="class")
tab<-table(pred,y_test)
CCR<-sum(diag(tab))/sum(tab)
CCR
tab

# output probabilities (what's actually used with the multiple regression model)
probs<-predict(cvfit,newx=x_test,s="lambda.min",type="response")
colnames(probs)<-"logistic probs"

# save work for use in model
write.csv(probs,"logistic_probs.csv")
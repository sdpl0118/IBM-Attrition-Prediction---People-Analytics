#Package Installation
install.packages("keras")
source("DataAnalyticsFunctions.R")
install.packages("glmnet")
install.packages("ggcorrplot")
installpkg("tree")
library(tree)
installpkg("partykit")
library(partykit)
library(ggplot2)

installpkg <- function(x){
  if(x %in% rownames(installed.packages())==FALSE) {
    if(x %in% rownames(available.packages())==FALSE) {
      paste(x,"is not a valid package - please check again...")
    } else {
      install.packages(x)           
    }
    
  } else {
    paste(x,"package already installed...")
  }
}

#Testing
FPR_TPR <- function(prediction, actual){
  
  TP <- sum((prediction)*(actual))
  FP <- sum((prediction)*(!actual))
  FN <- sum((!prediction)*(actual))
  TN <- sum((!prediction)*(!actual))
  result <- data.frame( FPR = FP / (FP + TN), TPR = TP / (TP + FN), ACC = (TP+TN)/(TP+TN+FP+FN) )
  
  return (result)
}

BinaryAccuracy <- function(prediction, actual){
  
  TP <- sum((prediction)*(actual))
  FP <- sum((prediction)*(!actual))
  FN <- sum((!prediction)*(actual))
  TN <- sum((!prediction)*(!actual))
  result <-  (TP+TN)/(TP+TN+FP+FN) 
  
  return (result)
}
deviance <- function(y, pred, family=c("gaussian","binomial")){
  family <- match.arg(family)
  if(family=="gaussian"){
    return( sum( (y-pred)^2 ) )
  }else{
    if(is.factor(y)) y <- as.numeric(y)>1
    return( -2*sum( y*log(pred) + (1-y)*log(1-pred) ) )
  }
}

devianceQR <- function(y, pred, tau){
  return( sum(  tau*max(0, y-pred ) + (1-tau)*max(0, pred-y ) ) )
  
}

## get null devaince too, and return R2
R2 <- function(y, pred, family=c("gaussian","binomial")){
  fam <- match.arg(family)
  if(fam=="binomial"){
    if(is.factor(y)){ y <- as.numeric(y)>1 }
  }
  dev <- deviance(y, pred, family=fam)
  dev0 <- deviance(y, mean(y), family=fam)
  return(1-dev/dev0)
}


### Returns the indices for which |x[i]| > tr
support<- function(x, tr = 10e-6) {
  m<- rep(0, length(x))
  for (i in 1:length(x)) if( abs(x[i])> tr ) m[i]<- i
  m <- m[m>0]
  m
}

### Penalty choice for Quantile Regression
lambda.BC<- function(X, R = 1000, tau = 0.5, c = 1, alpha = .05){
  n <- nrow(X)
  norm2n<-function(z){sqrt(mean(z^2))}
  sigs <- apply(X,2,norm2n)
  U <- matrix(runif(n * R),n)
  R <- (t(X) %*% (tau - (U < tau)))/(sigs*sqrt(tau*(1-tau)))
  r <- apply(abs(R),2,max)
  c * quantile(r, 1 - alpha) * sqrt(tau*(1-tau))*c(1,sigs)
}

kIC <- function(fit, rule=c("A","B","C")){
  df <- length(fit$centers) # K*dim
  n <- sum(fit$size)
  D <- fit$tot.withinss # deviance
  rule=match.arg(rule)
  if(rule=="A")
    #return(D + 2*df*n/max(1,n-df-1))
    return(D + 2*df)
  else if(rule=="B") 
    return(D + log(n)*df)
  else 
    return(D +  sqrt( n * log(df) )*df)
}

#Data Import
IBMdata<- read.csv("IBMemployee.csv")
names(IBMdata)

#DATA UNDERSTANDING ############
sum(IBMdata$Attrition=="Yes")
sum(IBMdata$Attrition=="No")

############### DATA PREPARATION ##################
#creating dummy variables
IBMdata1<-dummy_cols(IBMdata, select_columns = c("BusinessTravel", "Department","EducationField","JobRole","MaritalStatus"))
names(IBMdata1)
summary(IBMdata1)

#removing variables that have the same value for every observation
IBMdata1<-IBMdata1[-c(9,22,27)]

#changing certain variables to binary variables
IBMdata1$Attrition<-ifelse(IBMdata1$Attrition=="Yes",1,0) 
IBMdata1$OverTime<-ifelse(IBMdata1$OverTime=="Yes",1,0) 
IBMdata1$Gender<-ifelse(IBMdata1$Gender=="Female",1,0) 
names(IBMdata1)
head(IBMdata1)

#removing categorical variables since we already have them in binary form 
IBMdata2<-IBMdata1[-c(3,5,8,15,17)]

#variable name adjustments, removing hyphens 
colnames(IBMdata2) = gsub('-','_', colnames(IBMdata2))
colnames(IBMdata2) = gsub(' ','_', colnames(IBMdata2))
colnames(IBMdata2)[1] = "Age"
names(IBMdata2)

#create train and test set for cross validation 
set.seed(1)
holdout.indices <- sample(nrow(IBMdata2), 294)
IBMdata2.holdout <- IBMdata2[holdout.indices,]
IBMdata2 <- IBMdata2[-holdout.indices,]
nrow(IBMdata2.holdout)
nrow(IBMdata2)
names(IBMdata2)

#checking for balance b/w holdout and train, similar attrition rates 
mean(IBMdata2$Attrition==1)
mean(IBMdata2.holdout$Attrition==1)
head(IBMdata2)

######### DATA EXPLORATION #######
#correlation matrix 
library(ggcorrplot)
nums <- select_if(IBMdata, is.numeric)
corr <- round(cor(nums), 1)
ggcorrplot(corr, type = "lower", lab = TRUE, lab_size = 3,  method="square", colors = c("tomato2", "white", "#01A9DB"), title="Exploratory Correlogram",  ggtheme=theme_minimal())

#visualization 1
library(tidyverse)
p1 <- IBMdata2 %>% 
  count(Attrition) %>%
  ggplot(aes(reorder(Attrition, n), n)) +
  geom_col(width = 0.3) +
  coord_flip() +
  ggtitle("Total count of Attrition")
## percent of whole
p2 <- IBMdata2 %>% 
  count(Attrition) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(reorder(Attrition, pct), pct)) +
  geom_col(width = 0.3) +
  coord_flip() +
  ggtitle("Percent of Attrition")
gridExtra::grid.arrange(p1, p2, nrow = 1)

#visualization 2
p3 <- ggplot(IBMdata2, aes(x = factor(YearsWithCurrManager), y = DailyRate)) +
  scale_y_continuous(labels = scales::dollar) + labs(x="Years with Current Manager") + geom_boxplot()
p4 <- ggplot(IBMdata2, aes(x = factor(Attrition), y = YearsWithCurrManager)) +
  labs(x="Attrition", y="Years with Current Manager")+geom_boxplot() 
gridExtra::grid.arrange(p3, p4,nrow = 2)

#visalization 3
ggplot(IBMdata2, aes(x =IBMdata2$YearsAtCompany, y = DailyRate, color= Attrition)) +
  geom_point(alpha = .3) + scale_colour_gradientn(colours=rainbow(4)) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "Years At the Company", y = "Daily Wage")+
  ggtitle("Wage vs. Years At the Company",
          subtitle = "What are the wage trends for a longer tenure at IBM?") +theme_bw()

#visual 4
ggplot(data=IBMdata2,aes(x=JobSatisfaction, y=MonthlyIncome))+geom_point(aes(color=PerformanceRating))+facet_wrap(~Attrition)

#k-means
kmean1 <- scale(IBMdata2)
kmean1 <- model.matrix(Attrition ~ ., data=IBMdata2)
head(IBMdata2)

#centers
FourCenters <- kmeans(kmean1,4,nstart=30)
FourCenters$centers[1,]
FourCenters$centers[2,]
FourCenters$centers[3,]
FourCenters$centers[4,]

#cluster size 
FourCenters$size
aggregate(IBMdata2$Attrition==1~FourCenters$cluster, FUN=mean)

#variation explained with 4 clusters, around 17%
1 - FourCenters$tot.withinss/ FourCenters$totss

#PCA
x <- model.matrix(Attrition ~ ., data=IBMdata2)
x <- scale(IBMdata2)
pca.x <- prcomp(x,scale=TRUE)
summary(pca.x)
par(mar=c(4,4,4,4)+0.3)
plot(pca.x,main="PCA: Variance Explained by Factors")
mtext(side=1, "Factors",  line=1, font=2)

## Interpreting the four factors
loadings <- pca.x$rotation[,1:4]
loadings

###1st factor responsible for 3/4 of the squared norm of the loadings
v<-loadings[order(abs(loadings[,1]), decreasing=TRUE)[1:ncol(x)],1]
loadingfit <- lapply(1:ncol(x), function(k) ( t(v[1:k])%*%v[1:k] - 1/2 )^2)
v[1:which.min(loadingfit)]

#### 2nd factor 
v<-loadings[order(abs(loadings[,2]), decreasing=TRUE)[1:ncol(x)],2]
loadingfit <- lapply(1:ncol(x), function(k) ( t(v[1:k])%*%v[1:k] - 1/2 )^2)
v[1:which.min(loadingfit)]

#### 3rd factor 
v<-loadings[order(abs(loadings[,3]), decreasing=TRUE)[1:ncol(x)],3]
loadingfit <- lapply(1:ncol(x), function(k) ( t(v[1:k])%*%v[1:k] - 1/2 )^2)
v[1:which.min(loadingfit)]

#### 4th factor 
v<-loadings[order(abs(loadings[,4]), decreasing=TRUE)[1:ncol(x)],4]
loadingfit <- lapply(1:ncol(x), function(k) ( t(v[1:k])%*%v[1:k] - 1/2 )^2)
v[1:which.min(loadingfit)]

####### MODELING ##############

#logisitc regression 
logreg1 <-glm(Attrition~.,data=IBMdata2,family="binomial")
library(MASS)
library(glmnet)
backward.model <- stepAIC(logreg1, direction = "backward")
OOS <- data.frame(logistic.interaction=NA, logistic=NA, tree=NA, null=NA)

#Logistic regression with interactions
model.logistic.interaction = glm(Attrition ~ .^2, IBMdata2, family = 'binomial')

summary(model.logistic.interaction)

#logistic regression with high aic variables
model.logistic <-glm(Attrition~Age + DistanceFromHome + EnvironmentSatisfaction + 
                       Gender + JobInvolvement + JobSatisfaction + NumCompaniesWorked + 
                       OverTime + RelationshipSatisfaction + TotalWorkingYears + 
                       TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + 
                       YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager + 
                       BusinessTravel_Non_Travel + BusinessTravel_Travel_Frequently + 
                       Department_Human_Resources + EducationField_Life_Sciences + 
                       EducationField_Medical + EducationField_Other + JobRole_Healthcare_Representative + 
                       JobRole_Manager + JobRole_Manufacturing_Director + JobRole_Research_Director + 
                       JobRole_Research_Scientist + JobRole_Sales_Executive + 
                       MaritalStatus_Divorced + MaritalStatus_Married, data=IBMdata2, family="binomial")

#decision tree
model.tree <- tree(Attrition~Age + DistanceFromHome + EnvironmentSatisfaction + 
                     Gender + JobInvolvement + JobSatisfaction + NumCompaniesWorked + 
                     OverTime + RelationshipSatisfaction + TotalWorkingYears + 
                     TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + 
                     YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager + 
                     BusinessTravel_Non_Travel + BusinessTravel_Travel_Frequently + 
                     Department_Human_Resources + EducationField_Life_Sciences + 
                     EducationField_Medical + EducationField_Other + JobRole_Healthcare_Representative + 
                     JobRole_Manager + JobRole_Manufacturing_Director + JobRole_Research_Director + 
                     JobRole_Research_Scientist + JobRole_Sales_Executive + 
                     MaritalStatus_Divorced + MaritalStatus_Married, data=IBMdata2)

#null model
model.null <- glm(Attrition~1, data=IBMdata2, family="binomial")

#predictions for each model
pred.logistic.interaction = predict(model.logistic.interaction, IBMdata2.holdout, type = 'response')
pred.logistic             <- predict(model.logistic, newdata=IBMdata2.holdout, type="response")
pred.tree                 <- predict(model.tree, newdata=IBMdata2.holdout, type="vector")
pred.null <- predict(model.null, newdata=IBMdata2.holdout, type="response")

#Random Forest
library(randomForest)
fit <- randomForest(Attrition ~ Age + DistanceFromHome + EnvironmentSatisfaction + 
                      Gender + JobInvolvement + JobSatisfaction + NumCompaniesWorked + 
                      OverTime + RelationshipSatisfaction + TotalWorkingYears + 
                      TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + 
                      YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager + 
                      BusinessTravel_Non_Travel + BusinessTravel_Travel_Frequently + 
                      Department_Human_Resources + EducationField_Life_Sciences + 
                      EducationField_Medical + EducationField_Other + JobRole_Healthcare_Representative + 
                      JobRole_Manager + JobRole_Manufacturing_Director + JobRole_Research_Director + 
                      JobRole_Research_Scientist + JobRole_Sales_Executive + 
                      MaritalStatus_Divorced + MaritalStatus_Married,   data=IBMdata2, importance = T)
pred.rf = predict(fit, IBMdata2.holdout, family = 'binomial')


#Calculate R2 for all the four models
OOS$logistic <- R2(y=IBMdata2.holdout$Attrition, pred=pred.logistic, family="binomial")
rss.tree = sum((pred.tree - IBMdata2.holdout$Attrition)^2)
tss = sum((IBMdata2.holdout$Attrition - mean(IBMdata2.holdout$Attrition))^2)
rss.logint = sum((pred.logistic.interaction - IBMdata2.holdout$Attrition)^2)
tss = sum((IBMdata2.holdout$Attrition - mean(IBMdata2.holdout$Attrition))^2)
OOS$logistic.interaction = 1-(rss.logint/tss)
OOS$tree = 1-(rss.tree/tss)

#Null model (just intercept)
OOS$null <- R2(y=IBMdata2.holdout$Attrition, pred=pred.null, family="binomial")
OOS$RandomForest = R2(y=IBMdata2.holdout$Attrition, pred=pred.rf, family="binomial")

fit
OOS

#Significant variables
fit$importance

#Varaible significance based on Mean Squared Error percentage increase
library(plotly)
columns = c('Age', 'DistanceFromHome', 'EnvironmentSatisfaction', 'Gender', 'JobInvolvement', 'JobSatisfaction', 'NumCompaniesWorked', 'OverTime', 'RelationshipSatisfaction', 'TotalWorkingYears', 'TrainingTimesLastYear', 'WorkLifeBalance', 'YearsAtCompany', 'YearsInCurrentRole', 'YearsSinceLastPromotion', 'YearsWithCurrManager', 'BusinessTravel_Non_Travel', 'BusinessTravel_Travel_Frequently', 'Department_Human_Resources', 'EducationField_Life_Sciences', 'EducationField_Medical', 'EducationField_Other', 'JobRole_Healthcare_Representative', 'JobRole_Manager', 'JobRole_Manufacturing_Director', 'JobRole_Research_Director', 'JobRole_Research_Scientist', 'JobRole_Sales_Executive', 'MaritalStatus_Divorced', 'MaritalStatus_Married')
p <- plot_ly(x = fit$importance[,1], y = columns, type = 'bar', orientation = 'h', xlab = 'MSE_PercentageIncrease')
p


#ROC Curve Graph
plot( c( 0, 1 ), c(0, 1), type="n", xlim=c(0,1), ylim=c(0,1), bty="n", xlab = "False positive rate", ylab="True positive rate")
lines(c(0,1),c(0,1), lty=2)
val<- .5

values <- FPR_TPR( (model.logistic.interaction$fitted >= val) , model.logistic.interaction$y )
points( values$FPR , values$TPR )
ACC.model.logistic.interaction <- values$ACC
text( values$FPR+.12, values$TPR+.05)

values <- FPR_TPR( (model.logistic$fitted >= val) , model.logistic$y )
points( values$FPR , values$TPR)    
ACC.model.logistic <- values$ACC
text( values$FPR+.02, values$TPR, labels=c("LR"))

values <- FPR_TPR( (model.tree$fitted >=val) , model.logistic.interaction$y )
points( values$FPR , values$TPR )
ACC.model.tree <- values$ACC
text( values$FPR, values$TPR-.02, labels=c("tree"))

values <- FPR_TPR( (fit$fitted >=val) , fit$y )
points( values$FPR , values$TPR )
ACC.model.randomforest <- values$ACC
text( values$FPR, values$TPR-.02, labels=c("RF"))



for( val in seq(from=0,to=1,by=0.05)){
  values <- FPR_TPR( (model.logistic.interaction$fitted >= val) , model.logistic.interaction$y )
  points( values$FPR , values$TPR, pch = 21, bg="red" )
  values <- FPR_TPR( (model.logistic$fitted >= val) , model.logistic$y )
  points( values$FPR , values$TPR, pch = 22, bg="blue" )    
  values <- FPR_TPR( (predict(model.tree,type="vector") >= val) , model.logistic.interaction$y )
  points( values$FPR , values$TPR, pch = 23, bg="green" )
  values <- FPR_TPR( (predict(fit,type="response") >= val) , fit$y )
  points( values$FPR , values$TPR, pch = 24, bg="yellow" )
}
legend("right",values$FPR, values$TPR, legend=c("LR with interaction", "LR", "tree", "Random forest"),
       col=c("red", "blue", "green", "yellow"), fill=c("red", "blue", "green", "yellow"), cex=0.8)



#Accuracies
barplot(c(ACC.model.logistic.interaction, ACC.model.logistic, ACC.model.tree, ACC.model.randomforest), xpd=FALSE, ylim=c(0,1), xlab="Method", names = c("\n logistic \n interaction", "\n logistic \n", "\n classif. \n tree", "\nRandom forest"), ylab = "Accuracy")


#Lasso and Post lasso with k cross validation to calculate OOS R2 with aggressive, conservative and theoritical selections
library(glmnet)
Mx<- model.matrix(Attrition ~.^2, data=IBMdata2)[,-1] # only for training
My<- IBMdata2$Attrition
num.features <- ncol(Mx)
num.n <- nrow(Mx)
num.churn <- sum(My)
nfold = 10
w <- (num.churn/num.n)*(1-(num.churn/num.n))
lambda.theory <- sqrt(w*log(num.features/0.05)/num.n)
lassoTheory <- glmnet(Mx,My, family="binomial",lambda = lambda.theory)
summary(lassoTheory)
support(lassoTheory$beta)
colnames(Mx)[support(lassoTheory$beta)]
length(support(lassoTheory$beta))
lasso <- glmnet(Mx,My, family = "binomial")
lassoCV <- cv.glmnet(Mx,My, family = "binomial")
text(log(lassoCV$lambda.min), .95,"min",cex=1)
text(log(lassoCV$lambda.1se), 1,"1se",cex=1)

lines(c(log(lambda.theory),log(lambda.theory)),c(0.3,2.4),lty=3,col="blue")
text(log(lambda.theory), 1.05,"theory",cex=1)

lassomin  <- glmnet(Mx,My, family="binomial",lambda = lassoCV$lambda.min)
lasso1se  <- glmnet(Mx,My, family="binomial",lambda = lassoCV$lambda.1se)
lassoTheory <- glmnet(Mx,My, family="binomial",lambda = lambda.theory)

PL.OOS <- data.frame(PL.min=rep(NA,nfold), PL.1se=rep(NA,nfold), PL.theory=rep(NA,nfold)) 
L.OOS <- data.frame(L.min=rep(NA,nfold), L.1se=rep(NA,nfold), L.theory=rep(NA,nfold)) 
features.min <- support(lasso$beta[,which.min(lassoCV$cvm)])
length(features.min)
features.1se <- support(lasso$beta[,which.min( (lassoCV$lambda-lassoCV$lambda.1se)^2)])
length(features.1se) 
features.theory <- support(lassoTheory$beta)
length(features.theory)

data.min <- data.frame(Mx[,features.min],My)
data.1se <- data.frame(Mx[,features.1se],My)
data.theory <- data.frame(Mx[,features.theory],My)

for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  train
  ### This is the CV for the Post Lasso Estimates
  rmin <- glm(My~., data=data.min, subset=train, family="binomial")
  if ( length(features.1se) == 0){  r1se <- glm(Attrition~1, data=IBMdata2, family="binomial") 
  } else {r1se <- glm(My~., data=data.1se, subset=train, family="binomial")
  }
  
  if ( length(features.theory) == 0){ 
    rtheory <- glm(Attrition~1, data=IBMdata2, subset=train, family="binomial") 
  } else {rtheory <- glm(My~., data=data.theory, subset=train, family="binomial") }
  
  
  predmin <- predict(rmin, newdata=data.min, type="response")
  pred1se  <- predict(r1se, newdata=data.1se, type="response")
  predtheory <- predict(rtheory, newdata=data.theory, type="response")
  PL.OOS$PL.min[k] <- R2(y=My, pred=predmin, family="binomial")
  PL.OOS$PL.1se[k] <- R2(y=My, pred=pred1se, family="binomial")
  PL.OOS$PL.theory[k] <- R2(y=My, pred=predtheory, family="binomial")
  
  ### This is the CV for the Lasso estimates  
  lassomin  <- glmnet(Mx,My, family="binomial",lambda = lassoCV$lambda.min)
  lasso1se  <- glmnet(Mx,My, family="binomial",lambda = lassoCV$lambda.1se)
  lassoTheory <- glmnet(Mx,My, family="binomial",lambda = lambda.theory)
  
  predlassomin <- predict(lassomin, newx=Mx, type="response")
  predlasso1se  <- predict(lasso1se, newx=Mx, type="response")
  predlassotheory <- predict(lassoTheory, newx=Mx, type="response")
  L.OOS$L.min[k] <- R2(y=My, pred=predlassomin, family="binomial")
  L.OOS$L.1se[k] <- R2(y=My, pred=predlasso1se, family="binomial")
  L.OOS$L.theory[k] <- R2(y=My, pred=predlassotheory, family="binomial")
  
  print(paste("Iteration",k,"of",nfold,"completed"))
}

PL.OOS$PostLassoAggressive = mean(PL.OOS$PL.min)
PL.OOS$PostLassoConservative = mean(PL.OOS$PL.1se)
PL.OOS$PostLassoTheory = mean(PL.OOS$PL.theory)
PL.OOS = unique(subset(PL.OOS, select = c(PostLassoAggressive, PostLassoConservative, PostLassoTheory)))


L.OOS$LassoAggressive = mean(L.OOS$L.min)
L.OOS$LassoConservative = mean(L.OOS$L.1se)
L.OOS$LassoTheory = mean(L.OOS$L.theory)
L.OOS = unique(subset(L.OOS, select = c(LassoAggressive, LassoConservative, LassoTheory)))

#Combined all performance metrics
R2performance <- cbind(PL.OOS,L.OOS,OOS)
R2performance


#Neural Network 
set.seed(1)
holdout.indices <- sample(nrow(IBMdata2), 294)
IBMdata2.holdout <- IBMdata2[holdout.indices,]
IBMdata2 <- IBMdata2[-holdout.indices,]
library(keras)

x.holdout<- model.matrix(Attrition ~ ., data=IBMdata2.holdout)[,-1]
y.holdout<- IBMdata2.holdout$Attrition == 1

x.data<- model.matrix(Attrition ~ ., data=IBMdata2)[,-1]
y.data<- IBMdata2$Attrition == 1

#rescale (to be between 0 and 1)
x_train <- x.data %*% diag(1/apply(x.data, 2, function(x) max(x, na.rm = TRUE)))
y_train <- as.numeric(y.data)
x_test <- x.holdout %*% diag(1/apply(x.data, 2, function(x) max(x, na.rm = TRUE)))
y_test <- as.numeric(y.holdout) 
#rescale (unit variance and zero mean)
mean <- apply(x.data,2,mean)
std <- apply(x.data,2,sd)
x_train <- scale(x.data,center = mean, scale = std)
y_train <- as.numeric(y.data)
x_test <- scale(x.holdout,center = mean, scale = std)
y_test <- as.numeric(y.holdout) 
num.inputs <- ncol(x_test)
model <- keras_model_sequential() %>%
  layer_dense(units=16,activation="relu",input_shape = c(num.inputs)) %>%
  layer_dense(units=16,activation="relu") %>%
  layer_dense(units=16,activation="relu") %>%
  layer_dense(units=1,activation="sigmoid")
summary(model)
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)
history <- model %>% fit(
  x_train, y_train, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)
# return loss and accuracy for NN
results.NN1 <- model %>% evaluate(x_train,y_train)
results.NN1

results.NN1 <- model %>% evaluate(x_test,y_test)
results.NN1

pred.NN1 <- model%>% predict(x_test)
PerformanceMeasure(actual=y_test, prediction=pred.NN1, threshold=.5)

### Adding Regularizer
model <- keras_model_sequential() %>%
  layer_dense(units=16, kernel_regularizer = regularizer_l1(0.001), activation="relu",input_shape = c(num.inputs)) %>%
  layer_dense(units=16, kernel_regularizer = regularizer_l1(0.001), activation="relu") %>%
  layer_dense(units=16, kernel_regularizer = regularizer_l1(0.001), activation="relu") %>%
  layer_dense(units=1,activation="sigmoid")


model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model %>% fit(
  x_train, y_train, 
  epochs = 30, batch_size = 256, 
  validation_split = 0.2
)

results.NN2 <- model %>% evaluate(x_test,y_test)
results.NN2

### Adding Dropout

model <- keras_model_sequential() %>%
  layer_dense(units=128, activation="relu",input_shape = c(num.inputs)) %>%
  layer_dropout(rate=0.5) %>%
  layer_dense(units=128, activation="relu") %>%
  layer_dropout(rate=0.5) %>%
  layer_dense(units=64, activation="relu") %>%
  layer_dropout(rate=0.5) %>%
  layer_dense(units=1,activation="sigmoid")


model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model %>% fit(
  x_train, y_train, 
  epochs = 50, batch_size = 256, 
  validation_split = 0.2
)

results.NN3 <- model %>% evaluate(x_train,y_train)
results.NN3

results.NN3 <- model %>% evaluate(x_test,y_test)
results.NN3



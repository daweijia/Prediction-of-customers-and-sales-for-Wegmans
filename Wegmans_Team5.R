#### Load data ####
setwd("~/Desktop/Winter Quater/MKTG Research/Project/W7-full data/wegmans_data")
custdata = read.table("custdata.txt",sep = "|")
insiderdata = read.table("txdata_insiders.txt",sep = "|")
othertop30data = read.table("txdata_othertop30.txt",sep = "|")
library(readr)
surveydata = read_excel("Survey Data FINAL.xlsx")

#### Clean survey data ####
# delete 31 duplicated observations
a = which(duplicated(surveydata$HH)==TRUE)
b = which(duplicated(surveydata$HH, fromLast = TRUE)==TRUE)
## x[!duplicated(rev(x))]= x[!duplicated(x,fromLast = T)]
## delete 62 obs

duplicate.row = c(a,b)
surveydata = surveydata[-duplicate.row,] 

#### Is 'M' a major barrier####
# HHs who concern with 'M'
concernM = surveydata$HH[which(surveydata$M == 'N-')]
percentM = length(concernM)/nrow(surveydata) #7.06%
# HHs who concern with other attribute
concernC = surveydata$HH[which(surveydata$C == 'N-')]
percentC = length(concernC)/nrow(surveydata) #17.5%

concernE = surveydata$HH[which(surveydata$E == 'N-')]
percentE = length(concernE)/nrow(surveydata) #1.17%

concernV = surveydata$HH[which(surveydata$V == 'N-')]
percentV = length(concernV)/nrow(surveydata) # 5.6%
# HHs who will never purchase
npurchase = surveydata[
  surveydata$EndPoint == 'E2'|surveydata$EndPoint == 'E4'|surveydata$EndPoint == 'E6',]

# distribution of never-purchasing customers'concerns:
npurchaseM = npurchase[which(npurchase$M == 'N-'),]
npurchaseC = npurchase[which(npurchase$C == 'N-'),]
npurchaseV = npurchase[which(npurchase$V == 'N-'),]
npurchaseE = npurchase[which(npurchase$E == 'N-'),]
# percentage
percentNM = nrow(npurchaseM)/nrow(npurchase) #22.9%
percentNC = nrow(npurchaseC)/nrow(npurchase) #56.9%
percentNV = nrow(npurchaseV)/nrow(npurchase) #18.3%
percentNE = nrow(npurchaseE)/nrow(npurchase) #3.8%


# concernM insiderdata
concernMinsider = insiderdata[insiderdata$HH %in% concernM,]
# merge concernMinsider and survey data to more focus on customers who have concern with M
insiderSurvey = merge(concernMinsider,surveydata[surveydata$HH %in% concernM,],by.x='HH',by.y = 'HH')

# create a new database for question1
#barrierM = concernMinsider[,which(names(concernMinsider)%in%
#                                          c('HH','UNITS','UNITS_POST','ALTERNATIVE'))]


### What percent of those who have a concern will never purchase

agg.insiderSurvey<-aggregate(data=insiderSurvey,cbind(UNITS,UNITS_POST)~HH+ALTERNATIVE,sum)
agg.insiderSurvey$willbuy=NA
# Customers(concern with M) who will buy anything after the survey
agg.insiderSurvey$willbuy[which(agg.insiderSurvey$ALTERNATIVE == 'T0'&agg.insiderSurvey$UNITS_POST!=0)]=1
# Customers(concern with M) who will not buy anything after the survey
agg.insiderSurvey$willbuy[is.na(agg.insiderSurvey$willbuy)]=0
agg.insiderSurvey[agg.insiderSurvey$willbuy==1,]
# percentage
length(unique(agg.insiderSurvey$HH[which(agg.insiderSurvey$willbuy==1)])) #51 will buy
length(unique(agg.insiderSurvey$HH)) #628 customers have concern with M
1-(51/628) #92%

### What percent of customers could we persuade using a marketing message?
#over all persuaded customers
agg.fu.insiderSurvey<-aggregate(data=insiderSurvey,cbind(UNITS,UNITS_POST)~HH+ALTERNATIVE+EndPoint+FOLLOWUP,sum)
agg.fu.insiderSurvey$prsd=NA
agg.fu.insiderSurvey$prsd[which(agg.fu.insiderSurvey$ALTERNATIVE == 'T0'&agg.fu.insiderSurvey$UNITS_POST!=0 &
                               (agg.fu.insiderSurvey$EndPoint=='E2'|agg.fu.insiderSurvey$EndPoint=='E4'|agg.fu.insiderSurvey$EndPoint=='E6'))]=1
agg.fu.insiderSurvey$prsd[which(is.na(agg.fu.insiderSurvey$prsd))]=0
agg.fu.insiderSurvey[agg.fu.insiderSurvey$prsd==1,]
agg.fu.insiderSurvey[agg.fu.insiderSurvey$prsd==0,]
length(unique(agg.fu.insiderSurvey$HH[which(agg.fu.insiderSurvey$prsd==1)])) #37
length(unique(agg.fu.insiderSurvey$HH)) #377
37/377 #10% could be persuaded and purchase

###BY segments
##**Tried**
# % of tried customers that could be persuaded
length(unique(agg.fu.insiderSurvey$HH[which(agg.fu.insiderSurvey$prsd==1&agg.fu.insiderSurvey$EndPoint=='E2')])) #5
length(unique(agg.fu.insiderSurvey$HH[which(agg.fu.insiderSurvey$EndPoint=='E2')])) #31
5/31 #16%

# % of aware customers that could be persuaded 
length(unique(agg.fu.insiderSurvey$HH[which(agg.fu.insiderSurvey$prsd==1&agg.fu.insiderSurvey$EndPoint=='E4')])) #22
length(unique(agg.fu.insiderSurvey$HH[which(agg.fu.insiderSurvey$EndPoint=='E4')])) #174
22/174 #12%

# % of unaware customers that could be persuaded
length(unique(agg.fu.insiderSurvey$HH[which(agg.fu.insiderSurvey$prsd==1&agg.fu.insiderSurvey$EndPoint=='E6')])) #10
length(unique(agg.fu.insiderSurvey$HH[which(agg.fu.insiderSurvey$EndPoint=='E6')])) #172
10/172 #6%


#### Size of the prize for item-X ####
### Whether the customer will buy item-X ###
# whether customers bought item-X
insiderdata$boughtX = NA
insiderdata$boughtX[which(insiderdata$ALTERNATIVE == 'T0')] = 1
insiderdata$boughtX[which(insiderdata$ALTERNATIVE != 'T0')] = 0
s = aggregate(data=insiderdata,boughtX~HH,sum)
HHnbuyX = s$HH[s$boughtX == 0]
HHbuyX = s$HH[s$boughtX != 0]

# whether to buy prediction
buyX = custdata[(custdata$HH %in% HHbuyX) & (custdata$HH %in% surveydata$HH),]
buyX$whetherT0 = 'pos'
nbuyX = custdata[(custdata$HH %in% HHnbuyX) & (custdata$HH %in% surveydata$HH),]
nbuyX$whetherT0 = 'neg'

whetherbuyX = rbind(buyX,nbuyX)
write.csv(whetherbuyX, file = 'whetherbuyX.csv')

DB = whetherbuyX
row.names(DB) = DB$HH
# build dataset
set.seed(1)
train = sample(1:nrow(DB),nrow(DB)*0.666667)
traindb = DB[train,-c(1,2)] 
testdb = DB[-train,-c(1,2)]
# build tree
library(rpart)
fit.big <- rpart(whetherT0~., data = traindb, control = rpart.control(xval = 10, minsplit = 2, cp=0))
fit.big$cptable[which.min(fit.big$cptable[,"xerror"]),"CP"] #cp is 0.002598528
fit.small <- rpart(whetherT0~., data = traindb, control = rpart.control(xval = 10, cp=0.002598528))
plot(fit.small, uniform=T, branch=0.5, compress=T, main="Tree with best cp", margin=0.05)
text(fit.small, splits=T, all=F, use.n=T, pretty=T, fancy=F, cex=0.8)
# test
T0.pred=predict(fit.small,testdb,type="class") 
T0.actual=testdb$whetherT0
confusion.matrix=table(T0.pred,T0.actual)
confusion.matrix
library(caret)
confusionMatrix(confusion.matrix,positive="pos")   


# decision tree dataset
custdata$HH <- as.numeric(as.character(custdata$HH))
custdata$DECILE <- as.numeric(as.character(custdata$DECILE))
custdata$ZONE_NBR <- as.numeric(as.character(custdata$ZONE_NBR))
custdata$HOH_AGE <- as.numeric(as.character(custdata$HOH_AGE))
custdata$HH_INCOME <- as.numeric(as.character(custdata$HH_INCOME))
custdata$HH_SIZE <- as.numeric(as.character(custdata$HH_SIZE))
custdata$HH_ADULTS <- as.numeric(as.character(custdata$HH_ADULTS))
custdata$HH_CHILDREN <- as.numeric(as.character(custdata$HH_CHILDREN))

pos1 <- which(custdata$DECILE<1.5 & custdata$HH_CHILDREN>=0.5 & custdata$HOH_AGE<57.5 &
                custdata$HOH_AGE>=39.5 & custdata$HH_SIZE<6.5 & custdata$HOH_AGE <51.5)
pos2 <- which(custdata$DECILE<1.5 & custdata$HH_CHILDREN>=0.5 & custdata$HOH_AGE<57.5 &
                custdata$HOH_AGE>=39.5 & custdata$HH_SIZE<6.5 & custdata$HOH_AGE>=51.5&
                custdata$HH_ADULTS>=2.5)
pos3 <- which(custdata$DECILE<1.5 & custdata$HH_CHILDREN<0.5 & custdata$HH_SIZE>=2.5&
                custdata$HH_INCOME>=3e+04 & custdata$ZONE_NBR>=7.5)
pos <- c(pos1,pos2,pos3)
treedata <- custdata[pos,]



# units prediction
insiderdata$unitbought = insiderdata$UNITS + insiderdata$UNITS_POST
z = insiderdata[which(insiderdata$ALTERNATIVE == 'T0'),]
insider = aggregate(data=z,unitbought~HH,sum)
othertop30data$unitbought = othertop30data$UNITS + othertop30data$UNITS_POST
y = othertop30data[which(othertop30data$ALTERNATIVE == 'T0'),]
other30 = aggregate(data=y,unitbought~HH,sum)
units = rbind(insider, other30)

# create a new database for question2b
unitsell = merge(custdata,units, by.x = 'HH',by.y = 'HH')
write.csv(unitsell, file = 'unitsell.csv')

unitsell$HH_CHILDREN = unitsell$HH_CHILDREN + 1
unitsell = na.omit(unitsell)

standard_unitsell = apply(unitsell[,3:10],2,FUN = function(x) (x-mean(x))/sd(x))
sd_res = sd(unitsell$unitbought)
unitsell = standard_unitsell
unitsell = as.data.frame(unitsell)

#============================Predictive model:NNT =======================================
install.packages("nnet")
library(nnet)

set.seed(123)
nFold <- 10
valnum <- floor(runif(nrow(unitsell))*nFold) +1

unitsell_copy = unitsell
standard_unitsell = apply(unitsell[,3:10],2,FUN = function(x) (x-mean(x))/sd(x))
standard_unitsell = as.data.frame(standard_unitsell)
unitsell = standard_unitsell

regressors <- c("DECILE","factor(ZONE_NBR)","HOH_AGE","HH_INCOME","HH_SIZE","HH_ADULTS","HH_CHILDREN",
                "I(DECILE*HOH_AGE*HH_INCOME)","I(DECILE*HOH_AGE)","I(DECILE*HH_INCOME)","I(HH_SIZE*HH_ADULTS*HH_CHILDREN)",
                "HH_SIZE^4","log(HH_INCOME)")
regMat <- expand.grid(c(TRUE,FALSE), c(TRUE,FALSE),c(TRUE,FALSE),c(TRUE,FALSE),c(TRUE,FALSE),
                      c(TRUE,FALSE),c(TRUE,FALSE),c(TRUE,FALSE),c(TRUE,FALSE),c(TRUE,FALSE),
                      c(TRUE,FALSE),c(TRUE,FALSE),c(TRUE,FALSE))
regMat <- regMat[-(dim(regMat)[1]),]
names(regMat) <- regressors
meantable <- c()
allModelsList <- apply(regMat, 1, function(x) as.formula(paste(c("unitbought ~ 1", regressors[x]),collapse=" + ")))
timee <- proc.time()

set.seed(123)
nFold <- 10
valnum <- floor(runif(nrow(unitsell))*nFold) +1
modelperformance.nntl = matrix(NA,nFold,8191)
for (fold in 1:nFold) {
        trainingdata <- subset(unitsell,valnum!=fold)
        validationdata <- subset(unitsell,valnum==fold)
        allModelsResults <- lapply(allModelsList,function(x) nnet(x, data=trainingdata, linout=1,size=3,maxit=100))
        for (i in 1:8191) {meantable[i] <- mean((validationdata$unitbought-predict(allModelsResults[[i]],validationdata))^2)}
        modelperformance.nntl[fold,]<- meantable
}
proc.time()-timee
colMeans(modelperformance.nntl)
which.min(colMeans(modelperformance.nntl)) #8189
colMeans(modelperformance.nntl)[8189] #MSE 5.462437


#============================Predictive model:linear regression =======================================

unitsell = unitsell_copy
modelperformance.lm1 <- matrix(NA,nFold,1)
meantable.lm1 <- c()
for (fold in 1:nFold) {
        trainingdata <- subset(unitsell,valnum!=fold)
        validationdata <- subset(unitsell,valnum==fold)
        validationdata = na.omit(validationdata)
        modelperformance.lm1[fold,] <- mean((validationdata$unitbought-predict(
                lm(unitbought~DECILE+factor(ZONE_NBR)+HOH_AGE+HH_INCOME+HH_SIZE+HH_ADULTS+HH_CHILDREN, 
                   data = trainingdata),
                validationdata,na.rm = TRUE))^2)^(1/2)
}
colMeans(modelperformance.lm1)
#5.435854

modelperformance.lm2 <- matrix(NA,nFold,1)
meantable.lm2 <- c()
for (fold in 1:nFold) {
        trainingdata <- subset(unitsell,valnum!=fold)
        validationdata <- subset(unitsell,valnum==fold)
        validationdata = na.omit(validationdata)
        modelperformance.lm2[fold,] <- mean((validationdata$unitbought-predict(
                lm(unitbought~DECILE+factor(ZONE_NBR)+HOH_AGE+
                           HH_INCOME+HH_SIZE*HH_ADULTS*HH_CHILDREN,
                   data = trainingdata),
                validationdata,na.rm = TRUE))^2)^(1/2)
}
colMeans(modelperformance.lm2)
#5.438123

# linear loop
regressors <- c("DECILE","factor(ZONE_NBR)","HOH_AGE","HH_INCOME","HH_SIZE","HH_ADULTS","HH_CHILDREN",
                "I(DECILE*HOH_AGE*HH_INCOME)","I(DECILE*HOH_AGE)","I(DECILE*HH_INCOME)","I(HH_SIZE*HH_ADULTS*HH_CHILDREN)",
                "HH_SIZE^4","log(HH_INCOME)")
regMat <- expand.grid(c(TRUE,FALSE), c(TRUE,FALSE),c(TRUE,FALSE),c(TRUE,FALSE),c(TRUE,FALSE),
                      c(TRUE,FALSE),c(TRUE,FALSE),c(TRUE,FALSE),c(TRUE,FALSE),c(TRUE,FALSE),
                      c(TRUE,FALSE),c(TRUE,FALSE),c(TRUE,FALSE))
regMat <- regMat[-(dim(regMat)[1]),]
names(regMat) <- regressors
meantable <- c()
allModelsList <- apply(regMat, 1, function(x) as.formula(paste(c("unitbought ~ 1", regressors[x]),collapse=" + ")))
timee <- proc.time()
modelperformance.lml = matrix(NA,nFold,8191)
for (fold in 1:nFold) {
        trainingdata <- subset(unitsell,valnum!=fold)
        validationdata <- subset(unitsell,valnum==fold)
        allModelsResults <- lapply(allModelsList,function(x) lm(x, data=trainingdata))
        for (i in 1:8191) {meantable[i] <- mean((validationdata$unitbought-predict(allModelsResults[[i]],validationdata))^2)}
        modelperformance.lml[fold,]<- meantable
}
proc.time()-timee
colMeans(modelperformance.lml)
which.min(colMeans(modelperformance.lml)) #6453
colMeans(modelperformance.lml)[6453] #MSE 5.44856

#============================Predictive model: MARS=======================================
install.packages("earth")
library("earth") #load earth package
unitsell<-unitsell[complete.cases(unitsell), ]
unitsell$HH_CHILDREN<-unitsell$HH_CHILDREN+1

nFold <- 10
valnum <- floor(runif(nrow(unitsell))*nFold) +1
#--------------------------              earth(..., trace=2, thres=0.1)    -------------------------------------------------------------

# earth(x, trace=2, thres=0.1)
set.seed(1)
modelperformance.earth1 <- matrix(NA,nFold,1) #build a empty matrix for output
for (fold in 1:nFold) {
  trainingdata <- subset(unitsell,valnum!=fold)
  validationdata <- subset(unitsell,valnum==fold)
  modelperformance.earth1[fold,] <- mean((validationdata$unitbought-predict(
    earth(unitbought~DECILE+ZONE_NBR+HOH_AGE+HH_INCOME+HH_SIZE+HH_ADULTS+HH_CHILDREN, data = trainingdata,trace=2,thres=0.1),
    validationdata))^2)^0.5
}
#this for loop is to do earth by k-Fold cross validation
colMeans(modelperformance.earth1) # the MSE is 5.497656 #calculate the minimum MSE

# earth(x^2, trace=2, thres=0.1)
set.seed(1)
modelperformance.earth2 <- matrix(NA,nFold,1)
for (fold in 1:nFold) {
  trainingdata <- subset(unitsell,valnum!=fold)
  validationdata <- subset(unitsell,valnum==fold)
  modelperformance.earth2[fold,] <- mean((validationdata$unitbought-predict(
    earth(unitbought~DECILE^2+ZONE_NBR^2+HOH_AGE^2+HH_INCOME^2+HH_SIZE^2+HH_ADULTS^2+HH_CHILDREN^2, data = trainingdata,trace=2,thres=0.1),
    validationdata))^2)^0.5
}
colMeans(modelperformance.earth2) # 5.497656, the same as unsquare

# earth(log(x), trace=2, thres=0.1)
set.seed(1)
modelperformance.earth3 <- matrix(NA,nFold,1)
for (fold in 1:nFold) {
  trainingdata <- subset(unitsell,valnum!=fold)
  validationdata <- subset(unitsell,valnum==fold)
  modelperformance.earth3[fold,] <- mean((validationdata$unitbought-predict(
    earth(unitbought~log(DECILE)+log(ZONE_NBR)+log(HOH_AGE)+log(HH_INCOME)+log(HH_SIZE)+log(HH_CHILDREN), data = trainingdata,trace=2,thres=0.1),
    validationdata))^2)^0.5
}
colMeans(modelperformance.earth3) #the MSE is 5.497656

#--------------------------       basic fit  earth(...)    -------------------------------------------------------------

#earth(x, ...)
set.seed(1)
modelperformance.earth4 <- matrix(NA,nFold,1)
for (fold in 1:nFold) {
  trainingdata <- subset(unitsell,valnum!=fold)
  validationdata <- subset(unitsell,valnum==fold)
  modelperformance.earth4[fold,] <- mean((validationdata$unitbought-predict(
    earth(unitbought~DECILE+ZONE_NBR+HOH_AGE+HH_INCOME+HH_SIZE+HH_ADULTS+HH_CHILDREN, data = trainingdata),
    validationdata))^2)^0.5
}
colMeans(modelperformance.earth4) #the MSE is 5.413071

#earth(x^2, ...)
set.seed(1)
modelperformance.earth5 <- matrix(NA,nFold,1)
for (fold in 1:nFold) {
  trainingdata <- subset(unitsell,valnum!=fold)
  validationdata <- subset(unitsell,valnum==fold)
  modelperformance.earth5[fold,] <- mean((validationdata$unitbought-predict(
    earth(unitbought~DECILE^2+ZONE_NBR^2+HOH_AGE^2+HH_INCOME^2+HH_SIZE^2+HH_ADULTS^2+HH_CHILDREN^2, data = trainingdata),
    validationdata))^2)^0.5
}
colMeans(modelperformance.earth5) #the MSE is 5.413071

#earth(log(x), ...)
set.seed(1)
modelperformance.earth6 <- matrix(NA,nFold,1)
for (fold in 1:nFold) {
  trainingdata <- subset(unitsell,valnum!=fold)
  validationdata <- subset(unitsell,valnum==fold)
  modelperformance.earth6[fold,] <- mean((validationdata$unitbought-predict(
    earth(unitbought~log(DECILE)+log(ZONE_NBR)+log(HOH_AGE)+log(HH_INCOME)+log(HH_SIZE)+log(HH_ADULTS)+log(HH_CHILDREN), data = trainingdata),
    validationdata))^2)^0.5
}
colMeans(modelperformance.earth6) #the MSE is 5.411643

#--------------------------       earth(...,degree=i)    ----------------------------------------------------------

#earth(x,degree=2,...)
set.seed(1)
modelperformance.earth7 <- matrix(NA,nFold,1)
for (fold in 1:nFold) {
  trainingdata <- subset(unitsell,valnum!=fold)
  validationdata <- subset(unitsell,valnum==fold)
  modelperformance.earth7[fold,] <- mean((validationdata$unitbought-predict(
    earth(unitbought~DECILE+ZONE_NBR+HOH_AGE+HH_INCOME+HH_SIZE+HH_ADULTS+HH_CHILDREN, data = trainingdata,degree=2),
    validationdata))^2)^0.5
}
colMeans(modelperformance.earth7) #the MSE is 5.417003

#earth(log(x),degree=3,...)
set.seed(1)
modelperformance.earth8 <- matrix(NA,nFold,1)
for (fold in 1:nFold) {
  trainingdata <- subset(unitsell,valnum!=fold)
  validationdata <- subset(unitsell,valnum==fold)
  modelperformance.earth8[fold,] <- mean((validationdata$unitbought-predict(
    earth(unitbought~log(DECILE)+log(ZONE_NBR)+log(HOH_AGE)+log(HH_INCOME)+log(HH_SIZE)+log(HH_ADULTS)+log(HH_CHILDREN), data = trainingdata,degree=3),
    validationdata))^2)^0.5
}
colMeans(modelperformance.earth8) #the MSE is 5.4315

#earth(log(x)+x^2,degree=3,...)
set.seed(1)
modelperformance.earth9 <- matrix(NA,nFold,1)
for (fold in 1:nFold) {
  trainingdata <- subset(unitsell,valnum!=fold)
  validationdata <- subset(unitsell,valnum==fold)
  modelperformance.earth9[fold,] <- mean((validationdata$unitbought-predict(
    earth(unitbought~DECILE^2+ZONE_NBR^2+HOH_AGE^2+HH_INCOME^2+log(HH_SIZE)+log(HH_ADULTS)+log(HH_CHILDREN), data = trainingdata,degree = 3),
    validationdata))^2)^0.5
}
colMeans(modelperformance.earth9) #the MSE is 5.423601

#earth(log(x)+x,degree=3,...)
set.seed(1)
modelperformance.earth10 <- matrix(NA,nFold,1)
for (fold in 1:nFold) {
  trainingdata <- subset(unitsell,valnum!=fold)
  validationdata <- subset(unitsell,valnum==fold)
  modelperformance.earth10[fold,] <- mean((validationdata$unitbought-predict(
    earth(unitbought~DECILE+ZONE_NBR+HOH_AGE+HH_INCOME+HH_SIZE+log(HH_ADULTS)+log(HH_CHILDREN), data = trainingdata,degree = 3),
    validationdata))^2)^0.5
}
colMeans(modelperformance.earth10) #the MSE is 5.419799

#earth(log(x)+x,degree=3,...)
set.seed(1)
modelperformance.earth11 <- matrix(NA,nFold,1)
for (fold in 1:nFold) {
  trainingdata <- subset(unitsell,valnum!=fold)
  validationdata <- subset(unitsell,valnum==fold)
  modelperformance.earth11[fold,] <- mean((validationdata$unitbought-predict(
    earth(unitbought~DECILE+ZONE_NBR+HOH_AGE+HH_INCOME+log(HH_SIZE)+log(HH_ADULTS)+log(HH_CHILDREN), data = trainingdata,degree = 3),
    validationdata))^2)^0.5
}
colMeans(modelperformance.earth11) #the MSE is 5.417683

#apply best mars model to entire dataset
besteearth<-earth(unitbought~log(DECILE)+log(ZONE_NBR)+log(HOH_AGE)+log(HH_INCOME)+log(HH_SIZE)+log(HH_ADULTS)+log(HH_CHILDREN), data = unitsell)

#Using test data set to predict units customers will buy
library(caret)
treedata$unitsbought<-predict(besteearth,treedata)
write.csv(treedata, file = 'unitsprediction.csv')

#total weekly sales
weeklysales<-sum(treedata$unitsbought)/(13*4)
weeklysales


#loading and viewing file,replace with your file path

Fa <- read.csv("~/TDMS/FA_COST/Fa.csv")
View(Fa)
#load required packages, change with your file path
source('~/TDMS/FA_COST/FA_Final_Set/randomForest/Trees_Packages.R')
#M5
m5tree <- M5P(log(Chargeable_minutes) ~ ., data = training,control = Weka_control(M = 10))
M5tree.p<-predict(m5tree,xtest)
#sampling
set.seed(500)
#stratified random sampling sum
index<-createDataPartition(dm$F,p=.70,list=FALSE)
training<-dm[index,]
testing<-dm[-index,]
train.x<-training[,-1]
train.y<-training[,1]
xtest<-testing[,-1]
ytest<-testing[,1]
##gamma with log link
fit.gamma.log <- glm(Chargeable_minutes ~.,family=Gamma(link="log"),data =training, control = glm.control(maxit = 100, trace = TRUE))
gamma.p<-predict(fit.gamma.log,testing)
exp.gamma.p<-exp(gamma.p)
summary(exp.gamma.p)

#Formula form model
frm<-as.formula("Chargeable_minutes~.")
#Model 1 rpart
dt.model<-rpart(Chargeable_minutes~.,method="anova",data=training)

#find optimum parameters
dt.model$cptable
plotcp(dt.model)

#model2 with opt param.(minsplit=min no of observation required before split. minbucket=min # of obs. in any terminal node=minsplit/3...xval=# of cross val,cp=comp par.
dt.model2 <- rpart(Chargeable_minutes,method="anova",data=training,control =rpart.control(minsplit=10,cp=0.01,minbucket =10,xval=10))
#convert to a party object to view rules 
rparty_ob<-as.party(dt.model2)

#open a new window and expand the window
windows()

#plot tree
prp(dt.model2, faclen = 3,fallen.leaves = TRUE, type=4, extra=1, varlen=0, yesno.yshift=1,clip.right.labs = TRUE,cex=.5)
rpart.p<-predict(dt.model2,testing)
summary(testing$Chargeable_minutes)
#.................................

#Cubist

cub_opt_commit<-train(x=train.x,y=train.y,"cubist",tuneGrid = expand.grid(committees=c(1,5,10,50,75,100),neighbors=c(0,1,3,5,7)),trControl = trainControl(method = "cv"))

#predictions
cub_pred=predict(cub_opt_commit,testing,neighbors=7)
#compare distribution of predicted vs. actual values of "CHARGEABLE_MINS" in Test data set
summary(cub_pred) #predicted
summary(testing$Chargeable_minutes)#actual
#finding Root Mean Square Error
RMSE#finding correlation btw predicted vs. actual values in test data
cor(cub_pred, testing$CHARGEABLE_MINS) #strong correlation compare to rpart! Which is good

#random forest
control<- trainControl(method = "repeatedcv",repeats = 10,number = 20)
grid = expand.grid(.n.trees=seq(100,500, by=200), .interaction.depth=seq(1,4, by=1), .shrinkage=c(.001,.01,.1),.n.minobsinnode=10)
rf <- randomForest(x =train.x, y=train.y,ntree=1000,mtry=2,  xtest =test.x,ytest =test.y, importance=TRUE, keep.forest=TRUE,set.seed=12, methods=control)


#Lasso----
x <-model.matrix(Chargeable_minutes ~.,data =Model)[,-1]#convert dataframe to model matrix
x2<-model.matrix(~.,data=exa) #exa data matrix
frac <- cv_lars$fraction[which.min(cv_lars$cv)]#using cross-validation with the lasso to find near-optimal) value for the "fraction"


lasso<-lars( x = y[index,], y =Model$Chargeable_minutes[index]) #model
lasso.p<-predict(lasso,x[-index,],s =.18,mode ="fraction") #predictions
RMSElasso.004=sqrt(mean((predict(rig,x[-index,],s =0.04,mode ="fraction")$fit-M.l$Chargeable_minutes[-index])^2))
summary(lasso.p$fit) #compare distribution
summary(testing$Chargeable_minutes) #with actual y values of test data

#if you want to see the histogram of predicted values in the test dataset
ggplot(testing, aes(x =l.predict$fit)) + geom_histogram(fill="lightblue",colour="darkblue",binwidth=10) +theme(axis.title.x = element_text(face="bold", colour="#990000", size=10),axis.text.x  =
element_text(angle=0, vjust=1, size=10))+ggtitle("Chargeable Minutes Distribution for Testing Dataset")
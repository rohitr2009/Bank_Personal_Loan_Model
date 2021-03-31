getwd()
setwd("C:/Users/Rohit/Downloads")
getwd()
df=read.csv("Bank_Personal_Loan_Modelling.csv", header=TRUE)
dim(df)
head(df)
str(df)
any(is.na(df))
df=subset(df,select=-c(ID,ZIP.Code))
head(df)
fc=c("Education","Personal.Loan","Securities.Account", "CD.Account", "Online", "CreditCard")
df[fc] = lapply(df[fc], factor)
str(df)
head(df)
levels(df$Education)
levels(df$Personal.Loan)
summary(df) #checking for outliers,data entry errors
df$Experience=abs(df$Experience)
summary(df)
#install.packages("ggplot2")
library(ggplot2)
library(readr)
library(dplyr)
#install.packages("DataExplorer")
library(DataExplorer)
head(df)
age<-ggplot(df, aes(x=Age)) + 
  geom_histogram(color="black", fill="white")
age

library(purrr)
library(tidyr)

df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

plot_boxplot(df, by = "Personal.Loan", 
             geom_boxplot_args = list("outlier.color" = "Purple"))

head(df)
by_loan<-df %>% group_by(Personal.Loan)
head(by_loan)
averages<-by_loan %>% summarise(avg_age = mean(Age),avg_experience = mean(Experience), avg_income=mean(Income), avg_size=mean(Family))
ggplot(data=averages, aes(x=Personal.Loan, y=avg_age)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=avg_age), vjust=1.6, color="white", size=3.5)+
  theme_minimal()
ggplot(data=averages, aes(x=Personal.Loan, y=avg_experience)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=avg_experience), vjust=1.6, color="white", size=3.5)+
  theme_minimal()
ggplot(data=averages, aes(x=Personal.Loan, y=avg_income)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=avg_income), vjust=1.6, color="white", size=3.5)+
  theme_minimal()
ggplot(data=averages, aes(x=Personal.Loan, y=avg_size)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=avg_size), vjust=1.6, color="white", size=3.5)+
  theme_minimal()
a1 = ggplot(df, aes(Income, fill= Personal.Loan)) + geom_density(alpha=0.4)+ylab("Income_Density")
a2 = ggplot(df, aes(Mortgage, fill= Personal.Loan)) + geom_density(alpha=0.4)+ylab("Mortgage_Density")
a3 = ggplot(df, aes(Age, fill= Personal.Loan)) + geom_density(alpha=0.4)+ylab("Age_Density")
a4 = ggplot(df, aes(Experience, fill= Personal.Loan)) + geom_density(alpha=0.4)+ylab("Experience_Density")
a5 = ggplot(df, aes(Education, fill= Personal.Loan)) + geom_histogram(alpha=0.4,stat="count")+ylab("Education_frequency")
a6 = ggplot(df, aes(Income, Mortgage, color = Personal.Loan))+  geom_point(alpha = 0.7)
library(gridExtra)
grid.arrange(a1,a2,a3,a4,a5,a6, ncol = 2, nrow = 3)

b5=ggplot(df, aes(Income,y = CCAvg, color = Personal.Loan)) + 
  geom_point(size = 1)

b1=ggplot(df, aes(Securities.Account,fill= Personal.Loan)) + geom_bar(stat = "count", position = "dodge") +theme_minimal()
b2=ggplot(df, aes(CD.Account,fill= Personal.Loan)) + geom_bar(stat = "count", position = "dodge") +theme_minimal()
b3=ggplot(df, aes(Online,fill= Personal.Loan)) + geom_bar(stat = "count", position = "dodge") +theme_minimal()
b4=ggplot(df, aes(CreditCard,fill= Personal.Loan)) + geom_bar(stat = "count", position = "dodge") +theme_minimal()
grid.arrange(b1,b2,b3,b4,b5, ncol = 2, nrow = 3)

#EDA end

fc=c("Education","Securities.Account", "CD.Account", "Online", "CreditCard")

df[fc] = lapply(df[fc], factor)
dfclus=df %>% select_if(is.numeric)
df.sc = scale(dfclus, center = TRUE)
head(df.sc)
install.packages("factoextra")
library(cluster)
seg.dist<-daisy(df.sc)
as.matrix(seg.dist)
seg.hc<-hclust(seg.dist, method="complete")
plot(seg.hc)
plot(cut(as.dendrogram(seg.hc), h=7))
library(factoextra)
clus_plot1 = fviz_nbclust(df.sc, kmeans, method = "silhouette", k.max = 5)
clus_plot2 = fviz_nbclust(df.sc, kmeans, method = "wss", k.max = 5)

grid.arrange(clus_plot1, clus_plot2, ncol=2)
plot(rect.hclust(seg.hc, k=3, border="red"))
set.seed(1000)
df.k<-kmeans(df.sc, centers=3)
fviz_cluster(df.k, df.sc, geom = "point", ellipse = TRUE, pointsize = 0.4, ) + theme_minimal()
df.k$cluster
seg.summ<-function(data,groups){
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}
seg.df.num<-df
seg.df.num$Personal.Loan <- ifelse(df$Personal.Loan==0, 0, 1)
seg.df.num$Securities.Account <- ifelse(df$Securities.Account==0, 0, 1)
seg.df.num$CD.Account <- ifelse(df$CD.Account==0, 0, 1)
seg.df.num$Online <- ifelse(df$Online==0, 0, 1)
seg.df.num$CreditCard <- ifelse(df$CreditCard==0, 0, 1)
seg.summ(seg.df.num, df.k$cluster)
# Segmentation over
# Classification Starts
## sampling 70% of data for training the algorithms using random sampling 
set.seed(1000)
perc_train = sample(1:nrow(df), nrow(df)*0.80)
df_train = df[perc_train,]
df_test = df[-perc_train,]

dim(df_test)
set.seed(1000)
library(rpart)
dt = rpart(Personal.Loan~., data = df_train, method = "class")
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(dt)
dt$variable.importance
library(gmodels)
pred.cart<-predict(dt, newdata=df_test,type="class")
Cross1=CrossTable(df_test$Personal.Loan, pred.cart)
install.packages("caret")
library(caret)
confusionMatrix(table(df_test$Personal.Loan,pred.cart ))



library(nFactors)
library(psych)
library(GPArotation)
eigen(cor(df[,1:5]))
library(randomForest)
rf <- randomForest(Personal.Loan ~ ., ntree = 500, nodesize=10,data = df_train, importance = TRUE)
rf
Pred_rf <- predict(rf, df_test, type = 'class')
confusionMatrix(df_test$Personal.Loan, Pred_rf)
varImpPlot(rf, sort = T)

#Logistic Regression
fit.logit <- glm(Personal.Loan ~ . ,data=df_train,family=binomial("logit"))
pred.valid=predict(fit.logit, df_test[,-8],type="response" )
pred.valid <- ifelse(pred.valid> 0.5,1,0)
(ctv=table(df_test[,8], pred.valid))


#Neuralnets

library(neuralnet)
library(nnet)
library(caret)

indx <- sapply(df_train, is.factor)
df_train[indx] <- lapply(df_train[indx], function(x) as.numeric(as.character(x)))

nn<-neuralnet(Personal.Loan ~ . ,
              data=df_train,hidden = c(4,2),linear.output = FALSE)
plot(nn)



preds<-compute(nn,df_train[,-8])
preds.class<-ifelse(preds$net.result>0.5,1,0)
confusionMatrix(as.factor(preds.class),as.factor(df_train$Personal.Loan))

indx2 <- sapply(df_test, is.factor)
df_test[indx2] <- lapply(df_test[indx2], function(x) as.numeric(as.character(x)))

preds.valid<-compute(nn,df_test[,-8])
preds.valid.class<-ifelse(preds.valid$net.result>0.5,1,0)
confusionMatrix(as.factor(preds.valid.class),as.factor(df_test[,8]))


# Ensemble using Majority Voting
df_test$pred_majority<-as.factor(ifelse(Pred_rf=='1' & pred.cart=='1','1',
                                         ifelse(Pred_rf=='1' & preds.valid.class=='1','1',
                                                ifelse(pred.cart=='1' & preds.valid.class=='1','1','0'))))
ensemble.voting<-confusionMatrix(as.factor(df_test$Personal.Loan),as.factor(df_test$pred_majority))
ensemble.voting



# Ensemble using Weighted Average
# Taking weighted average of predictions
df_test$pred_weighted<-(as.numeric(Pred_rf)*0.25)+(as.numeric(pred.cart)*0.25)+(as.numeric(preds.valid.class)*0.5)
#Splitting into binary classes at 0.5
df_test$pred_weighted<-as.factor(ifelse(df_test$pred_weighted>0.5,'1','0'))
ensemble.weighted<-confusionMatrix(as.factor(df_test$Personal.Loan),as.factor(df_test$pred_weighted))
ensemble.weighted

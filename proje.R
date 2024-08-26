getwd()
setwd("C:/Users/kagan/Desktop")
df <- read.csv("marketing_campaign.csv",header = T,sep = "\t")

#EDA


head(df)

df$Kidhome <- as.factor(df$Kidhome)



str(df)
summary(df)

sum(is.na(df))

df <- na.omit(df)
options(scipen = 999)

library(ggplot2)

table(df$Education)


which(df$Income== 666666)
df <- df[-which(df$Income== 666666),]

bp_month <- ggplot(df,aes(x=Education,y=Income,fill=Education))+
  geom_boxplot()+
  labs(title="Box-Plot of Income \nAccording to Education Level ",x="Education Level",y="Yearly Income")+
  theme_minimal()
bp_month


df2 <- df[,c("Income","MntWines","MntFishProducts",
             "MntGoldProds","NumDealsPurchases","NumWebVisitsMonth","NumStorePurchases",
             "NumCatalogPurchases","NumWebPurchases")]

library(car)
#pair_scatter <- scatterplotMatrix(df2,diagonal = "histogram")

res <- cor(df2,method="pearson")
corrplot::corrplot(res, method= "color", order = "hclust")

library(MVN)

mvn(data = df[,c("NumWebPurchases","NumWebVisitsMonth")], mvnTest = "hz", multivariatePlot = "persp")
mvn(data = df[,c("NumWebPurchases","NumWebVisitsMonth")], mvnTest = "hz", multivariatePlot = "contour")
# INFERENCES ABOUT MEAN VECTOR




result <- mvn(data = df2, mvnTest = "hz", univariatePlot = "qqplot")
result2 <- mvn(data = df2, mvnTest = "hz", univariatePlot = "histogram", univariateTest = "SW" )




df3 <- df[,c("Income",
             "MntGoldProds","NumWebVisitsMonth",
             "NumWebPurchases")]

head(df3)

mvn(df3,mvnTest = "hz")


outlier_quan <- mvn(data = df3, mvnTest = "hz", multivariateOutlierMethod = "quan")

outlier_adj <- mvn(data = df3, mvnTest = "hz", multivariateOutlierMethod = "adj")

log_trans <- log(df3)


log_trans$MntGoldProds[!is.finite(log_trans$MntGoldProds)] <- NA
log_trans$Income[!is.finite(log_trans$Income)] <- NA
log_trans$NumWebVisitsMonth[!is.finite(log_trans$NumWebVisitsMonth)] <- NA
log_trans$NumWebPurchases[!is.finite(log_trans$NumWebPurchases)] <- NA

log_trans <- na.omit(log_trans)

mvn(log_trans,mvnTest = "hz")

library(MASS)

sqrt_transformed_data <- sqrt(df3)

mvn(sqrt_transformed_data,mvnTest = "hz")

inverse_transformed_data <- 1/df3

mvn(df3,mvnTest = "hz")

inverse_transformed_data$MntGoldProds[!is.finite(inverse_transformed_data$MntGoldProds)] <- NA

inverse_transformed_data$NumWebVisitsMonth[!is.finite(inverse_transformed_data$NumWebVisitsMonth)] <- NA
inverse_transformed_data$NumWebPurchases[!is.finite(inverse_transformed_data$NumWebPurchases)] <- NA

sum(is.na(inverse_transformed_data))
inverse_transformed_data <- na.omit(inverse_transformed_data)

mvn(inverse_transformed_data,mvnTest = "hz")

library(car)
library(bestNormalize)
library(MVN)

bestNormalize(df3$MntGoldProds)
bestNormalize(df3$NumWebVisitsMonth)
bestNormalize(df3$NumWebPurchases)
bestNormalize(df3$Income)

INCOME <- orderNorm(df3$Income)

GOLD <- orderNorm(df3$MntGoldProds)

Web_visit <- orderNorm(df3$NumWebVisitsMonth)

Web_Purchase <- orderNorm(df3$NumWebPurchases)

new_df <- data.frame(INCOME$x.t,GOLD$x.t,Web_visit$x.t,Web_Purchase$x.t)
mvn(new_df,mvnTest = "hz")



# trial<- sample(1:nrow(df), 1000)
# head(sample_data)
# head(df)
# sample_data <- df[trial,]

# df5 <- sample_data[,c("Income",
#              "MntGoldProds","NumWebVisitsMonth",
#              "NumWebPurchases")]
# mvn(df5,mvnTest = "royston")
# ?mvn

# bestNormalize(df5$Income)
# 
# df5 <- na.omit(df5)
# 
# x1 <- orderNorm(df5$Income)
# x2 <- orderNorm(df5$MntGoldProds)
# x3 <- orderNorm(df5$NumWebVisitsMonth)
# x4 <- orderNorm(df5$NumWebPurchases)
# 
# y <- data.frame(x1$x.t,x2$x.t,x3$x.t,x4$x.t)
# mvn(y,mvnTest = "royston")

library("ICSNP")


df$Complain <- as.factor(df$Complain)
cfactor <- df$Complain
mean_vector <- data.frame(cfactor,INCOME$x.t,GOLD$x.t,Web_visit$x.t,Web_Purchase$x.t)
colnames(mean_vector) <- c("v1","v2","v3","v4","v5")
head(mean_vector)
colnames(mean_vector)
library(rstatix)
library(dplyr)
library(MVN)



mean_vector %>% group_by(v1) %>%  shapiro_test(v2,v3,v4,v5)
## colnames complain,income,gold,visit,purchase)
mvn(new_df,mvnTest = "hz")

library(heplots)

attach(mean_vector)
boxM(Y = cbind(v2,v3,v4,v5), group = factor(v1))

HotellingsT2(cbind(v2,v3,v4,v5) ~ v1)


#Simultaneous Confidence Intervals
xbar_0<-colMeans(cbind(mean_vector[mean_vector$v1==0,]$v2,
                       mean_vector[mean_vector$v1==0,]$v3,
                       mean_vector[mean_vector$v1==0,]$v4,
                       mean_vector[mean_vector$v1==0,]$v5))

xbar_1<-colMeans(cbind(mean_vector[mean_vector$v1==1,]$v2,
                       mean_vector[mean_vector$v1==1,]$v3,
                       mean_vector[mean_vector$v1==1,]$v4,
                       mean_vector[mean_vector$v1==1,]$v5))


n1<-dim(mean_vector[mean_vector$v1==0,])[1]
n2<-dim(mean_vector[mean_vector$v1==1,])[1]

p<-4
F<-qf(0.05, p, (n1+n2-p-1), lower.tail=FALSE)

c_square<-(((n1+n2-2)*p)/(n1+n2-p-1))*F

sd1<-sd(v2)
sd2<-sd(v3)
sd3 <- sd(v4)
sd4 <- sd(v5)

LC1<-(xbar_0[1]-xbar_1[1])-sqrt(c_square)*sqrt((1/n1)+(1/n2))*sd1
UC1<-(xbar_0[1]-xbar_1[1])+sqrt(c_square)*sqrt((1/n1)+(1/n2))*sd1

SCI_1<-c(LC1, UC1)

LC2<-(xbar_0[2]-xbar_1[2])-sqrt(c_square)*sqrt((1/n1)+(1/n2))*sd2
UC2<-(xbar_0[2]-xbar_1[2])+sqrt(c_square)*sqrt((1/n1)+(1/n2))*sd2

SCI_2<-c(LC2, UC2)

LC3<-(xbar_0[3]-xbar_1[3])-sqrt(c_square)*sqrt((1/n1)+(1/n2))*sd3
UC3<-(xbar_0[3]-xbar_1[3])+sqrt(c_square)*sqrt((1/n1)+(1/n2))*sd3

SCI_3<-c(LC3, UC3)

LC4<-(xbar_0[4]-xbar_1[4])-sqrt(c_square)*sqrt((1/n1)+(1/n2))*sd4
UC4<-(xbar_0[4]-xbar_1[4])+sqrt(c_square)*sqrt((1/n1)+(1/n2))*sd4

SCI_4<-c(LC4, UC4)


#Bonferroni Confidence Intervals

m<-4
t<-qt(0.05/2*m, n1+n2-2, lower.tail=FALSE)


BLC1<-(xbar_0[1]-xbar_1[1])-t*sqrt((1/n1)+(1/n2))*sd1
BUC1<-(xbar_0[1]-xbar_1[1])+t*sqrt((1/n1)+(1/n2))*sd1

BSCI_1 <- c(BLC1,BUC1)

BLC2<-(xbar_0[2]-xbar_1[2])-t*sqrt((1/n1)+(1/n2))*sd2
BUC2<-(xbar_0[2]-xbar_1[2])+t*sqrt((1/n1)+(1/n2))*sd2

BSCI_2 <- c(BLC2,BUC2)


BLC3<-(xbar_0[3]-xbar_1[3])-t*sqrt((1/n1)+(1/n2))*sd3
BUC3<-(xbar_0[3]-xbar_1[3])+t*sqrt((1/n1)+(1/n2))*sd3

BSCI_3 <- c(BLC3,BUC3)


BLC4<-(xbar_0[4]-xbar_1[4])-t*sqrt((1/n1)+(1/n2))*sd4
BUC4<-(xbar_0[4]-xbar_1[4])+t*sqrt((1/n1)+(1/n2))*sd4

BSCI_4 <- c(BLC4,BUC4)


sat <- as.list(c("Income","Gld","Visit","Purchase"))
sut <- as.list(c("Upper","Lower")) 

confint_sim <- matrix(data= c(SCI_1,SCI_2,SCI_3,SCI_4),nrow=4)
rownames(confint_sim) <- sat
colnames(confint_sim) <- sut
confint_sim

ben_confint <- matrix(data= c(BSCI_1,BSCI_2,BSCI_3,BSCI_4),nrow=4)
rownames(ben_confint) <- sat
colnames(ben_confint) <- sut
ben_confint



detach(mean_vector)

# MANOVA
cfactor2 <- as.factor(df$Kidhome)

one_way<- data.frame(cfactor,INCOME$x.t,GOLD$x.t,Web_visit$x.t,Web_Purchase$x.t)

colnames(one_way) <- c("Complain","Income","Gld","Visit","Purchase")

head(one_way)

table(one_way$Complain)

a <- one_way %>% group_by(Complain) %>%  summarise(n = n(), 
                                              mean_Income = mean(Income), 
                                              sd_Income = sd(Income),
                                              mean_Gld = mean(Gld),
                                              sd_Gld = sd(Gld),
                                              mean_Visit = mean(Visit),
                                              sd_Visit = sd(Visit),
                                              mean_Purchase = mean(Purchase),
                                              sd_Purchase = sd(Purchase))

a[,5:10]


library(gridExtra)
library(ggplot2)

df3 <- df[,c("Income",
             "MntGoldProds","NumWebVisitsMonth",
             "NumWebPurchases")]



df_anova <- df[,c("Complain","Income",
                  "MntGoldProds","NumWebVisitsMonth",
                  "NumWebPurchases")]


str(df_anova)


p1 <- ggplot(df_anova, aes(x = Complain, y = Income, fill = Complain)) +
  geom_boxplot(outlier.shape = NA) + 
  theme(legend.position="top")+theme_minimal()+
  labs(title = "The Box Plot of Income by Complain")

p2 <- ggplot(df_anova, aes(x = Complain, y =MntGoldProds , fill = Complain)) + 
  geom_boxplot(outlier.shape = NA) +
  theme(legend.position="top")+theme_minimal()+
  labs(title = "The Box Plot of  Amount Spend on \nGold Products by Complain" )

p3 <- ggplot(df_anova, aes(x = Complain, y = NumWebVisitsMonth, fill = Complain)) + 
  geom_boxplot(outlier.shape = NA) +
  theme(legend.position="top")+theme_minimal()+
  labs(title = "The Box Plot of Number of Visits to \nCompany's Website by Complain" )

p4 <- ggplot(df_anova, aes(x = Complain, y = NumWebPurchases, fill = Complain)) + 
  geom_boxplot(outlier.shape = NA) +
  theme(legend.position="top")+theme_minimal()+
  labs(title = "The Box Plot of Number of Purchase made through \nCompany's Website by Complain" )



grid.arrange(p1, p2, ncol=2) 
grid.arrange(p3,p4,ncol=2)

library(rstatix)
one_way %>% group_by(Complain) %>%  shapiro_test(Income,Gld,Visit,Purchase)

attach(one_way)

library(heplots)
boxM(Y = cbind(Income,Gld,Visit,Purchase), group = factor(Complain))

m1 <- manova(cbind(Income,Gld,Visit,Purchase) ~ Complain, data = one_way)
summary(m1)

summary.aov(m1)


table(df$Teenhome)

df$Teenhome <- as.factor(df$Teenhome)

deneme_data <- data.frame(one_way,df$Teenhome)
head(deneme_data)
m2 <- manova(cbind(Income,Gld,Visit,Purchase)~Complain*df.Teenhome,data= deneme_data)
summary(m2)
summary.aov(m2)


### Principal Component Alaysis


pca <- df[,c("Income","Recency","MntWines","MntFruits","MntMeatProducts",
             "MntFishProducts","MntSweetProducts","MntGoldProds",
             "NumDealsPurchases","NumWebPurchases","NumCatalogPurchases",
             "NumStorePurchases","NumWebVisitsMonth")]

colnames(pca) <- c("Income","Recency","Wines","Fruits","Meat",
                   "Fish","Sweet","Gold",
                   "Deals","WebP","Catalog",
                   "Store","WebV")
rownames(pca) <- df$ID

head(pca)

dim(pca)

str(pca)

summary(pca)


library(car)
#scatterplotMatrix(pca,diagonal = "histogram")

cor(pca)

res <- cor(pca, method="pearson")
corrplot::corrplot(res, method= "color", order = "hclust")



pca_scaled<-scale(pca)
head(pca_scaled)

cov(pca_scaled)
round(cov(pca_scaled),digits = 4)

pca_scaled1<-pca_scaled[,-1]
pca_result <- prcomp(pca_scaled1)
summary(pca_result)


names(pca_result)

pca_result$rotation

pca_result$x

pca_result$sdev



library(factoextra)

fviz_eig(pca_result,addlabels=TRUE) #represent the proportion values





new_pca <-pca_result$x[,1:7]

head(new_pca)



res1 <- cor(new_pca, method="pearson")
corrplot::corrplot(res1, method= "color", order = "hclust")



cor(pca_scaled1,new_pca)


biplot(pca_result, col = c("lightblue", "black"))


fviz_pca_var(pca_result, col.var = "contrib")

fviz_pca_var(pca_result, select.var = list(contrib = 4))

fviz_pca_ind(pca_result, col.ind = "#00AFBB")

fviz_contrib(pca_result, choice = "ind", axes = 1:2,top=30,color = "darkgreen",fill = "darkgreen",
             ) +xlab("ID")+coord_flip()

fviz_pca_ind(pca_result, label="none", habillage=df$Kidhome,
             addEllipses=TRUE, ellipse.level=0.95)

ols.data <- data.frame(Income=pca_scaled[,1],new_pca)

head(ols.data)
options(scipen=999)
lmodel <- lm(Income ~ ., data = ols.data)
summary(lmodel)


lalala <- lm(Income~., data=pca)
summary(lalala)
library(car)
vif(lalala)

vif(lmodel)

mse_pca <- mean((ols.data$Income - predict(lmodel))^2) #mse
mse_origin <- mean((pca$Income - predict(lalala))^2)

rmse_pca <- sqrt(mean((ols.data$Income - predict(lmodel))^2)) #rmse 
rmse_origin <- sqrt(mean((df$Income - predict(lalala))^2)) #rmse 

mse_rmse <- matrix(data = c(mse_pca,mse_origin,rmse_pca,rmse_origin),nrow=2)
rownames(mse_rmse) <- c("PCA Applied","Original Data")
colnames(mse_rmse) <- c("MSE","RMSE")
mse_rmse


### FACTOR ANALYS??S
library(psych)

table(df$Education)

# Define the new labels
new_labels <- c("1", "2", "3", "4","5")

# Create an ordered factor with new labels
education_factor <- factor(df$Education, levels = c("2n Cycle", "Basic" ,
                                                    "Graduation", "Master", "PhD"),
                           labels = new_labels, ordered = TRUE)

education_factor <- as.integer(education_factor)

table(education_factor)
class(education_factor)


table(df$Marital_Status)

new_labels_for_ms <- c("1","2","3","4","5","6","7","8")

martial_stat_factor <- factor(df$Marital_Status,
                              levels = c("Absurd","Alone","Divorced",
                                         "Married","Single","Together","Widow","YOLO"),
                              labels = new_labels_for_ms,ordered = T)



martial_stat_factor <- as.integer(martial_stat_factor)

table(martial_stat_factor)
class(martial_stat_factor)


fa <- df[,c("Income","Recency","MntWines","MntFruits","MntMeatProducts",
            "MntFishProducts","MntSweetProducts","MntGoldProds",
            "NumDealsPurchases","NumWebPurchases","NumCatalogPurchases",
            "NumStorePurchases","NumWebVisitsMonth",
            "Kidhome","Teenhome","Complain")]

str(fa)

fa$Education <- (education_factor)
fa$Martial <- (martial_stat_factor)

fa$Kidhome <- as.integer(fa$Kidhome)
fa$Teenhome <- as.integer(fa$Teenhome)
fa$Complain <- as.integer(fa$Complain)

str(fa)

sum(is.na(fa))


cm <- cor(fa, method="pearson")



corrplot::corrplot(cm, method= "color", order = "hclust")

KMO(r=cm)

print(cortest.bartlett(cm,nrow(fa)))

library(psych)

eigen_plot <- fa.parallel(fa, fm = "minres", fa = "fa")

factanal(fa, factors = 4)$PVAL

factanal(fa, factors = 9)$PVAL

fa_test <- factanal(fa, factors = 9)

fa_test


par(mfrow=c(1,2))
f1_f2 <- fa_test$loadings[,1:2]
plot(f1_f2,type="n") # set up plot
text(f1_f2,labels=names(fa),cex=.7)

f1_f2 <- fa_test$loadings[,8:9]
plot(f1_f2,type="n") # set up plot
text(f1_f2,labels=names(fa),cex=.7)


names(fa_test$loadings[,1])[abs(fa_test$loadings[,1])>0.4]
names(fa_test$loadings[,2])[abs(fa_test$loadings[,2])>0.4]
names(fa_test$loadings[,3])[abs(fa_test$loadings[,3])>0.4]
names(fa_test$loadings[,4])[abs(fa_test$loadings[,4])>0.4]
names(fa_test$loadings[,5])[abs(fa_test$loadings[,5])>0.4]
names(fa_test$loadings[,6])[abs(fa_test$loadings[,6])>0.4]
names(fa_test$loadings[,7])[abs(fa_test$loadings[,7])>0.4]
names(fa_test$loadings[,8])[abs(fa_test$loadings[,8])>0.4]
names(fa_test$loadings[,9])[abs(fa_test$loadings[,9])>0.4]

factor_1<-fa[,names(fa_test$loadings[,2])[abs(fa_test$loadings[,2])>0.4]]
summary(alpha(factor_1,check.keys = ))

factor_scores<-factanal(fa, factors = 9,scores="regression")$scores
head(factor_scores)

factor_plot <- cor(factor_scores, method="pearson")

corrplot::corrplot(factor_plot, method= "number", order = "hclust")

### DISCRIMINATION AND CLASSIFICATION

library(MASS)
library(klaR)
library(ggplot2)
library(GGally)
library(mlbench)

# Enable the r-universe repo
options(repos = c(
  fawda123 = 'https://fawda123.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

# Install ggord
library(ggord)

table(df$Response)



LDA <- df[,c("Income","Recency","MntWines","MntFruits","MntMeatProducts",
             "MntFishProducts","MntSweetProducts","MntGoldProds",
             "NumDealsPurchases","NumWebPurchases","NumCatalogPurchases",
             "NumStorePurchases","NumWebVisitsMonth","Response")]

colnames(LDA) <- c("Income","Recency","Wines","Fruits","Meat",
                   "Fish","Sweet","Gold",
                   "Deals","WebP","Catalog",
                   "Store","WebV","Response")
LDA['Age']= 2023-df$Year_Birth


head(LDA)

LDA$Response <- as.factor(LDA$Response)

str(LDA)
summary(LDA)
sum(is.na(LDA))

#GGally::ggpairs(LDA,  aes(color = Response,  alpha = 0.5)) 


set.seed(189)

#Use 80% of dataset as training set and remaining 20% as testing set
sample <- sample(c(TRUE, FALSE), nrow(LDA), replace=TRUE, prob=c(0.8,0.2))
train <- LDA[sample, ]
test <- LDA[!sample, ] 

library(MASS)

model1 <- lda(Response~.,data = train)
model1

plot(model1,xlim=c(-4,4),col="orange")

pred_values <- predict(model1)
names(pred_values)

# pred_values$class
# head(pred_values$posterior)
# head(pred_values$x)
# LDA$Response

partimat(as.factor(Response)~.,data=train,method="lda") 


train_predict_1<- predict(model1,train)$class
table_train_1 <- table(Predicted =train_predict_1, Actual = train$Response)
table_train_1


sum(diag(table_train_1))/sum(table_train_1)

test_predict_1<- predict(model1,test)$class
table_test_2<- table(Predicted =test_predict_1, Actual = test$Response)
table_test_2

sum(diag(table_test_2))/sum(table_test_2)


### CLUSTER ANALYS??S

##Agglomerative hierarchical clustering

CA <- data.frame(df$Income,df$MntWines,LDA$Age)
colnames(CA) <- c("Income","Wine","Age")

head(CA)

Distances <- dist(CA[, c("Income", "Wine", "Age")])
goster <- dist(head(CA,15)[,c("Income", "Wine", "Age")])
goster



plot(cs <- hclust(goster, method = "single"),main = "Single Linkage")
plot(cc <- hclust(goster, method = "complete"),main = "Complete Linkage")
plot(ca <- hclust(goster, method = "average"),main = "Average Linkage")
plot(cw <- hclust(goster, method = "ward.D2"),main = "Ward Method") # use agglomeration method "ward"

par(mfrow=c(1,1))

## Divisive Hierarchical Clustering

library(cluster)
library(factoextra)



dando <- diana(head(CA,30)[,c("Income","Wine","Age")],stand = T)

hierar_plot <- fviz_dend(dando, cex = 0.5,k = 4, palette = "jco")
hierar_plot


## K-means Clustering

cluster_data <- df[,c("Income","Recency","MntWines","MntFruits","MntMeatProducts",
                      "MntFishProducts","MntSweetProducts","MntGoldProds",
                      "NumDealsPurchases","NumWebPurchases","NumCatalogPurchases",
                      "NumStorePurchases","NumWebVisitsMonth")]

cluster_data$Age <- LDA$Age

rownames(cluster_data) <- df$ID


head(cluster_data)

sapply(cluster_data, var)

rge <- sapply(cluster_data, function(x) diff(range(x)))
crime_s <- sweep(cluster_data, 2, rge, FUN = "/")
sapply(crime_s, var)

n <- nrow(crime_s)
wss <- rep(0, 8)
wss[1] <- (n - 1) * sum(sapply(crime_s, var))
for (i in 2:8)
  wss[i] <- sum(kmeans(crime_s,centers = i)$withinss)

plot(1:8, wss, type = "b", xlab = "Number of groups", ylab = "Within groups sum of squares")

kmeans(crime_s, centers = 3)$centers * rge

final_clust <- kmeans(crime_s, centers = 3)$cluster
table(final_clust)

### CANONICAL CORRELATION ANALYSIS

library(ggplot2)
library(GGally)
library(CCA)
library(CCP)

CCA <- df[,c("MntWines","MntFruits","MntMeatProducts","MntFishProducts",
             "MntSweetProducts","MntGoldProds","NumDealsPurchases","NumWebPurchases","NumCatalogPurchases",
             "NumStorePurchases","NumWebVisitsMonth")]
head(CCA)

Products <- CCA[,1:6]
head(Products)

Place <- CCA[,7:11]
head(Place)

ggpairs(Products,aes(colour="blue"))

ggpairs(Place,aes(colour="steelblue"))

m <- matcor(Products,Place)
m$XYcor

cc1 <- cc(Products, Place)

# display the canonical correlations
cc1$cor

names(cc1)

cc1[3:4]

# compute canonical loadings
cc2 <- comput(Products, Place, cc1)

# display canonical loadings
cc2[3:6]
names(cc2)

# tests of canonical dimensions
rho <- cc1$cor
## Define number of observations, number of variables in first set, and number of variables in the second set.
n <- dim(Place)[1]
p <- length(Products)
q <- length(Place)

## Calculate p-values using the F-approximations of different test statistics:
p.asym(rho, n, p, q, tstat = "Wilks")

p.asym(rho, n, p, q, tstat = "Hotelling")

p.asym(rho, n, p, q, tstat = "Pillai")

p.asym(rho, n, p, q, tstat = "Roy")

s1 <- diag(sqrt(diag(cov(Products))))
s1 %*% cc1$xcoef

s2 <- diag(sqrt(diag(cov(Place))))
s2 %*% cc1$ycoef

library("MASS")
data("chickwts")
chick=chickwts
chick
meanRes=aggregate(chick$weight~chick$feed,FUN=mean)
plot(meanRes)
organisedMean=t(meanRes[,-1])
organisedMean
colnames(organisedMean)=meanRes[,1]
barplot(organisedMean,
        main="Feeds Result",
        xlab="feeds",
        ylab="weight")


#single proportion test: compare 2 proportions and hleps for decide whether those proportions are significantly different or not

#A student got accepted for a Bachelor program in university. 14 universities sent him offer letter out of 20 universities he had applied in.Is the acceptance rate is significantly greater than 50%?
a=array(c(14,20),dim=c(1,2))
prop.table(a)
#h0=the accpetance rate is significantly not greater than 50%?
#h1=the accpetance rate is  significantly greater than 50%?
prop.test(14,20,alt='greater')
#p-value = 0.05.Since the p-value is high, we can say that the proportion are significantly greater than 50 percent.Accept H1.

#h0=the accpetance rate is equal or greater than 50%?
#h1=the accpetance rate is significantly less than 50%?
prop.test(14,20,alt='less',conf.level=0.95)
#p-value = 0.9412. #we can not say that the acceptance rate is equal or greater than 50% because our test only confirm alternative hypothesis 


#Double Proportion 
#h0=both students have equal acceptance rate
#h1=h0=both students have not equal acceptance rate
applieduniversities=c(20,20)
acceptancerate=c(14,11)
prop.test(acceptancerate,applieduniversities,conf.level=0.95)

#T-tests are a type of hypothesis test that allows you to compare means
#Describes how significant the differences between grooups are
#a large t-score tells you that the groups are different
#a small t-score tells you that the groups are similar
#A default t-test/one sample t-test:One-sample t-test can be used only,when the data are normally distributed. Tests the mean of a single group against a known mean. Default t-test (compares mean to 0)
#2 sample t-test compares the means for two groups
#A paried sample t-test compares means from the same group at different times. Before/after.


#One-sample t-test compares one sample mean to a null hypothesis value
#H0=true mean is equal to 0
#h1=true mean is not equal to 0
data=c(0.9,-0.8,1.3,-0.3,1.7)
t.test(data)
#As the p-value 0.3>0.05 that is why we can not say that the true mean is not equal to 0
library(datasets)
data("iris")
t.test(iris$Sepal.Width)
#As the p-value is very samll,that is why we can say that the true mean is not equal to 0
t.test(iris$Sepal.Width,alternative="greater") #Reject H0
#As the p-value is very samll,that is why we can say that the true mean is not equal to 0
t.test(iris$Sepal.Width,alternative="greater",mu=3)#mu is the population mu
t.test(iris$Sepal.Width,alternative="greater",mu=4)#we can not reject H0.
#As the p-value is very large>0.05 that is why we cannot say that the true mean is not equal to 0
#can not reject H0 as the true mean 3.03 is not greater than our population mean 4
browseURL("https://www.kaggle.com/nareshbhat/health-care-data-set-on-heart-attack-possibility")
heart=read.csv("heart.csv")
heartsub=data.frame(heart$sex,heart$thalach)
colnames(heartsub)
colnames(heartsub)=c('sex','maxheartrate')
attach(heartsub)
typeof(heartsub)
summary(heartsub)
hist(maxheartrate,col=colors()[12])
boxplot(maxheartrate~sex)#find outlinears
#H0=Max Heart Rate means are equals for both genders
#H1=There is a difference bwtween Max Heart Rate means for both genders
t.test(maxheartrate~sex)
#p-value is high we can not reject H0 and say that "we can not say that the heart rate is not the same for both genders"
t.test(maxheartrate~sex,alternative="greater") # one tailed test


#20 patient drawn at random from one area,
#their resting systoliic blood pressures is 
before=c(128,118,144,133,132,111,149,139,136,126)
after=c(127,115,142,140,131,132,122,119,129,128)
#h0=There is no difference
#h1=There is a difference
mean(before)
mean(after)
t.test(before,after,paired=TRUE)#can not rject h0
#one tailed test
t.test(before,after,paired=TRUE,alternative="greater")

#there is only 0.004 for very small probability of this result occurring by chance under the nuull hypothesis of no difference
#95 times the true value for the difference would lie in the 95% confidence interval


#F statistic to identify if one of more categories from a set of categories have a differnet mean
#ANOVA
#The results of an F test are presented as an ANOVA table
library(datasets)
data(iris)
levels(iris$Species)
dotchart(iris$Sepal.Length,groups=iris$Species,xlab="spal length",pch=16)
boxplot(Sepal.Length~Species,data=iris)

#h0:All means are equal for all categories
#h1:At least one pair of means are not equal

oneway.test(Sepal.Length~Species,data=iris,var.equal=TRUE)

#Having a low p-value means: reject H0
#compute the t statistic for each pair of categories
fit=aov(Sepal.Length~Species,data=iris)
fit
summary(fit)
fit$coefficients
fit$residuals
MSE=suummary(fit)[[1]][2,3] #[1] means coefficient

#HOC Comparison
fit=aov(Sepal.Length~Species,data=iris)
TukeyHSD(fit)
plot(TukeyHSD(fit))


#Chi-Square
#To compare if two samples of categorical data come from the same population or follow a given population 
#A one sample chi-square goodness of fit test: determines if a sample data matches a population
#A chi-square test for independence: compares two variables in a contingency table to see if they are related.
#In a more general sense, it tests to see whether distributions of categorical variables differ from each another.

caloriesPerDay=c(2000,2800,3000,1500,1800,3200,3500)
names(caloriesPerDay)=c('Mon','Tues','Wed','Thrus','Fri','Sat','Sun')
barplot(caloriesPerDay,col=colors())
chisq.test(caloriesPerDay)

#Two drugs are administered to patients to treat the same disease.
#Are the drugs equally effective?
treat=matrix(c(42,15,14,18),2,2)
treat
colnames(treat)=c('cured','not cured')
rownames(treat)=c('Drug Alpha','Drug Beta')
treat
#Test if there is a difference between drug alpha and drug beta effectiveness
#h0: There is no difference between drug alpha and drug beta effectiveness
#h1: There is a difference between drug alpha and drug beta effectiveness
chisq.test(treat) #as the p-value is low,reject h0. Hence, there is a difference between drug alpha and drug beta effectiveness
coof=chisq.test(treat)
coof$observed
coof$expected
coof$residuals

#In favour against no opinion
#Men 70 66 15
#Women 63 28 8 
#Do men and women have the same distribution of opinions?
ban=matrix(c(70,63,66,28,15,8),2,3)
colnames(ban)=c("In favour","Against","No opinion")
rownames(ban)=c("Men","Women")
ban
#h0: Men and women have the same distribtuion of opiniions
#h1: Men and women does not have the same distribtuion of opinions
#test if there is a difference between the men and women distributions
chisq.test(ban) #p value is low, so reject h0. Hence, men and women doest not have the same distribtuion of opinions 

#cor(variable1,variable2,method='spearman') #use spearman method only when the data in ordinal, first,second,third or nominal scale
data("cars")
head(cars)
cor(cars$speed,cars$dist)
cor.test(cars$speed,cars$dist)




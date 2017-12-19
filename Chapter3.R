

salesdata = read.csv("Sales_AnnArbor_48103.csv")

x_bar <- mean(salesdata$saleprice)
sd <- sd(salesdata$saleprice)
num_sd <- qt(0.975, 499)

t.test(salesdata$saleprice, conf.level = 0.95)$conf.int


library(Hmisc)
x = rbinom(1000000, 1, 0.5)
binconf(sum(x),1000000)

x=rnorm(1000,150,10)
y=rnorm(1000,150,10)
t.test(x,y)

t.test(x,y-2)


observed = c(84,93,105,113,35,70)
expected = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)
chisq.test(x=observed, p=expected)

library(pwr)
pwr.t.test(d = 0.2, sig.level = 0.05, power = 0.80, type = "two.sample", alternative="greater")

pwr.t.test(d=0.2, sig.level = 0.05, type = "two.sample", alternative = "two.sided", n= 200)

pwr.2p.test(h=0.52, sig.level=0.01, power = 0.90)
pwr.2p.test(h=0.52, sig.level=0.01, n=50)
# Use pwr.2p2n.test if you want different sample sizes in the two groups.
# The effect size is defined by:
# ℎ=2 sin^(−1)a〖√0.50−2 sin^(−1)a〖√0.25=0.52〗 〗

autorama = read.csv("autorama.csv")
seqx = seq(20000,100000,10000) 
labx = c("$20K","$30K","$40K","$50K","$60K","$70K","$80K","$90K","$100K")
seqy = seq(5000,30000,5000)
laby = c("$5K","$10K","$15K","$20K","$25K","$30K")
png("scatter.png")
with(autorama, plot(Income,Price,pch=16,axes=FALSE))
axis(1,at=seqx,labels=labx)
axis(2,at=seqy,labels=laby)
dev.off()


model = lm(Price ~ Income, data = autorama)
summary(model)



#package.csv has two columns Pack 1 and Pack2
#Pack1 has the sales for the districts using package 1
#Pack2 has the sales for the districts using package 2
package <- read.csv("package.csv")
rep1 = data.frame(package$Pack1)
rep1$package=1
colnames(rep1)=c("sales","package")
rep2 = data.frame(package$Pack2)
rep2$package=2
colnames(rep2)=c("sales","package")
#Stack the two datasets vertically
combined=rbind(rep1,rep2)
#Set packaging 2 as the reference category for package
combined$package = relevel(factor(combined$package),ref=2)

model = lm(sales ~ package, data = combined)
summary(model)

car = read.csv("cars.csv")
model_c = aov(mpg~factor(origin), data=car)
summary(model_c)

car$origin = factor(car$origin)
levels(car$origin) = c("American","European","Japanese")
model_c2=lm(mpg~origin, data=car)
summary(model_c2)


pairs(~Y+X1+X2+X3, data=example)
cor(example[,c("Y","X1","X2","X3")], use="complete.obs") 



model = lm(y ~ x1 + x2 + x3, data=example)
summary(model)

model = aov(mpg ~ factor(origin), data=cars)
summary(model)

cars$origin = factor(cars$origin)
levels(cars$origin) = c("American","European","Japanese")
model=lm(mpg~origin, data=cars)

# E(mpg)= β0 + βE *(origin==“European”) + βJ *(origin==“Japanese”) 

# head_model2 = lm(DAYS ~ SIZE + LOWERLEVEL + SIZE*LOWERLEVEL, data = headhunting)
# summary(head_model2)

# 
# We calculate the regression line based on ordinary least squares using all points except for the “outlier”.
# We estimate the residual standard deviation using all points except for the “outlier.”
# We calculate the residual for the outlier based on the regression line in 1).
# We “studentize” the residual by dividing by the residual standard deviation from 2) along with an additional term that accounts for the leverage of the “outlier”


newdata = data.frame(x1=10, x2=4, x3=11)
conf.band = predict(model, newdata, interval="confidence", level=0.90) #Confidence band: (22.80, 36.91) 
pred.interval = predict(model, newdata, interval="prediction", level=0.90)#Prediction interval: (-5.02,64.73)


#R code for scatterplot and correlation matrices
install.packages(“car”)
library(car)
pairs(~Y+X1+X2+X3, data=example)
cor(example[,c(“Y”,”X1”,”X2”,”X3”)], use=“complete.obs”) 

shortleaf = read.csv("shortleaf.csv")
plot(x=shortleaf$Diam, y=shortleaf$Vol)
model_leaf <- lm(Vol~Diam, data=shortleaf)
plot(x=model_leaf$fitted.values, y=model_leaf$residuals)

heteroscedasticity

Confidence Interval: 
  > exp(predict(model.3, interval="confidence",
                newdata=data.frame(lnDiam=log(10))))
#        fit      	lwr      		upr
# 20.75934 	19.92952 	21.62372

Prediction Interval:
  > exp(predict(model.3, interval="predict",
                newdata=data.frame(lnDiam=log(10))))
#       fit      	lwr      		upr
# 20.75934 	14.74349 	29.22986


# Introduction to R (see R-start.doc)
# Be careful -- R is case sensitive.
# Setting and getting the working directory
# Use File > Change dir...
# setwd("P:/Data/MATH/Hartlaub/Regression")
# getwd()
# Reading data (Creating a dataframe)
# mydata=read.csv(file=file.choose())
# mydata=read.table(file=file.choose()) #use to read in the txt files for the textbook exercises
# mydata=read.csv(~/Public/Regression/Airfreight.csv") #reading csv file from Regression folder into RStudio
#                 mydata <- read.csv("/shared/hartlaub@kenyon.edu/dataset_name.csv") #use to read a csv file from my shared folder on RStudio
#                 file.copy("/shared/hartlaub@kenyon.edu/test_file.R", "~/ExampleFolder/test_file.R") #use to copy test_file.R from my shared folder to your ExampleFolder
#                 Commands for dataframes
#                 mydata #shows the entire data set
#                 head(mydata) #shows the first 6 rows
#                 tail(mydata) #shows the last 6 rows
#                 str(mydata) #shows the variable names and types
#                 names(mydata) #shows the variable names
#                 rename(V1,Variable1, dataFrame=mydata) #renames V1 to Variable1; Note that epicalc package must be installed!
#                 ls() #shows a list of objects that are available
#                 attach(mydata) #attaches the dataframe to the R search path, which makes it easy to access variable names
#                 Descriptive Statistics
#                 mean(x) #computes the mean of the variable x
#                 median(x) #computes the median of the variable x
#                 sd(x) #computes the standard deviation of the variable x
#                 IQR(x) #computes the IQR of the variable x
#                 summary(x) #computes the 5-number summary and the mean of the variable x
#                 cor(x,y) #computes the correlation coefficient
#                 cor(mydata) #computes a correlation matrix
#                 cor.test(x,y) #test plus CI for rho
#                 Graphical Displays
#                 hist(x) #creates a histogram for the variable x
#                 boxplot(x) # creates a boxplot for the variable x
#                 boxplot(y~x) # creates side-by-side boxplots
#                 DOTplot(x) #creates a dotplot (UsingR package must be installed)
#                 stem(x) #creates a stem plot for the variable x
#                 plot(y~x) #creates a scatterplot of y versus x
#                 plot(mydata) #provides a scatterplot matrix
#                 abline(lm(y~x)) #adds regression line to plot
#                 lines(lowess(y~x)) # adds locally weighted scatterplot smoother line to plot
#                 qplot(x, y) #creates a quick plot (ggplot2 package must be installed)
#                 ci.plot(regmodel) #creates a scatterplot with fitted line, confidence bands, and prediction bands (HH package must be installed)
#                 Liner Regression Models
#                 regmodel=lm(y~x) #fit a regression model
#                 summary(regmodel) #get results from fitting the regression model
#                 anova(regmodel) #get the ANOVA table fro the regression fit
#                 plot(regmodel) #get four plots, including normal probability plot, of residuals
#                 fits=regmodel$fitted #store the fitted values in variable named "fits"
#                 resids=regmodel$residuals #store the residual values in a varaible named "resids"
#                 beta1hat=regmodel$coeff[2] #assign the slope coefficient to the name "beta1hat"
#                 confint(regmodel) #CIs for all parameters
#                 predict.lm(regmodel, interval="confidence") #make prediction and give confidence interval for the mean response
#                 predict.lm(regmodel, interval="prediction") #make prediction and give prediction interval for the mean response
#                 newx=data.frame(X=4) #create a new data frame with one new x* value of 4
#                 predict.lm(regmodel, newx, interval="confidence") #get a CI for the mean at the value x*
#                 Tests for homogeneity of variance
#                 bptest(regmodel) #get the Breusch-Pagan test (lmtest package must be installed)
#                 levene.test(Y, groupvariable) #get the Levene test (lawstat package must be installed)
#                 Tests for normality
#                 ad.test(resids) #get Anderson-Darling test for normality (nortest package must be installed)
#                 cvm.test(resids) #get Cramer-von Mises test for normaility (nortest package must be installed)
#                 lillie.test(resids) #get Lilliefors (Kolmogorov-Smirnov) test for normality (nortest package must be installed)
#                 pearson.test(resids) #get Pearson chi-square test for normaility (nortest package must be installed)
#                 sf.test(resids) #get Shapiro-Francia test for normaility (nortest package must be installed)
#                 Lack-of-fit test
#                 Reduced=lm(y~x)#fit reduced model
#                 Full=lm(y~0+as.factor(x)) #fit full model
#                 anova(Reduced, Full) #get lack-of-fit test
#                 boxcox(regmodel) #evaluate possible Box-Cox transformations (MASS package must be installed)
#                 Model Selection
#                 library(leaps) #load the leaps package
#                 allmods = regsubsets(y~x1+x2+x3+x4, nbest=2, data=mydata) #(leaps package must be loaded), identify best two models for 1, 2, 3 predictors
#                 summary(allmods) # get summary of best subsets
#                 summary(allmods)$adjr2 #adjusted R^2 for some models
#                 summary(allmods)$cp #Cp for some models
#                 plot(allmods, scale="adjr2") # plot that identifies models
#                 plot(allmods, scale="Cp") # plot that identifies models
#                 fullmodel=lm(y~., data=mydata) # regress y on everything in mydata
#                 MSE=(summary(fullmodel)$sigma)^2 # store MSE for the full model
#                 extractAIC(lm(y~x1+x2+x3), scale=MSE) #get Cp (equivalent to AIC)
#                 step(fullmodel, scale=MSE, direction="backward") #backward elimination
#                 none(lm(y~1) #regress y on the constant only
#                 step(none, scope=list(upper=fullmodel), scale=MSE) #use Cp in stepwise regression
#                 Diagnostics
#                 sresids=rstandard(regmodel) #store the standardized residuals in a variable named "sresids"
#                 standresid=stdres(regmodel) #store the standardized residuals in a variable named "standresids"
#                 stud.del.resids=rstudent(regmodel) #store the studentized deleted residuals in a variable named "stud.del.resids"
#                 hatvalues(regmodel) #get the leverage values (hi)
#                 cooks.distance(regmodel) #get Cook's distance
#                 dfbetas(regmodel) #print all dfbetas
#                 dfbetas(regmodel)[4,1] #dfbeta for case 4, first coefficient (i.e., b_0)
#                 dffits(regmodel) [4] #dffits for case 4
#                 influence(regmodel) #various influence statistics, including hat values and dfbeta (not dfbetas) values
#                 library(car) #load the package car
#                 vif(regmodel) #variance inflation factors
#                 avPlots(regmodel) #added variable plots

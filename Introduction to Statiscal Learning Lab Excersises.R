#February 2021.  Working through "Introduction to machine LEarning with R" by Scott Burger
#
#https://static1.squarespace.com/static/5ff2adbe3fe4fe33db902812/t/601cc86d7f828c4792e0bcae/1612499080032/ISLR+Seventh+Printing.pdf
# Pull relevant libraries
#
library(dplyr)
library(ISLR)
library(ggplot2)
# load data from ISLR package
auto<-ISLR::Auto

#check names
colnames(auto)
#make col names available wihtout specifying df
attach(auto)

auto<-auto %>% 
dplyr::mutate( cylinders=as.factor(cylinders))

#plot cylinders vs MPG
plot(cylinders , mpg , col ="red", xlab="cylinders ",
       ylab="MPG")

#Plot histogram. Note "col=2" equivalent to "col=red"
hist(mpg ,col=2, breaks =15)

#Make correlation plot for all columns
pairs(auto)

#subset columns and create pairs corr plot
pairs(∼ mpg + displacement + horsepower + weight +
         acceleration , auto)
#remove "auto" data set
rm(auto)

# Ch2_exercises -----------------------------------------------------------
#End of chapter questions, APPLIED
#2.4. 8

#data dictionary for "College" dataset

#Private : Public/private indicator
#Apps : Number of applications received
#Accept : Number of applicants accepted
#Enroll : Number of new students enrolled
#Top10perc : New students from top 10 % of high school class
#Top25perc : New students from top 25 % of high school class
#F.Undergrad : Number of full-time undergraduates
#P.Undergrad : Number of part-time undergraduates
#Outstate : Out-of-state tuition
#Room.Board : Room and board costs
#Books : Estimated book costs
#Personal : Estimated personal spending
#PhD : Percent of faculty with Ph.D.’s
#Terminal : Percent of faculty with terminal degree
#S.F.Ratio : Student/faculty ratio
#perc.alumni : Percent of alumni who donate
#Expend : Instructional expenditure per student
#Grad.Rate : Graduation rate

#load college data
college<-ISLR::College

#capture rownames in column for college name
college$university<- rownames (college)

# turn row names from college names to Null
rownames(college)<-NULL
head(college)

#2.4.8_c_i
#get summary of data frame 

#summary of df "college"
summary(college)


#2.4.8_c_ii
#corr matrix of first ten columns. 
pairs(college[,1:10])

#2.4.8_c_iii
#Use the plot() function to produce side-by-side boxplots of
#Outstate versus Private.

#plot in base R
priv_y<- college$Outstate[which(college$Private=="Yes")]
priv__n<- college$Outstate[which(college$Private=="No")]

par(mfrow=c(1,2))
boxplot(priv_y)
boxplot(priv__n)
par(mfrow=c(1,1))

boxplot(priv_y,priv__n,
                main = "Private School vs Public School",
                 ylab = "Out_of_state_tuition ($)",
                names = c("Private", "Public"))


#plot in ggplot
ggplot(college, aes(Private, Outstate))+geom_boxplot()

##2.4.8_c_iv Create a new qualitative variable, called Elite, by binning
#the Top10perc variable. We are going to divide universities
#into two groups based on whether or not the proportion
#of students coming from the top 10 % of their high school
#classes exceeds 50 %.

#label if greater than 50% of students are in the top 10% of their class.
college$elite<-ifelse(college$Top10perc>50,"Yes", "No")

# determine how many schools are "elite"
table(college$elite)


#plot side by side boxplots of Elite vs Out_of_State

#plot in base R
elite_y<- college$Outstate [which(college$elite=="Yes")]
elite_n<- college$Outstate[which(college$elite=="No")]

boxplot(elite_y)
boxplot(elite_n)
par(mfrow=c(1,1))

boxplot(elite_y,elite_n,
        main = "Elite School vs Non-Elite School",
        ylab = "Out_of_state_tuition ($)",
        names = c("Elite", "Non-Elite"))


##2.4.8_c_v 
#Use the hist() function to produce some histograms with
#differing numbers of bins for a few of the quantitative variables. You may find the command par(mfrow=c(2,2)) useful:
#it will divide the print window into four regions so that four
#plots can be made simultaneously. Modifying the arguments
#to this function will divide the screen in other ways.
attach(college)
par(mfrow=c(2,2))
hist(Top10perc, main="Histogram of Top10percent_bin5", breaks = 5) 
hist(Top10perc, main="Histogram of Top10percent_bin10", breaks = 10) 
hist(Top10perc, main="Histogram of Top10percent_bin20", breaks = 20) 
hist(Top10perc, main="Histogram of Top10percent_bin30",  breaks = 25) 

#Plot additional variables with variable bin widths
par(mfrow=c(2,2))
hist(Accept, main="Histogram of Acceptance_Rate_bin10", breaks = 10) 
hist(Accept, main="Histogram of Acceptance_Rate_bin30", breaks = 30) 
hist(Grad.Rate, main="Histogram of Grad.Rate_bin10", breaks = 10) 
hist(Grad.Rate, main="Histogram of Grad.Rate_bin30",  breaks = 100) 

#Large bins obscure the distributions in the data, highly granular bin sizes can obscure trends as well

##2.4.8_c_vi
library(GGally)
pairs(college[,1:10])
pairs(college[,10:15])
pairs(college[,1:5,10:15])
GGally::ggpairs(college[,1:10])
GGally::ggpairs(college[,10:18])


#continue to explore the data. Explain what you 
# Private Schools have higher room and board, greater proportion of out of state students, lower percentage of undergraduate, greater proportion of the top 25 and top 10 percent performers, lower acceptance and fewer applications.
# The following variables are uni-modal or approximately normal or mound shaped: "Room and Board"and  "top_25_prcnt".
# Out of state appears to be bimodal.
# The following variables are right skewed: applications, acceptance, enrollment, top10 percent, percent part time undergrad,sf_ratio, personal,  and percent full time undergrad
# The following variables are left skewed: PhD, and terminal



#2.4.9. This exercise involves the Auto data set studied in the lab. Make sure
#that the missing values have been removed from the data.

#data are pre-loaded.  Missing values are removed.
#(a) Which of the predictors are quantitative, and which are qualitative?


#get structure
str(auto)

# Quantitative: mpg, displacement, horsepower, weight, acceleration and year.
# Qualitative:  cylinders, origin, and name 

#(b) What is the range of each quantitative predictor? You can answer this using the range() function. range()

auto_quant<-auto %>% 
  select(mpg, displacement, horsepower, weight, acceleration)


# print range of each quantitative variable
#apply repeats the function on each column, the "2" indicates columns, "1" would indicate rows
apply(auto_quant, 2, range)


#(c) What is the mean and standard deviation of each quantitative predictor?
auto_mean<-apply(auto_quant, 2, mean) 
auto_sd<-apply(auto_quant, 2, sd)

#combine data table 
rbind(auto_mean, auto_sd)
#(d) Now remove the 10th through 85th observations. What is the range, mean, and standard deviation of each predictor in the subset of the data that remains?

auto_quant_subset <- auto_quant[c(1:9,86:392),]
auto_range<-apply(auto_quant_subset, 2, range) 
auto_mean<-apply(auto_quant_subset, 2, mean) 
auto_sd<-apply(auto_quant_subset, 2, sd)

#print range, means, and standard deviation of auto subset.
rbind(auto_range, auto_mean, auto_sd)

#remove misc auto df
rm(auto_quant, auto_quant_subset, auto_range)

#(e) Using the full data set, investigate the predictors graphically,
#using scatterplots or other tools of your choice. Create some plots
#highlighting the relationships among the predictors. Comment
#on your findings.

pairs(auto[1:7],lower.panel=NULL)

# create select histograms
attach(auto)
par(mfrow=c(3,3))
hist(mpg, main="Histogram of mpg", breaks=12) 
hist(displacement, main="Histogram of displacement") 
hist(horsepower, main="Histogram of horsepower", breaks=15) 
hist(weight, main="Histogram of Weight") 
hist(acceleration, main="Histogram of Acceleration") 

cor(auto[,c(1,3:7)], method = c("pearson", "kendall", "spearman"))
#findings:

# Negative correlation between mpg and cylinders, displacement, horsepower, and weight.
#Positive correlation with mpg and year and acceleration time.
#Distributions are principally mound-shaped or normal.The variables mpg and weight are right skewed.  Horsepower and discplacement appear to be slightly bimodal.
#

#(f) Suppose that we wish to predict gas mileage (mpg) on the basis
#of the other variables. Do your plots suggest that any of the
#other variables might be useful in predicting mpg? Justify your
#answer.
# Yes, horsepower, weight and displacement have a strong negative correlation with mpg.
# Weight has the highest correlation coefficient and is therefore likely the most important feature in a potenetial model

#2.4.10 This exercise involves the Boston housing data set
library(MASS)

#read about Boston dataset
?Boston

##2.4.10_a:  How many rows are in this data set? How many columns? What
##do the rows and columns represent?
Boston<-Boston
#view data
head(Boston)
str(Boston)

dim(Boston)
#506 rows and 14 columns
#Each row is a suburb or neighborhood of Boston

##2.4.10_b:  Make some pairwise scatterplots of the predictors (columns) in
##this data set. Describe your findings.
pairs(Boston[c(1:3, 5:7, 9:14)],lower.panel=NULL)
cor(Boston[c(1:3, 5:7, 9:14)])



##2.4.10_c:  Are any of the predictors associated with per capita crime rate?
##  If so, explain the relationship.

#Crime rate (crim), has a .63 and .58 correlation with  access to radial highways (rad) and  full-value property-tax rate per $10k (tax)  respectively.


##2.4.10_d:   Do any of the suburbs of Boston appear to have particularly
##high crime rates? Tax rates? Pupil-teacher ratios? Comment on
##the range of each predictor.



#Get range
apply(Boston, 2, range)
#get Summary
summary(Boston)

# The crime rate (crim), age, zn, indus, dis, rad, black, lstat, and medv has a wide range between the min and max values.
# The highest crime rate is 88 which seems quite high.

##2.4.10_e:   How many of the suburbs in this data set bound the Charles
##river?

#
#count suburbs that are bound by the Charles River
nrow( dplyr::filter(Boston,chas==1))

##2.4.10_f: What is the median pupil-teacher ratio among the towns in this
#data set?

Boston_med<-as.data.frame(  apply(Boston, 2, median))
#get_median pratio
Boston_med[11,]


##2.4.10_g: Which suburb of Boston has lowest median value of owneroccupied homes? What are the values of the other predictors
#for that suburb, and how do those values compare to the overall
#ranges for those predictors? Comment on your findings.

medv_min<-filter(Boston, medv==min(medv))
medv_min

# There are two suburbs tied for lowest medv. Crime rates are high in both as is the lower status of the population and proportion of blacks in town.


#2.4.10_h: In this data set, how many of the suburbs average more than
#seven rooms per dwelling? 

# count number of suburbs with average rooms greater than 7
nrow(dplyr::filter(Boston, rm>7))


# count number of suburbs with average rooms greater than 8
  Bost8<-dplyr::filter(Boston, rm>8)

nrow(Bost8)
#Comment on the suburbs that average more than eight rooms

Summary(Bost8)

#Suburbs  with more than 8 rooms have twice the median crime rate of the full data set.








# CH3_exercises -----------------------------------------------------------
#Lab: Linear Regression

library(MASS)
library(ISLR)

Boston<-MASS::Boston
names(Boston)


#fit model
lm.fit<-lm(medv∼lstat , data=Boston)

#summary for model
summary(lm.fit)

#Get coefficients
coef(lm.fit)

#get confidence interval
confint (lm.fit)

#predict for given set of values with confidence interval
predict (lm.fit ,data.frame(lstat=c(5,10 ,15)),interval ="confidence")

#predict for given set of values with prediction
predict (lm.fit ,data.frame(lstat=c(5,10 ,15)),
         interval ="prediction")

#Ranges are significantly larger than the 5-95 thresholds
#Experimeniting with plots
attach(Boston)
plot(lstat ,medv)
abline(lm.fit)
abline (lm.fit ,lwd =3)
plot(lstat ,medv ,col="red")
abline (lm.fit ,lwd=3,col ="blue")

#change symbols on plots to"+" 
 plot(lstat ,medv ,pch =20)
 plot(lstat ,medv ,pch ="+")
 plot(1:20,1:20,pch =1:20)
# non linear fit.
 
 
 #Diagnostic plots for regression
 par(mfrow=c(2,2))
 plot(lm.fit)
 plot(predict (lm.fit), residuals (lm.fit))
 plot(predict (lm.fit), rstudent (lm.fit))

 plot(hatvalues (lm.fit))
 which.max(hatvalues (lm.fit))


#Multiple linear regression
 #
 #
 mod<-lm(formula = medv ∼ ., data = Boston)
 summary(mod)

 library(car)
 
 #get VIF values
 vif(mod)

 #refit model, hold out age
 mod1<- lm(formula=medv∼.-age ,data=Boston )
summary(mod1)

#non-linear fit of predictors
lm.fit2=lm(medv∼lstat+I(lstat^2))

summary (lm.fit2)

lm.fit=lm(medv∼lstat)

#compare variance of transformed and non-transformed model
anova(lm.fit, lm.fit2)

#plot transformed model
par(mfrow=c(2,2))
plot(lm.fit2)

#fit polynomial model
lm.fit5=lm(medv∼poly(lstat ,5))
summary (lm.fit5)


#load car seats data

Carseats<- ISLR::Carseats
# fit model
lm.fit=lm(Sales∼.+Income :Advertising +Price:Age ,data=Carseats )
summary (lm.fit)

#dummy variable specification
attach(Carseats )
contrasts (ShelveLoc )


# Write Functions

LoadLibraries= function (){
   library(ISLR)
   library(MASS)
  library(dplyr)
   print("The libraries have been loaded .")
}
#run function
LoadLibraries
LoadLibraries()


#pg.132
#(a) Use the lm() function to perform a simple linear regression with
#mpg as the response and horsepower as the predictor. Use the
#summary() function to print the results. Comment on the output.
#For example:

#3.8.i
#Is there a relationship between the predictor and the response?

#load auto data
auto<-ISLR::Auto

#view data
glimpse(auto)

#fit model mpg to horsepower
lm.fit.auto<- lm(mpg~horsepower, auto)

summary(lm.fit.auto)

#3.8.aii. How strong is the relationship between the predictor and the response?
# Adjusted R squared of .6. Correlation coefficient. THere is a negative correlation with mpg, one additional horsepower results in .15 fewer miles per gallon(mpg)
  
#3.8.aiii.
#Is the relationship between the predictor and the response positive or negative?
#negative
  
#3.8.a.iv.
#What is the predicted mpg associated with a horsepower of 98? What are the associated 95 % confidence and prediction intervals?

predict(lm.fit.auto,data.frame(horsepower=c(98)),interval ="confidence")

#the prediction is 24.47 mpg for a car with 98 horsepower.  The 95% confidence interval is 23.97 to 24.96.

#3.8.b
#Plot the response and the predictor. Use the abline() function
#to display the least squares regression line
attach(auto)
plot(horsepower,mpg)
abline(lm.fit.auto, lwd=3,col ="red")

#3.8.c
#Use the plot() function to produce diagnostic plots of the least
#squares regression fit. Comment on any problems you see with
#the fit.

#create diagnostic plots
par(mfrow=c(2,2))
plot(lm.fit.auto)
plot(predict (lm.fit.auto), residuals (lm.fit.auto))
plot(predict (lm.fit.auto), rstudent (lm.fit.auto))

#Relationship has curvature and isn't totally linear,  Residuals and standardized residual show strong bias.  Model has high error at low and high mpg.  Residuals are normally distributed.

#3.9. This question involves the use of multiple linear regression on the Auto data set. 

#a)Produce a scatterplot matrix which includes all of the variables in the data set.

auto<- ISLR::Auto
pairs(auto)

#(b) Compute the matrix of correlations between the variables using the function cor(). You will need to exclude the name variable, cor() which is qualitative.
cor(auto[,c(1:8)], method = c("pearson"))

#(c) Use the lm() function to perform a multiple linear regression with mpg as the response and all other variables except name as the predictors.
#Use the summary() function to print the results.


lm.fit.auto<- lm(mpg~cylinders+ displacement+horsepower+weight+acceleration+year+origin, auto)
summary(lm.fit.auto)

#Comment on the output. For instance:
#  i. Is there a relationship between the predictors and the response?
# Yes, the model has a high F-statistic with high p-value indicating a good fit.

#  ii. Which predictors appear to have a statistically significant relationship to the response?
#displacement, weight, year, and origin appear statistically significant with very small p-values.

#  iii. What does the coefficient for the year variable suggest?
# The coefficient for "year" of .75 suggests for each additional year the mpg goes up .75.

#  (d) Use the plot() function to produce diagnostic plots of the linear regression fit. 
#Comment on any problems you see with the fit. 
#Do the residual plots suggest any unusually large outliers? Does
#the leverage plot identify any observations with unusually high leverage?

#create diagnostic plots
par(mfrow=c(3,2))
plot(lm.fit.auto)
plot(predict (lm.fit.auto), residuals (lm.fit.auto))
plot(predict (lm.fit.auto), rstudent (lm.fit.auto))

# the residuals are not evenly distributed across all x values, instead there is curvature where residuals are high from 0:10 and 30:35. 
#Residuals are mostly normally distributed except in the high positive quantiles.  The model does appear to have several outliers with high leverage.

#  (e) Use the * and : symbols to fit linear regression models with interaction effects. Do any interactions appear to be statistically significant?
attach(auto)
auto$cylinders=as.factor(cylinders)
lm.fit.auto.int<- lm(mpg~displacement+weight+year+ cylinders+weight*displacement, auto)

summary(lm.fit.auto.int)

#create diagnostic plots
par(mfrow=c(3,2))
plot(lm.fit.auto.int)
plot(predict (lm.fit.auto.int), residuals (lm.fit.auto.int))
plot(predict (lm.fit.auto.int), rstudent (lm.fit.auto.int))

# weight and displacement interactions are significant.
#residuals are much more evenly distributed. No obvious curvature in the residuals
par(mfrow=c(1,1))
plot(mpg, predict(lm.fit.auto.int))


#(f) Try a few different transformations of the variables, such as log(X), √
#X, X2. Comment on your findings.

#lm.fit.auto.x2=lm(medv∼lstat+I(lstat^2))
lm.fit.auto.x2 <- lm(mpg~displacement+weight+year+ cylinders+weight*displacement+I(weight^2), auto)
lm.fit.auto.x2 <- lm(mpg~displacement+weight+year+ cylinders+I(displacement^2), auto)

#plot ac vs predicted. tighter fit using the transformed displacement variable, not weight
plot(mpg, predict(lm.fit.auto.x2))

lm.fit.auto.log <- lm(mpg~displacement+weight+year+ cylinders+I(log(displacement)), auto)
plot(mpg, predict(lm.fit.auto.log))

lm.fit.auto.sqrt <- lm(mpg~displacement+weight+year+ cylinders+I(sqrt(displacement)), auto)
plot(mpg, predict(lm.fit.auto.sqrt))

#each transformation improved the fit and tightness of the residuals.  SOmetimes the transformed variable made the original varibale not significant.


##3.10. This question should be answered using the Carseats data set.
carseats<-ISLR::Carseats
#get data dictionary
?Carseats

str(carseats)
#(a) Fit a multiple regression model to predict Sales using Price,Urban, and US.

lm.carseats<- lm(Sales~ Price+Urban+US, data=carseats)

#model summary
summary(lm.carseats)

#(b) Provide an interpretation of each coefficient in the model. Be careful—some of the variables in the model are qualitative!
#  an increase in price by $1 results in $50 less in Sales at a given location
#  Sales dont change at a given location if it is urban vs suburban.  This feature isn't statistically significant.
#  Sales in US increase the sales $1200 at a given location.


#(c) Write out the model in equation form, being careful to handle the qualitative variables properly.

# Sales  =  (-.05 * Price)  +  ( -.02*UrbanYes)  +  (  1.2* USyes)+13.04

#(d) For which of the predictors can you reject the null hypothesis#H0 : βj = 0?
# The p-values are sufficiently small for Price and UsYes to reject the null hypothesis.

#(e) On the basis of your response to the previous question, fit a smaller model that only uses the predictors for which there is evidence of association with the outcome.
lm.carseats.sim<- lm(Sales~ Price+US, data=carseats)

#(f) How well do the models in (a) and (e) fit the data?
par(mfrow=c(1,2))
plot(carseats$Sales, predict(lm.carseats))
plot(carseats$Sales, predict(lm.carseats.sim))

# the models both have significant F-statistics.  THe simplified model has a higher F value and therefore a stronger fit.  In the predicted vs actual the fits look very similar and not particularly strong.


#(g) Using the model from (e), obtain 95 % confidence intervals for the coefficient(s).

confint(lm.carseats.sim, level=0.95)

#(h) Is there evidence of outliers or high leverage observations in the
#model from (e)?
par(mfrow=c(2,3))
plot(lm.carseats.sim)
plot(predict (lm.carseats.sim), residuals (lm.carseats.sim))
plot(predict (lm.carseats.sim), rstudent (lm.carseats.sim))

# No, the model does not appear to have any high-leverage observations.


#11. In this problem we will investigate the t-statistic for the null hypothesis H0 : β = 0 in simple linear regression without an intercept. To
#begin, we generate a predictor x and a response y as follows.
 set.seed(1)
 x=rnorm(100)
 y=2*x+rnorm (100)
#(a) Perform a simple linear regression of y onto x, without an intercept. Report the coefficient estimate βˆ, the standard error of
#this coefficient estimate, and the t-statistic and p-value associated with the null hypothesis H0 : β = 0. Comment on these
#results. 
 
lm.test <-lm(y∼ x+0)
#coefficient is 1.99, error of .1065, t=18.73, p=<2e-16.  We reject the null, the difference in x and y is significant.

#(b) Now perform a simple linear regression of x onto y without an
#intercept, and report the coefficient estimate, its standard error,
#and the corresponding t-statistic and p-values associated with
#the null hypothesis H0 : β = 0. Comment on these results.

lm.test <-lm(x∼ y+0)
summary(lm.test)
#coefficient is .39, error of .02, t=18.73, p=<2e-16.  We reject the null, the difference in x and y is significant.
#(c) What is the relationship between the results obtained in (a) and
#(b)?
# the coefficients (and error) for (a) and (b) are different but the t-statistic and p value are the same.

# (d) For the regression of Y onto X without an intercept, the statistic for H0 : β = 0 takes the form β/ˆ SE(βˆ), where βˆ is
#given by (3.38), and where
#

#12. This problem involves simple linear regression without an intercept.
#(a) Recall that the coefficient estimate βˆ for the linear regression of Y onto X without an intercept is given by (3.38).
#Under what circumstance is the coefficient estimate for the regression of X
#onto Y the same as the coefficient estimate for the regression of Y onto X?
#When the sum of the squares of the observed y-values are equal to the sum of the squares of the observed x-values.

#  (b) Generate an example in R with n = 100 observations in which
#the coefficient estimate for the regression of X onto Y is different
set.seed(1)
x=rnorm(100)
y=2*x+rnorm (100)
lm.simy<-lm(y~x+0)
lm.simx<-lm(x~y+0)

#different coefficients
summary(lm.simy)
summary(lm.simx)



#from the coefficient estimate for the regression of Y onto X.
#(c) Generate an example in R with n = 100 observations in which
#the coefficient estimate for the regression of X onto Y is the
#same as the coefficient estimate for the regression of Y onto X.

#same coefficients
set.seed(1)
x <- rnorm(100)
y <- -sample(x, 100)
sum(x)^2
sum(y)^2
lm.simy<-lm(y~x+0)
lm.simx<-lm(x~y+0)


#13. In this exercise you will create some simulated data and will fit simple
#linear regression models to it. Make sure to use set.seed(1) prior to
#starting part (a) to ensure consistent results.
#(a) Using the rnorm() function, create a vector, x, containing 100
#observations drawn from a N(0, 1) distribution. This represents
#a feature, X.

set.seed(1)
x = rnorm(100, mean=1.000, sd=1)

#(b) Using the rnorm() function, create a vector, eps, containing 100
#observations drawn from a N(0, 0.25) distribution i.e. a normal
#distribution with mean zero and variance 0.25.
set.seed(1)
eps=rnorm(100, mean=0.000, sd=.25)



#(c) Using x and eps, generate a vector y according to the model
#Y = −1+0.5X + eps

y=-1+.5*x + eps

#What is the length of the vector y? What are the values of β0
#and β1 in this linear model?
length(y)
#100. β0=-1, β1=.5

#  3.7 Exercises 125
#(d) Create a scatterplot displaying the relationship between x and
#y. Comment on what you observe.
par(mfrow=c(1,1))
plot(x,y)

#plot is as expected where x and y have a strong linear relationship.

#(e) Fit a least squares linear model to predict y using x. Comment
#on the model obtained. How do βˆ0 and βˆ1 compare to β0 and
#β1?
lm.fit<-lm(y~x)
summary(lm.fit)

#THe model coefficients and answer from (c) are nearly equal.  

#  (f) Display the least squares line on the scatterplot obtained in (d).
#Draw the population regression line on the plot, in a different
#color. Use the legend() command to create an appropriate legend.
plot(x,y)
abline(lm.fit, lwd=3, col="blue")
abline(-1, 0.5, lwd=3, col="red")
legend("topleft", legend = c("model fit", "pop. regression"), col=c("blue", "red"), lwd=3)

#(g) Now fit a polynomial regression model that predicts y using x
#and x2. Is there evidence that the quadratic term improves the
#model fit? Explain your answer.

lm.fit_poly=lm(y∼x+I(x^2))

summary(lm.fit_poly)
#The r2 values are roughly equivalent between the two models although the "x2" term is not significant.
# THe f statistic increase indicating a reduced goodness of fit.



#(h) Repeat (a)–(f) after modifying the data generation process in
#such a way that there is less noise in the data. The model (3.39)
#should remain the same. You can do this by decreasing the variance of the normal distribution used to generate the error term
#in (b). Describe your results.

set.seed(1)
x = rnorm(100, mean=1.000, sd=.1)

set.seed(1)
eps=rnorm(100, mean=0.000, sd=.1)
y=-1+.5*x + eps
lm.fit.lv<-lm(y~x)
#model error (mse) is reduced substantially. Confidence intervals tighten


#(i) Repeat (a)–(f) after modifying the data generation process in
#such a way that there is more noise in the data. The model
#(3.39) should remain the same. You can do this by increasing
#the variance of the normal distribution used to generate the
#error term  in (b). Describe your results.

#model error (mse) is increase substantially. Confidence intervals widen

x = rnorm(100, mean=1.000, sd=.5)

set.seed(1)
eps=rnorm(100, mean=0.000, sd=3)
y=-1+.5*x + eps
lm.fit.hv<-lm(y~x)



#(j) What are the confidence intervals for β0 and β1 based on the
#original data set, the noisier data set, and the less noisy data
#set? Comment on your results.#

confint(lm.fit)
confint(lm.fit.lv)
confint(lm.fit.hv)

#Noise influences the confidence intervals.  The greater the noise the larger the confidence intervals.  


#Question 14
#This problem focuses on the collinearity problem.

#(a) Perform the following commands in R:
set.seed(1)
 x1=runif (100)
 x2=0.5*x1+rnorm (100)/10
 y=2+2*x1+0.3*x2+rnorm (100)
 
 
#The last line corresponds to creating a linear model in which y is
#a function of x1 and x2. Write out the form of the linear model.
#What are the regression coefficients?
 # β0=2,β1=2,β3=0.3
 
 
#  (b) What is the correlation between x1 and x2? Create a scatterplot
#displaying the relationship between the variables.
 
 plot(x1,x2)
 cor(x1,x2)
 #.835 correlation
 
#(c) Using this data, fit a least squares regression to predict y using
#x1 and x2. Describe the results obtained. What are βˆ0, βˆ1, and
#βˆ2? 
# βˆ0=2.47
# βˆ1=1.33
# βˆ2 =.22#model has a weak fit, the p value for x1 is just below the alpha of .05 indicating weak statistical evidence of significance.
 
 #How do these relate to the true β0, β1, and β2? 
 #the predicted values are not very close the the "actuals"
 #Can you reject the null hypothesis H0 : β1 = 0? 
 # yes
 #How about the null hypothesis H0 : β2 = 0?
 
lm.fit<-lm(y~x1+x2)
summary(lm.fit)


# The regression coefficients are approximately equal to the true coefficients. We can reject the null hypothesis for β1 because its p-value is below 5%, although just barely.. 
# We cannot reject the null hypothesis for β2 because its p-value large.


#(d) Now fit a least squares regression to predict y using only x1.
#Comment on your results. Can you reject the null hypothesis
#H0 : β1 = 0?

lm.fit<-lm(y~x1)
summary(lm.fit)

# Yes, the model has a weak fit but the p value is very small so we can reject the null.

#(e) Now fit a least squares regression to predict y using only x2.
#Comment on your results. Can you reject the null hypothesis
#H0 : β1 = 0?

lm.fit1<-lm(y~x2)
summary(lm.fit1)

#Yes, the p value is small we can reject H0

#(f) Do the results obtained in (c)–(e) contradict each other? Explain
#your answer.

#No, with the two variables in one model the relationship between y and x2 was not significant because they have colinearity.  When included in the same regression the effect was difficult to divide appropriately. 
#(g) Now suppose we obtain one additional observation, which was unfortunately mismeasured.
#


#Chapter 15
#Use the Boston data set.
boston<-Boston
str(boston)
summary(boston)

colnames(boston)
#(a) For each predictor, fit a simple linear regression model to predict the response. 
#Describe your results. In which of the models is
#there a statistically significant association between the predictor
#and the response? Create some plots to back up your assertions.

lm.zn<-lm(crim~zn) #yes
summary(lm.zn)

lm.indus<-lm(crim~indus) #yes
summary(lm.indus)

lm.chas<-lm(crim~chas) #no
summary(lm.chas)

lm.nox<-lm(crim~nox)  #yes
summary(lm.nox)

lm.rm<-lm(crim~rm)    #yes
summary(lm.rm)

lm.age<-lm(crim~age)   #yes
summary(lm.age)

lm.dis<-lm(crim~dis)  #yes
summary(lm.dis)

lm.rad<-lm(crim~rad)  #yes
summary(lm.rad)

lm.tax<-lm(crim~tax)  #yes
summary(lm.tax)

lm.ptratio<-lm(crim~ptratio) #yes
summary(lm.ptratio)

lm.black<-lm(crim~black)     #yes
summary(lm.black)

lm.lstat<-lm(crim~lstat)     #yes
summary(lm.lstat)

lm.medv<-lm(crim~medv)      #yes
summary(lm.medv)


#All are significant except "chas"

#(b) Fit a multiple regression model to predict the response using
#all of the predictors. Describe your results. For which predictors
#can we reject the null hypothesis H0 : βj = 0?


lm.fit.bos<- lm(crim~.,data=boston)
#Only 5 of the variables are significant, compared to all but one being significant in the one-parameter models.



#  (c) How do your results from (a) compare to your results from (b)?
#Only 5 of the variables are significant, compared to all but one being significant in the one-parameter models.


#  Create a plot displaying the univariate regression coefficients
#from (a) on the x-axis, and the multiple regression coefficients
#from (b) on the y-axis. That is, each predictor is displayed as a
#single point in the plot. Its coefficient in a simple linear regression model is shown on the x-axis, and its coefficient estimate
#in the multiple linear regression model is shown on the y-axis.
#
x = c(coefficients(lm.zn)[2],
      coefficients(lm.indus)[2],
      coefficients(lm.chas)[2],
      coefficients(lm.nox)[2],
      coefficients(lm.rm)[2],
      coefficients(lm.age)[2],
      coefficients(lm.dis)[2],
      coefficients(lm.rad)[2],
      coefficients(lm.tax)[2],
      coefficients(lm.ptratio)[2],
      coefficients(lm.black)[2],
      coefficients(lm.lstat)[2],
      coefficients(lm.medv)[2])
y = coefficients(lm.fit.bos)[2:14]
plot(x, y)

#Coefficent for Nox is ~30 in the full model and -10 in the univariate model.  Otherwise coefficients are close but not strongly correlated.

#Is there evidence of non-linear association between any of the
#predictors and the response? To answer this question, for each
#predictor X, fit a model of the form
#Y = β0 + β1X + β2X2 + β3X3 + error


lm.zn = lm(crim~poly(zn,3))
summary(lm.zn) # poly 1&  2

lm.indus = lm(crim~poly(indus,3))
summary(lm.indus) # poly 1,2,3

lm.chas = lm(crim~poly(chas,3))
summary(lm.chas) # none 

lm.nox = lm(crim~poly(nox,3))
summary(lm.nox) # poly 1,2,3

lm.rm = lm(crim~poly(rm,3))
summary(lm.rm) # poly 1,2

lm.age = lm(crim~poly(age,3))
summary(lm.age) # poly 1,2,3

lm.dis = lm(crim~poly(dis,3))
summary(lm.dis) # poly 1,2,3


lm.rad = lm(crim~poly(rad,3))
summary(lm.rad) # poly 1&2

lm.tax = lm(crim~poly(tax,3))
summary(lm.tax) # poly 1&2


lm.ptratio = lm(crim~poly(ptratio,3))
summary(lm.ptratio) # poly1,2,3 

lm.black = lm(crim~poly(black,3))
summary(lm.black) # poly 1


lm.lstat = lm(crim~poly(lstat,3))
summary(lm.lstat) # poly 1 2

lm.medv = lm(crim~poly(medv,3))
summary(lm.medv) # poly 1,2,3





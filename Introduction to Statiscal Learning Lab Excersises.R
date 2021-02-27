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




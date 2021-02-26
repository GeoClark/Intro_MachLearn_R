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

par(mfrow=c(1,2))
boxplot(elite_y)
boxplot(elite_n)
par(mfrow=c(1,1))

boxplot(elite_y,elite_n,
        main = "Elite School vs Non-Elite School",
        ylab = "Out_of_state_tuition ($)",
        names = c("Elite", "Non-Elite"))

###############################################################################
# Final Project
# Date: April 29, 2021
#
# Author: Eliza Stone
###############################################################################


############# Step 0: Install/load packages ############################
library(haven)
#install.packages("collapse")
library(collapse)
library(stringi)
library(psych)
library(car)
#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
library(RColorBrewer)
library(ggthemes)
library(reshape2)


############# Step 1: Read in data ############################
setwd("C:/Users/eliza/OneDrive/Documents/PSYC_5323_R/Final Project")

#all csvs in directory
all_files <- list.files(path = ".", pattern = "*.csv")

##read in all csvs
for(i in 1:length(all_files)){
 assign(paste0("df",as.character(i)),read.delim(all_files[i], header=TRUE,sep=",",fill = TRUE))
}


############# Step 2: Clean our data ############################

##convert column names to lowercase
names(df4)[1:17]<-tolower(names(df4)[1:17])
names(df2)[1:17]<-tolower(names(df2)[1:17])


###### FRPL Data #######
##stack 2016-17 and 2017-18 data
lunch<-rbind(df1,df2)

##remove duplicative variables
lunch<- lunch[lunch$data_group == "Free and Reduced-price Lunch Table",c(1,5,11,12,14,15)]
lunch<- lunch[!(lunch$lunch_program == "Missing"),]
lunch <-lunch[order(lunch$school_year, lunch$ncessch, lunch$schid),]

##renaming variables
for(i in 1:nrow(lunch)){
  if(lunch$lunch_program[i]=="Free lunch qualified"){
    lunch$lunch_program[i]= "fl"
  } else if (lunch$lunch_program[i]=="Missing"){
    lunch$lunch_program[i]= "m"
  } else if (lunch$lunch_program[i]=="No Category Codes"){
    lunch$lunch_program[i]= "tot"
  }else if (lunch$lunch_program[i]=="Reduced-price lunch qualified"){
    lunch$lunch_program[i]= "rpl"
  }else {
    lunch$lunch_program[i]= "na"
  }
}

##reshape data
lunch.wide <- reshape(data=lunch, # our data
                  idvar=c( "school_year","ncessch", "schid", "sch_name"), 
                  timevar= "lunch_program", 
                  direction="wide",  
                  v.names=c("student_count")   
)


##identify charters
lunch.wide$charterflag <- stri_detect_fixed(lunch.wide$sch_name,pattern="Charter")

#recode 0/1
lunch.wide$charterflag <- recode(as.numeric(lunch.wide$charterflag), "1"=1, "0"=0)


########## Membership Data ###########
##stack years
member<-rbind(df3,df4)

##remove extra grades
member<- member[!(member$grade == "Pre-Kindergarten" | member$grade == "Grade 13" | member$grade == "No Category Codes" ),]

##remove duplicative variables
member<- member[,c(1,5,11,12,13,14,15,16)]
member <-member[order(member$school_year, member$ncessch, member$grade, member$race_ethnicity),]

##sum and collapse by grade/race/gender
collapse3 <- collap(member, student_count ~ school_year + sch_name + ncessch + schid + race_ethnicity, FUN = sum)
collapse3

##drop weird totals-will create total column later
collapse3<- collapse3[!(collapse3$race_ethnicity == "No Category Codes" | collapse3$race_ethnicity == "Not Specified"),]

##renaming
for(i in 1:nrow(collapse3)){
  if (collapse3$race_ethnicity[i]=="Asian"){
    collapse3$race_ethnicity[i]= "as"
  }else if (collapse3$race_ethnicity[i]=="White"){
      collapse3$race_ethnicity[i]= "wh"
  }else if (collapse3$race_ethnicity[i]=="American Indian or Alaska Native"){
    collapse3$race_ethnicity[i]= "am"
  }else if (collapse3$race_ethnicity[i]=="Native Hawaiian or Other Pacific Islander"){
    collapse3$race_ethnicity[i]= "hp"
  }else if (collapse3$race_ethnicity[i]=="Hispanic/Latino"){
    collapse3$race_ethnicity[i]= "hi"
  }else if (collapse3$race_ethnicity[i]=="Black or African American"){
    collapse3$race_ethnicity[i]= "bl"
  } else if (collapse3$race_ethnicity[i]=="Two or more races"){
    collapse3$race_ethnicity[i]= "tr"
  }else {
    collapse3$race_ethnicity[i]= "."
  }
}


##reshape
member.wide <- reshape(data=collapse3, # our data
                      idvar=c( "school_year", "ncessch"), 
                      timevar= "race_ethnicity", 
                      direction="wide",  
                      v.names=c("student_count")   
)


########## Generate new variables ############

##calculate total students by school/grade
member.wide$total<-0
member.wide$total<- rowSums(member.wide[,c(6,7,8,9,10,11,12)],na.rm=TRUE)

##free reduced lunch mean by school
tapply(lunch.wide$student_count.fl, lunch.wide$charterflag,mean, na.rm=TRUE)

#total students by charter flag
tapply(member.wide$total, member.wide$charterflag,mean, na.rm=TRUE)

# white students by charter flag
tapply(member.wide$student_count.wh, member.wide$charterflag,mean, na.rm=TRUE)

#charter schools by year
aggregate(charterflag ~ school_year , member.wide,sum)


########## Examine Data ################
lunch.wide <-lunch.wide[order(lunch.wide$school_year, lunch.wide$schid),]
head(lunch.wide)
tail(lunch.wide)
describe(lunch.wide)


member.wide <-member.wide[order(member.wide$school_year, member.wide$schid),]
describe(member.wide)
head(member.wide)
tail(member.wide)


#combine lunch and member datasets
merge<-cbind(member.wide,lunch.wide[,5:7])
colnames(merge) <- c("school_year", "sch_name", "ncessch", "schid", "American Indian", "Asian", "Black", "Hispanic", "Hawaiian/Pacific Islander", "Two or more races", "White", "total","charterflag", "Free_Lunch", "Reduced_Lunch", "total_lunch")

##examine new merged dataset
describe(merge)
head(merge)
tail(merge)

#function to calculate race % to total
race_percent_to_total <- function(race_count) {
  perc_to_total <- (race_count/merge$total)*100
  return(perc_to_total)
}

merge$percent_am <-race_percent_to_total(merge[,5])
merge$percent_wh <- race_percent_to_total(merge[,11])
merge$percent_bl <- race_percent_to_total(merge[,7])
merge$percent_as <-  race_percent_to_total(merge[,6])
##### FRPL #####
merge$percent_fl <- race_percent_to_total(merge[,16])


############# Step 3: Visualize our data ############################


##### Demographic bar chart ##### 
final<-merge[,c(1,5,6,7,8,9,10,11,13)]
##mean for all race categories by year and charter
meanStudents <- aggregate(.~school_year*charterflag,final,mean,na.rm=TRUE)

##melt data
melteddata<- melt(meanStudents, id.vars = c("school_year", "charterflag"))

##bar plot for race by year and charter status
ggplot(melteddata,aes(x=factor(charterflag),y=value,fill=factor(variable))) + 
  geom_bar(position="fill", stat="identity") + 	ggtitle("% of students by race at Charter and Public Schools") +
  xlab("Charter and Public Schools") + ylab("% of students") +
  scale_x_discrete(breaks=c("0","1"),
                   label=c("Public", "Charter")) + 
  facet_wrap(~ school_year)



##Scatter plot of FRPL by charter flag
ggplot(merge,aes(x=total, y=percent_fl)) + geom_point(aes(color=factor(charterflag))) +
  labs(title = "% FRPL by Charter School", 
       x = "Total Students", y = "% Free-Reduced Price Lunch") +  theme_grey() + ylim(c(0, 100)) 

############# Step 4: Output our data ############################
capture.output(describe(merge),file="Mergeddescribe.txt")

write.table(merge,"MergedData.txt")
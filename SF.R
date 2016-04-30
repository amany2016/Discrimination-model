setwd("C:/Users/Amoona/Desktop/")
SFsalary = read.csv("SFsalary.csv")
summary(SFsalary)
str(SFsalary)
data$BasePay <- suppressWarnings(as.numeric(as.character(data$BasePay)))
str(SFsalary)

# this code dealimg with zero values in basepay and replace them by the mean value of each job proffestions based on gender
setwd("C:/Users/Amoona/Desktop/")
SFsalary <- read.csv("SFsalary.csv",header = T) 

#create a list of unique Job Titles among male having zero Base Pay
jobs_male_0 <- unique(SFsalary$JobTitle[SFsalary$gender=="male" & SFsalary$BasePay==0])
#create a list of unique Job Titles among male having Non-zero Base Pay
jobs_male_1 <- unique(SFsalary$JobTitle[SFsalary$gender=="male" & SFsalary$BasePay!=0])
#create a list of unique Job Titles among male those can be substituted
jobs_male_substitute <- intersect(jobs_male_0,jobs_male_1)

#substitution of zero Base Pay values with the mean of Non-zero Base Pay values among male
for(j in jobs_male_substitute)
  SFsalary$BasePay[SFsalary$BasePay==0 & SFsalary$gender=="male" & SFsalary$JobTitle==j] <- mean(SFsalary$BasePay[SFsalary$BasePay!=0 & SFsalary$gender=="male" & SFsalary$JobTitle==j])



#create a list of unique Job Titles among female having zero Base Pay
jobs_female_0 <- unique(SFsalary$JobTitle[SFsalary$gender=="female" & SFsalary$BasePay==0])
#create a list of unique Job Titles among female having Non-zero Base Pay
jobs_female_1 <- unique(SFsalary$JobTitle[SFsalary$gender=="female" & SFsalary$BasePay!=0])
#create a list of unique Job Titles among female those can be substituted
jobs_female_substitute <- intersect(jobs_female_0,jobs_female_1)

#substitution of zero Base Pay values with the mean of Non-zero Base Pay values among female
for(j in jobs_female_substitute)
  SFsalary$BasePay[SFsalary$BasePay==0 & SFsalary$gender=="female" & SFsalary$JobTitle==j] <- mean(SFsalary$BasePay[SFsalary$BasePay!=0 & SFsalary$gender=="female" & SFsalary$JobTitle==j])


#prepering for classification model:

setwd("C:/Users/Amoona/Desktop/")
salary = read.csv("SFsalary.csv")

install.packages("tidyr")
install.packages("dplyr")
library("dplyr")
library("tidyr")

# normlizing job title attribute:
salary<- mutate(salary,JobTitle = tolower(JobTitle))
# classify jobtitle to managment and non mamngment postion
salary<- mutate(salary,ManagementPosition =ifelse(grepl("supervisor|manager|captain|chief|head|mayor|director", JobTitle), "ManagementPosition", "NonManagementPosition"))
salary<- mutate(salary,ManagementPosition = as.factor(ifelse(grepl("assistant", JobTitle), "NonManagementPosition", ManagementPosition)))
# converting the base pay data type to numeric ,then,classify pay salary to range group
salary$BasePay <- suppressWarnings(as.numeric(as.character(salary$BasePay)))
salary <- mutate(salary,SalaryGroup = (ifelse(BasePay < 50000, "< 50,000",ifelse(BasePay < 100000, "50,000 - 100,000", ifelse(BasePay < 150000, "100,000 - 150,000",ifelse(BasePay < 200000, "150,000 - 200,000", ">200,000"))))))

levels(salary$SalaryGroup) <- c("< 50,000", "50,000 - 100,000", "100,000 - 150,000", "150,000 - 200,000", ">200,000")
write.csv(salary, "Mnag_NonManag.csv")
str(salary)

# plotting the classification:
install.packages("ggplot2")
install.packages("scales")
library("ggplot2")
library("scales")
ggplot(salary, aes(Gender, BasePay, col = Gender)) + geom_boxplot() +  scale_y_continuous(labels = scales::comma, limits = c(0, 400000)) +labs(x="Gender", y="Base Pay", title = "Bas Pay by gender", col = "Gender") + theme_grey()

ggplot(salary, aes(Gender, BasePay, col = Gender)) +  geom_boxplot() +scale_y_continuous(labels = scales::comma, limits = c(0, 400000)) +  labs(x="Gender", y="BasePay", title = "BasePay by gender and managerial level", col = "Gender") + theme_grey() + facet_grid(~ManagementPosition)


ggplot(salary, aes( x= ManagementPosition, fill = Gender)) + geom_bar(position = "fill") +  scale_y_continuous(labels = scales::percent, breaks=seq(0,1,0.05)) + labs(x="Managerial Level", y="Ratio", fill ="Gender", title = "General male/female ratio") +   theme_grey()


ggplot(salary, aes( x = ManagementPosition, fill = SalaryGroup)) +geom_bar(position = "fill") + scale_y_continuous(labels = scales::percent, breaks=seq(0,1,0.05)) + labs(x="Managerial Level", y="Ratio", fill ="Salary group", title = "Salary groups ratio on different managerial levels") +  theme_grey()


ggplot(salary, aes( x = ManagementPosition, fill = Gender)) + geom_bar(position = "fill") + scale_x_discrete(labels = c("ManagementPosition", "NonManagementPosition"))+  scale_y_continuous(labels = scales::percent, breaks=seq(0,1,0.05)) +labs(x="Managerial Level", y="Ratio", fill ="Gender", title = "Male/female ratio by salary group") +theme_grey() +facet_grid(~SalaryGroup)


#comparing the mean base pay salary for male and female with the mean of manmagment and non-managment postion
install.packages("data.table")
library("data.table")
#browsing my file 
salary_managerial_data <- fread(choose.files(),stringsAsFactors = T)
salary_managerial_data$a <- as.factor(paste(salary_managerial_data$gender,salary_managerial_data$ManagerialPosition,sep = ""))
# Plot1
interaction.plot(x.factor = salary_managerial_data$ManagerialPosition,trace.factor = salary_managerial_data$gender,response = salary_managerial_data$BasePay,type = "p",legend = F)
library(lattice)
# Plot2
xyplot(BasePay~a,data = salary_managerial_data,type="p")


# replacing the zero and Na values by the mean of each job across gender
#for the entire data 
salary <- read.csv("salary.csv",header = T)

jobs_female_0 <- unique(salary$JobTitle[salary$Gender=="Female" & salary$BasePay==0])
jobs_female_NA <- unique(salary$JobTitle[unlist(subset(salary,subset = is.na(salary$BasePay) & salary$Gender=="Female")[1])])
jobs_female_1 <- unique(salary$JobTitle[salary$Gender=="Female" & salary$BasePay!=0])
jobs_female_sub <- intersect(union(jobs_female_0,jobs_female_NA),jobs_female_1)
jobs_female_0_sub <- intersect(jobs_female_0,jobs_female_sub)
jobs_female_NA_sub <- intersect(jobs_female_NA,jobs_female_sub)

for(j in jobs_female_0_sub)
  salary$BasePay[salary$BasePay==0 & salary$Gender=="Female" & salary$JobTitle==j] <- mean(salary$BasePay[salary$BasePay!=0 & salary$Gender=="Female" & salary$JobTitle==j],na.rm = T)

for(j in jobs_female_NA_sub)
  salary$BasePay[unlist(subset(salary,subset = is.na(salary$BasePay) & salary$Gender=="Female" & salary$JobTitle==j)[1])] <- mean(salary$BasePay[salary$BasePay!=0 & salary$Gender=="Female" & salary$JobTitle==j],na.rm = T)


jobs_male_0 <- unique(salary$JobTitle[salary$Gender=="Male" & salary$BasePay==0])
jobs_male_NA <- unique(salary$JobTitle[unlist(subset(salary,subset = is.na(salary$BasePay) & salary$Gender=="Male")[1])])
jobs_male_1 <- unique(salary$JobTitle[salary$Gender=="Male" & salary$BasePay!=0])
jobs_male_sub <- intersect(union(jobs_male_0,jobs_male_NA),jobs_male_1)
jobs_male_0_sub <- intersect(jobs_male_0,jobs_male_sub)
jobs_male_NA_sub <- intersect(jobs_male_NA,jobs_male_sub)

for(j in jobs_male_0_sub)
  salary$BasePay[salary$BasePay==0 & salary$Gender=="Male" & salary$JobTitle==j] <- mean(salary$BasePay[salary$BasePay!=0 & salary$Gender=="Male" & salary$JobTitle==j],na.rm = T)

for(j in jobs_male_NA_sub)
  salary$BasePay[unlist(subset(salary,subset = is.na(salary$BasePay) & salary$Gender=="Male" & salary$JobTitle==j)[1])] <- mean(salary$BasePay[salary$BasePay!=0 & salary$Gender=="Male" & salary$JobTitle==j],na.rm = T)




View(salary[is.na(salary$BasePay),])
View(salary[salary$BasePay==0,])


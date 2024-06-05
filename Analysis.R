#SAESHAV A?L G SUBASH
#TP060888
#install packages
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("readxl")
install.packages("ggtext")
install.packages("RColorBrewer")
install.packages("vtree")
install.packages("dplyr")
install.packages("ggforce")

#load library
library(ggplot2)
library(tidyverse)
library(readxl)
library(ggtext)
library(RColorBrewer)
library(dplyr)
library(vtree)
library(ggforce)

#import data
employee = read.csv ("D:\\R\\employee.csv",header=TRUE)
View(employee)
#Data Preprocessing
#remove redundant employee ID
employee = employee %>%group_by(employee$EmployeeID) %>%filter(age == max(age))
#change label for resignaton
employee$termreason_desc <- as.factor(employee$termreason_desc)
count(employee, termreason_desc)
employee<- mutate(employee, termreason_desc = recode(.x=termreason_desc, "Resignaton"="Resignation"))#
#change label for New Westminister
employee$city_name <- as.factor(employee$city_name)
count(employee, city_name)
employee <- mutate(employee, city_name = recode(.x=city_name, "New Westminister"="New Westminster"))



#Data Exploration
View(employee)
ncol(employee)
nrow(employee)
summary(employee)
summary (employee$age)
summary(employee$length_of_service)
nlevels(factor(employee$city_name))
unique(employee$city_name)
nlevels(factor(employee$department_name))
unique(employee$department_name)
nlevels(factor(employee$job_title))
unique(employee$job_title)
nlevels(factor(employee$store_name))
unique(employee$store_name)
nlevels(employee(data$gender_short))
unique(employee$gender_short)
nlevels(factor(employee$termreason_desc))
unique(employee$termreason_desc)
nlevels(factor(employee$department_name))
unique(employee$department_name)
nlevels(factor(employee$termtype_desc))
unique(employee$termtype_desc)
summary(employee$STATUS_YEAR)
nlevels(factor(employee$STATUS))
unique(employee$STATUS)
nlevels(factor(employee$BUSINESS_UNIT))
unique(employee$BUSINESS_UNIT)

#Does Age affect employee leaving?
#Analysis 1(Which age group is the mode amongst the population)
plot(employee$AgeGroup,type = "o", col = "red", xlab = "Age Group", ylab = "Count",
     main = "Population of employees based on age group")

#Analysis 2(Relationship between age and status)
ggplot(employee, aes(x=STATUS,y=age,fill=gender_short)) +
  labs(x="status",y="age",title="Relationship between age and status")+
  geom_boxplot(position=position_dodge())
#Analysis 3(Relationship between age and length of service)
employee %>% 
  ggplot(aes(x=age,y=length_of_service)) +
  geom_point() +
  labs(x="Age of Employees", y= "Length of Service (in years)",
       title="Relationship between Age and Length Of Service")+
  geom_smooth(method = 'lm',se = FALSE)+
  theme_bw(base_size = 16) +
  scale_y_log10() +
  scale_x_log10()
#Analysis 4(Relationship Age and termination reason)
employee$AgeGroup <- cut(employee$age,
                         breaks = c (19,30,40,50,60,70),
                         labels = c("19-29 years","30-39 years","40-49 years","50-59 years","60-69 years"),
                         right = FALSE)
employee %>%
  group_by(termreason_desc,AgeGroup) %>% summarise(N=n()) %>%
  filter(termreason_desc != "Not Applicable") %>%
  ggplot(aes(x=termreason_desc,y=N,fill=AgeGroup))+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  labs(x="Termination Reason",y="Number Of Employee",title="Bar Chart for Termination Reason Based on Age Group")+
  facet_wrap(~termreason_desc,scales ="free")



#Why are employees in populated cities leaving the company?
#Analysis 1(Relationship between age and city)

#most populated cities with age group 19-29
employee %>%as.data.frame%>%
  group_by(city_name,AgeGroup) %>%
  filter(AgeGroup=="19-29 years")%>%
  summarize(Freq=n())%>%
  arrange(desc(Freq))%>%
  print(n = 10)

#most populated cities with age group 30-39
employee %>%as.data.frame%>%
  group_by(city_name,AgeGroup) %>%
  filter(AgeGroup=="30-39 years")%>%
  summarize(Freq=n())%>%
  arrange(desc(Freq))%>%
  print(n = 10)

#most populated cities with age group 40-49
employee %>%as.data.frame%>%
  group_by(city_name,AgeGroup) %>%
  filter(AgeGroup=="40-49 years")%>%
  summarize(Freq=n())%>%
  arrange(desc(Freq))%>%
  print(n = 10)

#most populated cities with age group 50-59
employee %>%as.data.frame%>%
  group_by(city_name,AgeGroup) %>%
  filter(AgeGroup=="50-59 years")%>%
  summarize(Freq=n())%>%
  arrange(desc(Freq))%>%
  print(n = 10)
#most populated cities with age group 60-69
employee %>%as.data.frame%>%
  group_by(city_name,AgeGroup) %>%
  filter(AgeGroup=="60-69 years")%>%
  summarize(Freq=n())%>%
  arrange(desc(Freq))%>%
  print(n = 10)

# select the cities which have the highest mode of age groups
#1.vancouver
#2.nanaimo
#3.victoria
#4.New Westminister
#5.kelowna
#conduct analysis based on these cities

#Analysis 1 (relationship between city and number of employees)

employee%>%
  group_by(city_name,age) %>% summarise(N=n()) %>%
  filter(city_name %in% c("Vancouver","Nanaimo","Victoria","New Westminster","Kelowna")) %>%
  ggplot(aes(x=city_name,y=N,width=10))+
  geom_bar(stat = "identity",position= "stack",fill = "#FF6666")+
  labs(x="City",y="Number Of Employee",title="Number of employees in cities based on age groups for top 5 cities")+
  theme_bw()+
  theme(panel.border = element_rect(color = "black",fill = NA,size = 1))+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  ggforce::facet_row(vars(city_name), space = 'free')

#Analysis 2(Relationship between age groups and city)
employee%>%
  group_by(city_name,AgeGroup) %>% summarise(N=n()) %>%
  filter(city_name %in% c("Vancouver","Nanaimo","Victoria","New Westminster","Kelowna")) %>%
  ggplot(aes(x=city_name,y=N,fill=AgeGroup,width=10))+
  geom_bar(stat = "identity",position= "stack")+
  labs(x="City",y="Number Of Employee",title="Number of employees in cities based on age groups for top 5 cities")+
  theme_bw()+
  theme(panel.border = element_rect(color = "black",fill = NA,size = 1))+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  ggforce::facet_row(vars(city_name))

#Analysis 3 (Relationship between city and store name)
employee%>%
  group_by(city_name,store_name,STATUS) %>%
  filter(city_name %in% c("Vancouver","Nanaimo","Victoria","New Westminster","Kelowna")) %>%
  vtree(c("city_name", "store_name","STATUS"),
        fillcolor = c( city_name = "#FF33FF",store_name = "#99d8c9"),
        horiz = FALSE)




#Analysis 4 (relationship between department and number of terminations)
employee %>%
  group_by(store_name,department_name,STATUS) %>% summarise(N=n()) %>%
  filter(store_name %in% c("35","45","37"))%>%
  filter(STATUS=="TERMINATED")%>%
  print(n=Inf)%>%
  ggplot(aes(x=department_name,y=N,fill=department_name))+
  geom_bar(stat = "identity",position= "dodge")

#Analysis 5
employee %>%
  group_by(store_name,department_name,STATUS,AgeGroup) %>% summarise(N=n()) %>%
  filter(store_name %in% c("35","45","37"))%>%
  filter(department_name %in% c("Dairy","Meats","Produce"))%>%
  filter(STATUS=="TERMINATED")%>%
  print(n=Inf)%>%
  ggplot(aes(x=department_name,y=N,fill=AgeGroup))+
  geom_bar(stat = "identity",position= "dodge")

#Analysis 6
employee %>% 
  group_by(department_name,length_of_service) %>% summarise(N=n()) %>%
  filter(department_name %in% c("Dairy","Meats","Produce"))%>%
  ggplot(aes(x=length_of_service,y=N,colour= department_name))+
  geom_point(size = 5, alpha = 0.3)+
  geom_line(size = 1)+
  theme_minimal()+
  labs(title = "Employee count and length of service")
  
  
#How does Job Title affect Employee Termination?
 
#ANALYSIS 1 Relationship between job title and number of employees
employee %>%as.data.frame%>%
  group_by(job_title) %>%
  summarize(Freq=n())%>%
  arrange(desc(Freq))%>%
  print(n = Inf)
    
employee %>%as.data.frame%>%
group_by(job_title) %>%
filter(job_title %in% c("Meat Cutter","Cashier","Dairy Person","Produce Clerk","Baker","Shelf Stocker"))%>%
summarize(Freq=n())%>%
ggplot(aes(x=job_title,y=Freq,fill=job_title))+
geom_bar(stat = "identity",position= "dodge")
  
#Analysis 2 Relationship between job title and status
employee %>%as.data.frame%>%
  group_by(job_title,STATUS) %>%
  filter(job_title %in% c("Meat Cutter","Cashier","Dairy Person","Produce Clerk","Baker","Shelf Stocker"))%>%
  summarize(Freq=n())%>%
  ggplot(aes(x=STATUS,y=Freq,fill=job_title))+
  geom_bar(stat = "identity",position= "dodge")
#Analysis 3 Relationship between job title and age group
employee %>%as.data.frame%>%
  group_by(job_title,STATUS,AgeGroup) %>%
  filter(job_title %in% c("Meat Cutter","Cashier","Dairy Person","Produce Clerk","Baker","Shelf Stocker"))%>%
  filter(STATUS== "TERMINATED")%>%
  summarize(Freq=n())%>%
  ggplot(aes(x=job_title,y=Freq,fill= AgeGroup))+
  geom_bar(stat = "identity",position= "stack")
  
  

  
  
  
  

#import data
#cleaning data
#pre-processing data
#data-exploration
#data-visualization
#data transformation

#################################################################################################################################
#import data
data=read.csv("C:/Users/User/Desktop/PFDA/employee_attrition.csv",header=TRUE)

#################################################################################################################################
#import library
library(tidyverse)
library(stringi)
library(plotrix)
library(fmsb)
library(RColorBrewer)
library(skimr)

#################################################################################################################################
#Data Transformation
##Renaming the header of the dataset
names(data)=c("Employee_ID","Employee_Record_Date","Birth_Date","Hired_Date","Termination_Date","Age",
              "Year_of_Service","City","Department","Job","Store_Code","Gender_s","Gender_Full",
              "Reason_of_Termination","Type_of_Termination","Year_of_Status","Status","Business_Unit")

#################################################################################################################################
#Data Cleaning
##Removing spelling errors and removing empty values
data2=data %>%mutate(Termination_Date= case_when(Termination_Date == "1/1/1900" ~"None",Termination_Date !="1/1/1900" ~Termination_Date),
                     City = case_when(City == "New Westminister" ~ "New Westminster", City != "New Westminister" ~ City),
                     Birth_Month = stri_extract_first(Birth_Date,regex = "\\d+"),
                     Termination_Month = case_when(Termination_Date =="None"~"None",Termination_Date !="None" ~stri_extract_first(Termination_Date,regex = "\\d+")),
                     Hire_Month =stri_extract_first(Hired_Date,regex = "\\d+"),
                     Hire_Year =stri_extract_last(Hired_Date,regex = "\\d+"),
                     Year_of_Status = factor(Year_of_Status))%>%arrange(Employee_ID,Year_of_Service) %>%select(-Gender_s)


#################################################################################################################################
#Data Exploration
names(data2)

nrow(data2)
ncol(data2)

sum(is.na(data2))

unique(data2$Employee_ID)
unique(data2$Age)
unique(data2$Year_of_Service)
unique(data2$City)
unique(data2$Department)
unique(data2$Job)
unique(data2$Store_Code)
unique(data2$Type_of_Termination)
unique(data2$Year_of_Status)
unique(data2$Business_Unit)

distinct(data2,data2$Reason_of_Termination)

table(data2$Age)
table(data2$Gender_Full)
table(data2$City)
table(data2$Department)
table(data2 %>% filter(Termination_Date !="None") %>% select(Termination_Date))
table(data2 %>% filter(Termination_Month =="None") %>% select(Termination_Month))
table(data2$Termination_Date)#42450

summary(data2)

structure(data2)

#################################################################################################################################
#Pre-processing data
avg_age2006 =colMeans(data2 %>% filter(Year_of_Status=="2006") %>% select(Age))
avg_age2007 =colMeans(data2 %>% filter(Year_of_Status=="2007") %>% select(Age))
avg_age2008 =colMeans(data2 %>% filter(Year_of_Status=="2008") %>% select(Age))
avg_age2009 =colMeans(data2 %>% filter(Year_of_Status=="2009") %>% select(Age))
avg_age2010 =colMeans(data2 %>% filter(Year_of_Status=="2010") %>% select(Age))
avg_age2011 =colMeans(data2 %>% filter(Year_of_Status=="2011") %>% select(Age))
avg_age2012 =colMeans(data2 %>% filter(Year_of_Status=="2012") %>% select(Age))
avg_age2013 =colMeans(data2 %>% filter(Year_of_Status=="2013") %>% select(Age))
avg_age2014 =colMeans(data2 %>% filter(Year_of_Status=="2014") %>% select(Age))
avg_age2015 =colMeans(data2 %>% filter(Year_of_Status=="2015") %>% select(Age))

#################################################################################################################################
#1 What are the changes in between the year of 2006 to 2015

#Number of employee
data2 %>% group_by(Year_of_Status) %>% summarise(Employee=n()) %>%
  ggplot(aes(x=Year_of_Status,y=Employee,fill=Year_of_Status))+
  geom_bar(col="black", stat = "identity",position = "dodge")+
  ggtitle("Employee From 2006 to 2015")+coord_flip()+xlab("Year")+
  geom_text(aes(label=Employee),hjust=-0.1,vjust=0,col="black",size=3.5)+
  theme(axis.text.y = element_blank())+
  scale_fill_manual(values =c('#f72585','#b5179e','#7209b7','#560bad','#480ca8',
                              '#3a0ca3','#3f37c9','#4361ee','#4895ef','#4cc9f0'))

#Gender
ggplot(data2,aes(x=Year_of_Status,fill=Gender_Full))+geom_bar(position="dodge")+
  facet_wrap(~Year_of_Status)+theme_bw()+ggtitle("Employees' Gender between 2006 to 2015")+
  theme(axis.text.x = element_blank())

#Average Age Group
Year = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015")

avg_ageG = cbind(avg_age2006,avg_age2007,avg_age2008,avg_age2009,avg_age2010,avg_age2011,avg_age2012,avg_age2013,avg_age2014,avg_age2015)

plot(Year,avg_ageG,type="b",main="Average Age from Year 2006 to Year 2015",ylab = "Age",col="blue")

#City
ggplot(data2,aes(x=Year_of_Status))+geom_bar(position="dodge",fill="#ffa7a6")+facet_wrap(~City)+theme_bw()+
  ggtitle("Employees' City between 2006 to 2015")+xlab("Year (2006 - 2015)")+ylab("Number of Employee")+
  theme(axis.text.x = element_blank())


#Job
data2 %>% group_by(Year_of_Status,Job) %>% select(Job) %>% summarise(Employee = n()) %>% 
  ggplot(aes(y=Job,group=Year_of_Status,color=factor(Year_of_Status),x=Employee))+geom_point(size=1.5)+
  ggtitle("Job in Year 2006 - 2015")+theme_bw()

#Hire ( It show how many new emploee hired and the number of employee reduced year by year)
data2 %>% group_by(Year_of_Status,Hire_Year) %>% select(Hire_Year,Year_of_Status) %>% summarise(Employee = n()) %>%
  ggplot(aes(x=Hire_Year,y=Employee,fill = Year_of_Status))+geom_bar(stat="identity")+coord_flip()+
  facet_wrap(~Year_of_Status)

#hire percentage
hire_em_peryear = rbind(nrow(data2 %>% filter(Year_of_Status=="2006",Hire_Year=="2006") %>% select(Age)),
                        nrow(data2 %>% filter(Year_of_Status=="2007",Hire_Year=="2007") %>% select(Age)),
                        nrow(data2 %>% filter(Year_of_Status=="2008",Hire_Year=="2008") %>% select(Age)),
                        nrow(data2 %>% filter(Year_of_Status=="2009",Hire_Year=="2009") %>% select(Age)),
                        nrow(data2 %>% filter(Year_of_Status=="2010",Hire_Year=="2010") %>% select(Age)),
                        nrow(data2 %>% filter(Year_of_Status=="2011",Hire_Year=="2011") %>% select(Age)),
                        nrow(data2 %>% filter(Year_of_Status=="2012",Hire_Year=="2012") %>% select(Age)),
                        nrow(data2 %>% filter(Year_of_Status=="2013",Hire_Year=="2013") %>% select(Age)))

hire_en_peryear_percent <-paste0(round(hire_em_peryear/sum(hire_em_peryear)*100,2),"%")

colour = brewer.pal(length(hire_em_peryear),"Set2")

pie(hire_em_peryear,labels=hire_en_peryear_percent,cex=0.7,radius=1,main="Employee Hired between Year 2006 - 2015",border="black",
    clockwise = TRUE,col=colour)
legend("bottomright",c("2006","2007","2008","2009","2010","2011","2012","2013"),cex=0.7, 
       fill=colour)

#Number of terminate
terminate = table(data2 %>% filter(Status == "TERMINATED") %>% group_by(Year_of_Status) %>% select(Year_of_Status))
p_terminate = paste0(round(terminate/sum(terminate)*100,2),"%")
pie(terminate,label=p_terminate,main=("Number of Employee Terminated between Year 2006 - 2015"),
    col=c("#ffa7a6","#ffcbc4","#c87487","#f1d8b5","#f9ffa1","#fff060","#f2db00","#f0c200","#e7a300","#d1fbd8"))
legend("topleft",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"),cex = 0.8, 
       fill = c("#ffa7a6","#ffcbc4","#c87487","#f1d8b5","#f9ffa1","#fff060","#f2db00","#f0c200","#e7a300","#d1fbd8"),
       title="Year")

#Business Unit
ggplot(data2,aes(x=Year_of_Status,fill=Business_Unit))+geom_bar(position="dodge",col="black")+
  theme_bw()+ggtitle("Business Unit between 2006 to 2015")+scale_fill_manual(values=c("#f6e1f4","#b6e1f4"))

#################################################################################################################################
#2. What are the details of terminated employees?

#gender
gen_termi = table(data2 %>% filter(Status =="TERMINATED") %>% select(Gender_Full))
pie3D(gen_termi,col=hcl.colors(length(gen_termi),"Spectral"),
      main="Analysis on Terminated Employee's Gender",border = "black",shade=0.5,labels = gen_termi,labelcex = 1)
legend("topright",c("Female","Male"), cex = 0.7, fill=hcl.colors(length(gen_termi),"Spectral"))

#type
type_termi = table(data2 %>% filter(Status =="TERMINATED") %>% select(Type_of_Termination))
pie(type_termi,label=type_termi,main = "Analysis on Type of Termination",col=c("#ecd5e3","lavender"),border="white" )
legend("topright",c("Voluntary","Involuntary"), cex =0.8, fill=c("#ecd5e3","lavender"),title="Type")

#reason
layoff_t = matrix(rbind(max=1000,min=0,values=as.integer(data2 %>% filter( Reason_of_Termination=="Layoff") %>% 
                                                           summarise(Layoff=n()))))
resign_t = matrix(rbind(max=1000,min=0,values=as.integer(data2 %>% filter( Reason_of_Termination=="Resignaton") %>% 
                                                           summarise(Layoff=n()))))
retire_t = matrix(rbind(max=1000,min=0,values=as.integer(data2 %>% filter( Reason_of_Termination=="Retirement") %>% 
                                                           summarise(Layoff=n()))))

terminate_emp = data.frame(cbind(layoff_t,resign_t,retire_t))
radarchart(terminate_emp,title = "Reason of Termination between Year 2006 - 2015",
           vlabels = c("Layoff","Resignaton","Retirement"),
           pcol=rgb(0.7,0.5,0.1,0.9) , pfcol= rgb(0.7,0.5,0.1,0.4) ,plwd=2)

#job
data2 %>% filter(Status == "TERMINATED") %>% group_by(Job) %>% summarise(Employee = n()) %>% 
  ggplot(aes(x=Job,y = Employee))+geom_bar(stat="identity",fill = "pink",col="white")+
  coord_flip()+ggtitle("Employees' Job who are Terminated")+
  geom_text(aes(label=Employee),hjust=-0.1,vjust=0,col="black",size=3)+theme_bw()

#city
city_t = data2 %>% filter(Status == "TERMINATED") %>% group_by(City)%>% summarise(Employee = n())

city_t %>% arrange(Employee) %>% ggplot(aes(x=Employee,y = City))+geom_point(shape=21,col="black",fill="#69b3a2")+
  ggtitle("City vs Terminated Employees")

#age
data2 %>% filter(Status == "TERMINATED") %>% group_by(Age)  %>% ggplot(aes(x=Age)) + 
  geom_histogram(fill="#e28274",col="white")+ggtitle("Age of Terminated Employee")+ylab("Employee")+
  theme(plot.title=element_text(size=12,face="bold",hjust = 0.5))

#department
data2 %>% filter(Status == "TERMINATED") %>% group_by(Department) %>% summarise(Employee=n()) %>%
  ggplot(aes(x=Department,y=Employee))+geom_bar(stat="identity", position = "dodge",
                                                fill = "#d59890",col = "black")+coord_flip()+
  ggtitle("Department of Terminated Department")

#length
data2 %>% filter(Status == "TERMINATED") %>% group_by(Year_of_Service,Year_of_Status) %>% summarise(Employee=n()) %>%
  ggplot(aes(x=Year_of_Service,y=Employee,fill=Year_of_Status))+geom_bar(stat="identity",col="white")+
  facet_wrap(~Year_of_Status)+
  ggtitle("Terminated Employee's Year of Service VS Year of Status")

#month
data2 %>% filter(Status == "TERMINATED") %>% group_by(Termination_Month) %>% summarise(Employee=n()) %>% 
  ggplot(aes(x=3,y=Employee,fill=Termination_Month))+geom_col(col="black")+coord_polar(theta="y")+
  xlim(c(0.2,3.5))+theme(panel.background = element_rect(fill = "white"),
                         axis.title = element_blank(),axis.ticks = element_blank(),axis.text = element_blank())+
  geom_text(aes(label=Employee),position = position_stack(vjust =0.5))+ ggtitle("Employee Terminated Month")+
  scale_fill_discrete(labels=c("January","October","November","December","February","March","April","May",
                               "June","July","August","September"))+
  theme(plot.title=element_text(size=12,face="bold",hjust = 0.5))

#year of status
data2 %>% filter(Status == "TERMINATED") %>% group_by(Year_of_Status) %>% summarise(Employee=n()) %>% 
  ggplot(aes(x=Year_of_Status,y=Employee))+geom_bar(stat="identity",fill = "#4a8bad",col = "white")+theme_bw()+
  ggtitle("Number of Employee Terminated in Year 2006 - 2015")+xlab("Year")

##########################################################################################################################
#3. gender

#gender
gender_t = table(data2 %>% filter(Status == "TERMINATED") %>% select(Gender_Full))
pie3D(gender_t,labels=gender_t,cex=0.7,radius=1,main="Terminated Employee's Gender",border="black",
      col=c("#feda75","#ff9966"),explode = 0.1)
legend("right",c("Female","Male"),cex=1,fill=c("#feda75","#ff9966"))

#gender + age
data2 %>% filter(Status == "TERMINATED") %>% group_by(Gender_Full,Age) %>% summarise(Employee = n()) %>%
  ggplot(aes(x=Age,y=Employee,fill=Gender_Full))+geom_bar(stat="identity",position="dodge")+
  ggtitle("Gender vs Age of Terminated Employee between year 2006 - 2015")+theme_bw()

#gender +job + age
data2 %>% filter(Status =="TERMINATED") %>% group_by(Gender_Full,Job,Age) %>% summarise(Employee =n()) %>%
  ggplot(aes(x=Age,y=Job,col=Gender_Full,size=Employee))+geom_point(shape = 19,alpha=0.5,stroke=1)+
  ggtitle("Terminated Employee's Gender Comparing the Job and Age")

#type + gender
data2 %>% filter(Status == "TERMINATED") %>% group_by(Type_of_Termination,Age,Gender_Full)  %>%
  ggplot(aes(x=Age,y=Gender_Full,col=Type_of_Termination))+geom_boxplot() + ggtitle("Gender vs Type of Termination")

#gender+city
data2 %>% filter(Status == "TERMINATED") %>% group_by(Gender_Full,City) %>% summarise(Employee = n()) %>%
  ggplot(aes(x=City,y=Employee,fill=Gender_Full))+geom_bar(stat="identity",position="dodge")+coord_flip()+
  ggtitle("Terminated Employee's Gender Vs City")

#department + gender
data2 %>% filter(Status =="TERMINATED") %>% group_by(Gender_Full,Department,Age) %>% summarise(Employee =n()) %>%
  ggplot(aes(x=Age,y=Department,col=Gender_Full,size=Employee))+geom_point(shape = 19,alpha=0.5,stroke=1)+
  scale_color_manual(values=c("#84a9f3","#ca7166"))+ ggtitle("Terminated Employee's Department vs Age and Gender")

#length + gender
data2 %>% filter(Status =="TERMINATED") %>% group_by(Gender_Full,Year_of_Service) %>% summarise(Employee =n()) %>%
  ggplot(aes(x=factor(Year_of_Service),y=Employee,fill=Gender_Full)) + geom_bar(stat="identity")+xlab("Year of Service")+
  coord_flip()+ggtitle("Terminated Employee's Year of Service vs Gender")

#gender + reason
data2 %>% filter(Status == "TERMINATED") %>% group_by(Gender_Full,Reason_of_Termination) %>% summarise(Employee = n()) %>%
  ggplot(aes(Reason_of_Termination,y=Employee,fill=Gender_Full))+geom_bar(stat="identity",position="dodge")+
  ggtitle("Gender vs Reason of Terminated Employee")+theme_bw()+scale_fill_manual(values=c("#f6a6b2","#90d2d8"))+
  xlab("Reason of Termination")

##########################################################################################################################
# 4. why involuntary?
#which year has the most?
data2 %>% filter(Status =="TERMINATED" & Type_of_Termination=="Involuntary") %>% 
  group_by(Year_of_Status) %>% summarise(Employee =n())%>%
  ggplot(aes(x=Year_of_Status,y=Employee))+geom_bar(stat="identity",fill="#e8702a",col="black")+
  ggtitle("Year VS Employee who is Involuntary to Terminate")

#job and department 
data2 %>% filter(Status =="TERMINATED" & Type_of_Termination=="Involuntary") %>% group_by(Department,Job) %>% 
  summarise(Employee =n()) %>% ggplot(aes(x=Department,y=Employee,fill=Job))+
  geom_bar(stat="identity",position ="dodge")+ scale_fill_manual(values=c("#d2a56d","#ce8b54","#f98787",
                                                                          "#e76a6a","#ffb38a","#ff9248","#52bf90",
                                                                          "#419873","#d9b380","#f1cc8f","#8b9dc3",
                                                                          "#3b5998"))+
  ggtitle("Involuntary Employee's Department vs Job")+theme_classic()

#year of service vs Job
data2 %>% filter(Status =="TERMINATED" & Type_of_Termination=="Involuntary") %>% group_by(Year_of_Service,Job) %>% 
  summarise(Employee =n()) %>% ggplot(aes(x=factor(Year_of_Service),y=factor(Employee),col=Job))+
  geom_point(size=4)+theme_bw()+ ggtitle("Involunatry Employee's Job Vs Year of Service")+xlab("Year of Service") +
  ylab("Employee")

#layoff ()
#age 
data2 %>% filter(Status =="TERMINATED" & Type_of_Termination=="Involuntary" & Reason_of_Termination=="Layoff") %>% 
  group_by(Age) %>% summarise(Employee=n()) %>% ggplot(aes(x=Age,y=Employee))+geom_bar(stat="identity",fill="#e485b4",col="white")+
  geom_text(aes(label=Employee),position = position_stack(vjust =1.1))+theme_gray()+
  ggtitle("Age of Involuntary Layoff Employee")

#length 
data2 %>% filter(Status =="TERMINATED" & Type_of_Termination=="Involuntary" & Reason_of_Termination=="Layoff") %>% 
  group_by(Year_of_Service) %>% summarise(Employee=n()) %>% ggplot(aes(x=Year_of_Service,y=Employee))+
  geom_bar(stat="identity",fill ="#678c83",col="black")+xlab("Year of Service")+
  geom_text(aes(label=Employee),position = position_stack(vjust =1.1))+theme_bw()+
  ggtitle("Year of Service of Involuntary Layyoff Employee")


#job 
data2 %>% filter(Status =="TERMINATED" & Type_of_Termination=="Involuntary" & Reason_of_Termination=="Layoff") %>% 
  group_by(Year_of_Service,Job) %>% summarise(Employee=n()) %>% ggplot(aes(x=Job,y=Year_of_Service,size=Employee))+
  geom_point(color="black",fill="#69b3a2",shape=22,alpha=0.5,stroke=1)+coord_flip()+ylab("Year of Service")+
  ggtitle("Involunatry Layoff Employee's Year of Service vs Job")

#gender age job
data2 %>% filter(Status =="TERMINATED" & Type_of_Termination=="Involuntary" & Reason_of_Termination=="Layoff") %>% 
  group_by(Age,Job,Gender_Full) %>% ggplot(aes(x=Age,y=Job,col=Gender_Full))+
  geom_point(shape=22,fill="white",alpha=0.4,stroke=1,size=2)+ggtitle("Involuntary Layoff Employee's Age vs Job and Gender")

########################################################################################################################
# 5. why voluntary
#reason
r_volun= table(data2 %>% filter(Status =="TERMINATED" & Type_of_Termination=="Voluntary") %>% select(Reason_of_Termination))
rVPercent = round(100*r_volun/sum(r_volun),2)
pie3D(r_volun,labels=paste0(rVPercent,"%"),cex=0.7,radius=1,main="Reason of Voluntary Employee Terminate",border="white", col=c("#feda75","#ff9966"))
legend("topright",c("Resignaton","Retirement"),cex=0.9,fill=c("#feda75","#ff9966"))

#age + job
data2 %>% filter(Status =="TERMINATED" & Type_of_Termination=="Voluntary") %>% group_by(Age,Job) %>% 
  summarise(Employee =n()) %>% ggplot(aes(x=Age,y=Job,size=Employee))+geom_point(col="#008080")+ 
  ggtitle("Voluntary Employee's Job vs Age")


#city
data2 %>% filter(Status =="TERMINATED" & Type_of_Termination=="Voluntary") %>% group_by(City) %>% summarise(Employee =n()) %>%
  ggplot(aes(x=City,y=Employee))+geom_bar(stat="identity",fill="#ff6f69")+coord_flip()+ggtitle("Voluntary Terminated Employee vs City")+
  theme_bw()

#resign()
#resign + age 
data2 %>% filter(Status =="TERMINATED" & Type_of_Termination=="Voluntary" & Reason_of_Termination=="Resignaton") %>% group_by(Age) %>%
  summarise(Employee=n()) %>% ggplot(aes(x=Age,y=Employee))+geom_bar(stat="identity",fill="#1a472a",col="white")+theme_classic()+
  ggtitle("Employee who voluntary resign vs Age")

#resign vs job
data2 %>% filter(Status =="TERMINATED" & Type_of_Termination=="Voluntary" & Reason_of_Termination=="Resignaton") %>% 
  group_by(Job) %>% summarise(Employee=n()) %>% ggplot(aes(x=Job,y=Employee))+
  geom_bar(stat="identity",fill="#cc4d48",col="white")+coord_flip()+
  ggtitle("Voluntary Employee Resign Vs Job")+theme_bw()

#resign vs length
data2 %>% filter(Status =="TERMINATED" & Type_of_Termination=="Voluntary" & Reason_of_Termination=="Resignaton") %>% 
  group_by(Year_of_Service) %>% summarise(Employee=n()) %>% 
  ggplot(aes(x=Year_of_Service,y=Employee))+geom_bar(stat="identity",fill="#ffac6e",col="black")+
  geom_text(aes(label=Employee),hjust=0.7,vjust=-1,col="black",size=3)+theme_bw()+
  ggtitle("Voluntary Resign Employee's Year of Service")

#retire()
#retire: job vs gender vs age
data2 %>% filter(Status =="TERMINATED" & Type_of_Termination=="Voluntary" & Reason_of_Termination=="Retirement") %>% 
  group_by(Age,Gender_Full,Job) %>% summarise(Employee=n()) %>% 
  ggplot(aes(x=Job,size=Age,y=Employee,alpha=Gender_Full))+geom_point()+coord_flip()+
  ggtitle("Voluntary Retired Employee's Age, Gender vs Job")+theme_bw()

#length
data2 %>% filter(Status =="TERMINATED" & Type_of_Termination=="Voluntary" & Reason_of_Termination=="Retirement") %>% 
  group_by(Year_of_Service) %>% summarise(Employee=n()) %>% ggplot(aes(x=3,y=Employee,fill=factor(Year_of_Service)))+
  geom_col(col="black")+coord_polar(theta="y")+ xlim(c(0.2,3.5))+theme(panel.background = element_rect(fill = "white"),
                         axis.title = element_blank(),axis.ticks = element_blank(),axis.text = element_blank())+
  geom_text(aes(label=Employee),position = position_stack(vjust =0.5))+ 
  ggtitle("Voluntary Retired Employees' Year of Service")+
  scale_fill_manual(values=c("#698d5d","#aacb96","#cddfbf","#f8ffe7","#fff2cc","white"))+
  theme(plot.title=element_text(size=15,face="bold",hjust = 0.5))

#################################################################################################################
# 6. which year, what they hire
#age
#in 2006 
hire_2006 = table(data2 %>% filter(Year_of_Status=="2006",Hire_Year=="2006") %>% select(Age))
pie(hire_2006,label=hire_2006,main="Age of Employee Hired in Year 2006",col=c("#ecd5e3","#8b9dc3","white"),border="black")
legend("topright",c("25","26","27"), cex =1, fill=c("#ecd5e3","#8b9dc3","white"),title="Age")

#in 2007
hire_2007 = table(data2 %>% filter(Year_of_Status=="2007",Hire_Year=="2007") %>% select(Age))
pie(hire_2007,label=hire_2007,main="Employee's Age Hired in Year 2007",col=c("#ff9a9a","#5f5fa4","#aaed94"),border="black")
legend("topright",c("24","25","26"), cex =1, fill=c("#ff9a9a","#5f5fa4","#aaed94"),title="Age")

#in 2008
hire_2008 = table(data2 %>% filter(Year_of_Status=="2008",Hire_Year=="2008") %>% select(Age))
pie(hire_2008,label=hire_2008,main="Employee's Age Hired in Year 2008",col=c("#a6b57c","#99ccff","#dabcff"),border="black")
legend("topright",c("23","24","25"), cex =1, fill=c("#a6b57c","#99ccff","#dabcff"),title="Age")

#in 2009
hire_2009 = table(data2 %>% filter(Year_of_Status=="2009",Hire_Year=="2009") %>% select(Age))
pie(hire_2009,label=hire_2009,main="Employee's Age Hired in Year 2009",col=c("#dbb5fd","#f5abe4","#a4f7d4"),border="black")
legend("topright",c("22","23","24"), cex =1, fill=c("#dbb5fd","#f5abe4","#a4f7d4"),title="Age")

#in 2010
hire_2010 = table(data2 %>% filter(Year_of_Status=="2010",Hire_Year=="2010") %>% select(Age))
pie(hire_2010,label=hire_2010,main="Employee's Age Hired in Year 2010",col=c("#93c47d","#c9c2e8","#cfe2f3"),border="black")
legend("topright",c("22","23","24"), cex =1, fill=c("#93c47d","#c9c2e8","#cfe2f3"),title="Age")

#in 2011
hire_2011 = table(data2 %>% filter(Year_of_Status=="2011",Hire_Year=="2011") %>% select(Age))
pie3D(hire_2011,col=c("#eccdd0","#9dcfca","#fff8dc"),main="Employee's Age Hired in Year 2011",
      border="black",shade=0.5,labels=hire_2011,labelcex = 1)
legend("topright",c("20","21","22"), cex = 1, fill=c("#eccdd0","#9dcfca","#fff8dc"),title="Age")

#in 2012
hire_2012 = table(data2 %>% filter(Year_of_Status=="2012",Hire_Year=="2012") %>% select(Age))
pie3D(hire_2012,col=c("#d4cfff","#cff0ff","#ffcfcf"),main="Employee's Age Hired in Year 2012",
      border="black",shade=0.5,labels=hire_2012,labelcex = 1,explode = 0.1)
legend("topright",c("19","20","21"), cex = 1, fill=c("#d4cfff","#cff0ff","#ffcfcf"),title="Age")

#in 2013
hire_2013 = table(data2 %>% filter(Year_of_Status=="2013",Hire_Year=="2013") %>% select(Age))
pie3D(hire_2013,col=c("#b7dc87","#ffcc99"),main="Employee's Age Hired in Year 2013",
      border="black",shade=0.5,labels=hire_2013,labelcex = 1,explode = 0.1)
legend("topright",c("19","20"), cex = 1, fill=c("#b7dc87","#ffcc99"),title="Age")

#job
#2006
job_2006 = data2 %>% filter(Year_of_Status=="2006",Hire_Year=="2006") %>% group_by(Job) %>%summarise(Emplyee=n())
barplot(height=job_2006$Emplyee, beside = TRUE,main = "Job Hire in Year 2006",names.arg = c("Baker","Cashier","Dairy Person","Shelf Stocker"),
        col = "#d2e9b4")

#2007
job_2007 = data2 %>% filter(Year_of_Status=="2007",Hire_Year=="2007") %>% group_by(Job) %>%summarise(Emplyee=n())
barplot(height=job_2007$Emplyee, beside = TRUE,main = "Job Hire in Year 2007",names.arg = job_2007$Job,
        col = "#ffe599")

#2008
job_2008 = data2 %>% filter(Year_of_Status=="2008",Hire_Year=="2008") %>% group_by(Job) %>%summarise(Emplyee=n())
barplot(height=job_2008$Emplyee, beside = TRUE,main = "Job Hire in Year 2008",names.arg = job_2008$Job,
        col = "#ffcc99")

#2009
job_2009 = data2 %>% filter(Year_of_Status=="2009",Hire_Year=="2009") %>% group_by(Job) %>%summarise(Emplyee=n())
barplot(height=job_2009$Emplyee, beside = TRUE,main = "Job Hire in Year 2009",names.arg = job_2009$Job,
        col = "#9aa5d9")

#2010
job_2010 = data2 %>% filter(Year_of_Status=="2010",Hire_Year=="2010") %>% group_by(Job) %>%summarise(Emplyee=n())
barplot(height=job_2010$Emplyee, beside = TRUE,main = "Job Hire in Year 2010",names.arg = job_2010$Job,
        col = "#a3c0c6",horiz = TRUE)

#2011
job_2011 = data2 %>% filter(Year_of_Status=="2011",Hire_Year=="2011") %>% group_by(Job) %>%summarise(Emplyee=n())
barplot(height=job_2011$Emplyee, beside = TRUE,main = "Job Hire in Year 2011",names.arg = job_2011$Job,
        col = "#ccaa80",horiz = TRUE)

#2012
job_2012 = data2 %>% filter(Year_of_Status=="2012",Hire_Year=="2012") %>% group_by(Job) %>%summarise(Emplyee=n())
barplot(height=job_2012$Emplyee, beside = TRUE,main = "Job Hire in Year 2012",names.arg = job_2012$Job,
        col = "#fa7e04")

#2013
job_2013 = data2 %>% filter(Year_of_Status=="2013",Hire_Year=="2013") %>% group_by(Job) %>%summarise(Emplyee=n())
barplot(height=job_2013$Emplyee, beside = TRUE,main = "Job Hire in Year 2013",names.arg = job_2013$Job,
        col = "#ffb891")

#################################################################################################################

#7. How many stores and Headoffice?
#store
data2 %>% filter(Business_Unit=="STORES")%>%group_by(Year_of_Status,Business_Unit) %>%summarise(Employee=n()) %>% ggplot(aes(x=Year_of_Status,y=Employee,group=Business_Unit))+
  geom_line(color="#69b3a2", size=1, alpha=0.9, linetype=2)+geom_point(shape=21, color="black", fill="#00676a", size=3)+
  ggtitle("Number of Employee Working in Stores between Year 2006 - 2015")+theme_classic()

#Headoffice
data2 %>% filter(Business_Unit=="HEADOFFICE")%>%group_by(Year_of_Status,Business_Unit) %>%summarise(Employee=n()) %>% ggplot(aes(x=Year_of_Status,y=Employee,group=Business_Unit))+
  geom_line(color="black", size=1, alpha=0.9, linetype=2)+geom_point(shape=23, color="#c71585", fill="#e485b4", size=2)+
  ggtitle("Number of Employee Working in Headoffice in Year 2006 - 2015")+theme_classic()

#store vs City
#2006 
data2 %>% filter(Business_Unit=="STORES")%>%group_by(City,Year_of_Status) %>% summarise(Employee=n()) %>%
  ggplot(aes(fill=Year_of_Status,x=City,y=Employee))+geom_point(size=3,col="black",shape=25)+
  coord_flip()+scale_fill_manual(values=c("#9aa5d9","#ff7373","#0fd99b","#ae9cde","#f7b066",
                                          "#dfbbb8","#5176be","#ffb0bb","#cc4040","#abce29","#b86e7d"))+
  ggtitle("Number of Stores in each City between Year 2006 - 2015")

#headoffice vs city vs Department
data2 %>% filter(Business_Unit=="HEADOFFICE")%>%group_by(City,Department) %>%summarise(Employee=n()) %>%
  ggplot(aes(x=Department,y=Employee,fill=City))+geom_bar(stat="identity")+coord_flip()+
  ggtitle("Headoffice vs Department")+theme_bw()

#62 Analysis



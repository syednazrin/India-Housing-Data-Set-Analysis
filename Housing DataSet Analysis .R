#increase=green
#decrease=red
#Delhi=goldenrod
#Mumbai=green
#Hyderabad=blue
#Chennai=purple
#Kolkata=salmon
#Bangalore=pink
#=========================================================================================================
#Using tidy verse library 
#install.packages("tidyverse")
library(tidyverse)
library(treemapify)
#install.packages("waterfalls")
library(waterfalls)
library(ggpubr)
#install.packages("ggalt") #installed ggalt package
library(ggalt) # to gain access to quantile function
library(plotrix)  # to create 3d pie charts
#=========================================================================================================
#import DATA
AssignmentData<-read.csv("C:\\Users\\Nadira\\Documents\\Downloads\\R tuition\\House_Rent_Dataset.csv",
                         stringsAsFactor=FALSE,
                         header = TRUE)
#=========================================================================================================
#PreProcessing
#assigning new headers to the columns
names(AssignmentData)<-c("Date","No.Bedrooms","MonthlyRent",
                         "ApartmentSize_Sqft","ResidingFloor","PropertyAreaType",
                         "AreaLocation", "CityLocated", "Furnishing",
                         "PreferenceOfTenants", "No.Bathrooms", "WayofContacting")

names(AssignmentData)

class(AssignmentData)

print(paste("Number of columns",ncol(AssignmentData),sep=':'))#no of column

print(paste("Number of rows",nrow(AssignmentData),sep=':'))#no of row

#summarizing numerical data// 
#columnstaken : No.Bedrooms, MontlyRent, ApartmentSize, No.Bathrooms (4/12)
AssignmentData%>%
  select(No.Bedrooms,MonthlyRent,ApartmentSize_Sqft,No.Bathrooms)%>%
  summary()

#summarizing
#columns taken:ResidingFloor, PropertAreaType,  AreaLocation, 
#CityLocated ,  Furnishing  , PreferenceOfTenants, WayofContacting
summary(factor(AssignmentData$ResidingFloor)) 
summary(factor(AssignmentData$PropertyAreaType))
summary(factor(AssignmentData$AreaLocation))
summary(factor(AssignmentData$CityLocated))
summary(factor(AssignmentData$Furnishing))
summary(factor(AssignmentData$PreferenceOfTenants))
summary(factor(AssignmentData$WayofContacting))
#====================================================
#CLEANING DATA
#====================================================
D1<-AssignmentData%>%filter(str_detect(ResidingFloor, "out of")) %>%  #only rows that have the words "out of"  
  separate(ResidingFloor, sep="out of", c("ResidingFloor","Total"))%>% #seperating residing floor into two new columns; residing floor and total
  mutate(Total= as.numeric(Total)) %>%
  mutate(ResidingFloor = ifelse(str_trim(ResidingFloor)=="Ground","0",str_trim(ResidingFloor))) %>% #assign number zero if word has ground
  mutate(ResidingFloor = ifelse(str_detect(ResidingFloor,"Basement"),"-1",str_trim(ResidingFloor))) %>% #assign -1 if they have word basement
  mutate(ResidingFloor= as.numeric(ResidingFloor)) 

D2<-AssignmentData%>%filter(!str_detect(ResidingFloor, "out of")) %>%  #calling only data that doesnt  that have the words "out of"  
  mutate(ResidingFloor = ifelse(str_trim(ResidingFloor)=="Ground","0",str_trim(ResidingFloor))) %>%
  mutate(ResidingFloor = ifelse(str_detect(ResidingFloor,"Basement"),"-1",str_trim(ResidingFloor))) %>%
  mutate(ResidingFloor= as.numeric(ResidingFloor))%>%
  mutate(Total= as.numeric(ResidingFloor))

#adding these new data into dataset
UpdatesAssigmentDataSet<-rbind(D1,D2)
#=====================================================
#CHECKING THE DATA (Before cleaning)
#--->logically the the value of a ratio can be between -1 and 1, 
#however some homeowners have entered the entries in reverse
#these will be updated
UpdatesAssigmentDataSet[which(UpdatesAssigmentDataSet$ResidingFloor>UpdatesAssigmentDataSet$Total),
                        c("ResidingFloor","Total")]

#swapping values
k<-which(UpdatesAssigmentDataSet$ResidingFloor>UpdatesAssigmentDataSet$Total)
tmp<-UpdatesAssigmentDataSet[k,"ResidingFloor"]
UpdatesAssigmentDataSet[k,"ResidingFloor"]<-UpdatesAssigmentDataSet[k,"Total"]
UpdatesAssigmentDataSet[k,"Total"]<-tmp


#rechecking
UpdatesAssigmentDataSet[k,c("ResidingFloor","Total")]
#=====================================================
#ADDING NEW COLUMNS
UpdatesAssigmentDataSet<- UpdatesAssigmentDataSet%>%
  mutate(MonthlyRentInThousand=MonthlyRent/1000, #dividng the rent by thousand
         floorRatio=ResidingFloor/Total, #adding house floor ratio
         rentpersquarefeet=MonthlyRent/ApartmentSize_Sqft) #creating rent per square feet

#creating bedroom to bathroom ratio 
UpdatesAssigmentDataSet$BBRatio<-UpdatesAssigmentDataSet$No.Bedrooms/UpdatesAssigmentDataSet$No.Bathrooms

UpdatesAssigmentDataSet$FormatedDate<- as.Date(UpdatesAssigmentDataSet$Date,"%m/%d/%Y") #formatting date
#================================================================================
city_colours<-c("Delhi"="goldenrod","Mumbai"="green","Hyderabad"="blue",
                "Chennai"="purple","Kolkata"="salmon","Bangalore"="pink")
ggplot(data=UpdatesAssigmentDataSet,
       mapping=aes(x=CityLocated,y=ApartmentSize_Sqft,group=CityLocated,fill=CityLocated))+
  geom_boxplot(varwidth=TRUE,fill='orchid') +
  labs(title="Box plot of Apartment Size(sq. ft.) according to city", 
       subtitle="Before removing outliers",
       x="Cities",
       y="Apartment Size")+
  theme_bw()

#checking for any outliers
p1<-ggplot(data=UpdatesAssigmentDataSet, 
           mapping=aes(x=CityLocated,y=MonthlyRentInThousand))+
  geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box plot of Monthly rent according to city", 
       subtitle = "Monthly rent depicted in thousands",
       x="Cities",
       y="Monthly rent")+
  theme_bw()

#---> as you can see we have outliers which can distort the outcomes
quartiles <- quantile(UpdatesAssigmentDataSet$MonthlyRentInThousand, 
                      probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(UpdatesAssigmentDataSet$MonthlyRentInThousand)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
print(paste("Number of rows before removing outliers",nrow(UpdatesAssigmentDataSet),sep=':'))
UpdatesAssigmentDataSet <- subset(UpdatesAssigmentDataSet,
                                  UpdatesAssigmentDataSet$MonthlyRentInThousand > Lower & UpdatesAssigmentDataSet$MonthlyRentInThousand< Upper)

print(paste("Number of rows after removing outliers",nrow(UpdatesAssigmentDataSet),sep=':'))
#=============================================================================================
p2<-ggplot(UpdatesAssigmentDataSet, aes(x=CityLocated,y=MonthlyRentInThousand))+
  geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box plot of Monthy rent according to city", 
       subtitle="After removing outliers",
       caption = "Monthly rent depicted in thousands",
       x="Cities",
       y="Monthly rent")+
  theme_bw()
ggarrange(p1,p2,nrow=1,ncol=2)
#==============================================================================
#EARLY UNDERSTANDING OF THE DATASET ---> where does this data come from

#pie chart
citydata<-UpdatesAssigmentDataSet %>% group_by(City=CityLocated)%>%
  summarise(count= n()/nrow(UpdatesAssigmentDataSet)*100)

Cities<-citydata$City
v2<-round(citydata$count,2)

df = data.frame(Cities,v2)
df

p1<-ggplot(data=df, aes(x = Cities,y = v2,
                        fill = Cities,label=paste0(v2,'%'))
) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  coord_polar()+
  geom_label()+
  scale_fill_manual(values=city_colours)+
  theme_void()+
  ggtitle("Distribution of listings across Cities")

#pie chart
o<-UpdatesAssigmentDataSet %>% group_by(Furnishing)%>%
  summarise(count= n()/nrow(UpdatesAssigmentDataSet)*100)
m<-o$count
m

names<-c("furnished", "semi-furnished", "unfurnished")

pie(m,names, radius = 1, main = "percentage of data according to furnishing type", col = c("green","blue","red"), clockwise = TRUE)


#3dpie chart
w<-UpdatesAssigmentDataSet %>% group_by(PropertAreaType)%>%
  summarise(count= n()/nrow(UpdatesAssigmentDataSet)*100)
w
m1<-w$count
m1

names2=c("Built area", "carpet area","super area")
#one extra parameter that 3d pie has is explode
pie3D(m1, labels = names2, explode = 0.5,main = "percentage of data according to Area type",col = c("green","blue","red"))
#================================================================================
#tree map
citydata<- UpdatesAssigmentDataSet%>%
  group_by(CityLocated)%>%
  summarize(count=n())

p2<-ggplot(citydata, mapping=aes(area = count, fill = CityLocated, 
                                 label=paste0(CityLocated,'\n',count)))+
  geom_treemap()+
  geom_treemap_text()+
  scale_fill_manual(values=city_colours)+
  ggtitle('Count of listings across Cities')
ggarrange(p1,p2,nrow=1,ncol=2,common.legend=TRUE)
#=================================================================================
#QUESTION 1:Are there any general correlations and do the cities follow these behaviours?

#1.1
#a)1.1.1 No of bedrooms to average monthly rent
#step1: group all the rent by their number of bedrooms and calculate the average for each numbered group
d1<- UpdatesAssigmentDataSet%>%
  group_by(No.Bedrooms)%>%
  summarise(AverageRent=mean(MonthlyRentInThousand))
d1

#step2: plotting
ggplot(data=d1, mapping=aes(x=No.Bedrooms, y=AverageRent))+
  geom_line(color="black")+
  ggtitle("Average Rent to Number of bedrooms")+
  ylab("Average Rent in rupees (In thousands)")+
  xlab("Number of Bedrooms")+
  theme_bw()

#1.1.2
#----> does this apply to all the states?
#lets compare average rent according to bedroom according to cities
#step 1: create specfic dataset and apply the same instructins as before
BglrBd<-filter(UpdatesAssigmentDataSet, CityLocated == "Bangalore")%>%
  group_by(No.Bedrooms)%>%
  summarise(AverageRent=mean(MonthlyRentInThousand))

ChnBd<-filter(UpdatesAssigmentDataSet, CityLocated == "Chennai")%>%
  group_by(No.Bedrooms)%>%
  summarise(AverageRent=mean(MonthlyRentInThousand))

DelhBd<-filter(UpdatesAssigmentDataSet, CityLocated == "Delhi")%>%
  group_by(No.Bedrooms)%>%
  summarise(AverageRent=mean(MonthlyRentInThousand))

HdbdBd<-filter(UpdatesAssigmentDataSet, CityLocated == "Hyderabad")%>%
  group_by(No.Bedrooms)%>%
  summarise(AverageRent=mean(MonthlyRentInThousand))

KlktBd<-filter(UpdatesAssigmentDataSet, CityLocated == "Kolkata")%>%
  group_by(No.Bedrooms)%>%
  summarise(AverageRent=mean(MonthlyRentInThousand))

MmbBd<-filter(UpdatesAssigmentDataSet, CityLocated == "Mumbai")%>%
  group_by(No.Bedrooms)%>%
  summarise(AverageRent=mean(MonthlyRentInThousand))
v<-rbind(BglrBd,ChnBd, DelhBd, HdbdBd, KlktBd, MmbBd)
v

#step2
plot(x=BglrBd$No.Bedrooms,y=BglrBd$AverageRent,
     type = "o", 
     xlab = "No Of Bedroooms", 
     ylab = "Average Rent in Rupees (thousands)", 
     main = "Average rent  by bedroom according to city", 
     col="red",
     xlim=c(1,6),
     ylim=c(0,80))
points(x=ChnBd$No.Bedrooms,y=ChnBd$AverageRent,
       type = "o", 
       xlab = "No Of Bedroooms", 
       ylab = "AverageRent", 
       main = "Average rent  by bedroom according to city", col="blue", lty=2 ) 
points(x=DelhBd$No.Bedrooms,y=DelhBd$AverageRent,
       type = "o", 
       xlab = "No Of Bedroooms", 
       ylab = "AverageRent", 
       main = "Average rent  by bedroom according to city", col="black", lty=3)
points(x=HdbdBd$No.Bedrooms,y=HdbdBd$AverageRent,
       type = "o", 
       xlab = "No Of Bedroooms", 
       ylab = "AverageRent", 
       main = "Average rent  by bedroom according to city", col="green", lty=4)
points(x=KlktBd$No.Bedrooms,y=KlktBd$AverageRent,
       type = "o", 
       xlab = "No Of Bedroooms", 
       ylab = "AverageRent", 
       main = "Average rent  by bedroom according to city", col="purple", lty=5)
points(x=MmbBd$No.Bedrooms,y=MmbBd$AverageRent,
       type = "o", 
       xlab = "No Of Bedroooms", 
       ylab = "AverageRent", 
       main = "Average rent  by bedroom according to city", col="orange", lty=6)
legend(1, 80, legend=c("Bangalore", "Chennai", "Delhi", "Hyderabad", "Kolkata", "Mumbai"),
       col=c("pink","purple","goldenrod","blue","salmon","green"), lty=1:6, cex=0.8)

#b) No of bathrooms to average monthly rent 
#1.2.1
#step1: group all the rent by their number of bathrooms and calculate the average
d2<-UpdatesAssigmentDataSet%>%
  group_by(No.Bathrooms)%>%
  summarise(AverageRent=mean(MonthlyRent)) 
d2

#step2: plot the data using  a line graph
ggplot(data=d2, mapping=aes(x=No.Bathrooms, y=AverageRent))+
  geom_line(col="black") + 
  labs(title="Average Montly Rent according to Number of Bathrooms", 
  )+
  theme_bw()

#1.2.2
#----> does this apply to all the states?
#lets compare average rent according to bedroom according to cities
#step 1: 
BglrBd<-filter(UpdatesAssigmentDataSet, CityLocated == "Bangalore")%>%
  group_by(No.Bathrooms)%>%summarise(AverageRent=mean(MonthlyRentInThousand))

ChnBd<-filter(UpdatesAssigmentDataSet, CityLocated == "Chennai")%>%
  group_by(No.Bathrooms)%>%summarise(AverageRent=mean(MonthlyRentInThousand))

DelhBd<-filter(UpdatesAssigmentDataSet, CityLocated == "Delhi")%>%
  group_by(No.Bathrooms)%>%summarise(AverageRent=mean(MonthlyRentInThousand))

HdbdBd<-filter(UpdatesAssigmentDataSet, CityLocated == "Hyderabad")%>%
  group_by(No.Bathrooms)%>%summarise(AverageRent=mean(MonthlyRentInThousand))

KlktBd<-filter(UpdatesAssigmentDataSet, CityLocated == "Kolkata")%>%
  group_by(No.Bathrooms)%>%summarise(AverageRent=mean(MonthlyRentInThousand))

MmbBd<-filter(UpdatesAssigmentDataSet, CityLocated == "Mumbai")%>%
  group_by(No.Bathrooms)%>%summarise(AverageRent=mean(MonthlyRentInThousand))

v<-rbind(BglrBd,ChnBd, DelhBd, HdbdBd, KlktBd, MmbBd)
v

#step2
plot(x=BglrBd$No.Bathrooms,y=BglrBd$AverageRent,
     type = "o", 
     xlab = "No Of Bathrooms", 
     ylab = "Average Rent in Rupees (thousands)", 
     main = "Average rent  by bathroom according to city", 
     col="red",
     xlim=c(1,6),
     ylim=c(0,80))
points(x=ChnBd$No.Bathrooms,y=ChnBd$AverageRent,
       type = "o",
       xlab = "No Of Bathrooms", 
       ylab = "AverageRent", 
       main = "Average rent  by bathroom  according to city", col="blue", lty=2 ) 
points(x=DelhBd$No.Bathrooms,y=DelhBd$AverageRent,
       type = "o",
       xlab = "No Of Bathrooms", 
       ylab = "AverageRent", 
       main = "Average rent  by bathroom  according to city", col="black", lty=3)
points(x=HdbdBd$No.Bathrooms,y=HdbdBd$AverageRent,
       type = "o",
       xlab = "No Of Bathrooms", 
       ylab = "AverageRent", 
       main = "Average rent  by bathroom  according to city", col="green", lty=4)
points(x=KlktBd$No.Bathrooms,y=KlktBd$AverageRent,
       type = "o",
       xlab = "No Of Bathrooms", 
       ylab = "AverageRent", 
       main = "Average rent  by bathroom  according to city", col="purple", lty=5)
points(x=MmbBd$No.Bathrooms,y=MmbBd$AverageRent,
       type = "o",
       xlab = "No Of Bathrooms", 
       ylab = "AverageRent", 
       main = "Average rent  by bathroom  according to city", col="orange", lty=6)
legend(1, 80, legend=c("Bangalore", "Chennai", "Delhi", "Hydrebad", "Kolkata", "Mumbai"),
       col=c("pink","purple","goldenrod","blue","salmon","green"), lty=1:6, cex=0.8)
#====================================================================================
#c)correlation between apartment size and monthly rent?

#1.3.1 is there any general correlation between apartment size and montly rent?
ggplot(data=UpdatesAssigmentDataSet,
       mapping=aes(x = ApartmentSize_Sqft, y = MonthlyRentInThousand))+
  geom_point()+
  ggtitle('Apartment Size vs Monthly Rent(in thousands)')+
  theme_bw()

UpdatesAssigmentDataSet%>%
  select(ApartmentSize_Sqft,MonthlyRentInThousand)%>%
  cor()

#1.3.2
#check each state correlation between apartment size and rent for each city
ggplot(data=UpdatesAssigmentDataSet, 
       mapping=aes(x=ApartmentSize_Sqft, y=MonthlyRentInThousand,color = CityLocated))+
  geom_point()+
  facet_wrap(~CityLocated)+
  theme_bw()+
  ggtitle('Apartment Size vs Monthly Rent(in Thousands) across Cities')

UpdatesAssigmentDataSet%>%
  select(CityLocated,ApartmentSize_Sqft,MonthlyRentInThousand)%>%
  group_by(CityLocated)%>%
  summarize(corrcoef=cor(ApartmentSize_Sqft,MonthlyRentInThousand))

#===================================================================================
#==================================================================
#d)average rent depending on furnishing 
#1.4.1
#step 1: find the average rent according to furnishing
d3<-aggregate(x = UpdatesAssigmentDataSet$MonthlyRentInThousand, 
              by = list(UpdatesAssigmentDataSet$Furnishing), FUN = "mean")
d3

#step 2: plot
ggplot(data=d3, mapping=aes(x=Group.1, y=x, label=round(x,2)))+
  geom_bar(stat="identity", width = 0.5, fill="tomato2",) + 
  labs(title="Average Montly Rent according to furnishing", 
       subtitle="Entire Data Set")+
  geom_label()+
  theme_bw()

#1.4.2 Average rent according to furnishing based on city

e = UpdatesAssigmentDataSet %>% 
  group_by(Cities=CityLocated, Furnishing)  %>%
  summarise(AverageRent=mean(MonthlyRentInThousand))
e

#step 2 :plot
# Create grouped bar plot using ggplot2
ggplot(data=e,mapping=aes(x = Cities, y =AverageRent, fill = Furnishing)) +
  geom_bar(stat = "identity", position = "dodge")+ 
  labs(title="Average Monthly Rent according to furnishing by City")+
  theme_bw()

#1.4.1
#step 1: find the median rent according to furnishing
d3<-aggregate(x = UpdatesAssigmentDataSet$MonthlyRentInThousand, 
              by = list(UpdatesAssigmentDataSet$Furnishing), FUN = "median")
d3

#step 2: plot
ggplot(data=d3, mapping=aes(x=Group.1, y=x, label=round(x,2)))+
  geom_bar(stat="identity", width = 0.5, fill="tomato2",) + 
  labs(title="Median Monthly Rent according to furnishing", 
       subtitle="Entire Data Set")+
  geom_label()+
  theme_bw()

#1.4.2 Median rent according to furnishing based on city

e = UpdatesAssigmentDataSet %>% 
  group_by(Cities=CityLocated, Furnishing)  %>%
  summarise(AverageRent=median(MonthlyRentInThousand))
e

#step 2 :plot
# Create grouped bar plot using ggplot2
ggplot(data=e,mapping=aes(x = Cities, y =AverageRent, fill = Furnishing)) +
  geom_bar(stat = "identity", position = "dodge")+ 
  labs(title="Median Monthly Rent according to furnishing by City")+
  theme_bw()
#=============================================================================
#QUESTION 2: what are the difference monthly rent 
waterfall_colours<-c("Decrease" = "blue", "Increase" = "orange")
#Difference between number of bedrooms in general
d1_1<-UpdatesAssigmentDataSet%>%
  group_by(No.Bedrooms)%>%
  summarise(AverageRent=mean(MonthlyRentInThousand))%>%
  mutate(roundedRent=round(AverageRent-lag(AverageRent),2))%>%
  replace_na(list(roundedRent=0))%>%
  select(No.Bedrooms,roundedRent)
d1_1


#lag will take in step backwards
d1_2<-d1_1%>%
  mutate(No.Bedrooms = factor(No.Bedrooms),
         ymin = round(cumsum(roundedRent), 3),
         ymax = lag(cumsum(roundedRent), default = 0),
         xmin = c(head(No.Bedrooms, -1), NA),
         xmax = c(tail(No.Bedrooms, -1), NA),
         Impact = ifelse(roundedRent > 0, "Increase", "Decrease")
  )
w <- 0.4
ggplot(d1_2) +
  theme_bw()+
  theme(legend.position = "right", panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(y = "Monthly Rent in thousands", 
       x = "Number of Bedrooms", 
       title = "Change in Rent with number of bedrooms")+
  geom_rect(aes(xmin = as.integer(No.Bedrooms) - w/2,
                xmax = as.integer(No.Bedrooms) + w/2, 
                ymin = ymin, ymax = ymax,
                fill = Impact), colour = "black") +
  scale_fill_manual(values = waterfall_colours)+
  geom_segment(data = d1_2[1:(nrow(d1_2) -1),],
               aes(x = xmin,xend = xmax,y = ymin,yend = ymin))+
  coord_cartesian(ylim=c(4, max(d1_2$ymax)+.025)) 


#for every city
BglrBd<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Bangalore")%>%
  group_by(No.Bedrooms)%>%
  summarise(AverageRent=mean(MonthlyRentInThousand))%>%
  mutate(roundedRent=round(AverageRent-lag(AverageRent),2))%>%
  replace_na(list(roundedRent=0))
BglrBd
d1_2<-BglrBd%>%
  mutate(No.Bedrooms = factor(No.Bedrooms),
         ymin = round(cumsum(roundedRent), 3),
         ymax = lag(cumsum(roundedRent), default = 0),
         xmin = c(head(No.Bedrooms, -1), NA),
         xmax = c(tail(No.Bedrooms, -1), NA),
         Impact = ifelse(roundedRent > 0, "Increase", "Decrease")
  )
w <- 0.4
ggplot(d1_2) +
  theme_bw()+
  theme(legend.position = "right", panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(y = "Monthly Rent in thousands", 
       x = "Number of Bedrooms", 
       title = "Change in Rent with number of bedrooms in Bnagalore")+
  geom_rect(aes(xmin = as.integer(No.Bedrooms) - w/2,
                xmax = as.integer(No.Bedrooms) + w/2, 
                ymin = ymin, ymax = ymax,
                fill = Impact), colour = "black") +
  scale_fill_manual(values = waterfall_colours)+
  geom_segment(data = d1_2[1:(nrow(d1_2) -1),],
               aes(x = xmin,xend = xmax,y = ymin,yend = ymin))+
  coord_cartesian(ylim=c(4, max(d1_2$ymax)+.025)) 


ChnBd<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Chennai")%>%
  group_by(No.Bedrooms)%>%
  summarise(AverageRent=mean(MonthlyRentInThousand))%>%
  mutate(roundedRent=round(AverageRent-lag(AverageRent),2))%>%
  replace_na(list(roundedRent=0))
ChnBd
d1_2<-ChnBd%>%
  mutate(No.Bedrooms = factor(No.Bedrooms),
         ymin = round(cumsum(roundedRent), 3),
         ymax = lag(cumsum(roundedRent), default = 0),
         xmin = c(head(No.Bedrooms, -1), NA),
         xmax = c(tail(No.Bedrooms, -1), NA),
         Impact = ifelse(roundedRent > 0, "Increase", "Decrease")
  )
w <- 0.4
ggplot(d1_2) +
  theme_bw()+
  theme(legend.position = "right", panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(y = "Monthly Rent in thousands", 
       x = "Number of Bedrooms", 
       title = "Change in Rent with number of bedrooms Chennai")+
  geom_rect(aes(xmin = as.integer(No.Bedrooms) - w/2,
                xmax = as.integer(No.Bedrooms) + w/2, 
                ymin = ymin, ymax = ymax,
                fill = Impact), colour = "black") +
  scale_fill_manual(values = waterfall_colours)+
  geom_segment(data = d1_2[1:(nrow(d1_2) -1),],
               aes(x = xmin,xend = xmax,y = ymin,yend = ymin))+
  coord_cartesian(ylim=c(4, max(d1_2$ymax)+.025)) 

DelhBd<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Delhi")%>%
  group_by(No.Bedrooms)%>%
  summarise(AverageRent=mean(MonthlyRentInThousand))%>%
  mutate(roundedRent=round(AverageRent-lag(AverageRent),2))%>%
  replace_na(list(roundedRent=0))
DelhBd
d1_2<-DelhBd%>%
  mutate(No.Bedrooms = factor(No.Bedrooms),
         ymin = round(cumsum(roundedRent), 3),
         ymax = lag(cumsum(roundedRent), default = 0),
         xmin = c(head(No.Bedrooms, -1), NA),
         xmax = c(tail(No.Bedrooms, -1), NA),
         Impact = ifelse(roundedRent > 0, "Increase", "Decrease")
  )
w <- 0.4
ggplot(d1_2) +
  theme_bw()+
  theme(legend.position = "right", panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(y = "Monthly Rent in thousands", 
       x = "Number of Bedrooms", 
       title = "Change in Rent with number of bedrooms Delhi")+
  geom_rect(aes(xmin = as.integer(No.Bedrooms) - w/2,
                xmax = as.integer(No.Bedrooms) + w/2, 
                ymin = ymin, ymax = ymax,
                fill = Impact), colour = "black") +
  scale_fill_manual(values =waterfall_colours)+
  geom_segment(data = d1_2[1:(nrow(d1_2) -1),],
               aes(x = xmin,xend = xmax,y = ymin,yend = ymin))+
  coord_cartesian(ylim=c(4, max(d1_2$ymax)+.025)) 

HdbdBd<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Hyderabad")%>%
  group_by(No.Bedrooms)%>%
  summarise(AverageRent=mean(MonthlyRentInThousand))%>%
  mutate(roundedRent=round(AverageRent-lag(AverageRent),2))%>%
  replace_na(list(roundedRent=0))
HdbdBd
d1_2<-HdbdBd%>%
  mutate(No.Bedrooms = factor(No.Bedrooms),
         ymin = round(cumsum(roundedRent), 3),
         ymax = lag(cumsum(roundedRent), default = 0),
         xmin = c(head(No.Bedrooms, -1), NA),
         xmax = c(tail(No.Bedrooms, -1), NA),
         Impact = ifelse(roundedRent > 0, "Increase", "Decrease")
  )

w <- 0.4
ggplot(d1_2) +
  theme_bw()+
  theme(legend.position = "right", panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(y = "Monthly Rent in thousands", 
       x = "Number of Bedrooms", 
       title = "Change in Rent with number of bedrooms for Hyderabad")+
  geom_rect(aes(xmin = as.integer(No.Bedrooms) - w/2,
                xmax = as.integer(No.Bedrooms) + w/2, 
                ymin = ymin, ymax = ymax,
                fill = Impact), colour = "black") +
  scale_fill_manual(values =waterfall_colours)+
  geom_segment(data = d1_2[1:(nrow(d1_2) -1),],
               aes(x = xmin,xend = xmax,y = ymin,yend = ymin))+
  coord_cartesian(ylim=c(4, max(d1_2$ymax)+.025)) 

KlktBd<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Kolkata")%>%
  group_by(No.Bedrooms)%>%
  summarise(AverageRent=mean(MonthlyRentInThousand))%>%
  mutate(roundedRent=round(AverageRent-lag(AverageRent),2))%>%
  replace_na(list(roundedRent=0))
KlktBd
d1_2<-KlktBd%>%
  mutate(No.Bedrooms = factor(No.Bedrooms),
         ymin = round(cumsum(roundedRent), 3),
         ymax = lag(cumsum(roundedRent), default = 0),
         xmin = c(head(No.Bedrooms, -1), NA),
         xmax = c(tail(No.Bedrooms, -1), NA),
         Impact = ifelse(roundedRent > 0, "Increase", "Decrease")
  )
w <- 0.4
ggplot(d1_2) +
  theme_bw()+
  theme(legend.position = "right", panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(y = "Monthly Rent in thousands", 
       x = "Number of Bedrooms", 
       title = "Change in Rent with number of bedrooms Kolkata")+
  geom_rect(aes(xmin = as.integer(No.Bedrooms) - w/2,
                xmax = as.integer(No.Bedrooms) + w/2, 
                ymin = ymin, ymax = ymax,
                fill = Impact), colour = "black") +
  scale_fill_manual(values = waterfall_colours)+
  geom_segment(data = d1_2[1:(nrow(d1_2) -1),],
               aes(x = xmin,xend = xmax,y = ymin,yend = ymin))+
  coord_cartesian(ylim=c(4, max(d1_2$ymax)+.025)) 

MmbBd<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Mumbai")%>%
  group_by(No.Bedrooms)%>%
  summarise(AverageRent=mean(MonthlyRentInThousand))%>%
  mutate(roundedRent=round(AverageRent-lag(AverageRent),2))%>%
  replace_na(list(roundedRent=0))
MmbBd
d1_2<-MmbBd%>%
  mutate(No.Bedrooms = factor(No.Bedrooms),
         ymin = round(cumsum(roundedRent), 3),
         ymax = lag(cumsum(roundedRent), default = 0),
         xmin = c(head(No.Bedrooms, -1), NA),
         xmax = c(tail(No.Bedrooms, -1), NA),
         Impact = ifelse(roundedRent > 0, "Increase", "Decrease")
  )
w <- 0.4
ggplot(d1_2) +
  theme_bw()+
  theme(legend.position = "right", panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(y = "Monthly Rent in thousands", 
       x = "Number of Bedrooms", 
       title = "Change in Rent with number of bedrooms in Mumbai")+
  geom_rect(aes(xmin = as.integer(No.Bedrooms) - w/2,
                xmax = as.integer(No.Bedrooms) + w/2, 
                ymin = ymin, ymax = ymax,
                fill = Impact), colour = "black") +
  scale_fill_manual(values = waterfall_colours)+
  geom_segment(data = d1_2[1:(nrow(d1_2) -1),],
               aes(x = xmin,xend = xmax,y = ymin,yend = ymin))+
  coord_cartesian(ylim=c(4, max(d1_2$ymax)+.025))   
#=============================================================================================
#1)WHAT ARE THEMOST EXPENSIVE RENT PER SQUARE FOOT
averageRentpersquarefoot<-aggregate(x = UpdatesAssigmentDataSet$rentpersquarefeet, by = list(UpdatesAssigmentDataSet$CityLocated), FUN = "mean")
averageRentpersquarefoot
#entering the data into a bar graph and arrangin in Ascending order
p1<-ggplot(averageRentpersquarefoot, aes(x= reorder(Group.1,x), y=x),) + 
  geom_bar(stat="identity", colour="black",fill='plum') + 
  xlab("City") + ylab("Average Rent per Squarefoot (in rupees)")+
  labs(title="Average Rent Per Squarefoot according to City")+
  theme_bw()

#1)WHAT ARE THEMOST EXPENSIVE RENT PER SQUARE FOOT
medianRentpersquarefoot<-aggregate(x = UpdatesAssigmentDataSet$rentpersquarefeet, by = list(UpdatesAssigmentDataSet$CityLocated), FUN = "median")
medianRentpersquarefoot
#entering the data into a bar graph and arrangin in Ascending order
p2<-ggplot(medianRentpersquarefoot, aes(x= reorder(Group.1,x), y=x),) + 
  geom_bar(stat="identity", colour="black",fill='plum') + 
  xlab("City") + ylab("Median Rent per Squarefoot (in rupees)")+
  labs(title="Median Rent Per Squarefoot according to City")+
  theme_bw()
ggarrange(p1,p2,ncol=2,nrow=1)
#=============================================================================================
#============================================================================================= 
# QUESTION 3:  How do the number of bedrooms and bathrooms vary?

#analysis 1.1: How do bedrooms vary according to the type of Property Area Type?
BedroomsOfPropertyArea <- UpdatesAssigmentDataSet %>%
  group_by(PropertAreaType, No.Bedrooms)  %>%
  summarise(Tenantcount= n())
BedroomsOfPropertyArea

ggplot(data=BedroomsOfPropertyArea,
       mapping=aes(x =No.Bedrooms,y = Tenantcount,
                   label = Tenantcount, group=PropertAreaType)) +
  geom_bar(stat = "identity", position = "dodge",fill='orchid')+ 
  geom_label()+
  labs(title="Number of bedrooms by Property Area Type")+
  facet_wrap(~ PropertAreaType)+
  xlab('Number of Bedrooms')+
  theme_bw()

#analysis 1.2: by city
#Bangalore
BdPBglr<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Bangalore")%>%
  group_by(PropertAreaType, No.Bedrooms)%>%
  summarise(Tenantcount= n())
BdPBglr

ggplot(data=BdPBglr,
       mapping=aes(x =No.Bedrooms , y = Tenantcount,
                   label = Tenantcount,group=PropertAreaType)) +
  geom_bar(stat = "identity", position = "dodge",fill='orchid')+ 
  geom_label()+
  labs(title="Number of bedrooms by Property Area Type in the city of Bangalore")+
  facet_wrap(~ PropertAreaType)+theme_bw()

#Chennai
BdPChnr<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Chennai")%>%
  group_by(PropertAreaType, No.Bedrooms)%>%
  summarise(Tenantcount= n())
BdPChnr

ggplot(data=BdPChnr,mapping=aes(x =No.Bedrooms , y = Tenantcount,
                                label = Tenantcount,group=PropertAreaType)) +
  geom_bar(stat = "identity", position = "dodge",fill='orchid')+ 
  geom_label()+
  labs(title="Number of bedrooms by Property Area Type in the city of Chennai")+
  facet_wrap(~ PropertAreaType)+theme_bw()

#Delhi
BdPDhl<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Delhi") %>%
  group_by(PropertAreaType, No.Bedrooms)%>%
  summarise(Tenantcount= n())
BdPDhl

ggplot(data=BdPDhl,
       mapping=aes(x =No.Bedrooms , y = Tenantcount,label = Tenantcount)) +
  geom_bar(stat = "identity", position = "dodge",fill='orchid')+ 
  geom_label()+
  labs(title="Number of bedrooms by Property Area Type in the city of Delhi")+
  facet_wrap(~ PropertAreaType)+theme_bw()

#Hyderabad
BdPHdbd<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Hyderabad") %>%
  group_by(PropertAreaType, No.Bedrooms)%>%
  summarise(Tenantcount= n())
BdPHdbd

ggplot(data=BdPDhl,
       mapping=aes(x =No.Bedrooms , y = Tenantcount,label = Tenantcount)) +
  geom_bar(stat = "identity", position = "dodge",fill='orchid')+ 
  geom_label()+
  labs(title="Number of bedrooms by Property Area Type in the city of DHyderabad")+
  facet_wrap(~ PropertAreaType)+theme_bw()

#Kolkata
BdPKlkt<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Kolkata") %>%
  group_by(PropertAreaType, No.Bedrooms)%>%
  summarise(Tenantcount= n())
BdPKlkt

ggplot(data=BdPKlkt,
       mapping=aes(x =No.Bedrooms , y = Tenantcount,label = Tenantcount)) +
  geom_bar(stat = "identity", position = "dodge",fill='orchid')+ 
  geom_label()+
  labs(title="Number of bedrooms by Property Area Type in the city of Kolkata")+
  facet_wrap(~ PropertAreaType)+theme_bw()

#Mumbai
BtPMmb<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Mumbai") %>%
  group_by(PropertAreaType, No.Bedrooms)%>%
  summarise(Tenantcount= n())
BtPMmb

ggplot(data=BtPMmb,
       mapping=aes(x =No.Bedrooms , y = Tenantcount,label = Tenantcount)) +
  geom_bar(stat = "identity", position = "dodge",fill='orchid')+ 
  geom_label()+
  labs(title="Number of bedrooms by Property Area Type in the city of Mumbai")+
  facet_wrap(~ PropertAreaType)+theme_bw()
#===================================================================================

#Analysis 2.1: How do bathrooms vary according to the type of Property Area Type?
BathroomstoPropertyArea = UpdatesAssigmentDataSet %>%
  group_by(PropertyAreaType, No.Bathrooms)  %>%
  summarise(Tenantcount= n())
BathroomstoPropertyArea

ggplot(data=BathroomstoPropertyArea,
       mapping=aes(x =No.Bathrooms , y = Tenantcount,
                   label = Tenantcount,group=PropertyAreaType, )) +
  geom_bar(stat = "identity", position = "dodge", fill="salmon")+ 
  geom_label()+
  scale_fill_manual(values = Property_color)+
  labs(title="Number of bathrooms by Property Area Type")+
  facet_wrap(~PropertyAreaType)+theme_bw()

#analysis 2.2: by city
#Bangalore
BtPBglr<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Bangalore") %>%
  group_by(PropertyAreaType, No.Bathrooms)   %>%
  summarise(Tenantcount= n())
BtPBglr

ggplot(data=BtPBglr,
       mapping=aes(x =No.Bathrooms, y = Tenantcount,
                   label = Tenantcount,group=PropertyAreaType)) +
  geom_bar(stat = "identity", position = "dodge",fill='salmon')+ 
  geom_label()+
  labs(title="Number of bathrooms by Property Area Type in the city of Bangalore")+
  facet_wrap(~ PropertyAreaType)+theme_bw()

#Chennai
BtPChn<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Chennai") %>%
  group_by(PropertyAreaType, No.Bathrooms)   %>%
  summarise(Tenantcount= n())
BtPChn

ggplot(data=BtPChn,
       mapping=aes(x =No.Bathrooms , y = Tenantcount,
                   label = Tenantcount,group=PropertyAreaType)) +
  geom_bar(stat = "identity", position = "dodge",fill='salmon')+ 
  geom_label()+
  labs(title="Number of bathrooms by Property Area Type in the city of Chennai")+
  facet_wrap(~ PropertyAreaType)+theme_bw()


#Delhi
Btdhl<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Delhi") %>%
  group_by(PropertyAreaType, No.Bathrooms)   %>%
  summarise(Tenantcount= n())
Btdhl

ggplot(data=BtPHbd,
       mapping=aes(x =No.Bathrooms , y = Tenantcount,
                   label = Tenantcount,group=PropertyAreaType)) +
  geom_bar(stat = "identity", position = "dodge",fill='salmon')+ 
  geom_label()+
  labs(title="Number of bathrooms by Property Area Type in the city of Delhi")+
  facet_wrap(~ PropertyAreaType)+theme_bw()


#Hyderabad
BtPHbd<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Hyderabad") %>%
  group_by(PropertyAreaType, No.Bathrooms)   %>%
  summarise(Tenantcount= n())
BtPHbd

ggplot(data=BtPHbd,
       mapping=aes(x =No.Bathrooms , y = Tenantcount,
                   label = Tenantcount,group=PropertyAreaType)) +
  geom_bar(stat = "identity", position = "dodge",fill='salmon')+ 
  geom_label()+
  labs(title="Number of bathrooms by Property Area Type in the city of Hyderabad")+
  facet_wrap(~ PropertyAreaType)+theme_bw()

#Kolkata
BtPKlkt<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Kolkata")%>%
  group_by(PropertyAreaType, No.Bathrooms)   %>%
  summarise(Tenantcount= n())
BtPKlkt

ggplot(data=BtPKlkt,
       mapping=aes(x =No.Bathrooms , y = Tenantcount, 
                   label = Tenantcount,group=PropertyAreaType)) +
  geom_bar(stat = "identity", position = "dodge",fill='salmon')+ 
  geom_label()+
  labs(title="Number of bathrooms by Property Area Type in the city of Kolkata")+
  facet_wrap(~ PropertyAreaType)+theme_bw()

#Mumbai
BtPMmb<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Mumbai") %>%
  group_by(PropertyAreaType, No.Bathrooms)   %>%
  summarise(Tenantcount= n())
BtPMmb

ggplot(data=BtPMmb,
       mapping=aes(x =No.Bathrooms , y = Tenantcount,
                   label = Tenantcount,group=PropertyAreaType)) +
  geom_bar(stat = "identity", position = "dodge",fill='salmon')+ 
  geom_label()+
  labs(title="Number of bathrooms by Property Area Type in the city of Mumbai")+
  facet_wrap(~ PropertyAreaType)+
  theme_bw()
#=================================================================================
#Analysis 3.1: How do bedrooms vary across tenant types?
BedroomsToTenant<-UpdatesAssigmentDataSet %>%
  group_by(No.Bedrooms,PreferenceOfTenants)  %>%
  summarise(Tenantcount= n())
BedroomsToTenant

ggplot(data=BedroomsToTenant,
       mapping=aes(x =No.Bedrooms , y = Tenantcount, 
                   label = Tenantcount,group=PreferenceOfTenants,fill=PreferenceOfTenants)) +
  geom_bar(stat = "identity", position = "dodge")+ 
  geom_label()+
  labs(title="Number of bedrooms to Tenant Preference")+
  facet_wrap(~PreferenceOfTenants ) +theme_bw()

#Analysis3.2: by city
#Bangalore
BtPBglr<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Bangalore") %>%
  group_by(PreferenceOfTenants, No.Bathrooms)   %>%
  summarise(Tenantcount= n())
BtPBglr

ggplot(data=BtPBglr,
       mapping=aes(x =No.Bathrooms , y = Tenantcount,
                   label = Tenantcount, group=PreferenceOfTenants,fill=PreferenceOfTenants)) +
  geom_bar(stat = "identity", position = "dodge")+ 
  geom_label()+
  labs(title="Number of bathrooms by Property Area Type in the city of Bangalore")+
  facet_wrap(~ PreferenceOfTenants)+theme_bw()

#Delhi
BtDelhi<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Delhi")%>%
  group_by(PreferenceOfTenants, No.Bathrooms)%>%
  summarise(Tenantcount= n())
BtDelhi

ggplot(BtPHbd,aes(x =No.Bathrooms , y = Tenantcount,
                  label = Tenantcount,group=PreferenceOfTenants,fill=PreferenceOfTenants)) +
  geom_bar(stat = "identity", position = "dodge")+ 
  geom_label()+
  labs(title="Number of bathrooms by PreferenceOfTenants in the city of Delhi")+
  facet_wrap(~ PreferenceOfTenants)+theme_bw()


#Hyderabad
BtPHbd<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Hyderabad")%>%
  group_by(PreferenceOfTenants, No.Bathrooms)%>%
  summarise(Tenantcount= n())
BtPHbd

ggplot(BtPHbd,aes(x =No.Bathrooms , y = Tenantcount,
                  label = Tenantcount,group=PreferenceOfTenants,fill=PreferenceOfTenants)) +
  geom_bar(stat = "identity", position = "dodge")+ 
  geom_label()+
  labs(title="Number of bathrooms by PreferenceOfTenants in the city of Hyderabad")+
  facet_wrap(~ PreferenceOfTenants)+theme_bw()

#Kolkata
BtPKlkt<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Kolkata")%>%
  group_by(PreferenceOfTenants, No.Bathrooms)%>%
  summarise(Tenantcount= n())
BtPKlkt

ggplot(data=BtPKlkt,
       mapping=aes(x =No.Bathrooms , y = Tenantcount,
                   label = Tenantcount,group=PreferenceOfTenants,fill=PreferenceOfTenants)) +
  geom_bar(stat = "identity", position = "dodge")+ 
  geom_label()+
  labs(title="Number of bathrooms by PreferenceOfTenants in the city of Kolkata")+
  facet_wrap(~ PreferenceOfTenants)+theme_bw()

#Mumbai
BtPMmb<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Mumbai") %>%
  group_by(PreferenceOfTenants, No.Bathrooms)%>%
  summarise(Tenantcount= n())
BtPMmb

ggplot(data=BtPMmb,
       mapping=aes(x =No.Bathrooms , y = Tenantcount,
                   label = Tenantcount,group=PreferenceOfTenants,fill=PreferenceOfTenants)) +
  geom_bar(stat = "identity", position = "dodge")+ 
  geom_label()+
  labs(title="Number of bathrooms by Property Area Type in the city of Mumbai")+
  facet_wrap(~ PreferenceOfTenants)+theme_bw()

#=============================================================================
#Analysis 4.1: How do bathrooms vary across tenant types?

BathroomsToTenant <- UpdatesAssigmentDataSet%>%
  group_by(No.Bathrooms,PreferenceOfTenants)%>%
  summarise(Tenantcount= n())
BedroomsToTenant

ggplot(data=BathroomsToTenant,
       mapping=aes(x =No.Bathrooms , y = Tenantcount,
                   label = Tenantcount,group=PreferenceOfTenants,fill=PreferenceOfTenants)) +
  geom_bar(stat = "identity", position = "dodge")+ 
  geom_label()+
  labs(title="Number of bathrooms by Tenant Preference")+
  facet_wrap(~PreferenceOfTenants )+theme_bw()

#analysis 4.2: by city
#Bangalore
BtPBglr<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Bangalore")%>% 
  group_by(No.Bathrooms,PreferenceOfTenants)%>%
  summarise(Tenantcount= n())
BtPBglr

ggplot(data=BtPBglr,
       mapping=aes(x =No.Bathrooms , y = Tenantcount,
                   label = Tenantcount,group=PreferenceOfTenants,fill=PreferenceOfTenants)) +
  geom_bar(stat = "identity", position = "dodge")+ 
  geom_label()+
  labs(title="Number of bathrooms by Tenant Preference in the city of Bangalore")+
  facet_wrap(~PreferenceOfTenants )+theme_bw()

#Chennai
BtPChn<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Chennai")%>%
  group_by(No.Bathrooms,PreferenceOfTenants)%>%
  summarise(Tenantcount= n())
BtPChn

ggplot(data=BtPChn,
       mapping=aes(x=No.Bathrooms , y = Tenantcount,
                   label = Tenantcount,group=PreferenceOfTenants,fill=PreferenceOfTenants)) +
  geom_bar(stat = "identity", position = "dodge")+ 
  geom_label()+
  labs(title="Number of bathrooms by Tenant Preference in the city of Chennai")+
  facet_wrap(~PreferenceOfTenants )+theme_bw()

#Delhi
BtPDhl<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Delhi")%>%
  group_by(No.Bathrooms,PreferenceOfTenants)%>%
  summarise(Tenantcount= n())
BtPDhl

ggplot(data=BtPDhl,
       mapping=aes(x=No.Bathrooms , y = Tenantcount,
                   label = Tenantcount,group=PreferenceOfTenants,fill=PreferenceOfTenants)) +
  geom_bar(stat = "identity", position = "dodge")+ 
  geom_label()+
  labs(title="Number of bathrooms by Tenant Preference in the city of delhi")+
  facet_wrap(~PreferenceOfTenants )+theme_bw()

#Hyderabad
BtPHbd<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Hyderabad")%>%
  group_by(No.Bathrooms,PreferenceOfTenants)%>%
  summarise(Tenantcount= n())
BtPHbd

ggplot(data=BtPHbd,
       mapping=aes(x =No.Bathrooms , y = Tenantcount,
                   label = Tenantcount,group=PreferenceOfTenants,fill=PreferenceOfTenants)) +
  geom_bar(stat = "identity", position = "dodge")+ 
  geom_label()+
  labs(title="Number of bathrooms by Tenant Preference in the city of Hyderabad")+
  facet_wrap(~PreferenceOfTenants )+theme_bw()

#Kolkota
BtPKlkt<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Kolkata")%>%
  group_by(No.Bathrooms,PreferenceOfTenants)%>%
  summarise(Tenantcount= n())
BtPKlkt

ggplot(data=BtPKlkt,
       mapping=aes(x =No.Bathrooms , y = Tenantcount,
                   label = Tenantcount,group=PreferenceOfTenants,fill=PreferenceOfTenants)) +
  geom_bar(stat = "identity", position = "dodge")+ 
  geom_label()+
  labs(title="Number of bathrooms by Tenant Preference in the city of Kolkata")+
  facet_wrap(~PreferenceOfTenants )+theme_bw()

#Mumbai
BtPMmb<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Mumbai") %>%
  group_by(No.Bathrooms,PreferenceOfTenants)%>%
  summarise(Tenantcount= n())
BtPMmb

ggplot(data=BtPMmb,
       mapping=aes(x =No.Bathrooms , y = Tenantcount,
                   label = Tenantcount,group=PreferenceOfTenants,fill=PreferenceOfTenants)) +
  geom_bar(stat = "identity", position = "dodge")+ 
  geom_label()+
  labs(title="Number of bathrooms by Tenant Preference in the city of Mumbai")+
  facet_wrap(~PreferenceOfTenants )+theme_bw()

#=================================================================================
#Analysis  5.1: How do bedrooms vary across furnishing type
BedroomsToFurnishing<- UpdatesAssigmentDataSet %>%
  group_by(Furnishing, No.Bedrooms) %>%
  summarise(Tenantcount= n())
BedroomsToFurnishing

ggplot(data=BedroomsToFurnishing,
       mapping=aes(x =No.Bedrooms , y = Tenantcount, 
                   label = Tenantcount,group=Furnishing,fill=Furnishing)) +
  geom_bar(stat = "identity", position = "dodge")+ 
  geom_label()+
  labs(title="Number of bedrooms by Furnishing Type")+
  facet_wrap(~Furnishing )+theme_bw()

#Analysis5.2: by city
#Bangalore
BtPBglr<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Bangalore")%>%
  group_by(Furnishing, No.Bedrooms)%>%
  summarise(Tenantcount= n())
BtPBglr

ggplot(data=BtPBglr,
       mapping=aes(x =No.Bedrooms , y = Tenantcount,
                   label = Tenantcount,group=Furnishing,fill=Furnishing)) +
  geom_bar(stat = "identity", position = "dodge")+ 
  geom_label()+
  labs(title="Number of bedrooms by Furnishing Type in the city of Bangalore")+
  facet_wrap(~ Furnishing)+theme_bw()

#Chennai
BtPChn<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Chennai")%>%
  group_by(Furnishing, No.Bedrooms)%>%
  summarise(Tenantcount= n())
BtPChn

ggplot(data=BtPChn,
       mapping=aes(x =No.Bedrooms , y = Tenantcount,
                   label = Tenantcount,group=Furnishing,fill=Furnishing)) +
  geom_bar(stat = "identity", position = "dodge")+ 
  geom_label()+
  labs(title="Number of bedroomss by Furnishing Type in the city of Chennai")+
  facet_wrap(~ Furnishing)+theme_bw()


#Delhi
BtPDlh<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Delhi")%>%
  group_by(Furnishing, No.Bedrooms)%>%
  summarise(Tenantcount= n())
BtPDlh

ggplot(data=BtPDlh,
       mapping=aes(x =No.Bedrooms , y = Tenantcount,
                   label = Tenantcount,group=Furnishing,fill=Furnishing)) +
  geom_bar(stat = "identity", position = "dodge")+ 
  geom_label()+
  labs(title="Number of bedrooms by Furnishing Type in the city of Delhi")+
  facet_wrap(~ Furnishing)+theme_bw()

#Hyderabad
BtPHbd<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Hyderabad")%>%
  group_by(Furnishing, No.Bedrooms)%>%
  summarise(Tenantcount= n())
BtPHbd

ggplot(data=BtPHbd,
       mapping=aes(x =No.Bedrooms , y = Tenantcount,
                   label = Tenantcount,group=Furnishing,fill=Furnishing)) +
  geom_bar(stat = "identity", position = "dodge")+ 
  geom_label()+
  labs(title="Number of bedroomss by Furnishing Type in the city of Hyderabad")+
  facet_wrap(~ Furnishing)+theme_bw()

#Kolkata
BtPKlkt<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Kolkata")%>%
  group_by(Furnishing, No.Bedrooms)%>%
  summarise(Tenantcount= n())
BtPKlkt

ggplot(data=BtPKlkt,
       mapping=aes(x =No.Bedrooms , y = Tenantcount,
                   label = Tenantcount,group=Furnishing,fill=Furnishing)) +
  geom_bar(stat = "identity", position = "dodge")+ 
  geom_label()+
  labs(title="Number of bedrooms by Furnishing Type in the city of Kolkata")+
  facet_wrap(~ Furnishing)+theme_bw()

#Mumbai
BtPMmb<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Mumbai")%>%
  group_by(Furnishing, No.Bedrooms)%>%
  summarise(Tenantcount= n())
BtPMmb

ggplot(data=BtPMmb,
       mapping=aes(x =No.Bedrooms , y = Tenantcount, 
                   label = Tenantcount,group=Furnishing,fill=Furnishing)) +
  geom_bar(stat = "identity", position = "dodge")+ 
  geom_label()+
  labs(title="Number of bedrooms by Furnishing Type in the city of Mumbai")+
  facet_wrap(~ Furnishing)+theme_bw()
#=========================================================
#QUESTION 5: QUESTIION ON CITY LOCATION

#highest average rent accodrding to city

#1)Bangalore
Bangalore<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Bangalore")%>%
  group_by(AreaLocation)%>%
  summarise(count=n(),RentPerLocation=mean(MonthlyRentInThousand))%>%
  arrange(desc(count))%>%
  head(10)
Bangalore
pal<-c('Average Rent Per Location'='pink','Number of lisiting'='blue')

ggplot(data = Bangalore)+
  geom_col(mapping=aes(x=reorder(AreaLocation,RentPerLocation), y=RentPerLocation,fill='Average Rent Per Location'),
           color="black")+
  geom_col(mapping=aes(x=reorder(AreaLocation,RentPerLocation), y=count,fill='Number of lisiting'),
           alpha=0.3,width=0.3)+
  scale_fill_manual(values=pal)+
  labs(legend=fill)+
  coord_flip()+
  ylab("Rent per location ( in thousands) / count of listings")+
  xlab("Areas in the city of Bangalore")+
  labs(title="Average Montly Rent according to furnishing by Area type")

#2)Chennai
Chennai<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Chennai")%>%
  group_by(AreaLocation)%>%
  summarise(count=n(),RentPerLocation=mean(MonthlyRentInThousand))%>%
  arrange(desc(count))%>%
  head(10)
Chennai

ggplot(data = Chennai)+
  geom_col(mapping=aes(x=reorder(AreaLocation,RentPerLocation), y=RentPerLocation,fill='Average Rent Per Location'),
           color="black")+
  geom_col(mapping=aes(x=reorder(AreaLocation,RentPerLocation), y=count,fill='Number of lisiting'),
           alpha=0.3,width=0.3)+
  scale_fill_manual(values=pal)+
  labs(legend=fill)+
  coord_flip()+
  ylab("Rent per location ( in thousands) / count of listings")+
  xlab("Areas in the city of Chennai")+
  labs(title="Average Montly Rent according to furnishing by Area type")

#3)Delhi
Delhi<-UpdatesAssigmentDataSet%>%filter(CityLocated == "Delhi")%>%
  group_by(AreaLocation)%>%
  summarise(count=n(),RentPerLocation=mean(MonthlyRentInThousand))%>%
  arrange(desc(count))%>%
  head(10)
Delhi

ggplot(data = Delhi)+
  geom_col(mapping=aes(x=reorder(AreaLocation,RentPerLocation), y=RentPerLocation,fill='Average Rent Per Location'),
           color="black")+
  geom_col(mapping=aes(x=reorder(AreaLocation,RentPerLocation), y=count,fill='Number of lisiting'),
           alpha=0.3,width=0.3)+
  scale_fill_manual(values=pal)+
  labs(legend=fill)+
  coord_flip()+
  ylab("Rent per location ( in thousands) / count of listings")+
  xlab("Areas in the city of Delhi")+
  labs(title="Average Montly Rent according to furnishing by Area type")

#4)Hyderabad
Hyderabad<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Hyderabad")%>%
  group_by(AreaLocation)%>%
  summarise(count=n(),RentPerLocation=mean(MonthlyRentInThousand))%>%
  arrange(desc(count))%>%
  head(10)
Hyderabad

ggplot(data = Hyderabad)+
  geom_col(mapping=aes(x=reorder(AreaLocation,RentPerLocation), y=RentPerLocation,fill='Average Rent Per Location'),
           color="black")+
  geom_col(mapping=aes(x=reorder(AreaLocation,RentPerLocation), y=count,fill='Number of lisiting'),
           alpha=0.3,width=0.3)+
  scale_fill_manual(values=pal)+
  labs(legend=fill)+
  coord_flip()+
  ylab("Rent per location ( in thousands) / count of listings")+
  xlab("Areas in the city of Hyderabad")+
  labs(title="Average Montly Rent according to furnishing by Area type")

#5)Kolkata
Kolkata<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Kolkata")%>%
  group_by(AreaLocation)%>%
  summarise(count=n(),RentPerLocation=mean(MonthlyRentInThousand))%>%
  arrange(desc(count))%>%
  head(10)
Kolkata

ggplot(data = Kolkata)+
  geom_col(mapping=aes(x=reorder(AreaLocation,RentPerLocation), y=RentPerLocation,fill='Average Rent Per Location'),
           fill="pink")+
  geom_col(mapping=aes(x=reorder(AreaLocation,RentPerLocation), y=count,fill='Number of lisiting'),
           alpha=0.3,width=0.3)+
  scale_fill_manual(values=pal)+
  labs(legend=fill)+
  coord_flip()+
  ylab("Rent per location ( in thousands) / count of listings")+
  xlab("Areas in the city of Kolkata")+
  labs(title="Average Montly Rent according to furnishing by Area type")

#6)Mumbai
Mumbai<-UpdatesAssigmentDataSet%>%
  filter(CityLocated == "Mumbai")%>%
  group_by(AreaLocation)%>%
  summarise(count=n(),RentPerLocation=mean(MonthlyRentInThousand))%>%
  arrange(desc(count))%>%
  head(10)
Mumbai

ggplot(data = Mumbai)+
  geom_col(mapping=aes(x=reorder(AreaLocation,RentPerLocation), y=RentPerLocation,fill='Average Rent Per Location'),
           color="black")+
  geom_col(mapping=aes(x=reorder(AreaLocation,RentPerLocation), y=count,fill='Number of lisiting'),
           alpha=0.3,width=0.3)+
  scale_fill_manual(values=pal)+
  labs(legend=fill)+
  coord_flip()+
  ylab("Rent per location ( in thousands) / count of listings")+
  xlab("Areas in the city of Mumbai")+
  labs(title="Average Montly Rent according to furnishing by Area type")
#================================================================
#Range of renr according to residing floor

#In general

RangeOfRentbasedonfloor<-UpdatesAssigmentDataSet %>%
  group_by(ResidingFloor) %>%
  summarise(thirdQ=quantile(MonthlyRentInThousand,0.75),
            FirstQ=quantile(MonthlyRentInThousand,0.25))
RangeOfRentbasedonfloor

gg<-ggplot(data=RangeOfRentbasedonfloor,
           mapping=aes(x=FirstQ, xend=thirdQ, y=ResidingFloor, group=ResidingFloor)) + 
  geom_dumbbell(color="#a3c4dc", size_x = 4, size_xend = 4, size = 1) + 
  labs(x="Rentin Rupees(in thousands)", 
       y= "Residingfloor",
       title="Dumbbell Chart showing range of rent by residing floor") +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
plot(gg)

#lets look city by city and find the biggest range and analyze why

#Bangalore
RangeOfRentbasedonfloorBglr<-UpdatesAssigmentDataSet %>%
  filter(CityLocated =="Bangalore")%>%
  group_by(ResidingFloor) %>%
  summarise(thirdQ=quantile(MonthlyRentInThousand,0.75), FirstQ=quantile(MonthlyRentInThousand,0.25))
RangeOfRentbasedonfloorBglr

gg=ggplot(RangeOfRentbasedonfloorBglr,aes(x=FirstQ, xend=thirdQ, y=ResidingFloor, group=ResidingFloor)) + 
  geom_dumbbell(color="#a3c4dc", size_x = 4, size_xend = 4, size = 1) + 
  labs(x="Rentin Rupees(in thousands)", 
       y= "Residingfloor", 
       title="Dumbbell Chart for showing range of rent by residing floor for Bangalore") +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
plot(gg)

#Delhi
RangeOfRentbasedonfloorDlhi<-UpdatesAssigmentDataSet %>%
  filter(CityLocated =="Delhi")%>%
  group_by(ResidingFloor) %>%
  summarise(thirdQ=quantile(MonthlyRentInThousand,0.75), FirstQ=quantile(MonthlyRentInThousand,0.25))
RangeOfRentbasedonfloorDlhi

gg=ggplot(RangeOfRentbasedonfloorDlhi,aes(x=FirstQ, xend=thirdQ, y=ResidingFloor, group=ResidingFloor)) + 
  geom_dumbbell(color="#a3c4dc", size_x = 4, size_xend = 4, size = 1) + 
  labs(x="Rentin Rupees(in thousands)", 
       y= "Residingfloor", 
       title="Dumbbell Chart showing range of rent by residing floor for Delhi") +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
plot(gg)

#Chennai
RangeOfRentbasedonfloorChn<-UpdatesAssigmentDataSet %>%
  filter(CityLocated =="Chennai")%>%
  group_by(ResidingFloor) %>%
  summarise(thirdQ=quantile(MonthlyRentInThousand,0.75), FirstQ=quantile(MonthlyRentInThousand,0.25))
RangeOfRentbasedonfloorChn

gg=ggplot(RangeOfRentbasedonfloorChn,aes(x=FirstQ, xend=thirdQ, y=ResidingFloor, group=ResidingFloor)) + 
  geom_dumbbell(color="#a3c4dc", size_x = 4, size_xend = 4, size = 1) + 
  labs(x="Rentin Rupees(in thousands)", 
       y= "Residingfloor", 
       title="Dumbbell Chart showing range of rent by residing floor for Chennai") +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
plot(gg)

#Hyderabad
RangeOfRentbasedonfloorhdbd<-UpdatesAssigmentDataSet %>%
  filter(CityLocated =="Hyderabad")%>%
  group_by(ResidingFloor) %>%
  summarise(thirdQ=quantile(MonthlyRentInThousand,0.75),
            FirstQ=quantile(MonthlyRentInThousand,0.25))
RangeOfRentbasedonfloorhdbd

gg=ggplot(RangeOfRentbasedonfloorhdbd,aes(x=FirstQ, xend=thirdQ, y=ResidingFloor, group=ResidingFloor)) + 
  geom_dumbbell(color="#a3c4dc", size_x = 4, size_xend = 4, size = 1) + 
  labs(x="Rentin Rupees(in thousands)", 
       y= "Residingfloor", 
       title="Dumbbell Chart showing range of rent by residing floor for Hyderabad")+ 
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
plot(gg)

#Kolkata
RangeOfRentbasedonfloorKol<-UpdatesAssigmentDataSet %>%
  filter(CityLocated =="Kolkata")%>%
  group_by(ResidingFloor) %>%
  summarise(thirdQ=quantile(MonthlyRentInThousand,0.75), FirstQ=quantile(MonthlyRentInThousand,0.25))
RangeOfRentbasedonfloorKol

gg=ggplot(RangeOfRentbasedonfloorKol,aes(x=FirstQ, xend=thirdQ, y=ResidingFloor, group=ResidingFloor)) + 
  geom_dumbbell(color="#a3c4dc", size_x = 4, size_xend = 4, size = 1) + 
  labs(x="Rentin Rupees(in thousands)", 
       y= "Residingfloor", 
       title="Dumbbell Chart showing range of rent by residing floor for  Kolkata") +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
plot(gg)

#Mumbai
RangeOfRentbasedonfloorMum<-UpdatesAssigmentDataSet %>%
  filter(CityLocated =="Mumbai")%>%
  group_by(ResidingFloor) %>%
  summarise(thirdQ=quantile(MonthlyRentInThousand,0.75), 
            FirstQ=quantile(MonthlyRentInThousand,0.25))
RangeOfRentbasedonfloorMum

gg=ggplot(RangeOfRentbasedonfloorMum,aes(x=FirstQ, xend=thirdQ, y=ResidingFloor, group=ResidingFloor)) + 
  geom_dumbbell(color="#a3c4dc", size_x = 4, size_xend = 4, size = 1) + 
  labs(x="Rentin Rupees(in thousands)", 
       y= "Residingfloor", 
       title="Dumbbell Chart showing range of rent by residing floor for Mumbai") +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
plot(gg)
#==============================================================================================
#QUESTION 4: What cities have the most variety in terms of price

#In terms of the price range of monthly rent?
#install.packages("ggalt") #installed ggalt package
theme_set(theme_classic())

#using a dumbell to show range; but will be using range between first quartile and third quartles
RangeOfRent<-UpdatesAssigmentDataSet %>%
  group_by(CityLocated) %>% 
  summarise(thirdQ=quantile(MonthlyRentInThousand,0.75), FirstQ=quantile(MonthlyRentInThousand,0.25), iqr=thirdQ-FirstQ)
#using quatile function to devide the data into equal parts, o.75 and 0,5 denote the portion of the data wanted 
RangeOfRent


gg=ggplot(RangeOfRent,aes(x=FirstQ, xend=thirdQ, y=CityLocated, group=CityLocated)) + 
  geom_dumbbell(color="#a3c4dc", size_x = 4, size_xend = 4, size = 1) + #setting the sizes of starting and ending dots and line
  labs(x="Monly Rent (in rupees)", 
       y="City", 
       title="Range of Rent According to city", 
       caption="Montly rent is shown in thousands") +
  theme(plot.title = element_text(hjust=0.5, face="bold"), #adding background lines to every 10,000 increase in rupees
        plot.background=element_rect(fill="#f7f7f7"), #masking and lines grey
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
plot(gg)

#how strong does price range very 
install.packages("ggExtra")
library(ggExtra)

#regression
# Scatterplot
theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(UpdatesAssigmentDataSet, aes(x=ApartmentSize_Sqft, y=rentpersquarefeet, group=CityLocated, color=CityLocated)) + 
  geom_point() + 
  geom_smooth(method="lm", se=F, color="black")+
  facet_wrap(.~CityLocated,scales = "free")
g

#ggMarginal(g, type = "histogram", fill="transparent") 
ggMarginal(g, type = "boxplot", fill="transparent")
# ggMarginal(g, type = "density", fill="transparent")

#general
theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(UpdatesAssigmentDataSet, aes(x=ApartmentSize_Sqft, y=rentpersquarefeet)) + 
  geom_point(color="red") + 
  geom_smooth(method="lm", se=F, color="black")


ggMarginal(g, type = "histogram", fill="transparent") 
#ggMarginal(g, type = "boxplot", fill="transparent")
#ggMarginal(g, type = "density", fill="transparent")

#=========================================
#aeverage rent of the top 10 based on contact

#1)agent type
Bangalore1<-filter(UpdatesAssigmentDataSet, CityLocated == "Bangalore" & WayofContacting == "Contact Agent")%>%
  group_by(AreaLocation)%>%
  summarise(count=n(),RentPerLocation=mean(MonthlyRentInThousand))%>%
  arrange(desc(count))%>%
  head(10)
Bangalore1

p1<-ggplot(Bangalore1, aes(x= reorder(AreaLocation, RentPerLocation), y=count),) + 
  geom_bar(stat="identity", colour="black",fill='plum') + 
  xlab("City") + ylab("Average Rent per Squarefoot (in rupees)")+
  labs(title="Average Rent Per Squarefoot according to City for agents")+
  theme_bw()+
  coord_flip()

#2)owner type
Bangalore2<-filter(UpdatesAssigmentDataSet, CityLocated == "Bangalore" & WayofContacting == "Contact Owner")%>%
  group_by(AreaLocation)%>%
  summarise(count=n(),RentPerLocation=mean(MonthlyRentInThousand))%>%
  arrange(desc(count))%>%
  head(10)
Bangalore2

p2<-ggplot(Bangalore2, aes(x= reorder(AreaLocation, RentPerLocation), y=count),) + 
  geom_bar(stat="identity", colour="black",fill='plum') + 
  xlab("City") + ylab("Average Rent per Squarefoot (in rupees)")+
  labs(title="Average Rent Per Squarefoot according to City for owner contact")+
  theme_bw()+
  coord_flip()

ggarrange(p1,p2,ncol=2,nrow=1)



#2)chennai
Chennai<-filter(UpdatesAssigmentDataSet, CityLocated == "Chennai")%>%
  group_by(AreaLocation)%>%
  summarise(count=n(),RentPerLocation=mean(MonthlyRentInThousand))%>%
  arrange(desc(count))%>%
  head(10)
Chennai

ggplot(data = Chennai)+
  geom_col(mapping=aes(x=reorder(AreaLocation,RentPerLocation), y=RentPerLocation), color="blue", fill="white")+
  geom_line(mapping=aes(x=reorder(AreaLocation,RentPerLocation), y=count), group=1, color="red")+
  coord_flip()+
  ylab("Rent per location ( in thousands) / count of listings")+
  xlab("Areas in the city of Chennai")+
  labs(title="Average Montly Rent according to furnishing by Area type")

#3)Delhi
Delhi<-filter(UpdatesAssigmentDataSet, CityLocated == "Delhi")%>%
  group_by(AreaLocation)%>%
  summarise(count=n(),RentPerLocation=mean(MonthlyRentInThousand))%>%
  arrange(desc(count))%>%
  head(10)
Delhi

ggplot(data = Delhi)+
  geom_col(mapping=aes(x=reorder(AreaLocation,RentPerLocation), y=RentPerLocation), color="blue", fill="white")+
  geom_line(mapping=aes(x=reorder(AreaLocation,RentPerLocation), y=count), group=1, color="red")+
  coord_flip()+
  ylab("Rent per location ( in thousands) / count of listings")+
  xlab("Areas in the city of Delhi")+
  labs(title="Average Montly Rent according to furnishing by Area type")

#4)Hyderabad
Hyderabad<-filter(UpdatesAssigmentDataSet, CityLocated == "Hyderabad")%>%
  group_by(AreaLocation)%>%
  summarise(count=n(),RentPerLocation=mean(MonthlyRentInThousand))%>%
  arrange(desc(count))%>%
  head(10)
Hyderabad

ggplot(data = Hyderabad)+
  geom_col(mapping=aes(x=reorder(AreaLocation,RentPerLocation), y=RentPerLocation), color="blue", fill="white")+
  geom_line(mapping=aes(x=reorder(AreaLocation,RentPerLocation), y=count), group=1, color="red")+
  coord_flip()+
  ylab("Rent per location ( in thousands) / count of listings")+
  xlab("Areas in the city of Hyderabad")+
  labs(title="Average Montly Rent according to furnishing by Area type")

#5)Kolkata
Kolkata<-filter(UpdatesAssigmentDataSet, CityLocated == "Kolkata")%>%
  group_by(AreaLocation)%>%
  summarise(count=n(),RentPerLocation=mean(MonthlyRentInThousand))%>%
  arrange(desc(count))%>%
  head(10)
Kolkata

ggplot(data = Kolkata)+
  geom_col(mapping=aes(x=reorder(AreaLocation,RentPerLocation), y=RentPerLocation), color="blue", fill="white")+
  geom_line(mapping=aes(x=reorder(AreaLocation,RentPerLocation), y=count), group=1, color="red")+
  coord_flip()+
  ylab("Rent per location ( in thousands) / count of listings")+
  xlab("Areas in the city of Kolkata")+
  labs(title="Average Montly Rent according to furnishing by Area type")

#6)Mumbai
Mumbai<-filter(UpdatesAssigmentDataSet, CityLocated == "Mumbai")%>%
  group_by(AreaLocation)%>%
  summarise(count=n(),RentPerLocation=mean(MonthlyRentInThousand))%>%
  arrange(desc(count))%>%
  head(10)
Mumbai

ggplot(data = Mumbai)+
  geom_col(mapping=aes(x=reorder(AreaLocation,RentPerLocation), y=RentPerLocation), color="blue", fill="white")+
  geom_line(mapping=aes(x=reorder(AreaLocation,RentPerLocation), y=count), group=1, color="red")+
  coord_flip()+
  ylab("Rent per location ( in thousands) / count of listings")+
  xlab("Areas in the city of Mumbai")+
  labs(title="Average Montly Rent according to furnishing by Area type")
#==============================================================
#5.9 using boxplot to see most occuring bb ratio
ggplot(UpdatesAssigmentDataSet, aes(x=BBRatio, fill = CityLocated))+geom_histogram()+
  xlab("count") + ylab("Ratio")+
  labs(title="Histogram showing countn of different Bedrooom to Bathroom ratio according to each city")+
  theme_bw()




#UNUSED
#==============================================================
averageSizeofArea<-UpdatesAssigmentDataSet%>%
  group_by(PropertyAreaType)%>%
  summarize(averagesize=mean(rentpersquarefeet))
averageSizeofArea
#entering the data into a bar graph and arranging in Ascending order
ggplot(averageSizeofArea, aes(x= reorder(PropertyAreaType,averagesize), y=averagesize),) + 
  geom_bar(stat="identity", colour="black", fill= "blue") + 
  xlab("Area Type") + ylab("Average Size of house (inf Sqft)")+
  labs(title="Average Montly Rent according to furnishing by Area type")
#===================================================================
tenantpreferenofCity = UpdatesAssigmentDataSet %>%
  group_by(PreferenceOfTenants, CityLocated)  %>%
  summarise(Tenantcount= n())
tenantpreferenofCity

ggplot(tenantpreferenofCity,aes(x = CityLocated, y = Tenantcount, fill = PreferenceOfTenants)) +
  geom_bar(stat = "identity", position = "dodge")+ 
  labs(title="Average Montly Rent according to furnishing by City")
#===================================================================
m = UpdatesAssigmentDataSet %>% 
  group_by(AreaType=PropertyAreaType, PreferenceOfTenants)  %>%
  summarise(Tenantcount= n())
m

ggplot(m, aes(x=PreferenceOfTenants,y= Tenantcount,fill=AreaType))+
  geom_col(position='dodge', width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Preference of Tenants according to city", 
       subtitle="Tenant Preference")


#=========================================================================================
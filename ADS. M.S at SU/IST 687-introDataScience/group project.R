# IST687, Standard Homework Heading
#
# Group Project
# Final 

# Run these three functions to get a clean test of homework code
dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!

# Set working directory 
# Change to the folder containing your homework data files
setwd("~/Desktop/IST687")

#install.packages("dplyr")
library(dplyr)
#install.packages("readr")
library(readr)
#install.packages("sqldf")
library(sqldf)


Survey <- read.csv("spring19survey.csv")
Survey <- data.frame(Survey,stringsAsFactors = FALSE)

summary(Survey)

nrow(Survey) # there are 194833 rows in this dataset
ncol(Survey) # there are 29 columns in this dataset
dim(Survey)
summary(Survey)
# there are 
# 8 NAs satisfaction, 
# 3538 NAs in Departure.Delay.in.Minutes, 
# 4105 NAs in Arrival.Delay.in.Minutes
# 4105 NAs in Flight.time.in.minutes


# Delete the NA values in Satisfaction
Survey <- Survey[which(!is.na(Survey$Satisfaction)),]
nrow(Survey)
# there are still 194825 records left


# Characteristics of Customers
#---------------------------------------------------------------------------------
# ****** Basic Information ******
prop.table(table(Survey$Gender)) # the proportion of gender
pie(table(Survey$Gender)) # pie chart 
# Female accounts for more than a half, 56.40%

summary(Survey$Age) # the descriptive statistics of Age, average age is 46.17
sd(Survey$Age) # 17.30
hist(Survey$Age) #histogram for age distribution
# The age of customers ranges from 15 to 85, the standard deviation is 17.3. Outliers are 85,15,80
length(which(Survey$Age==80)) # there are 5569 customers aged 80.
length(which(Survey$Age==85)) # there are 2305 customers aged 85.
length(which(Survey$Age==15)) # there are 1620 customers aged 15.
length(which(Survey$Age<=20))/length(Survey$Age) # there are 13766 customers yonger than or equal to 20, which accounts for 7.07%
length(which(Survey$Age>20 & Survey$Age <=25))/length(Survey$Age) # there are 9834 customers between 20~25 years old, which accounts for 5.05%
length(which(Survey$Age <=25))/length(Survey$Age) # there are 23,600 customers yonger than 25 years old, which accounts for 12.11%
length(which(Survey$Age>=55))/length(Survey$Age)# there are 61216 customers older than or equal to 55, which accounts for 31.42%

boxplot(Survey$Age~Survey$Gender) # box plot shows the difference of age by gender
# Female customers are relatively older than male customers

# ***** Flight Profile *****
#---------------------------------------------------------------------------------
# ***** The proportion of the Airline Status *****
prop.table(table(Survey$Airline.Status))[c(1,4,2,3)] #68.37% are blue, 19.99% are silver, 8.39% are gold, 3.25% are platinum
pie(table(Survey$Airline.Status)) #make a pie chart
barplot(table(Survey$Airline.Status)[c(1,4,2,3)]) #make a bar plot 
# As the level of the status becomes higher, the proportion of customers is smaller.
# Blue is the biggest group of customers(68.37%), Silver comes the second(19.99%).

# ***** The proportion of the Year of first Flight *****
prop.table(table(Survey$Year.of.First.Flight)) 
hist(Survey$Year.of.First.Flight) #make a histogram for year of first flight distribution
pie(table(Survey$Year.of.First.Flight))
# Year 2003 has the most first-flight customers, accounts for 15.00%, the proportion of the rest are almost same.

# ***** Descriptive of frequent flyer Accounts *****
summary(Survey$Total.Freq.Flyer.Accts) # The average accounts number is 0.8895, the max amount of accounts is 12 
which(Survey$Total.Freq.Flyer.Accts==12) # 2 customers have 12 accounts
# Most of customers don't have frequent flyer accounts, 3/4 of customers have less than 2 accounts. 2 customers have 12 accounts.
hist(Survey$Total.Freq.Flyer.Accts)
length(which(Survey$Total.Freq.Flyer.Accts>=4))/length(Survey$Total.Freq.Flyer.Accts)
# There are 5498 customers with more than 4 frequent flyer accounts, accounting for 2.82%

# ***** Proportion of Class *****
prop.table(table(Survey$Class)) # 8.23% are Business class, 82.58% are Eco class, 10.30% are Eco Plus
barplot(table(Survey$Class)[c(2,3,1)])#make a bar plot of distribution of differernt class 
pie(table(Survey$Class))
# Eco Class has the most customers, accounting for 82.58%

# ***** Proportion of type of travel *****
prop.table(table(Survey$Type.of.Travel))[c(1,3,2)] #61.31% are Business travel, 30.84% are Personal Travel, 7.85% are Mileage tickets 
boxplot(Survey$Total.Freq.Flyer.Accts~Survey$Type.of.Travel)
# Business travel and Mileage tickets has more accounts than persoanl travel
barplot((table(Survey$Type.of.Travel))[c(1,3,2)])
pie(table(Survey$Type.of.Travel))

# ***** Contigency table of Type of Travel and Class *****
prop.table(table(Survey$Type.of.Travel,Survey$Class))[,c(2,3,1)]
barplot((table(Survey$Type.of.Travel,Survey$Class))[,c(2,3,1)]) 
# Customers of Eco Class, Travel for Business and Personal accounts for 75.0%

# ***** Shopping & Eating *****
summary(Survey$Shopping.Amount.at.Airport) # The average amount is 26.51, and max amount is 879 
summary(Survey$Eating.and.Drinking.at.Airport) # The average amount is 68.1, and max amout is 895
hist(Survey$Eating.and.Drinking.at.Airport,breaks = 80,col='dark blue',ylim = c(0,168000),xlim=c(0,300),main = 'Histogram of Eating and Shopping At Airport') #make a histogram about eating and drinking 
hist(Survey$Shopping.Amount.at.Airport,breaks = 80,add=T,xlab='',main='',col='light blue') # Add Shopping histogram onto the previous one 

# ***** Shopping & Eating by Gender *****
boxplot(Survey$Shopping.Amount.at.Airport~Survey$Gender,main='Shopping By Gender')
boxplot(Survey$Eating.and.Drinking.at.Airport~Survey$Gender,main='Eating By Gender')
# Female spent more on Shopping while male spent more on eating.

# ***** Shopping & Eating by Airline Status ***** 
boxplot(Survey$Shopping.Amount.at.Airport~Survey$Airline.Status,main='Shopping By Status')
boxplot(Survey$Eating.and.Drinking.at.Airport~Survey$Airline.Status,main='Eating By Status')
# Blue spent most at Airport.

# ***** Shopping & Eating by Class *****
boxplot(Survey$Shopping.Amount.at.Airport~Survey$Class,main='Shopping By Class')
boxplot(Survey$Eating.and.Drinking.at.Airport~Survey$Class,main='Eating By Class')
# Eco Spent most at Airport. 

# ***** Shopping & Eating by Class *****
boxplot(Survey$Shopping.Amount.at.Airport~Survey$Type.of.Travel,main='Shopping By Travel Type')
boxplot(Survey$Eating.and.Drinking.at.Airport~Survey$Type.of.Travel,main='Eating By Travel Type')
# Business travel spent more on Eating and shopping.
# Outlier of shopping amount comes from Mileage tickets.

# ***** Price Sensitivity *****
hist(Survey$Price.Sensitivity)# Distribution of price Sensitivity
summary(Survey$Price.Sensitivity) # The average level of price sensitivity is 1.278
boxplot(Survey$Shopping.Amount.at.Airport~Survey$Price.Sensitivity)
boxplot(Survey$Eating.and.Drinking.at.Airport~Survey$Price.Sensitivity)
table(Survey$Price.Sensitivity,Survey$Class)
boxplot(Survey$Price.Sensitivity~Survey$Class)

# ***** Loyalty *****
# Descriptive Statistics of Loyalty
summary(Survey$Loyalty) # The average of loyalty is -0.27545, The loyalty of the customers are quite low.
hist(Survey$Loyalty,breaks=50,main='Histogram of Loyalty') # Outliers : 1
length(which(Survey$Loyalty==1))/length(Survey$Loyalty) # There are 9389 extreme loyal customers, accounting for 4.82%


# ***** Satisfaction *****
boxplot(Survey$Satisfaction)
hist(Survey$Satisfaction) #Distribution of Satisfaction
boxplot(Survey$Satisfaction~Survey$Class,main='Satisfaction by Class')
boxplot(Survey$Satisfaction~Survey$Airline.Status,main='Satisfaction by Status')
boxplot(Survey$Satisfaction~Survey$Gender,main='Satisfaction by Gender')

# ***** Characteristics of Flights *****
#----------------------------------------------------------------------------------
# ***** Partner *****
# ***** Rename Partner *****
Survey$Partner.Name[which(Survey$Partner.Name=='Cheapseats Airlines Inc.')] <- 'CheapSeats'
Survey$Partner.Name[which(Survey$Partner.Name=='Cool&Young Airlines Inc.')] <- 'Cool&Young'
Survey$Partner.Name[which(Survey$Partner.Name=='EnjoyFlying Air Services')] <- 'EnjoyFlying'
Survey$Partner.Name[which(Survey$Partner.Name=='FlyFast Airways Inc.')] <- 'FlyFast'
Survey$Partner.Name[which(Survey$Partner.Name=='FlyHere Airways')] <- 'FlyHere'
Survey$Partner.Name[which(Survey$Partner.Name=='FlyToSun Airlines Inc.')] <- 'FlyToSun'
Survey$Partner.Name[which(Survey$Partner.Name=='GoingNorth Airlines Inc.')] <- 'GoingNorth'
Survey$Partner.Name[which(Survey$Partner.Name=='Northwest Business Airlines Inc.')] <- 'Northwest'
Survey$Partner.Name[which(Survey$Partner.Name=='OnlyJets Airlines Inc.')] <- 'OnlyJets'
Survey$Partner.Name[which(Survey$Partner.Name=='Paul Smith Airlines Inc.')] <- 'Paul Smiths'
Survey$Partner.Name[which(Survey$Partner.Name=='Sigma Airlines Inc.')] <- 'Sigma'
Survey$Partner.Name[which(Survey$Partner.Name=='Southeast Airlines Co.')] <- 'Southeast'
Survey$Partner.Name[which(Survey$Partner.Name=='West Airways Inc.')] <- 'West'
Survey$Partner.Name[which(Survey$Partner.Name=='Oursin Airlines Inc.')] <- 'Oursin'

# ***** Volumes of Partner*****
NumOfPartner <- table(Survey$Partner.Name)
View(NumOfPartner[order(-NumOfPartner)])
NumOfPartner <- NumOfPartner[order(-NumOfPartner)]
prop.table(NumOfPartner)
# Cheap Seats, Sigma, FlyFast are the top three Airline Companies with highest volumes, the total of which accounts for almost half of the Airline companies.
# Cheap Seats, 39441, 20.25%
# Sigma, 25364,13.02%
# FlyFast, 23058,11.84%

# ***** Cancelled by Partner *****
Survey$Partner<- Survey$Partner.Name
Survey$Partner <- factor(Survey$Partner,level=names(NumOfPartner))

# ***** Delay by Partner ***** 
NumOfPartner<- NumOfPartner[order(table(Survey$Partner.Name))] 
barplot(NumOfPartner,horiz=T,las=1,cex.names = 0.6,main='Volumes of Partner Airlines')

# ***** New Column - Filght Performance ***** 
Survey$FlightPerformance[Survey$Flight.cancelled=='Yes'] <- 'Cancelled'
Survey$FlightPerformance[Survey$Departure.Delay.in.Minutes>5 ] <- 'Departure Delay > 5'
Survey$FlightPerformance[Survey$Departure.Delay.in.Minutes<=5] <- 'Slightly Delayed'
Survey$FlightPerformance[Survey$Departure.Delay.in.Minutes==0] <- 'Ontime'
Survey$FlightPerformance <- factor(Survey$FlightPerformance,level=c('Cancelled','Ontime','Slightly Delayed','Departure Delay > 5'))
prop.table(table(Survey$FlightPerformance))
barplot(table(Survey$FlightPerformance),main='Flight Performance')
pie(table(Survey$FlightPerformance),main='Flight Performance')
# there are 3538 flights cancelled(1.84%), 106,307 flights ontime(54.67%), 20432 flight slightly delayed(10.49%), 64,537 Departure delay greater than 5 minutes(33.11%). Flight Delay is ubiquitous.
View(table(Survey$Partner,Survey$FlightPerformance))


# ***** Proportion of Cancelled Flights *****
prop.table(table(Survey$Flight.cancelled))
# There are 3603 flights were cancelled, accounting for 1.85%
barplot(table(Survey$Flight.cancelled),main = 'Flight Cancelled')

# ***** Contigency table of Parnter and Cancelled flights *****
CancelledPartner <- table(Survey$Partner.Name,Survey$Flight.cancelled)[order(table(Survey$Partner.Name)),]
CancelledPartner
barplot(CancelledPartner[,2],horiz=T,las=1,cex.names = 0.6,col='Red')

# ***** Subset Cancelled Flights *****
CancelledDF <- Survey[Survey$Flight.cancelled=='Yes',]
View(CancelledDF)
nrow(CancelledDF) # there are 3603 rows 
sum(is.na(CancelledDF)) 
summary(CancelledDF)
sum(!is.na(CancelledDF$Departure.Delay.in.Minutes))
# There are 65 records in Departure.Delay.in.Minutes are not N/A.
hist(CancelledDF$Departure.Delay.in.Minutes,breaks = 50)
# These flights might be announced cancelled after delay.
CancelledDelay <- CancelledDF[which(!is.na(CancelledDF$Departure.Delay.in.Minutes)),]
hist(CancelledDelay$Satisfaction)

# ***** Subset On-duty Flights *****
OnflightDF1 <- Survey[Survey$Flight.cancelled=='No',] 
barplot(table(OnflightDF1$Arrival.Delay.greater.5.Mins),main='Arrival Delay > 5 mins')
View(OnflightDF1)
table(OnflightDF1$Arrival.Delay.greater.5.Mins) # There are 124593 no and 66618 yes
sum(is.na(OnflightDF1))
summary(OnflightDF1)
# There are 502 NA in Arrival.Delay.in.Minutes and 502 in Flight.time.in.minutes.

# ***** Delay *****
boxplot(Survey$Departure.Delay.in.Minutes,main='boxplot of Departure Delay')
boxplot(Survey$Arrival.Delay.in.Minutes,main='boxplot of Arrival Delay')
OnflightDF <- OnflightDF1[which(!is.na(OnflightDF1$Flight.time.in.minutes)),]
sum(is.na(OnflightDF))
summary(OnflightDF)

# ***** Contigency table of Partner and delayed flights *****
DelayPartner <- table(OnflightDF$Partner.Name,OnflightDF$Arrival.Delay.greater.5.Mins)[order(table(Survey$Partner.Name)),]
DelayPartner
barplot(DelayPartner[,2],horiz=T,las=1,cex.names = 0.6,col='red')


boxplot(Survey$Scheduled.Departure.Hour~Survey$Flight.cancelled,main='Boxplot of Scheduled Departure Time by Cancelled')
boxplot(Survey$Scheduled.Departure.Hour~Survey$Arrival.Delay.greater.5.Mins,main='Boxplot of Scheduled Departure Time by Arrival Greater Than 5 min')

# ***** Plot of Departure and Arrival Delay *****
plot(Survey$Departure.Delay.in.Minutes,Survey$Arrival.Delay.in.Minutes,main='Y=0.955x+0.263,Adjusted R-Squared=0.933')

# ***** Create a new column, depature Delay *****
Survey$Departure.Delay.greater.5.Mins <- 'Yes'
Survey$Departure.Delay.greater.5.Mins [Survey$Departure.Delay.in.Minutes<=5] <-'<=5'
Survey$Departure.Delay.greater.5.Mins [Survey$Departure.Delay.in.Minutes>5&Survey$Departure.Delay.in.Minutes<=20] <- '5~20'
Survey$Departure.Delay.greater.5.Mins [Survey$Departure.Delay.in.Minutes>20&Survey$Departure.Delay.in.Minutes<=100] <- '20~100'
Survey$Departure.Delay.greater.5.Mins [Survey$Departure.Delay.in.Minutes>100] <- '>100'
Survey$Departure.Delay.greater.5.Mins[is.na(Survey$Departure.Delay.in.Minutes)]<-'Na'

# ***** None Na Value *****
S<- Survey[which(Survey$Departure.Delay.greater.5.Mins!='Na'),] #remove the na rows 
barplot(table(S$Departure.Delay.greater.5.Mins),main='Departure Delay Time')
barplot(table(S$Arrival.Delay.greater.5.Mins),main='Arrival Delay >5 ')

# ***** Depature delay > 5 minutes *****
prop.table(table(Survey$Departure.Delay.greater.5.Mins))
# Yes,126,739(33.2%), NO,63537(65.06%), Na, 3538(1.82%)
hist(Survey$Departure.Delay.in.Minutes,breaks = 100)
boxplot(Survey$Departure.Delay.in.Minutes)

# ***** Co Partner & Delay *****
barplot(t(table(S$Partner.Name,S$Departure.Delay.greater.5.Mins)),cex.names = 0.7,las=2)
boxplot(S$Scheduled.Departure.Hour~S$Departure.Delay.greater.5.Mins,main='Boxplot of Scheduled Departure Time by Delay Greater Than 5 min')
table(S$Departure.Delay.greater.5.Mins) 
boxplot(Survey$Scheduled.Departure.Hour~Survey$Departure.Delay.greater.5.Mins)

# ***** Loyalty and Satsfaction ***** 
boxplot(Survey$Loyalty~Survey$Satisfaction,main='Loyalty by satisfaction') 
# Generally, the higher of the satisfaction, the higher of the loyalty

# ***** Scheduled Departure Hour *****
barplot(table(Survey$Scheduled.Departure.Hour))
#Change the order
Survey$Departure.Delay.greater.5.Mins <- factor(Survey$Departure.Delay.greater.5.Mins,level=c('<=5','5~20','20~100','>100'))
#View(Survey$Departure.Delay.greater.5.Mins)

hist(Survey$Scheduled.Departure.Hour)
which(Survey$Departure.Delay.greater.5.Mins=='Slightly Delayed')
Survey$Departure.Delay.greater.5.Mins

hist((Survey$Scheduled.Departure.Hour)[which(Survey$Departure.Delay.greater.5.Mins=='<=5')])
hist((Survey$Scheduled.Departure.Hour)[which(Survey$Departure.Delay.greater.5.Mins=='5~20')],add=TRUE,col="light grey")
hist((Survey$Scheduled.Departure.Hour)[which(Survey$Departure.Delay.greater.5.Mins=='20~100')],add=TRUE,col="dark grey")
hist((Survey$Scheduled.Departure.Hour)[which(Survey$Departure.Delay.greater.5.Mins=='>100')],add=TRUE,col="black")


#LoyalCus <- Survey[Survey$Loyalty>0.5,]
#dim(LoyalCus)
#prop.table(table(LoyalCus$Satisfaction))
#prop.table(table(LoyalCus$Gender))
#prop.table(table(LoyalCus$Class))


# ***** Satisfaction *****
#----------------------------------------------------------------------------------
# ***** Derive a new column, standaridized the original value to determining the exact concentration *****
Survey$StandardizedSatis <- (Survey$Satisfaction-mean(Survey$Satisfaction))/sd(Survey$Satisfaction)
hist(Survey$StandardizedSatis)

# ***** Group the satisfaction *****
Survey$SatisfiedCustomer <- 'RelativelySatis'
Survey$SatisfiedCustomer[which(Survey$Satisfaction<3)] <-'DissatisfiedCus'
Survey$SatisfiedCustomer[which(Survey$Satisfaction==5)] <-'SastifiedCus'
View(table(Survey$SatisfiedCustomer))
View(prop.table(table(Survey$SatisfiedCustomer)))
pie(table(Survey$SatisfiedCustomer))
boxplot(Survey$Satisfaction~Survey$Departure.Delay.greater.5.Mins,main='Satisfaction by Departure Delay')

# ***** Delete the N/A values *****
summary(Survey)
View(SurveyNotNa)
SurveyNotNa <- Survey[which(!is.na(Survey$Departure.Delay.in.Minutes)),]
SurveyNotNa <- Survey[which(!is.na(Survey$Arrival.Delay.in.Minutes)),]

# ***** Correlation between Satisfaction and other variables *****
c<-cor(SurveyNotNa[,c(1,3,5:8,10:12,14,22:24,26:27)]) #correlation table 
View(c)

# ***** Relatively Related variables: ******

#Satisfaction		~ Age, Flights.Per.Year, Loyalty
#Age		~ Flights.Per.Year, Loyalty, Total.Freq.Flyer.Accts
#Flights.Per.Year	~ Loyalty,Total.Freq.Flyer.Accts
#Loyalty 		~ Total.Freq.Flyer.Accts

boxplot(Survey$StandardizedSatis~Survey$Gender,main='SatisfactionBy Gender')
boxplot(Survey$StandardizedSatis~Survey$ByAge,main='SatisfactionBy Age')
boxplot(Survey$StandardizedSatis~Survey$Year.of.First.Flight,main='SatisfactionBy Gender')
boxplot(Survey$StandardizedSatis~Survey$Class,main='SatisfactionBy Class')
boxplot(Survey$StandardizedSatis~Survey$Type.of.Travel,main='SatisfactionBy TravelType')

Bucket <- function(vec){
  q <- quantile(vec,c(0.4,0.6))
  newVec<-replicate(length(vec),'Average')
  newVec[vec>=q[2]] <-'High'
  newVec[vec<q[1]] <- 'Low'
  return(newVec)
}

Survey$LoyalCustomer <- Bucket(Survey$Loyalty)
boxplot(Survey$StandardizedSatis~Survey$LoyalCustomer,main='SatisfactionBy Loyalty')

Survey$PriceOriented[which(Survey$Price.Sensitivity<=1)] <- 'LowPriceSentivity'
Survey$PriceOriented[which(Survey$Price.Sensitivity>1)] <- 'PriceOriented'

boxplot(Survey$StandardizedSatis~Survey$PriceOriented,main='SatisfactionBy PriceSensitivity')
boxplot(Survey$StandardizedSatis~Survey$FlightPerformance,main='SatisfactionBy Flight')

plot(Survey$Age,Survey$Flights.Per.Year) 
# the plot is kinda mesay

plot(Survey$Age,Survey$Loyalty)


# ***** Status *****
barplot(t(table(Survey$Airline.Status,Survey$ByAge)))
prop.table(table(Survey$Airline.Status,Survey$ByAge))
Survey$Airline.Status<-factor(Survey$Airline.Status,level=c("Blue","Silver",'Gold',"Platinum"))
Survey$ByAge <- factor(Survey$ByAge,level=c("Young","MiddleAge",'Aged'))

StatusDF <- group_by(Survey,Airline.Status)
View(StatusDF)
View(summarise(StatusDF,AverSatis=mean(StandardizedSatis),SdStais=sd(StandardizedSatis),AvgFlightFreq=mean(Flights.Per.Year),AvgAge=mean(Age),AvgPriceSen=mean(Price.Sensitivity),AvgLoyalty=mean(Loyalty),AvgShopping=mean(Shopping.Amount.at.Airport),AvgEating=mean(Eating.and.Drinking.at.Airport)))
summarise(StatusDF,mean(Flights.Per.Year))
colnames(Survey)

# ***** Partner *****
ParF<- table(Survey$Partner,Survey$FlightPerformance)
ParF[,1]
Par<- table(Survey$Partner)
Par

# ***** Subset TopPartner *****
TopPartner <- Survey[which(Survey$Partner=='Sigma'|Survey$Partner=='FlyFast'|Survey$Partner=='CheapSeats'),]

FlightDF <- group_by(TopPartner,FlightPerformance)
View(summarise(FlightDF,mean(StandardizedSatis),sd(StandardizedSatis),mean(Age),mean(Price.Sensitivity),mean(Loyalty),mean(Shopping.Amount.at.Airport),mean(Eating.and.Drinking.at.Airport)))
#Significant factors are age, gender, status, yearly flight frequency,
#loyalty, price sensitivity, flight cancellation and delay.


# ***** Model For Whole Dataset *****
#----------------------------------------------------------------------------------
Survey$ByAge[which(Survey$Age< 60 & Survey$Age>25)] <- 'MiddleAge'
Survey$ByAge[which(Survey$Age>=60)] <- 'Aged'
Survey$ByAge[which(Survey$Age<=25)] <- 'Young'
colnames(Survey)
# ***** Ontimeflight *****
summary(OnflightDF)
AgeDF <- OnflightDF[which(OnflightDF$Age< 60 &OnflightDF$Age>25), ]

# ***** AgedCustom *****
AgedDF <- OnflightDF[which(OnflightDF$Age> 60), ]
View(OnflightDF)

lm1 <- lm(Satisfaction~ Airline.Status+ByAge+Gender+Price.Sensitivity+Type.of.Travel+Eating.and.Drinking.at.Airport+Shopping.Amount.at.Airport+Flights.Per.Year+Loyalty+Class+FlightPerformance,data=Survey)
summary(lm1)

lm2 <- lm(Satisfaction~ Airline.Status+ByAge+Gender+Price.Sensitivity+Type.of.Travel+Shopping.Amount.at.Airport+Flights.Per.Year+Loyalty+Class+FlightPerformance,data=Survey)
summary(lm2)

lm3 <- lm(Satisfaction~ Airline.Status*FlightPerformance+Airline.Status+ByAge+Gender+Price.Sensitivity+Type.of.Travel+Shopping.Amount.at.Airport+Flights.Per.Year+Loyalty+Class+FlightPerformance,data=Survey)
summary(lm3)

lm4 <- lm(Satisfaction~Airline.Status+ByAge+Gender+Price.Sensitivity+Loyalty,data=Survey)
summary(lm4)

anova(lm1,lm2)


# interaction between age
lm2 <- lm(Satisfaction~ ByAge*Price.Sensitivity+Airline.Status+ByAge+Gender+Price.Sensitivity+Type.of.Travel+Shopping.Amount.at.Airport+Flights.Per.Year+Loyalty+Class+FlightPerformance,data=Survey)
summary(lm2)

# interaction between age

lm4 <- lm(Satisfaction~ Partner+Airline.Status*Shopping.Amount.at.Airport+Airline.Status+ByAge+Gender+Price.Sensitivity+Type.of.Travel+Shopping.Amount.at.Airport+Flights.Per.Year+Loyalty+Class+FlightPerformance,data=Survey)
summary(lm4)

lm3 <- lm(Satisfaction~ Airline.Status*Shopping.Amount.at.Airport+Airline.Status+ByAge+Gender+Price.Sensitivity+Type.of.Travel+Shopping.Amount.at.Airport+Flights.Per.Year+Loyalty+Class+FlightPerformance,data=Survey)
summary(lm3)

anova(lm3,lm4)

lm4 <- lm(Satisfaction~ Partner+Airline.Status*Eating.and.Drinking.at.Airport+Airline.Status*Shopping.Amount.at.Airport+Airline.Status+ByAge+Gender+Price.Sensitivity+Type.of.Travel+Shopping.Amount.at.Airport+Flights.Per.Year+Loyalty+Class+FlightPerformance,data=Survey)
summary(lm4)


# ***** construct linear model for the top 3 airlines *****
lm3 <- lm(Satisfaction~ Partner+Airline.Status*Shopping.Amount.at.Airport+Airline.Status+ByAge+Gender+Price.Sensitivity+Type.of.Travel+Shopping.Amount.at.Airport+Flights.Per.Year+Loyalty+Class+FlightPerformance,data=TopPartner)
summary(lm3)

lm4 <- lm(Satisfaction~ Partner+Partner*FlightPerformance+Airline.Status+ByAge+Gender+Price.Sensitivity+Type.of.Travel+Shopping.Amount.at.Airport+Flights.Per.Year+Loyalty+Class+FlightPerformance,data=TopPartner)
summary(lm4)

anova(lm4,lm3)









# ***** Linear Model for CheapSeats *****
WN <- Survey[which(Survey$Partner.Code=='WN'),] #Create a dataset only contains wn
WN <- WN[,-which(names(WN)=="Partner.Code")]
WN <- WN[,-which(names(WN)=="Partner.Name")]

status_lm <- lm(formula = Satisfaction~Airline.Status,data=WN)
summary.lm(status_lm)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.6282 -0.9414 -0.1254  0.8746  1.8746 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             3.12541    0.00552  566.21   <2e-16 ***
#   Airline.StatusGold      0.59919    0.01710   35.05   <2e-16 ***
#   Airline.StatusPlatinum  0.50282    0.02641   19.04   <2e-16 ***
#   Airline.StatusSilver    0.81598    0.01163   70.18   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9094 on 39437 degrees of freedom
# Multiple R-squared:  0.1253,	Adjusted R-squared:  0.1252 
# F-statistic:  1883 on 3 and 39437 DF,  p-value: < 2.2e-16


age_lm <- lm(formula = Satisfaction~Age,data = WN)
summary.lm(age_lm)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.6723 -0.6724  0.1215  0.6452  2.0604 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.9165994  0.0135367  289.33   <2e-16 ***
#   Age         -0.0122125  0.0002743  -44.53   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9488 on 39439 degrees of freedom
# Multiple R-squared:  0.04787,	Adjusted R-squared:  0.04784 
# F-statistic:  1983 on 1 and 39439 DF,  p-value: < 2.2e-16


gender_lm <- lm(formula =Satisfaction~Gender,data=WN )
summary.lm(gender_lm)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.6723 -0.6724  0.1215  0.6452  2.0604 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.9165994  0.0135367  289.33   <2e-16 ***
#   Age         -0.0122125  0.0002743  -44.53   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9488 on 39439 degrees of freedom
# Multiple R-squared:  0.04787,	Adjusted R-squared:  0.04784 
# F-statistic:  1983 on 1 and 39439 DF,  p-value: < 2.2e-16


price_lm <- lm(formula = Satisfaction~Price.Sensitivity,data = WN)
summary.lm(price_lm)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.59019 -0.40484 -0.03414  0.59516  2.15121 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        3.590188   0.012483  287.62   <2e-16 ***
#   Price.Sensitivity -0.185349   0.008967  -20.67   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9671 on 39439 degrees of freedom
# Multiple R-squared:  0.01072,	Adjusted R-squared:  0.01069 
# F-statistic: 427.2 on 1 and 39439 DF,  p-value: < 2.2e-16

year_lm <- lm(formula = Satisfaction~Year.of.First.Flight,data=WN)
summary.lm(year_lm)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3672 -0.3642 -0.3399  0.6510  1.6601 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)          -2.744365   3.302821  -0.831   0.4060  
# Year.of.First.Flight  0.003038   0.001645   1.846   0.0649 .
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9723 on 39439 degrees of freedom
# Multiple R-squared:  8.64e-05,	Adjusted R-squared:  6.104e-05 
# F-statistic: 3.408 on 1 and 39439 DF,  p-value: 0.0649


peryear_lm <- lm(formula = Satisfaction~Flights.Per.Year,data=WN)
summary.lm(peryear_lm)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3672 -0.3642 -0.3399  0.6510  1.6601 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)          -2.744365   3.302821  -0.831   0.4060  
# Year.of.First.Flight  0.003038   0.001645   1.846   0.0649 .
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9723 on 39439 degrees of freedom
# Multiple R-squared:  8.64e-05,	Adjusted R-squared:  6.104e-05 
# F-statistic: 3.408 on 1 and 39439 DF,  p-value: 0.0649


loyalty_lm <- lm(formula = Satisfaction ~Loyalty,data=WN)
summary.lm(loyalty_lm)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.7610 -0.5745 -0.1334  0.7186  1.8686 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 3.441294   0.005428   634.0   <2e-16 ***
#   Loyalty     0.319694   0.009006    35.5   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9571 on 39439 degrees of freedom
# Multiple R-squared:  0.03096,	Adjusted R-squared:  0.03094 
# F-statistic:  1260 on 1 and 39439 DF,  p-value: < 2.2e-16

Type_lm <- lm(formula = Satisfaction ~Type.of.Travel,data=WN)
summary.lm(Type_lm)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.7510 -0.5229  0.2490  0.4771  2.4771 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                  3.750986   0.005126  731.83   <2e-16 ***
#   TypeofTravelMileage tickets -0.229773   0.015107  -15.21   <2e-16 ***
#   TypeofTravelPersonal Travel -1.228089   0.008839 -138.93   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7957 on 39438 degrees of freedom
# Multiple R-squared:  0.3303,	Adjusted R-squared:  0.3303 
# F-statistic:  9728 on 2 and 39438 DF,  p-value: < 2.2e-16

freq_lm <- lm(formula = Satisfaction ~ Total.Freq.Flyer.Accts,data=WN)
summary.lm(freq_lm)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.7696 -0.4278 -0.2911  0.7089  1.7089 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         3.291141   0.006209  530.10   <2e-16 ***
#   TotalFreqFlyerAccts 0.068344   0.004266   16.02   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9692 on 39439 degrees of freedom
# Multiple R-squared:  0.006466,	Adjusted R-squared:  0.00644 
# F-statistic: 256.7 on 1 and 39439 DF,  p-value: < 2.2e-16

shopping_lm <- lm(formula = Satisfaction ~ Shopping.Amount.at.Airport,data=WN)
summary.lm(shopping_lm)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.4330 -0.3669 -0.3448  0.6552  1.6552 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             3.345e+00  5.472e-03 611.272   <2e-16 ***
#   ShoppingAmountatAirport 2.938e-04  9.196e-05   3.195   0.0014 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9722 on 39439 degrees of freedom
# Multiple R-squared:  0.0002588,	Adjusted R-squared:  0.0002335 
# F-statistic: 10.21 on 1 and 39439 DF,  p-value: 0.001398


eating_lm <- lm(formula = Satisfaction~Eating.and.Drinking.at.Airport,data=WN)
summary.lm(eating_lm)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.4833 -0.3749 -0.3239  0.6570  1.6761 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                3.324e+00  8.106e-03 410.039  < 2e-16 ***
#   EatingandDrinkingatAirport 4.253e-04  9.552e-05   4.452 8.51e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9721 on 39439 degrees of freedom
# Multiple R-squared:  0.0005024,	Adjusted R-squared:  0.0004771 
# F-statistic: 19.82 on 1 and 39439 DF,  p-value: 8.514e-06

Class_lm <- lm(formula = Satisfaction~Class, data=WN)
summary.lm(Class_lm)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.5055 -0.3450 -0.2893  0.6550  1.7107 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    3.50548    0.01695 206.866   <2e-16 ***
#   ClassEco      -0.16046    0.01779  -9.019   <2e-16 ***
#   ClassEco Plus -0.21616    0.02277  -9.492   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9711 on 39438 degrees of freedom
# Multiple R-squared:  0.002546,	Adjusted R-squared:  0.002495 
# F-statistic: 50.32 on 2 and 39438 DF,  p-value: < 2.2e-16



day_lm <- lm(formula = Satisfaction~Day.of.Month,data = WN)
summary.lm(day_lm)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3615 -0.3591 -0.3439  0.6491  1.6561 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 3.3433334  0.0102452 326.332   <2e-16 ***
#   DayofMonth  0.0005854  0.0005668   1.033    0.302    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9723 on 39439 degrees of freedom
# Multiple R-squared:  2.704e-05,	Adjusted R-squared:  1.689e-06 
# F-statistic: 1.067 on 1 and 39439 DF,  p-value: 0.3017

schedule_lm <- lm(formula = Satisfaction ~ Scheduled.Departure.Hour,data=WN)
summary.lm(schedule_lm)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.4022 -0.3899 -0.3036  0.6594  1.7026 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             3.433060   0.015093 227.454  < 2e-16 ***
#   ScheduledDepartureHour -0.006167   0.001095  -5.633 1.78e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9719 on 39439 degrees of freedom
# Multiple R-squared:  0.000804,	Adjusted R-squared:  0.0007786 
# F-statistic: 31.73 on 1 and 39439 DF,  p-value: 1.78e-08


depatureD_lm <- lm(formula = Satisfaction~Departure.Delay.in.Minutes,data=WN)
summary.lm(depatureD_lm)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3948 -0.3948  0.6051  0.6203  2.4714 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              3.394853   0.005524  614.59   <2e-16 ***
#   DepartureDelayinMinutes -0.002160   0.000140  -15.43   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9707 on 38946 degrees of freedom
# (493 observations deleted due to missingness)
# Multiple R-squared:  0.006076,	Adjusted R-squared:  0.006051 
# F-statistic: 238.1 on 1 and 38946 DF,  p-value: < 2.2e-16


arrivalD_lm <- lm(formula = Satisfaction~Arrival.Delay.in.Minutes,data=WN)
summary.lm(arrivalD_lm)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3933 -0.3933  0.6067  0.6136  2.5300 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            3.3932724  0.0054317  624.71   <2e-16 ***
#   ArrivalDelayinMinutes -0.0022853  0.0001401  -16.31   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9706 on 38844 degrees of freedom
# (595 observations deleted due to missingness)
# Multiple R-squared:  0.006799,	Adjusted R-squared:  0.006773 
# F-statistic: 265.9 on 1 and 38844 DF,  p-value: < 2.2e-16

cancelled_lm <- lm(formula = Satisfaction~Flight.cancelled,data=WN)
summary.lm(cancelled_lm)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.35606 -0.35606 -0.08114  0.64394  1.91886 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         3.356064   0.004924 681.521  < 2e-16 ***
#   FlightcancelledYes -0.274929   0.044045  -6.242 4.37e-10 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9718 on 39439 degrees of freedom
# Multiple R-squared:  0.0009869,	Adjusted R-squared:  0.0009616 
# F-statistic: 38.96 on 1 and 39439 DF,  p-value: 4.366e-10

time_lm <- lm(formula = Satisfaction~Flight.time.in.minutes,data=WN)
summary.lm(time_lm)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3593 -0.3584  0.6408  0.6443  1.6559 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          3.361e+00  1.033e-02 325.309   <2e-16 ***
#   Flighttimeinminutes -4.827e-05  9.029e-05  -0.535    0.593    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9739 on 38844 degrees of freedom
# (595 observations deleted due to missingness)
# Multiple R-squared:  7.358e-06,	Adjusted R-squared:  -1.839e-05 
# F-statistic: 0.2858 on 1 and 38844 DF,  p-value: 0.5929

distance_lm <-lm(formula = Satisfaction ~Flight.Distance,data=WN)
summary.lm(distance_lm)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3580 -0.3544 -0.3509  0.6482  1.6492 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    3.350e+00  9.286e-03   360.8   <2e-16 ***
#   FlightDistance 3.359e-06  1.121e-05     0.3    0.764    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9723 on 39439 degrees of freedom
# Multiple R-squared:  2.279e-06,	Adjusted R-squared:  -2.308e-05 
# F-statistic: 0.08987 on 1 and 39439 DF,  p-value: 0.7643

five_lm <- lm(formula = Satisfaction~Arrival.Delay.greater.5.Mins,data=WN)
summary.lm(five_lm)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.4879 -0.4879 -0.1590  0.5121  1.8410 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                  3.487856   0.006292  554.37   <2e-16 ***
#   ArrivalDelaygreater5Minsyes -0.328845   0.009811  -33.52   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9588 on 39439 degrees of freedom
# Multiple R-squared:  0.0277,	Adjusted R-squared:  0.02767 
# F-statistic:  1123 on 1 and 39439 DF,  p-value: < 2.2e-16

long_lm<- lm(formula = Satisfaction ~Long.Duration.Trip,data=WN)
summary.lm(long_lm)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3802 -0.3802 -0.3219  0.6781  1.6781 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           3.380190   0.006741 501.445  < 2e-16 ***
#   LongDurationTripTRUE -0.058275   0.009802  -5.945 2.78e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9719 on 39439 degrees of freedom
# Multiple R-squared:  0.0008954,	Adjusted R-squared:  0.0008701 
# F-statistic: 35.35 on 1 and 39439 DF,  p-value: 2.783e-09


lm.1.wn <- lm(Satisfaction ~Airline.Status + Age + Gender,data=WN)
lm.2.wn <- lm(Satisfaction ~ Airline.Status + Age +Gender +Price.Sensitivity,data=WN)
anova(lm.1.wn,lm.2.wn) #lm.2.wn is better
# Analysis of Variance Table
# 
# Model 1: Satisfaction ~ Airline.Status + Age + Gender
# Model 2: Satisfaction ~ Airline.Status + Age + Gender + Price.Sensitivity
# Res.Df   RSS Df Sum of Sq      F    Pr(>F)    
# 1  39435 30255                                  
# 2  39434 29977  1    278.62 366.53 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



lm.3.wn <- lm(Satisfaction ~ Airline.Status + Age + Gender + Price.Sensitivity +Flights.Per.Year + Loyalty + Type.of.Travel ,data=WN)
anova(lm.2.wn,lm.3.wn)  #lm.3.wn is better
# Analysis of Variance Table
# 
# Model 1: Satisfaction ~ Airline.Status + Age + Gender
# Model 2: Satisfaction ~ Airline.Status + Age + Gender + Price.Sensitivity
# Res.Df   RSS Df Sum of Sq      F    Pr(>F)    
# 1  39435 30255                                  
# 2  39434 29977  1    278.62 366.53 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


lm.4.wn <- lm(Satisfaction ~ Airline.Status + Age + Gender + Price.Sensitivity + Flights.Per.Year + Loyalty + Type.of.Travel + Class, data = WN)
anova(lm.3.wn,lm.4.wn) 
# Analysis of Variance Table
# 
# Model 1: Satisfaction ~ Airline.Status + Age + Gender
# Model 2: Satisfaction ~ Airline.Status + Age + Gender + Price.Sensitivity
# Res.Df   RSS Df Sum of Sq      F    Pr(>F)    
# 1  39435 30255                                  
# 2  39434 29977  1    278.62 366.53 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


lm.5.wn <- lm(Satisfaction ~ Airline.Status + Age + Gender + Price.Sensitivity + Flights.Per.Year  + Type.of.Travel + Class + Scheduled.Departure.Hour + Flight.cancelled, data = WN)
summary.lm(lm.5.wn)
anova(lm.4.wn, lm.5.wn)
# Analysis of Variance Table
# 
# Model 1: Satisfaction ~ Airline.Status + Age + Gender
# Model 2: Satisfaction ~ Airline.Status + Age + Gender + Price.Sensitivity
# Res.Df   RSS Df Sum of Sq      F    Pr(>F)    
# 1  39435 30255                                  
# 2  39434 29977  1    278.62 366.53 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary.lm(lm.5.wn) 
#lm.5.wn is the best model we choose to predict the WN flights.

#--------------------------------------
#Linear Model for Cancelled Fights in WN Dataset
WN.YES <- WN[which(WN$Flight.cancelled=='Yes'),]
WN.YES <- WN.YES[,-which(names(WN.YES)=="Departure.Delay.in.Minutes")] #remove deparutredelay column
WN.YES <- WN.YES[,-which(names(WN.YES)=="Arrival.Delay.in.Minutes")]
WN.YES <- WN.YES[,-which(names(WN.YES)=="Flight.time.in.minutes")]
WN.YES <- WN.YES[,-which(names(WN.YES)=="Arrival.Delay.greater.5.Mins")]
WN.YES <- WN.YES[,-which(names(WN.YES)=="Long.Duration.Trip")]
WN.YES <- WN.YES[,-which(names(WN.YES)=="Flight.cancelled")]


summary(WN.YES)


status_lm_yes <- lm(formula = Satisfaction~Airline.Status,data=WN.YES)
summary.lm(status_lm_yes)
# Call:
#   lm(formula = Satisfaction ~ AirlineStatus, data = WN.YES)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.98841 -0.69880  0.01159  0.30120  2.73684 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            2.98841    0.04067  73.471  < 2e-16 ***
#   AirlineStatusGold      0.01159    0.11859   0.098    0.922    
# AirlineStatusPlatinum -0.72525    0.17803  -4.074 5.40e-05 ***
#   AirlineStatusSilver    0.71039    0.09236   7.691 8.09e-14 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7555 on 489 degrees of freedom
# Multiple R-squared:  0.1458,	Adjusted R-squared:  0.1406 
# F-statistic: 27.83 on 3 and 489 DF,  p-value: < 2.2e-16

age_lm_yes <- lm(formula = Satisfaction~Age,data = WN.YES)
summary.lm(age_lm_yes)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.2117 -0.2874  0.0157  0.8021  1.9192 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.418357   0.100924  33.871  < 2e-16 ***
#   Age         -0.006890   0.001924  -3.581 0.000377 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8053 on 491 degrees of freedom
# Multiple R-squared:  0.02545,	Adjusted R-squared:  0.02346 
# F-statistic: 12.82 on 1 and 491 DF,  p-value: 0.000377

gender_lm_yes <- lm(formula =Satisfaction~Gender,data=WN.YES )
summary.lm(gender_lm_yes)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.10497 -0.10497 -0.06731  0.89503  1.93269 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.06731    0.04617  66.431   <2e-16 ***
#   GenderMale   0.03766    0.07620   0.494    0.621    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8156 on 491 degrees of freedom
# Multiple R-squared:  0.0004973,	Adjusted R-squared:  -0.001538 
# F-statistic: 0.2443 on 1 and 491 DF,  p-value: 0.6213

price_lm_yes <- lm(formula = Satisfaction~Price.Sensitivity,data = WN.YES)
summary.lm(price_lm_yes)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.1444 -0.1444 -0.1444  0.8556  2.0707 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       3.35955    0.09603  34.985  < 2e-16 ***
#   PriceSensitivity -0.21514    0.06867  -3.133  0.00183 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8077 on 491 degrees of freedom
# Multiple R-squared:  0.0196,	Adjusted R-squared:  0.0176 
# F-statistic: 9.815 on 1 and 491 DF,  p-value: 0.001835

year_lm_yes <- lm(formula = Satisfaction~Year.of.First.Flight,data=WN.YES)
summary.lm(year_lm_yes)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.10243 -0.10925 -0.06833  0.89757  1.95213 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)       16.76986   23.94506   0.700    0.484
# YearofFirstFlight -0.00682    0.01193  -0.572    0.568
# 
# Residual standard error: 0.8155 on 491 degrees of freedom
# Multiple R-squared:  0.0006652,	Adjusted R-squared:  -0.00137 
# F-statistic: 0.3268 on 1 and 491 DF,  p-value: 0.5678

peryear_lm_yes <- lm(formula = Satisfaction~Flights.Per.Year,data=WN.YES)
summary.lm(peryear_lm_yes)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.14783 -0.20943 -0.05542  0.85217  2.01851 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     3.209431   0.066784  48.057   <2e-16 ***
#   FlightsPerYear -0.006160   0.002684  -2.295   0.0221 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8114 on 491 degrees of freedom
# Multiple R-squared:  0.01061,	Adjusted R-squared:  0.0086 
# F-statistic: 5.268 on 1 and 491 DF,  p-value: 0.02214

loyalty_lm_yes <- lm(formula = Satisfaction ~Loyalty,data=WN.YES)
summary.lm(loyalty_lm_yes)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.10089 -0.12316 -0.07074  0.91004  1.93635 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.09099    0.04236  72.978   <2e-16 ***
#   Loyalty      0.03217    0.06882   0.467     0.64    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8156 on 491 degrees of freedom
# Multiple R-squared:  0.0004448,	Adjusted R-squared:  -0.001591 
# F-statistic: 0.2185 on 1 and 491 DF,  p-value: 0.6404

Type_lm_yes <- lm(formula = Satisfaction ~Type.of.Travel,data=WN.YES)
summary.lm(Type_lm_yes)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3380 -0.6667  0.2596  0.6620  1.6620 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                  3.33796    0.05059  65.987  < 2e-16 ***
#   TypeofTravelMileage tickets  0.32870    0.12537   2.622  0.00902 ** 
#   TypeofTravelPersonal Travel -0.59754    0.07008  -8.527  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7434 on 490 degrees of freedom
# Multiple R-squared:  0.1712,	Adjusted R-squared:  0.1678 
# F-statistic: 50.59 on 2 and 490 DF,  p-value: < 2.2e-16


freq_lm_yes <- lm(formula = Satisfaction ~ Total.Freq.Flyer.Accts,data=WN.YES)
summary.lm(freq_lm_yes)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.13722 -0.18514 -0.04138  0.86278  1.95862 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          3.04138    0.04460  68.195   <2e-16 ***
#   TotalFreqFlyerAccts  0.04792    0.03063   1.564    0.118    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8137 on 491 degrees of freedom
# Multiple R-squared:  0.004959,	Adjusted R-squared:  0.002932 
# F-statistic: 2.447 on 1 and 491 DF,  p-value: 0.1184

shopping_lm_yes <- lm(formula = Satisfaction ~ Shopping.Amount.at.Airport,data=WN.YES)
summary.lm(shopping_lm_yes)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.08317 -0.08317 -0.08271  0.91683  1.92230 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              3.083e+00  4.171e-02  73.917   <2e-16 ***
#   ShoppingAmountatAirport -9.114e-05  8.852e-04  -0.103    0.918    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8158 on 491 degrees of freedom
# Multiple R-squared:  2.159e-05,	Adjusted R-squared:  -0.002015 
# F-statistic: 0.0106 on 1 and 491 DF,  p-value: 0.918


eating_lm_yes <- lm(formula = Satisfaction~Eating.and.Drinking.at.Airport,data=WN.YES)
summary.lm(eating_lm_yes)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.07824 -0.09356 -0.08227  0.91289  1.92660 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 3.0935629  0.0635498   48.68   <2e-16 ***
#   EatingandDrinkingatAirport -0.0001613  0.0006730   -0.24    0.811    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8157 on 491 degrees of freedom
# Multiple R-squared:  0.000117,	Adjusted R-squared:  -0.001919 
# F-statistic: 0.05743 on 1 and 491 DF,  p-value: 0.8107

Class_lm_yes <- lm(formula = Satisfaction~Class, data=WN.YES)
summary.lm(Class_lm_yes)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.1062 -0.1062 -0.1062  0.8938  1.8938 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    3.100000   0.257156  12.055   <2e-16 ***
#   ClassEco       0.006236   0.260109   0.024    0.981    
# ClassEco Plus -0.240000   0.281700  -0.852    0.395    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8132 on 490 degrees of freedom
# Multiple R-squared:  0.008329,	Adjusted R-squared:  0.004281 
# F-statistic: 2.058 on 2 and 490 DF,  p-value: 0.1289



day_lm_yes <- lm(formula = Satisfaction~Day.of.Month,data = WN.YES)
summary.lm(day_lm_yes)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.10620 -0.13485 -0.07277  0.86992  1.91768 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.144407   0.063206  49.748   <2e-16 ***
#   DayofMonth  -0.004776   0.003885  -1.229     0.22    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8145 on 491 degrees of freedom
# Multiple R-squared:  0.003068,	Adjusted R-squared:  0.001038 
# F-statistic: 1.511 on 1 and 491 DF,  p-value: 0.2196

schedule_lm_yes <- lm(formula = Satisfaction ~ Scheduled.Departure.Hour,data=WN.YES)
summary.lm(schedule_lm_yes)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.18963 -0.23255 -0.06085  0.83184  1.93915 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             2.78183    0.11472  24.248  < 2e-16 ***
#   ScheduledDepartureHour  0.02146    0.00780   2.752  0.00615 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8096 on 491 degrees of freedom
# Multiple R-squared:  0.01519,	Adjusted R-squared:  0.01318 
# F-statistic: 7.571 on 1 and 491 DF,  p-value: 0.00615

distance_lm_yes <-lm(formula = Satisfaction ~Flight.Distance,data=WN.YES)
summary.lm(distance_lm_yes)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.16726 -0.13637 -0.06472  0.90277  1.95520 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    3.025e+00  7.331e-02  41.263   <2e-16 ***
#   FlightDistance 7.722e-05  8.709e-05   0.887    0.376    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8151 on 491 degrees of freedom
# Multiple R-squared:  0.001599,	Adjusted R-squared:  -0.0004349 
# F-statistic: 0.7861 on 1 and 491 DF,  p-value: 0.3757

lm.1.yes <- lm(Satisfaction ~ Airline.Status+Age,data = WN.YES)
lm.2.yes <- lm(Satisfaction ~ Airline.Status + Age + Price.Sensitivity,data=WN.YES)
summary.lm(lm.2.yes)
anova(lm.1.yes,lm.2.yes) #lm.2 better

lm.3.yes <- lm(Satisfaction ~ Airline.Status + Age + Price.Sensitivity + Flights.Per.Year,data=WN.YES)
anova(lm.2.yes,lm.3.yes)

lm.4.yes <- lm(Satisfaction ~ Airline.Status + Age + Price.Sensitivity + Type.of.Travel, data=WN.YES)
summary.lm(lm.4.yes)
anova(lm.3.yes,lm.4.yes)

lm.5.yes <- lm(Satisfaction ~ Airline.Status + Age + Price.Sensitivity + Type.of.Travel + Flights.Per.Year, data=WN.YES)
anova(lm.4.yes,lm.5.yes) #LM4 BETTER

lm.6.yes <- lm(Satisfaction ~ Airline.Status + Age + Price.Sensitivity + Type.of.Travel + Scheduled.Departure.Hour, data=WN.YES)
anova(lm.4.yes,lm.6.yes) #lm 4 best

#LM.4.YES is the best linear model for cancelled flights in WN

#--------------------------------------------------------------------------------------------
#Linear Model for Not Cancelled Flights in WN Dataset
WN.NO <- WN[which(WN$Flight.cancelled=='No'),]
summary(WN.NO)

WN.NO <- WN.NO[,-which(names(WN.NO)=="Partner")]
WN.NO <- WN.NO[,-which(names(WN.NO)=="PriceOriented")]

WN.NO <- na.omit(WN.NO) #remove NA value
summary(WN.NO)

WN.NO <- WN.NO[,-which(names(WN.NO)=="Flight.cancelled")]
str(WN.NO)

status_lm_no <- lm(formula = Satisfaction~Airline.Status,data=WN.NO)
summary.lm(status_lm_no)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.65568 -0.94388  0.05612  0.87348  1.87348 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           3.126525   0.005568  561.49   <2e-16 ***
#   AirlineStatusGold     0.608097   0.017258   35.23   <2e-16 ***
#   AirlineStatusPlatinum 0.529159   0.026711   19.81   <2e-16 ***
#   AirlineStatusSilver   0.817357   0.011712   69.79   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9102 on 38842 degrees of freedom
# Multiple R-squared:  0.1265,	Adjusted R-squared:  0.1264 
# F-statistic:  1874 on 3 and 38842 DF,  p-value: < 2.2e-16

age_lm_no <- lm(formula = Satisfaction~Age,data = WN.NO)
summary.lm(age_lm_no)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.6775 -0.6775  0.2733  0.6423  2.0606 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.9234876  0.0136674  287.07   <2e-16 ***
#   Age         -0.0123007  0.0002772  -44.38   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9501 on 38844 degrees of freedom
# Multiple R-squared:  0.04826,	Adjusted R-squared:  0.04824 
# F-statistic:  1970 on 1 and 38844 DF,  p-value: < 2.2e-16


gender_lm_no <- lm(formula =Satisfaction~Gender,data=WN.NO )
summary.lm(gender_lm_no)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.5047 -0.5047  0.4953  0.7617  1.7616 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 3.238354   0.006549  494.45   <2e-16 ***
#   GenderMale  0.266342   0.009859   27.02   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9649 on 38844 degrees of freedom
# Multiple R-squared:  0.01844,	Adjusted R-squared:  0.01842 
# F-statistic: 729.8 on 1 and 38844 DF,  p-value: < 2.2e-16

price_lm_no <- lm(formula = Satisfaction~Price.Sensitivity,data = WN.NO)
summary.lm(price_lm_no)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.5927 -0.4079  0.4073  0.5920  2.1462 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       3.592655   0.012593  285.28   <2e-16 ***
#   PriceSensitivity -0.184707   0.009045  -20.42   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9687 on 38844 degrees of freedom
# Multiple R-squared:  0.01062,	Adjusted R-squared:  0.01059 
# F-statistic:   417 on 1 and 38844 DF,  p-value: < 2.2e-16

year_lm_no <- lm(formula = Satisfaction~Year.of.First.Flight,data=WN.NO)
summary.lm(year_lm_no)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3710 -0.3679  0.6290  0.6479  1.6573 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)       -2.957142   3.334469  -0.887   0.3752  
# YearofFirstFlight  0.003145   0.001661   1.893   0.0583 .
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9738 on 38844 degrees of freedom
# Multiple R-squared:  9.227e-05,	Adjusted R-squared:  6.653e-05 
# F-statistic: 3.584 on 1 and 38844 DF,  p-value: 0.05833


peryear_lm_no <- lm(formula = Satisfaction~Flights.Per.Year,data=WN.NO)
summary.lm(peryear_lm_no)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.6894 -0.6062  0.3106  0.6267  2.4585 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     3.689424   0.008267  446.27   <2e-16 ***
#   FlightsPerYear -0.016637   0.000336  -49.51   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9445 on 38844 degrees of freedom
# Multiple R-squared:  0.05936,	Adjusted R-squared:  0.05934 
# F-statistic:  2451 on 1 and 38844 DF,  p-value: < 2.2e-16

loyalty_lm_no <- lm(formula = Satisfaction ~Loyalty,data=WN.NO)
summary.lm(loyalty_lm_no)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.7684 -0.5784  0.2316  0.7161  1.8677 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 3.445363   0.005475  629.28   <2e-16 ***
#   Loyalty     0.323025   0.009085   35.56   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9584 on 38844 degrees of freedom
# Multiple R-squared:  0.03152,	Adjusted R-squared:  0.0315 
# F-statistic:  1264 on 1 and 38844 DF,  p-value: < 2.2e-16

Type_lm_no <- lm(formula = Satisfaction ~Type.of.Travel,data=WN.NO)
summary.lm(Type_lm_no)
# Min      1Q  Median      3Q     Max 
# -2.7544 -0.5183  0.2456  0.4817  2.4817 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                  3.754364   0.005155  728.25   <2e-16 ***
#   TypeofTravelMileage tickets -0.236014   0.015241  -15.48   <2e-16 ***
#   TypeofTravelPersonal Travel -1.236056   0.008924 -138.50   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7959 on 38843 degrees of freedom
# Multiple R-squared:  0.3322,	Adjusted R-squared:  0.3322 
# F-statistic:  9661 on 2 and 38843 DF,  p-value: < 2.2e-16

freq_lm_no <- lm(formula = Satisfaction ~ Total.Freq.Flyer.Accts,data=WN.NO)
summary.lm(freq_lm_no)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.7748 -0.4314  0.3626  0.7059  1.7060 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         3.294050   0.006269  525.43   <2e-16 ***
#   TotalFreqFlyerAccts 0.068676   0.004308   15.94   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9707 on 38844 degrees of freedom
# Multiple R-squared:  0.006501,	Adjusted R-squared:  0.006476 
# F-statistic: 254.2 on 1 and 38844 DF,  p-value: < 2.2e-16

shopping_lm_no <- lm(formula = Satisfaction ~ Shopping.Amount.at.Airport,data=WN.NO)
summary.lm(shopping_lm_no)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.4374 -0.3703  0.5447  0.6521  1.6521 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             3.348e+00  5.523e-03 606.216  < 2e-16 ***
#   ShoppingAmountatAirport 2.983e-04  9.259e-05   3.222  0.00128 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9737 on 38844 degrees of freedom
# Multiple R-squared:  0.0002671,	Adjusted R-squared:  0.0002414 
# F-statistic: 10.38 on 1 and 38844 DF,  p-value: 0.001276


eating_lm_no <- lm(formula = Satisfaction~Eating.and.Drinking.at.Airport,data=WN.NO)
summary.lm(eating_lm_no)
# Residuals:
# Min      1Q  Median      3Q     Max 
# -2.4927 -0.3792  0.5251  0.6541  1.6742 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                3.326e+00  8.175e-03 406.816   <2e-16 ***
#   EatingandDrinkingatAirport 4.449e-04  9.647e-05   4.612    4e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9736 on 38844 degrees of freedom
# Multiple R-squared:  0.0005473,	Adjusted R-squared:  0.0005216 
# F-statistic: 21.27 on 1 and 38844 DF,  p-value: 3.997e-06


Class_lm_no <- lm(formula = Satisfaction~Class, data=WN.NO)
summary.lm(Class_lm_no)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.5069 -0.3482  0.4931  0.6518  1.7062 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    3.50689    0.01702 206.014   <2e-16 ***
#   ClassEco      -0.15872    0.01788  -8.876   <2e-16 ***
#   ClassEco Plus -0.21307    0.02292  -9.296   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9727 on 38843 degrees of freedom
# Multiple R-squared:  0.002492,	Adjusted R-squared:  0.00244 
# F-statistic: 48.51 on 2 and 38843 DF,  p-value: < 2.2e-16

day_lm_no <- lm(formula = Satisfaction~Day.of.Month,data = WN.NO)
summary.lm(day_lm_no)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3628 -0.3610  0.6372  0.6454  1.6509 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 3.3485873  0.0103781   322.7   <2e-16 ***
#   DayofMonth  0.0004590  0.0005734     0.8    0.423    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9739 on 38844 degrees of freedom
# Multiple R-squared:  1.649e-05,	Adjusted R-squared:  -9.249e-06 
# F-statistic: 0.6407 on 1 and 38844 DF,  p-value: 0.4235

schedule_lm_no <- lm(formula = Satisfaction ~ Scheduled.Departure.Hour,data=WN.NO)
summary.lm(schedule_lm_no)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.4085 -0.3954  0.5980  0.6570  1.7028 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             3.441260   0.015234 225.899  < 2e-16 ***
#   ScheduledDepartureHour -0.006550   0.001106  -5.924 3.17e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9734 on 38844 degrees of freedom
# Multiple R-squared:  0.0009026,	Adjusted R-squared:  0.0008769 
# F-statistic: 35.09 on 1 and 38844 DF,  p-value: 3.171e-09


depatureD_lm_no <- lm(formula = Satisfaction~Departure.Delay.in.Minutes,data=WN.NO)
summary.lm(depatureD_lm_no)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3950 -0.3950  0.6050  0.6203  2.4811 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              3.3949668  0.0055308  613.83   <2e-16 ***
#   DepartureDelayinMinutes -0.0021846  0.0001406  -15.54   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9709 on 38844 degrees of freedom
# Multiple R-squared:  0.006175,	Adjusted R-squared:  0.00615 
# F-statistic: 241.4 on 1 and 38844 DF,  p-value: < 2.2e-16


arrivalD_lm_no <- lm(formula = Satisfaction~Arrival.Delay.in.Minutes,data=WN.NO)
summary.lm(arrivalD_lm_no)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3933 -0.3933  0.6067  0.6136  2.5300 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            3.3932724  0.0054317  624.71   <2e-16 ***
#   ArrivalDelayinMinutes -0.0022853  0.0001401  -16.31   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9706 on 38844 degrees of freedom
# Multiple R-squared:  0.006799,	Adjusted R-squared:  0.006773 
# F-statistic: 265.9 on 1 and 38844 DF,  p-value: < 2.2e-16

time_lm_no <- lm(formula = Satisfaction~Flight.time.in.minutes,data=WN.NO)
summary.lm(time_lm_no)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3593 -0.3584  0.6408  0.6443  1.6559 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          3.361e+00  1.033e-02 325.309   <2e-16 ***
#   Flighttimeinminutes -4.827e-05  9.029e-05  -0.535    0.593    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9739 on 38844 degrees of freedom
# Multiple R-squared:  7.358e-06,	Adjusted R-squared:  -1.839e-05 
# F-statistic: 0.2858 on 1 and 38844 DF,  p-value: 0.5929


distance_lm_no <-lm(formula = Satisfaction ~Flight.Distance,data=WN.NO)
summary.lm(distance_lm_no)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3608 -0.3575  0.6392  0.6449  1.6458 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    3.354e+00  9.363e-03 358.184   <2e-16 ***
#   FlightDistance 3.105e-06  1.130e-05   0.275    0.784    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9739 on 38844 degrees of freedom
# Multiple R-squared:  1.942e-06,	Adjusted R-squared:  -2.38e-05 
# F-statistic: 0.07544 on 1 and 38844 DF,  p-value: 0.7836

five_lm_no <- lm(formula = Satisfaction~Arrival.Delay.greater.5.Mins,data=WN.NO)
summary.lm(five_lm_no)
# Residuals:
#   Min     1Q Median     3Q    Max 
# -2.497 -0.497  0.503  0.503  1.841 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                  3.497017   0.006379  548.23   <2e-16 ***
#   ArrivalDelaygreater5Minsyes -0.338006   0.009872  -34.24   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9595 on 38844 degrees of freedom
# Multiple R-squared:  0.0293,	Adjusted R-squared:  0.02927 
# F-statistic:  1172 on 1 and 38844 DF,  p-value: < 2.2e-16

long_lm_no<- lm(formula = Satisfaction ~Long.Duration.Trip,data=WN.NO)
summary.lm(long_lm_no)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3873 -0.3873  0.6127  0.6781  1.6781 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           3.387282   0.006850 494.519  < 2e-16 ***
#   LongDurationTripTRUE -0.065367   0.009885  -6.613 3.81e-11 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9733 on 38844 degrees of freedom
# Multiple R-squared:  0.001125,	Adjusted R-squared:  0.001099 
# F-statistic: 43.73 on 1 and 38844 DF,  p-value: 3.813e-11



lm.1.no <- lm(Satisfaction ~ Airline.Status + Age + Gender + Price.Sensitivity + Flights.Per.Year + Loyalty + Type.of.Travel + 
                Total.Freq.Flyer.Accts + Eating.and.Drinking.at.Airport + Class + Scheduled.Departure.Hour + Departure.Delay.in.Minutes+
                Arrival.Delay.in.Minutes + Arrival.Delay.greater.5.Mins + Long.Duration.Trip, data=WN.NO)

summary(lm.1.no)

lm.2.no <- lm(Satisfaction ~ Airline.Status + Age + Gender + Price.Sensitivity + Flights.Per.Year + Type.of.Travel + 
                Class + Scheduled.Departure.Hour +  Arrival.Delay.greater.5.Mins + Long.Duration.Trip, data=WN.NO)

summary(lm.2.no)
anova(lm.1.no,lm.2.no) #2 is better

lm.3.no <- lm(Satisfaction ~ Airline.Status + Age + Gender + Price.Sensitivity + Flights.Per.Year + Type.of.Travel + 
                Class + Scheduled.Departure.Hour +  Arrival.Delay.greater.5.Mins, data=WN.NO)

summary.lm(lm.3.no)
anova(lm.2.no,lm.3.no) #3 is better   

#The lm.3.no is the best linear model for the not cancelled flights in WN dataset

# Based on the three linear model I choose, the lm.3.no which represents the linear model based on the not cancelled flights in WN dataset has the biggest R-square value















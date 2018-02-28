MyData <- read.csv(file="M:/PhD/Masters Modules/Understanding Data and their Environment (Manchester)/Report/Assessment/Joined_data_cleaned.csv", header=TRUE, sep=",")
install.packages("ggplot2")

library(ggplot2)


#Check data has inputed correctly by checking head and tail
head(MyData)
tail(MyData)

#Store size cleaning visulisation

#Input store size and type data into a dataframe 

SIZETYPE <- read.csv(file="M:/PhD/Masters Modules/Understanding Data and their Environment (Manchester)/Report/Assessment/Store_size_cleaning.csv", header=TRUE, sep=",")

head(SIZETYPE)
attach(mtcars)
#3 rows, 1 column 
par(mfrow=c(1,1)) 
plot(SIZETYPE$Type, SIZETYPE$Size, ylim=c(0,220000))
lines(30000,0)

#Create different data frames for each store type
#where A = supercentre
#where B = superstore
#where c = supermarket

TypeA <- subset(MyData, MyData$Type == "A")
TypeB <- subset(MyData, MyData$Type == "B")
TypeC <- subset(MyData, MyData$Type == "C")

#Check each dataframe contains the correct store type
head(TypeA)
head(TypeB)
head(TypeC)

attach(mtcars)
#3 rows, 1 column 
par(mfrow=c(1,3)) 

#Look at departments in each store type
plot(TypeA$Store, TypeA$Dept)
plot(TypeB$Store, TypeB$Dept)
plot(TypeC$Store, TypeC$Dept)

attach(mtcars)
#3 rows, 1 column 
par(mfrow=c(1,3)) 

#look at weekly sales by store type
plot(TypeA$Week_Number, TypeA$Weekly_Sales, xlim=c(1,52), ylim=c(0,650000))
plot(TypeB$Week_Number, TypeB$Weekly_Sales, xlim=c(1,52), ylim=c(0,650000))
plot(TypeC$Week_Number, TypeC$Weekly_Sales, xlim=c(1,52), ylim=c(0,650000))

#Note: A, B affected by christmas, C seemignly not affected by holidays 



# look further into whether holidays affectet sales for different store types:

#Create subsets of dataframe to look at each store type by year

#For store A
TypeA2010 <- subset(TypeA, TypeA$Year == "2010")
TypeA2011 <- subset(TypeA, TypeA$Year == "2011")
TypeA2012 <- subset(TypeA, TypeA$Year == "2012")

#For store B
TypeB2010 <- subset(TypeB, TypeB$Year == "2010")
TypeB2011 <- subset(TypeB, TypeB$Year == "2011")
TypeB2012 <- subset(TypeB, TypeB$Year == "2012")

#For store
TypeC2010 <- subset(TypeC, TypeC$Year == "2010")
TypeC2011 <- subset(TypeC, TypeC$Year == "2011")
TypeC2012 <- subset(TypeC, TypeC$Year == "2012")




#How holidays affect store A weekly sales, by year

attach(mtcars)
#1 rows, 1 column 
par(mfrow=c(3,1)) 

#plot Type A, 2010

TypeAIsholiday2010 <-subset(TypeA2010, TypeA2010$IsHoliday == 'TRUE')
head(TypeAIsholiday2010)
TypeAIsNOTholiday2010 <-subset(TypeA2010, TypeA2010$IsHoliday == 'FALSE')
head(TypeAIsNOTholiday2010)

plot(TypeAIsholiday2010$Week_Number, TypeAIsholiday2010$Weekly_Sales, col ='red', sub = 'Holidays and Weekly Sales For Store A (2010)', ylab
     = 'Weekly Sales ($)', xlab = "Week of the Year", xlim=c(1,52), ylim=c(0,700000), pch =18,cex=0.75)
points(TypeAIsNOTholiday2010$Week_Number, TypeAIsNOTholiday2010$Weekly_Sales, col = 'blue', pch =18, cex=0.75)

#plot Type A, 2011

TypeAIsholiday2011 <-subset(TypeA2011, TypeA2011$IsHoliday == 'TRUE')
head(TypeAIsholiday2011)
TypeAIsNOTholiday2011 <-subset(TypeA2011, TypeA2011$IsHoliday == 'FALSE')
head(TypeAIsNOTholiday2011)

plot(TypeAIsholiday2011$Week_Number, TypeAIsholiday2011$Weekly_Sales, col ='red', sub = 'Holidays and Weekly Sales For Store A (2011)', ylab
     = 'Weekly Sales ($)', xlab = "Week of the Year", xlim=c(1,52), ylim=c(0,700000) , pch =18,cex=0.75)
points(TypeAIsNOTholiday2011$Week_Number, TypeAIsNOTholiday2011$Weekly_Sales, col = 'blue', pch =18,cex=0.75)

#plot Type A, 2012

TypeAIsholiday2012 <-subset(TypeA2012, TypeA2012$IsHoliday == 'TRUE')
head(TypeAIsholiday2012)
TypeAIsNOTholiday2012 <-subset(TypeA2012, TypeA2012$IsHoliday == 'FALSE')
head(TypeAIsNOTholiday2012)

plot(TypeAIsholiday2012$Week_Number, TypeAIsholiday2012$Weekly_Sales, col ='red', sub = 'Holidays and Weekly Sales For Store A (2012)', ylab
     = 'Weekly Sales ($)', xlab = "Week of the Year", xlim=c(1,52), ylim=c(0,700000), pch =18,cex=0.75)
points(TypeAIsNOTholiday2012$Week_Number, TypeAIsNOTholiday2012$Weekly_Sales, col = 'blue', pch =18,cex=0.75)



#How holidays affect store B weekly sales, by year

attach(mtcars)
#1 rows, 1 column 
par(mfrow=c(3,1)) 

#plot Type B, 2010

TypeBIsholiday2010 <-subset(TypeB2010, TypeB2010$IsHoliday == 'TRUE')
head(TypeBIsholiday2010)
TypeBIsNOTholiday2010 <-subset(TypeB2010, TypeB2010$IsHoliday == 'FALSE')
head(TypeBIsNOTholiday2010)

plot(TypeBIsholiday2010$Week_Number, TypeBIsholiday2010$Weekly_Sales, col ='red', sub = 'Holidays and Weekly Sales For Store B (2010)', ylab
     = 'Weekly Sales ($)', xlab = "Week of the Year", xlim=c(1,52), ylim=c(0,700000), pch =18,cex=0.75)
points(TypeBIsNOTholiday2010$Week_Number, TypeBIsNOTholiday2010$Weekly_Sales, col = 'blue', pch =18,cex=0.75)

#plot Type B, 2011

TypeBIsholiday2011 <-subset(TypeB2011, TypeB2011$IsHoliday == 'TRUE')
head(TypeBIsholiday2011)
TypeBIsNOTholiday2011 <-subset(TypeB2011, TypeB2011$IsHoliday == 'FALSE')
head(TypeBIsNOTholiday2011)

plot(TypeBIsholiday2011$Week_Number, TypeBIsholiday2011$Weekly_Sales, col ='red', sub = 'Holidays and Weekly Sales For Store B (2011)', ylab
     = 'Weekly Sales ($)', xlab = "Week of the Year", xlim=c(1,52), ylim=c(0,700000), pch =18,cex=0.75)
points(TypeBIsNOTholiday2011$Week_Number, TypeBIsNOTholiday2011$Weekly_Sales, col = 'blue', pch =18,cex=0.75)

#plot Type B, 2012

TypeBIsholiday2012 <-subset(TypeB2012, TypeB2012$IsHoliday == 'TRUE')
head(TypeBIsholiday2012)
TypeBIsNOTholiday2012 <-subset(TypeB2012, TypeB2012$IsHoliday == 'FALSE')
head(TypeBIsNOTholiday2012)

plot(TypeBIsholiday2012$Week_Number, TypeBIsholiday2012$Weekly_Sales, col ='red', sub = 'Holidays and Weekly Sales For Store B (2012)', ylab
     = 'Weekly Sales ($)', xlab = "Week of the Year", xlim=c(1,52), ylim=c(0,700000), pch =18,cex=0.75)
points(TypeBIsNOTholiday2012$Week_Number, TypeBIsNOTholiday2012$Weekly_Sales, col = 'blue', pch =18,cex=0.75)



#How holidays affect store C weekly sales, by year

attach(mtcars)
#1 rows, 1 column 
par(mfrow=c(3,1)) 

#plot Type C, 2010

TypeCIsholiday2010 <-subset(TypeC2010, TypeC2010$IsHoliday == 'TRUE')
head(TypeCIsholiday2010)
TypeCIsNOTholiday2010 <-subset(TypeC2010, TypeC2010$IsHoliday == 'FALSE')
head(TypeCIsNOTholiday2010)

plot(TypeCIsholiday2010$Week_Number, TypeCIsholiday2010$Weekly_Sales, col ='red', sub = 'Holidays and Weekly Sales For Store C (2010)', ylab
     = 'Weekly Sales ($)', xlab = "Week of the Year", xlim=c(1,52), ylim=c(0,700000), pch =18,cex=0.75)
points(TypeCIsNOTholiday2010$Week_Number, TypeCIsNOTholiday2010$Weekly_Sales, col = 'blue', pch =18,cex=0.75)

#plot Type C, 2011

TypeCIsholiday2011 <-subset(TypeC2011, TypeC2011$IsHoliday == 'TRUE')
head(TypeCIsholiday2011)
TypeCIsNOTholiday2011 <-subset(TypeC2011, TypeC2011$IsHoliday == 'FALSE')
head(TypeCIsNOTholiday2011)

plot(TypeCIsholiday2011$Week_Number, TypeCIsholiday2011$Weekly_Sales, col ='red', sub = 'Holidays and Weekly Sales For Store C (2011)', ylab
     = 'Weekly Sales ($)', xlab = "Week of the Year", xlim=c(1,52), ylim=c(0,700000), pch =18,cex=0.75)
points(TypeCIsNOTholiday2011$Week_Number, TypeCIsNOTholiday2011$Weekly_Sales, col = 'blue', pch =18,cex=0.75)

#plot Type C, 2012

TypeCIsholiday2012 <-subset(TypeC2012, TypeC2012$IsHoliday == 'TRUE')
head(TypeCIsholiday2012)
TypeCIsNOTholiday2012 <-subset(TypeC2012, TypeC2012$IsHoliday == 'FALSE')
head(TypeCIsNOTholiday2012)

plot(TypeCIsholiday2012$Week_Number, TypeCIsholiday2012$Weekly_Sales, col ='red', sub = 'Holidays and Weekly Sales For Store C (2012)', ylab
     = 'Weekly Sales ($)', xlab = "Week of the Year", xlim=c(1,52), ylim=c(0,700000), pch =18,cex=0.75)
points(TypeCIsNOTholiday2012$Week_Number, TypeCIsNOTholiday2012$Weekly_Sales, col = 'blue', pch =18,cex=0.75)

#check correaltion between weekly sales and vairbales for store A
cor(TypeA$Weekly_Sales, TypeA$Fuel_Price)
cor(TypeA$Weekly_Sales, TypeA$Temperature)
cor(TypeA$Weekly_Sales, TypeA$CPI)
cor(TypeA$Weekly_Sales, TypeA$Unemployment)
cor(TypeA$Weekly_Sales, TypeA$Size..sq.ft.)


#check correaltion between weekly sales and vairbales for store A
cor.test(TypeA$Weekly_Sales, TypeA$Fuel_Price)
cor.test(TypeA$Weekly_Sales, TypeA$Temperature)
cor.test(TypeA$Weekly_Sales, TypeA$CPI)
cor.test(TypeA$Weekly_Sales, TypeA$Unemployment)
cor.test(TypeA$Weekly_Sales, TypeA$Size..sq.ft.)

#check correaltion between weekly sales and vairbales for store B
cor.test(TypeB$Weekly_Sales, TypeB$Fuel_Price)
cor.test(TypeB$Weekly_Sales, TypeB$Temperature)
cor.test(TypeB$Weekly_Sales, TypeB$CPI)
cor.test(TypeB$Weekly_Sales, TypeB$Unemployment)
cor.test(TypeB$Weekly_Sales, TypeB$Size..sq.ft.)

#check correaltion between weekly sales and vairbales for store C
cor.test(TypeC$Weekly_Sales, TypeC$Fuel_Price)
cor.test(TypeC$Weekly_Sales, TypeC$Temperature)
cor.test(TypeC$Weekly_Sales, TypeC$CPI)
cor.test(TypeC$Weekly_Sales, TypeC$Unemployment)
cor.test(TypeC$Weekly_Sales, TypeC$Size..sq.ft.)



attach(mtcars)
#1 rows, 1 column 
par(mfrow=c(1,1)) 
plot(MyData$Dept,MyData$Weekly_Sales)



TypeA2010_Week47 <- subset(TypeA2010, TypeA2010$Week_Number == "47")
head(TypeA2010_Week47)

TypeA2010_Week46 <- subset(TypeA2010, TypeA2010$Week_Number == "46")
head(TypeA2010_Week46)
plot(TypeA2010_Week47$Dept,TypeA2010_Week47$Weekly_Sales, col = 'red')
points(TypeA2010_Week46$Dept,TypeA2010_Week46$Weekly_Sales, col ='blue')

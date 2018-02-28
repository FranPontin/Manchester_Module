MyData <- read.csv(file="M:/PhD/Masters Modules/Understanding Data and their Environment (Manchester)/Report/Assessment/WORK_QUERY_FOR_TRAIN_2_0000.csv", header=TRUE, sep=",")
install.packages("ggplot2")

library(ggplot2)


#Check data has inputed correctly by checking head and tail
head(MyData)
tail(MyData)



#Plot store type against store size
plot(MyData$Type, MyData$Size..sq.ft.)
#Obvious correlation between store size and type, outliers identifiable


#How many of each department type there are (possible total)
table(MyData$Dept)

#Plot week number against departments, identify where departments are missing
plot(MyData$Week_Number,MyData$Dept)


#if(MyData$Year == 2010) {plot(MyData$Week_Number, MyData$Weekly_Sales, type="h"}

#work out how many records are in each year:
table(MyData$Year)

# create subsets for the years 2010, 2011, 2012 

My2010 <- subset(MyData, MyData$Year == "2010")
head(My2010)
My2011 <- subset(MyData, MyData$Year == "2011")
head(My2011)
My2012 <- subset(MyData, MyData$Year == "2012")
head(My2012)


MyIsholiday <-subset(MyData, MyData$IsHoliday == 'TRUE')
head(MyIsholiday)
MyIsNOTholiday <-subset(MyData, MyData$IsHoliday == 'FALSE')
head(MyIsNOTholiday)

plot(MyIsholiday$Week_Number, MyIsholiday$Weekly_Sales, col ='red', main = 'Holidays and Weekly Sales', ylab
= 'Weekly Sales ($)', xlab = "Week of the Year")
points(MyIsNOTholiday$Week_Number, MyIsNOTholiday$Weekly_Sales, col = 'blue')

#change limit where number becomes exponential
options(scipen=10000)
#plot 3 graphs on one plot
attach(mtcars)
#3 rows, 1 column 
par(mfrow=c(3,1)) 

# plot weekly sales per week of 2010
MyIsholiday2010 <-subset(My2010, My2010$IsHoliday == 'TRUE')
head(MyIsholiday2010)
MyIsNOTholiday2010 <-subset(My2010, My2010$IsHoliday == 'FALSE')
head(MyIsNOTholiday2010)

plot(MyIsholiday2010$Week_Number, MyIsholiday2010$Weekly_Sales, col ='red', main = 'Holidays and Weekly Sales 2010', ylab
     = 'Weekly Sales ($)', xlab = "Week of the Year", xlim=c(1,52), ylim=c(0,650000))
points(MyIsNOTholiday2010$Week_Number, MyIsNOTholiday2010$Weekly_Sales, col = 'blue')

# plot weekly sales per week of 2011
MyIsholiday2011 <-subset(My2011, My2011$IsHoliday == 'TRUE')
head(MyIsholiday2011)
MyIsNOTholiday2011 <-subset(My2011, My2011$IsHoliday == 'FALSE')
head(MyIsNOTholiday2011)

plot(MyIsholiday2011$Week_Number, MyIsholiday2011$Weekly_Sales, col ='red', main = 'Holidays and Weekly Sales 2011', ylab
     = 'Weekly Sales ($)', xlab = "Week of the Year", xlim=c(1,52), ylim=c(0,650000))
points(MyIsNOTholiday2011$Week_Number, MyIsNOTholiday2011$Weekly_Sales, col = 'blue')

# plot weekly sales per week of 2012
MyIsholiday2012 <-subset(My2012, My2012$IsHoliday == 'TRUE')
head(MyIsholiday2012)
MyIsNOTholiday2012 <-subset(My2012, My2012$IsHoliday == 'FALSE')
head(MyIsNOTholiday2012)

plot(MyIsholiday2012$Week_Number, MyIsholiday2012$Weekly_Sales, col ='red', main = 'Holidays and Weekly Sales 2012', ylab
     = 'Weekly Sales ($)', xlab = "Week of the Year", xlim=c(1,52), ylim=c(0,650000))
points(MyIsNOTholiday2012$Week_Number, MyIsNOTholiday2012$Weekly_Sales, col = 'blue')



cor.test(MyData$Weekly_Sales, MyData$Fuel_Price)
cor.test(MyData$Weekly_Sales, MyData$Temperature)
cor.test(MyData$Weekly_Sales, MyData$CPI)
cor.test(MyData$Weekly_Sales, MyData$Unemployment)
cor.test(MyData$Weekly_Sales, MyData$Size..sq.ft.)


plot(My2010$Week_Number, My2010$Fuel_Price)
plot(My2010$Fuel_Price, My2010$Weekly_Sales)

plot(My2011$Week_Number, My2011$Fuel_Price)
plot(My2012$Week_Number, My2012$Fuel_Price)

summary(lm(MyData$Weekly_Sales~MyData$Fuel_Price))

MyStore1 <-subset(MyData, MyData$Store == "1")
plot(MyStore1$Fuel_Price, MyStore1$Weekly_Sales)

plot(MyStore1$Week_Number, MyStore1$Weekly_Sales)

cor(MyStore1$Weekly_Sales, MyStore1$Fuel_Price)
cor(MyStore1$Weekly_Sales, MyStore1$Temperature)

TypeA <- subset(MyData, MyData$Type == 'A')

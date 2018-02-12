MyData <- read.csv(file="M:/PhD/Masters Modules/Understanding Data and their Environment (Manchester)/Report/Assessment/WORK_QUERY_FOR_TRAIN_2_0000.csv", header=TRUE, sep=",")


#Check data has inputed correctly by checking head and tail
head(MyData)
tail(MyData)


#Plot Temperature against sales
plot(MyData$Temperature, MyData$Weekly_Sales)


#Plot stores against sales
plot(MyData$Store, MyData$Weekly_Sales)


#Plot store type against sales
plot(MyData$Type, MyData$Weekly_Sales)


#Plot store type against store size
plot(MyData$Type, MyData$Size..sq.ft.)
#Obvious correlation between store size and type, outliers identifiable


#How many of each department type there are (possible total)
table(MyData$Dept)

#Plot week number against departments, identify where departments are missing
plot(MyData$Week_Number,MyData$Dept)

#Plot 
plot(MyData$Week_Number, MyData$Weekly_Sales, type="h")

if(MyData$Year == 2010) {plot(MyData$Week_Number, MyData$Weekly_Sales, type="h"}

  
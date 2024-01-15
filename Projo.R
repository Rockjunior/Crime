library(readxl)
crime = read_xlsx("C:/Users/ROMROM/Desktop/crime_raw.xlsx") #Change the directory path
attach(crime)
View(crime)

crime = data.frame(crime)

#checkout
names(crime)
str(crime)
head(crime)
tail(crime)

#summary
summary(crime)
summary.warnings(crime)

##mean
#col means

mean(crime[["Homicide"]])

#subset m to get means
colMeans(crime[, c(3:19)])

colMeans(crime[,c(3,6,7,9,16)])

#row means
colMeans(crime[, 3:19])
rowMeans(crime)
#mean of Homicide and Robbery
rowMeans(crime[,c("Homicide","Robbery")])
#calculate row means of the crime for all years
Avg_crimes_per_year = rowMeans(crime[, 3:19], na.rm = TRUE)
#create and insert new col for yearly averare crime rate
crime$Avg_crime_rate = Avg_crimes_per_year
head(crime)

#aggregate
aggregate(Homicide~Year, summary, data = crime)

#Variance
var(crime[,3:19])

#correlation
cor(crime$Corruption, crime$OFFAP)
cor(crime[,3:19])

#covariance
cov(crime[, 3:19])
cov(crime$Corruption, crime$OFFAP)


#print the means of crimes per year
library(dplyr)
Yrly_Avg_crime_rate = select(crime, Year, Avg_crime_rate)
print(Yrly_Avg_crime_rate)

#plot the yearly average crime rate

#check the outliers in the yearly averae crime rate
boxplot(crime$Avg_crime_rate)

#other varia varibales
boxplot(crime[, 3:19])

library(ggplot2)
#Line plot
plot(data = crime, Year, Avg_crimes_per_year,
     type = "o", frame.plot = TRUE,
     pch = 19, col = "blue", xlab = "Year.",
     ylab = "Avarage crimes per Year.")

#bar plot
barplot(Avg_crimes_per_year~Year, data = crime)
#barplot2
ggplot(crime, aes(factor(Year), Avg_crimes_per_year, fill = factor(Year))) +
  geom_col()

ggplot(crime,aes(Avg_crimes_per_year, fill = factor(Year))) +
  geom_histogram(color = "lightgray")+
  facet_wrap(~Year, nrow = 11)

#box plot
library(lattice)
bwplot(Breaking~Year)

plot(Homicide~Year,
     data = crime,
     main = "Homicide.",
     xlab="Year",
     ylab = "Rate of Homicide.",
     cex = 0.75,
     ylim = c(0,3500))
points(Breaking~Year,
       data = crime,
       pch = 1,
       col = "red",
       cex = 0.8)
grid()
legend("bottomright",
       legend = c("Homicide"),
       col = "black","red",
       pch = 3)
lines(lowess(crime$Year, crime$Homicide))
lines(lowess(crime$Year, crime$Breaking))
lines(lowess(crime$Year[!is.na(crime$Breaking)],
             na.omit(crime$Breaking)),
      col = "red")


#boxplot
boxplot(Breaking~Year,
        data = crime,
        ylab = "Years",
        xlab = "Breakin Crime rates",
        horizontal = TRUE,
        col = "blue")
boxplot(crime[,3]~crime[,5],
        main = "Boxplot of Homicide and OFFAP.",
        ylab = "Homicide",
        xlab = "OFFAP",
        col = "lightgray",
        boxlty = 3,
        whisklty = 1.5,
        horizontal = TRUE)

#pairs
pairs(crime[,3:13])
pairs(crime[14:19])

#scatterplot
smoothScatter(crime$Homicide, crime$Robbery)

#smooth scatter line plot
scatter.smooth(crime$OFFAM,
               crime$Robbery,
               main = "Distribution and realionship between OFFAM and Robbery",
               xlab = "OFFAM",
               ylab = "Robbery")
#heatmap
heetmap = as.matrix(dist(crime[,3:19]))
heatmap(heetmap)# to be continued for editing

#qplot
qplot(Homicide, Breaking, data = crime, facets = Year ~.)






#Regresion







library(datasets)
data("airquality")
airquality$Temp

summary(airquality$Temp)
boxplot(airquality$Temp, col ="blue")

boxplot(airquality$Temp, col ="blue")
abline(h=75 )


hist(airquality$Temp, col="green")
rug(airquality$Temp)
hist(airquality$Temp, col="green", breaks = 20)
abline(v=65)
abline(v=median(airquality$Temp), col="magenta", lwd = 10)


barplot(table(airquality$Month), col="wheat", main = "Number of Monthd in each month")
table(airquality$Month)


boxplot(Ozone~Temp, data = airquality, col="red")

par(mfrow=c(2,1), mar = c(4,4,2,1))
hist(subset(airquality, Month = "8")$Ozone, col="green")
hist(subset(airquality, Month = "6")$Ozone, col="green")

with(airquality, plot(Temp, Ozone))
abline(h=70, lwd = 2, lty = 3) 
str(airquality)
airquality$Mon = as.factor(airquality$Month)
with(airquality, plot(Temp, Ozone, color =Mon))
abline(h=70, lwd = 2, lty = 3) 
str(airquality)

par(mfrow=c(1,2), mar = c(5,4,2,1))
with(subset(airquality, Mon = "8"), plot(Temp, Ozone), main="8" )
with(subset(airquality, Mon = "6"), plot(Temp, Ozone), main="6" )


#Plotting
#base plot
library(datasets)
data(cars)
with(cars,  plot(speed, dist))

# Lattice plot
library(lattice)
state<- data.frame(state.x77, region = state.region)
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4,1))

#ggplot2 system (grammer graphics)
library(ggplot2)
data(mpg)
qplot(displ, hwy, data = mpg)



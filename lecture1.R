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

library(datasets)
airquality<-transform(airquality, Month = factor(Month))
boxplot(Ozone ~Month, airquality, xlab = "Month", ylab = "Ozone(ppb)")

par("lty")
par("col")
par("pch")
par("bg")
par("mar")
par("mfrow")
par("mfcol")

with(airquality, plot(Wind, Ozone))
title(main="Ozone and Wind in New York City")
with(airquality, plot(Wind, Ozone, main="Ozone and Wind in New York City"), type = "n")
with(subset(airquality, Month ==5), points(Wind, Ozone, col = "red"))
with(subset(airquality, Month !=5), points(Wind, Ozone, col = "blue"))
legend("topright", pch =1, col=c("blue", "red"), legend=c("May", "Other Months"))

# add regression line to plots
with(airquality, plot(Wind, Ozone, main="Ozone and Wind"), pch =20)
model<-lm(Ozone~Wind, airquality)
abline(model, lwd=2)

#multiple base plots
par(mfrow = c(1,3))
with(airquality, {
    plot(Wind, Ozone, main="Ozone and wind")
    plot(Solar.R, Ozone, main ="Ozone and Solar Radiation")
    plot(Temp, Ozone, main ="Ozone and Temperature")
})

par(mfrow = c(1,3), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(airquality, {
    plot(Wind, Ozone, main="Ozone and wind")
    plot(Solar.R, Ozone, main ="Ozone and Solar Radiation")
    plot(Temp, Ozone, main ="Ozone and Tem")
    mtext("Ozone and Weather in NYC", outer = TRUE)
})


par(mfrow=c(1,1))
x<- rnorm(100)
hist(x)
y <- rnorm(100)
plot(x,y, pch =20)
legend("topleft", legend="Data", pch=20)
text(-2,-2, "Label")
z<-rpois(100,2)
par(mfrow=c(2,1))
plot(x,y,pch=20)
plot(x,z,pch=19)
par("mar")
z
hist(z)
z<-rpois(1000,5)
hist(z)
rug(z)
str(rug)
zz=rnorm(1000)
hist(zz)
rug(zz)

par(mfrow=c(1,1))
x<-rnorm(100)
y<-x+rnorm(100)
g<-gl(2,50, labels = c("Male", "Female") )
str(g)
g
plot(x, y, type = "n")
points(x[g=="Male"],y[g=="Male"], col="blue")
points(x[g=="Female"],y[g=="Female"], col="red", pch = 19)

dev.copy(png, file="mytest.png")
dev.off()
getwd()

str(pollution)
str(mpg)
help(siwrl)
hist(airquality$Ozone)

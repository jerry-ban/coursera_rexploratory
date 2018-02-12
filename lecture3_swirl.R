library(swirl)
swirl()
#lattice plot
library(lattice)
library(ggplot2)

head(airquality)
xyplot(Ozone~Wind, data=airquality)
xyplot(Ozone~Wind, data=airquality, pch=8, col="red", main="Big Apple Data")
xyplot(Ozone~Wind|as.factor(Month), data=airquality, layout=c(5,1))
xyplot(Ozone~Wind|Month, data=airquality, layout=c(5,1))

p<- xyplot(Ozone~Wind, data = airquality)
p
names(p)
mynames[myfull]  #to show not null properties
p[["formula"]]
p[["x.limits"]]
xyplot(y~x|f)
xyplot(y~x|f, layout=c(2,1))

#setwd("C:/tools/R/R-3.4.3/library/swirl/Courses/Exploratory_Data_Analysis/Lattice_Plotting_System")
source(pathtofile("plot1.R"),local=TRUE)
p <- xyplot(y ~ x | f, panel = function(x, y, ...) {
    panel.xyplot(x, y, ...)  ## First call the default panel function for 'xyplot'
    panel.abline(h = median(y), lty = 2)  ## Add a horizontal line at the median
})

p2 <- xyplot(y ~ x | f, panel = function(x, y, ...) {
    panel.xyplot(x, y, ...)  ## First call default panel function
    panel.lmline(x, y, col = 2)  ## Overlay a simple linear regression line
})
print(p2)

str(diamonds)
table(diamonds$color)
table(diamonds$color, diamonds$cut)
myxlab <- "Carat"
myylab <- "Price"
mymain <- "Diamonds are Sparkly!"
xyplot(price~carat|color*cut, data=diamonds, strip=FALSE, pch=20, xlab=myxlab, ylab=myylab, main=mymain)
xyplot(price~carat|color*cut, data=diamonds, pch=20, xlab=myxlab, ylab=myylab, main=mymain)
# the strip argument labels each panel

heat.colors(5)
topo.colors()
sample(colors(), 10)
pal<-colorRamp(c("red", "blue")) # will never includes green (RGB)
pal(0)  # vlaue between 0-1
pal(seq(0.5))
pal(seq(0,1,len=6))

p1<-colorRampPalette(c("red", "blue"))
p1(2)
p1(6)
0xcc

p2<-colorRampPalette(c("red","yellow"))

?rgb

p3<-colorRampPalette(c("blue","green"), alpha = 0.5)
p3(5)

plot(x,y, pch=19, col=rgb(0,0.5,0.5))
plot(x,y, pch=19, col=rgb(0,0.5,0.5, 0.3))

#  RColorBrewer Package, 3 types:sequential, divergent, and qualitative
cols<- brewer.pal(3, "BuGn")
showMe(cols)
pal<- colorRampPalette(cols)
showMe(pal(3))
showMe(pal(20))

image(volcano, col=pal(20))


str(mpg)
qplot(displ,hwy, data=mpg)
qplot(displ,hwy, data=mpg, color=drv)
qplot(displ,hwy, data=mpg, color=drv, geom=c("point", "smooth"))

qplot(y=hwy, data=mpg, color=drv) # without x, plot the y values in it's order in data set
qplot(drv, hwy, data=mpg, geom="boxplot")
qplot(drv, hwy, data=mpg, geom="boxplot", color=manufacturer)

qplot(hwy, data=mpg, fill=drv) #hist plot

qplot(displ, hwy, data=mpg, facets=.~drv) # 3 columns of plot 
qplot(hwy, data=mpg, facets=drv~., binwidth=2) #3 row of hist plots


qplot(displ, hwy, data=mpg, geom=c("point", "smooth"), facets = .~drv)

g<- ggplot(mpg, aes(displ, hwy) )
g+geom_point()
g+geom_point()+geom_smooth()
g+geom_point()+geom_smooth(method="lm")
g+geom_point()+geom_smooth(method="lm") + facet_grid(.~drv)

g+geom_point()+geom_smooth(method="lm") + facet_grid(.~drv) + ggtitle("Swirl Rules!")
g+geom_point(aes(color=drv), size=4, alpha=0.5)
g+geom_point(aes(color=drv)) + labs(title="Swirl Rules!") + labs(x="Displacement", y="Hwy Mileage")

g+geom_point(color="pink", size=4, alpha=0.5)
g+geom_point(aes(color=drv), size=4, alpha=0.5)
g + geom_point(aes(color = drv),size=2,alpha=1/2) + geom_smooth(size=4,linetype=3,method="lm",se=FALSE)

g+geom_point(aes(color=drv)) + theme_bw(base_family="Times")


plot(myx, myy, type="l", ylim=c(-3,3))

g <- ggplot(testdat, aes(myx, myy))
g+geom_line()
g+geom_line() + ylim(-3,3) # this will show plot without outlier
g+geom_line() + coord_cartesian(ylim=c(-3,3)) # will show all data but outlier will be outlise of plot(not visible)


g<- ggplot(mpg, aes(x=displ,y=hwy, color=factor(year)))
g+ geom_point()
g+ geom_point() + facet_grid(drv~cyl, margins=TRUE) # 4 by 5 plots, with all by all added
g+ geom_point() + facet_grid(drv~cyl, margins=TRUE) + geom_smooth(method="lm", se=FALSE, size=2,color="black")
g+ geom_point() + facet_grid(drv~cyl, margins=TRUE) + geom_smooth(method="lm", se=FALSE, size=2,color="black") + labs(x="Displacement", y="Highway Mileage", title="Swirl Rules!")


str(diamonds)
qplot(price, data = diamonds)
range(diamonds$price)
qplot(price, data = diamonds, binwidth=18497/30, fill=cut)

qplot(price, data=diamonds, geom="density")
qplot(price, data=diamonds, geom="density", color=cut)

qplot(carat, price, data=diamonds)
qplot(carat, price, data=diamonds, shape=cut)
qplot(carat, price, data=diamonds, color=cut)
qplot(carat, price, data=diamonds, color=cut, geom_smooth(method="lm)"))
qplot(carat, price, data=diamonds, color=cut, facets = .~cut) + geom_smooth(method="lm")



g<- ggplot(diamonds, aes(depth, price))

g+geom_point(alpha=1/3)

#to check relationship (between depth and price) is affected by cut or carat, namely new levels 
cutpoints<-quantile(diamonds$carat, seq(0,1,length=4), na.rm=TRUE)
diamonds$car2 <- cut(diamonds$carat, cutpoints)
g<-ggplot(diamonds, aes(depth, price))
diamonds$car2 <- cut(diamond$carat, cutpoints) #NA if value is exact lower border of the cuts
g + geom_point(alpha=1/3) + facet_grid(cut~car2) + geom_smooth(method="lm", size=3, color="pink")

ggplot(diamonds, aes(cared, price)) + geom_boxplot()+ facet_grid(.~cut)




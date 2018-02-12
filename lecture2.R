library(datasets)
library(lattice)

xyplot(Ozone~Wind, data=airquality)

#with month as factor
airquality<-transform(airquality, Month=factor(Month))
xyplot(Ozone~Wind| Month, data = airquality, layout = c(5,1)) 
 
set.seed(10)
x<-rnorm(100)
f<-rep(0:1, each =50)
y<- x+f -f*x + rnorm(100, sd=0.5)
f<-factor(f, labels = c("Group1", "Group2"))
xyplot(y~x|f, layout=c(2,1))

xyplot(y~x|f, panel=function(x,y,...){
    panel.xyplot(x,y,...)
    panel.abline(h=median(y), lty=2)
})


xyplot(y~x|f, panel=function(x,y,...){
    panel.xyplot(x,y,...)
    panel.lmline(x,y, col=2)
})

library(ggplot2)
str(mpg)
qplot(displ, hwy, data = mpg)
qplot(displ, hwy, data = mpg, color=drv)
qplot(displ, hwy, data = mpg, geom = c("point","smooth"))
qplot(hwy, data = mpg, fill = drv)

#Facets
qplot(displ, hwy, data=mpg, facets=.~drv)
#facts parameters: right: determines the columns of the panel, the left side indicates the rows of the plot
# so the plot will be 1 row and 3 columns in this case

qplot(hwy, data=mpg,facets= drv ~ .,  binwidth=2)

getwd()
setwd("C:/_research/coursera_rexplor")
maacs= load("maacs.rda")
str(maacs)
qplot(log(eno),data=maacs)
qplot(log(eno), data=maacs, fill=mopos)
qplot(log(eno), data=maacs, geom="density")
qplot(log(eno), data=maacs, geom="density", color=mopos)
qplot(log(pm25), log(eno), data=maacs)
qplot(log(pm25), log(eno), data=maacs, shape=mopos)
qplot(log(pm25), log(eno), data=maacs, color=mopos)
qplot(log(pm25), log(eno), data=maacs, color=mopos) + geom_smooth(method="lm")
qplot(log(pm25), log(eno), data=maacs, facets=.~mopos) + geom_smooth(method="lm")

qplot(logpm25, NocturnalSympt, data=maacs, facets= . ~ mopos, geom =c("point", "smooth"), method ="lm")
head(maacs[, 1:3])


### step by step to build above plot+
g<-qplot(maacs, aes(logpm25, NocturnalSympt))
summary(g)
p<-g + geom_point()  #explitly save and print ggplot object
print(p)

g + geom_point()  # auto print plot object without saving
g+geom_point() + geom_smooth()
g+geom_point() + geom_smooth(mtthod="lm")
g+geom_point() + facet_grid(.~bmicat) + geom_smooth(mtthod="lm")

g + geom_point(color="steelblue", size=4,alpha=1/2)  # change color to half transparent
g + geom_point(aes(color=bmicat), size=4,alpha=1/2)  # set color to a  data variable, wrapped in aes()
g + geom_point(aes(color=bmicat))+ labs(title="MAACS Cohort") + labs(x=expression("log " * PM[2.5]), y="Noctunal Symptoms") 
g + geom_point(aes(color=bmicat), size=2,alpha=1/2) + geom_smooth(size=4,linetype=3, mtthod="lm", se=FALSE) # turn off confidence intervals
g + geom_point(aes(color=bmicat))+theme_bw(base_family = "Times")


qplot(logpm25, NocturnalSympt, data=maacs, facets= . ~ mopos, geom =c("point", "smooth"), method ="lm")
head(maacs[, 1:3])

#about the Axis Limits
testdat<- data.frame(x=1:100, y=rnorm(100))
testdat[50,2]<-100  #outlier!
plot(testdat$x, testdat$y, type = "l", ylim=c(03,3))

g<-ggplot(testdat, aes(x=x, y=y))
g+geom_line()
g + geom_line() + ylim(-3,3) #outlier  missed
g + geom_line() + coord_cartesian(ylim=c(-3,3)) # outlier included
#if a condition variable is continuous, cut() function may help to convert it to categorical/factor variable
cutpoints <- quantile(maacs$logno2_new, seq(0,1, length =4), na.rm = TRUE)

#cut the data at the deciles and create a new factor variable
maacs$no2dec <- cut(maccs$logno2_new, cutpoints)
# see the levels  off the newl created factor variable
levels(maccs$no2dec)

#2*4 plots
g<-ggplot(maacs, aes(logpm25, NocturnalSympt))
g   + geom_point(alpha=1/3)
    + facet_wrap(bmicat~no2dec, nrow=2, ncol =4)
    + geom_smooth(method="lm", se=FALSE, col="steelblue")
    + theme_bw(base_family = "Avenir", base_size=10)
    + labs(x=expression("log " * PM[2.5]))
    + labs(y= "Nocturnal Sysmptoms")
    + labs(title = "MAACS Cohort")

#





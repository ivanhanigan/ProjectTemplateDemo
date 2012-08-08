
########################
#Synthetic dataset:
########################

#Timedata
years<-2004:2007
months<-1:12
lines<-1:10
timedata<-merge(months,years)
timedata<-merge(timedata,data.frame(id=lines))
names(timedata)<-c("months","years","id")
timedata$date<-as.Date(apply(timedata[,c("years","months")],1,function(x) sprintf("%d-%02d-01",x[1],x[2])))
timedata$count<-timedata$id*6+as.numeric(timedata$date)/2000+round(rnorm(mean=0,sd=3,n=nrow(timedata)))
timedata<-subset(timedata,select=-c(years,months))
write.csv(timedata,"timedata.csv",row.names=FALSE)


########################
#Dealing with many time series at once
#Data reshaping
timedata_wide<-reshape(subset(timedata,select=-year),direction="wide",idvar="date",timevar="line")
str(timedata)
str(timedata_wide)
timedata_wide_ts<-ts(
	data=subset(timedata_wide,select=-date),
	start=min(timedata$year),
	freq=12
)
plot(timedata_wide_ts)

#Stl decomposition is only for univariate series
#Need programming concepts:

#Looping through first three time series
colnames(timedata_wide_ts)
par(ask=TRUE)
for (series in colnames(timedata_wide_ts)[1:3]){
	s<-timedata_wide_ts[,series]
	print(s)
	plot(stl(s,s.window=3))
}


########################
#PACKAGES
########################

library(foreign)
library(rgdal)
library(RODBC)
library(shapefiles)

#http://cran.ms.unimelb.edu.au/bin/windows/contrib/2.6/sp_0.9-25.zip
#http://cran.ms.unimelb.edu.au/bin/windows/contrib/2.6/rgdal_0.5-25.zip
#http://cran.ms.unimelb.edu.au/bin/windows/contrib/2.6/RODBC_1.2-3.zip
#http://cran.ms.unimelb.edu.au/bin/windows/contrib/2.7/shapefiles_0.6.zip


########################
##Coords: vertices of a line
x<-1:10
line_coords<-matrix(c(x,x,20-(x-6)^2),ncol=3)
linesplit<-plot_breaks(line_coords[,3],debug=FALSE)
newlines<-lapply(linesplit,function(x) line_coords[x,])

#line_orig<-Lines(list(Line(line_coords[,1:2])),ID="x1")


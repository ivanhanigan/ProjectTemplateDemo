#Introduction to R
# Joseph Guillaume 2009-08-13
"
statistics & graphics environment
similar functions to SPlus

command window
text editor
change directory

scripting language
command and arguments - like arcgis but text rather than graphical
result in an object rather than just display
	flexible
why scripting:
	easy to redo from scratch
	full documentation of process
	as concise as possible once you understand it
some argue it is less intuitive
	?help
	autocomplete
	cran website
	mailing lists


#Datasets used here
Synthetic dataset
timedata
	date,id,count
	2004-2007 by month
shpfile of lines with z values

#Packages
made of many packages
many ship with R in the first place:
	foreign

others can install within it:
	rodbc
	rgdal, sp
	shapefiles
"

#########################
#Syntax
#########################
#objects and assignment
s<- -1
s

#operators
t<- s+5
t==s
t

#functions 
seq
?seq
series<-seq(from=s,to=t,length.out=5)
series

#comments

#########################
# Loading data
#########################

#Reading from CSV file
# Readable in text format
timedata<-read.csv("timedata.csv")

#Reading from MS Excel spreadsheet
# Problems with NAs
library(RODBC)
chan<-odbcConnectExcel("timedata.xls")
sqlTables(chan)$TABLE_NAME
sheet<-sqlFetch(chan,"timedata")

#Reading SHP dbf data file
library(foreign)
shpdbf<-read.dbf("3d_line.dbf")
shpdbf

#########################
#Looking at the data
#########################
#The object
timedata
#Summarize data
summary(timedata)

#Subsetting
#First 10 rows, all columns
timedata[1:10,]
#110th to 120th row, 2nd and 3rd column
timedata[110:120,2:3]
#Using column names
names(timedata)
summary(timedata$count)
#Using boolean expressions - all rows that have count>5 
timedata[timedata$count>70,]
#Using subset
subset(timedata,subset=timedata$count<10,select=date)

#Look at structure
str(timedata)
#Data frame. Table of columns of different data types
#Usual type of object when loading data
#Data types
# numeric,text,date (will come back to it)

#Other ways of storing data
#named vectors
v<-c(1,3,4,5)
names(v)<-c("one","two","three")
v

#matrices: single data type, multiple columns & rows
m<-matrix(1:100,ncol=10,nrow=10)
m

#List, can hold any objects of different types
L<-list(
	vector=v,
	matrix=m,
	data.frame=timedata[1:5,]
)
L

#Other types of objects for specific tasks, we'll see later

########################
#Basic analyses
########################

plot(timedata$id,timedata$count)
plot(
	count~id,
	data=timedata,
	main="Distribution of counts by line number",
	xlab="ID",
	ylab="Count",
	col="blue",
	lwd=2,
	type="p"
	)
?plot
hist(timedata$count)
plot(density(timedata$count))

boxplot(timedata$count)

#png("boxplot.png")
win.metafile("boxplot.wmf")
boxplot(count~id,
	data=timedata,
	main="Boxplots of count by line",
	xlab="Line number",
	ylab="Count"
)
dev.off()

#Linear regression
linreg<-lm(count~id,data=timedata)
linreg
summary(linreg)


########################
#Time series analysis
########################

#Convert between data types
# as.numeric, as.data.frame, as.matrix, as.character etc.

timedata$date<-as.Date(timedata$date)
timedata$year<-as.numeric(format(timedata$date,"%Y"))
str(timedata)

?ts
timedata_ts<-ts(data=timedata$count[timedata$id==5],
	start=min(timedata$year),
	freq=12
)

timedata_ts
plot(timedata_ts)

#Loess decomposition
s5l<-stl(timedata_ts,s.window=3)
plot(s5l)
#Periodic decomposition
s5p<-stl(timedata_ts,s.window="periodic")
plot(s5p)

#Show autocorrelation of remainder
acf(s5l$time.series[,"remainder"])


########################
# More complex tasks:
# Programming concepts
########################
#Doing stl decompositions for every the time series of every even numbered line

#Rather than repeating code for each time series separately, we can use programming concepts:
# Functions
# Loops - for loops
# Conditional branching - if statements

#Functions
#Grouping code that is frequently reused
plot_ts_stl<-function(id){
	timedata_ts<-ts(data=timedata$count[timedata$id==id],
		start=2004,
		freq=12
	)
	s<-stl(timedata_ts,s.window=3)
	plot(s,main=sprintf("STL decomposition of line %d",id))
}

par(ask=TRUE)
#Loop
for (id in 1:10){
	#Conditional
	if (id %% 2==0) plot_ts_stl(id)
}


########################
# Spatial data
########################

library(rgdal)

#Creating a line from scratch
x<-1:10
lines<-SpatialLines(list(
	Lines(list(
		Line(matrix(c(x,x),ncol=2))
		),ID=c("x1")
	),
	Lines(list(
		Line(matrix(c(x,x+1),ncol=2))
		),ID=c("x2")
	)
	)#List of Lines
)
lines
data<-data.frame(ID=c(1,2))
row.names(data)<-c("x1","x2")

a<-SpatialLinesDataFrame(lines,data)
writeOGR(a,"lines.shp","lines","ESRI Shapefile")

b<-readOGR("lines.shp","lines")
identical(coordinates(a),coordinates(b))
data.frame(a)
data.frame(b)

########################
# Example application - splitting lines at crests
########################

plot_breaks<-function(z,return.which=TRUE,debug=TRUE){
	wanted<-2:(length(z)-1)
	#find vertices that are greater than those on either side
	localmaxes<-which(sapply(wanted,function(i) z[i]>=z[i-1] & z[i]>=z[i+1]))

	#Z values of local maximums
	if (debug) print(z[wanted][localmaxes])

	#Split line
	breaks<-unique(c(1,localmaxes+1,length(z)))
	aa<-rep(NA,length(z))
	for (i in 2:length(breaks)) {
		aa[breaks[i-1]:breaks[i]]<-i
	}

	if (debug) plot(z,col=aa,type="b")
	if (return.which) return(split(1:length(z),aa))
	else return(split(z,aa))

}#plot_breaks


#Two parabolas
z<-c(50-(x-5)**2,30-(x-3)**2)
plot_breaks(z)

#Random walk
z<-rep(NA,100)
z[1]<-10
for (i in 2:100){
	z[i]<-z[i-1]+runif(min=-5,max=5,n=1)
}
plot_breaks(z)


######################################
# With an actual 3d line shapefile
######################################

library(shapefiles)
line3d_coords<-read.shp("3d_line.shp")
str(line3d_coords)

lines<-lapply(line3d_coords$shp,function(x) x$points)
lines

final_data<-NULL
final_lines<-list()
for (i in 1:length(lines)){

	coords<-lines[[i]] #coordinates(lines[[i]])
	origid<-as.character(i)
	linesplit<-plot_breaks(coords[,3],debug=FALSE)

	#Add next coordinate to all split lines
	for (i in 2:length(linesplit)){
		linesplit[[i-1]]<-c(linesplit[[i-1]],linesplit[[i]][1])
	}

	newlines<-lapply(linesplit,function(x) coords[x,])
	newlines<-lapply(1:length(newlines),function(x) Lines(list(Line(newlines[[x]][,1:2])),ID=sprintf("x%s.%s",origid,x)))
	ids<-sapply(1:length(newlines),function(x) sprintf("x%s.%s",origid,x))

	data<-data.frame(origID=rep(1,length(newlines)),newID=ids)
	row.names(data)<-ids

	final_lines<-c(final_lines,newlines)
	final_data<-rbind(final_data,data)

}#lines
final<-SpatialLinesDataFrame(SpatialLines(final_lines),final_data)
writeOGR(final,"split_lines.shp","lines","ESRI Shapefile")

#Output in KML (e.g. for google earth)
writeOGR(final,"split_lines.kml","lines","KML")


########################
# Resources
########################
http://cran.r-project.org/

http://cran.r-project.org/doc/contrib/Paradis-rdebuts_en.pdf
http://cran.r-project.org/doc/contrib/Short-refcard.pdf
http://cran.r-project.org/web/views/
http://cran.r-project.org/web/views/TimeSeries.html
http://cran.r-project.org/web/views/Spatial.html

http://geodacenter.asu.edu/r-spatial-projects
http://www.bostongis.com/


#################################################################
# ~/NMMAPSlite-tute.r
# author:
# ihanigan
# date:
# 2012-08-16
# description:
# a tute to demo R codes for multiple regression modelling
# homework is to get the RR and 95 percent CIs for pm10tmeant
#################################################################
getwd()
setwd('~/tools/ProjectTemplateDemo/analysis')

######################################################
# tools
if(!require(ProjectTemplate)) install.packages('ProjectTemplate'); require(ProjectTemplate)
load.project()
ls()
if(!require(mgcv)) install.packages('mgcv');require(mgcv)
require(splines)
if(!require(NMMAPSlite)) install.packages('NMMAPSlite');require(NMMAPSlite)
initDB('data')

######################################################
# load
cities <- getMetaData('cities')
head(cities)
# use a variable, we can loop thru city_i's later
city_i <- 'Chicago'
city <- subset(cities, cityname == city_i)$city
data <- readCity(city)
data$yy <- substr(data$date,1,4)
numYears<-length(names(table(data$yy)))
str(data)

######################################################
# do NMMAPS model as glm
fit2 <- glm(cvd ~ agecat + pm10tmean +
 ns(tmax, df = 8) + ns(dptp, df = 4) + ns(time, df = 7*numYears),
 data = data, family = poisson)

# plot responses
par(mfrow=c(2,2))
termplot(fit2, se =T,terms='agecat')
termplot(fit2, se =T,terms='pm10tmean')
termplot(fit2, se =T,terms='ns(tmax, df = 8)')
attr(terms(fit2),'term.labels')
termplot(fit2, se =T,terms='ns(time, df = 7 * numYears)')
dev.off()

######################################################
# get coefficients and RRs
collectResults <- function(fit, name, covar, modeloutputsTable = NA){
  # a tool for extracting the results from our models
  # in to a table for the document
  modout <- data.frame(name,
   t(
    summary(fit)$coeff[
     which(row.names(summary(fit)$coeff) == covar),]
    ))

  modout$RR <- exp(modout[,2])
  modout$RRlci <- exp(modout[,2] - 1.96 * modout[,3])
  modout$RRuci <- exp(modout[,2] + 1.96 * modout[,3])

  modout$RRPct <- (exp(modout[,2])-1)*100
  modout$RRlciPct <- (exp(modout[,2] - 1.96 * modout[,3])-1)*100
  modout$RRuciPct <- (exp(modout[,2] + 1.96 * modout[,3])-1)*100
  colnames <- c('model', 'beta', 'se', 'z', 'p', 'RR','RRlci','RRuci','RRPct','RRlciPct','RRuciPct')
  names(modout) <-  colnames

  if(exists('modeloutputsTable')){
   modeloutputs <- rbind(modeloutputsTable,modout)
  } else {
   modeloutputs <- as.data.frame(matrix(nrow=0,ncol=11))
   names(modeloutputs) <-  colnames
   modeloutputs <- rbind(modeloutputs,modout)
  }

 return(modeloutputs)

 }

######################################################
# get the RRs
results_out <- as.data.frame(matrix(nrow=0,ncol=11))
names(results_out) <- c('model', 'beta', 'se', 'z', 'p', 'RR','RRlci','RRuci','RRPct','RRlciPct','RRuciPct')
results_out <- collectResults(fit=fit2, name='nmmaps',
                          covar='pm10tmean',
                          modeloutputsTable = results_out)
t(as.data.frame(results_out))
results_out

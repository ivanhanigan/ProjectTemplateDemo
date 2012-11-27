
#################################################################
# ~/NMMAPSlite-intro2regression.r
# author:
# ihanigan
# date:
# 2012-08-16
# description:
# unabridged tute to demo R codes for multiple regression modelling
#################################################################


######################################################
# tools
setwd("~/projects/ProjectTemplateDemo/analysis")
if(!require(ProjectTemplate)) install.packages('ProjectTemplate'); require(ProjectTemplate)
#load.project()
#ls()
if(!require(mgcv)) install.packages('mgcv');require(mgcv)
require(splines)
if(!require(NMMAPSlite)) install.packages('NMMAPSlite');require(NMMAPSlite)
# initDB('NMMAPS')
# this sometimes can't connect to the source webserver, can still run with local Chicago data

######################################################
# load
# cities <- getMetaData('cities')
# head(cities)
# city_i <- 'Chicago'
# city <- subset(cities, cityname == city_i)$city
# data <- readCity(city)
# data$yy <- substr(data$date,1,4)
# dir.create('data/NMMAPSraw')
# write.table(data, file.path('data', paste(city_i, '.csv',sep='')), row.names = F)
#data <- read.table("data/Chicago.csv", header=T, quote="\"")
load.project()
data <- Chicago
data$date <- as.Date(data$date)

######################################################
# check
par(mfrow=c(2,1), mar=c(4,4,3,1))
with(subset(data[,c(1,15:25)], agecat == '75p'),
  plot(date, tmax)
 )
with(subset(data[,c(1,4,15:25)], agecat == '75p'),
        plot(date, cvd, type ='l', col = 'grey')
        )
with(subset(data[,c(1,4,15:25)], agecat == '75p'),
        lines(lowess(date, cvd, f = 0.015))
        )
# I am worried about that outlier
data$date[which(data$cvd > 100)]
# [1] "1995-07-15" "1995-07-16"

######################################################
# do standard NMMAPS timeseries poisson GAM model
numYears<-length(names(table(data$yy)))
df <- subset(data, agecat == '75p')
df$time <- as.numeric(df$date)
fit <- gam(cvd ~ s(pm10tmean) + s(tmax) + s(dptp) + s(time, k= 7*numYears, fx=T), data = df, family = poisson)
# plot of response functions
par(mfrow=c(2,2))
plot(fit)
dev.off()

######################################################
# some diagnostics
summary(fit)
# note the R-sq.(adj) =   0.21
gam.check(fit)
# note the lack of a leverage plot.  for that we need glm

######################################################
# do same model as glm
fit2 <- glm(cvd ~ pm10tmean + ns(tmax, df = 8) + ns(dptp, df = 4) + ns(time, df = 7*numYears), data = df, family = poisson)
# plot responses
par(mfrow=c(2,2))
termplot(fit2, se =T)
dev.off()

# plot prediction
df$predictedCvd <- predict(fit2, df, 'response')
# baseline is given by the intercept
fit3 <- glm(cvd ~ 1, data = df, family = poisson)
df$baseline <-  predict(fit3, df, 'response')
with(subset(df, date>=as.Date('1995-01-01') & date <= as.Date('1995-07-31')),
 plot(date, cvd, type ='l', col = 'grey')
        )
with(subset(df, date>=as.Date('1995-01-01') & date <= as.Date('1995-07-31')),
        lines(date,predictedCvd)
        )
with(subset(df, date>=as.Date('1995-01-01') & date <= as.Date('1995-07-31')),
 lines(date,baseline)
        )
######################################################
# some diagnostics
# need to load a function to calculate poisson adjusted R squared
# original S code from
# The formula for pseudo-R^2 is taken from G. S. Maddalla,
# Limited-dependent and Qualitative Variables in Econometrics, Cambridge:Cambridge Univ. Press, 1983. page 40, equation 2.50.
RsquaredGlm <- function(o) {
 n <- length(o$residuals)
 ll <- logLik(o)[1]
 ll_0 <- logLik(update(o,~1))[1]
 R2 <- (1 - exp(((-2*ll) - (-2*ll_0))/n))/(1 - exp( - (-2*ll_0)/n))
 names(R2) <- 'pseudo.Rsquared'
 R2
 }
RsquaredGlm(fit2)
# 0.51
# the difference is presumably due to the arguments about how to account for unexplainable variance in the poisson distribution?

# significance of spline terms
drop1(fit2, test='Chisq')
# also note AIC. best model includes all of these terms
# BIC can be computed instead (but still labelled AIC) using
drop1(fit2, test='Chisq', k = log(nrow(data)))

# diagnostic plots
par(mfrow=c(2,2))
plot(fit2)
dev.off()
# note high leverage plus residuals points are labelled
# leverage doesn't seem to be too high though which is good
# NB the numbers refer to the row.names attribute which still refer to the original dataset, not this subset
df[row.names(df) %in% c(9354,9356),]$date
# as suspected [1] "1995-07-15" "1995-07-16"

######################################################
# so lets re run without these obs
df2 <- df[!row.names(df) %in% c(9354,9356),]
# to avoid duplicating code just re run fit2, replacing data=df with df2
# tmax still significant but not so extreme
# check diagnostic plots again
par(mfrow=c(2,2))
plot(fit2)
dev.off()
# looks like a well behaved model now.

# if we were still worried about any high leverage values we could identify these with
df3 <- na.omit(df2[,c('cvd','pm10tmean','tmax','dptp','time')])
df3$hatvalue <- hatvalues(fit2)
df3$res <- residuals(fit2, 'pearson')
with(df3, plot(hatvalue, res))
# this is the same as the fourth default glm diagnostic plot, which they label x-axis as leverage
summary(df3$hatvalue)
# gives us an idea of the distribution of hat values
# decide on a threshold and look at it
hatThreshold <- 0.1
with(subset(df3, hatvalue > hatThreshold), points(hatvalue, res, col = 'red', pch = 16))
abline(0,0)
segments(hatThreshold,-2,hatThreshold,15)
dev.off()

fit3 <- glm(cvd ~ pm10tmean + ns(tmax, df = 8) + ns(dptp, df = 4) + ns(time, df = 7*numYears), data = subset(df3, hatvalue < hatThreshold), family = poisson)
par(mfrow=c(2,2))
termplot(fit3, se = T)
# same same
plot(fit3)
# no better

# or we could go nuts with a whole number of ways of estimating influence
# check all influential observations
infl <- influence.measures(fit2)
# which observations 'are' influential
inflk <- which(apply(infl$is.inf, 1, any))
length(inflk)


######################################################
# now what about serial autocorrelation in the residuals?

par(mfrow = c(2,1))
with(df3, acf(res))
with(df3, pacf(res))
dev.off()



######################################################
# just check for overdispersion
fit <- gam(cvd ~ s(pm10tmean) + s(tmax) + s(dptp) + s(time, k= 7*numYears, fx=T), data = df, family = quasipoisson)
summary(fit)
# note the Scale est. = 1.1627
# alternatively check the glm
fit2 <- glm(cvd ~ pm10tmean + ns(tmax, df = 8) + ns(dptp, df = 4) + ns(time, df = 7*numYears), data = df, family = quasipoisson)
summary(fit2)
# (Dispersion parameter for quasipoisson family taken to be 1.222640)
# this is probably near enough to support a standard poisson model...

# if we have overdispersion we can use QAIC (A quasi- mode does not have a likelihood and so does not have an AIC,  by definition)
# we can use the poisson model and calculate the overdispersion
fit2 <- glm(cvd ~ pm10tmean + ns(tmax, df = 8) + ns(dptp, df = 4) + ns(time, df = 7*numYears), data = df, family = poisson)
1- pchisq(deviance(fit2), df.residual(fit2))

# QAIC, c is the variance inflation factor, the ratio of the residual deviance of the global (most complicated) model to the residual degrees of freedom
c=deviance(fit2)/df.residual(fit2)
QAIC.1=-2*logLik(fit2)/c + 2*(length(coef(fit2)) + 1)
QAIC.1

# Actually lets use QAICc which is more conservative about parameters,
QAICc.1=-2*logLik(fit2)/c + 2*(length(coef(fit2)) + 1) + 2*(length(coef(fit2)) + 1)*(length(coef(fit2)) + 1 + 1)/(nrow(na.omit(df[,c('cvd','pm10tmean','tmax','dptp','time')]))- (length(coef(fit2))+1)-1)
QAICc.1


######################################################
# the following is old work, some may be interesting
# such as the use of sinusoidal wave instead of smooth function of time


# # sine wave
# timevar <- as.data.frame(names(table(df$date)))
# index <- 1:length(names(table(df$date)))
# timevar$time2 <- index / (length(index) / (length(index)/365.25))
# names(timevar) <- c('date','timevar')
# timevar$date <- as.Date(timevar$date)
# df <- merge(df,timevar)

# fit <- gam(cvd ~ s(tmax) + s(dptp) + sin(timevar * 2 * pi) + cos(timevar * 2 * pi) + ns(time, df = numYears), data = df, family = poisson)
# summary(fit)
# par(mfrow=c(3,2))
# plot(fit, all.terms = T)
# dev.off()

# # now just explore the season fit
# fit <- gam(cvd ~ sin(timevar * 2 * pi) + cos(timevar * 2 * pi) + ns(time, df = numYears), data = df, family = poisson)
# yhat <- predict(fit)
# head(yhat)

# with(df, plot(date,cvd,type = 'l',col='grey', ylim = c(15,55)))
# lines(df[,'date'],exp(yhat),col='red')


# # drop1(fit, test= 'Chisq')

# # drop1 only works in glm?
# # fit with weather variables, use degrees of freedom estimated by gam
# fit <- glm(cvd ~ ns(tmax,8) + ns(dptp,2) + sin(timevar * 2 * pi) + cos(timevar * 2 * pi) + ns(time, df = numYears), data = df, family = poisson)
# drop1(fit, test= 'Chisq')
# # use plot.glm for diagnostics
# par(mfrow=c(2,2))
# plot(fit)
# par(mfrow=c(3,2))
# termplot(fit, se=T)
# dev.off()

# # cyclic spline, overlay on prior sinusoidal
# with(df, plot(date,cvd,type = 'l',col='grey', ylim = c(0,55)))
# lines(df[,'date'],exp(yhat),col='red')

# df$daynum <- as.numeric(format(df$date, "%j"))
# df[360:370,c('date','daynum')]
# fit <- gam(cvd ~ s(daynum, k=3, fx=T, bs = 'cp') +  s(time, k = numYears, fx = T), data = df, family = poisson)
# yhat2 <- predict(fit)
# head(yhat2)

# lines(df[,'date'],exp(yhat2),col='blue')


# par(mfrow=c(1,2))
# plot(fit)


# # fit weather with season
# fit <- gam(cvd ~ s(tmax) + s(dptp) +
  # s(daynum, k=3, fx=T, bs = 'cp') +  s(time, k = numYears, fx = T), data = df, family = poisson)
# par(mfrow=c(2,2))
# plot(fit)

# summary(fit)

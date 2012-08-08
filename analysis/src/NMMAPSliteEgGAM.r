
# func
initDB('data')

# load
cities <- getMetaData('cities')
head(cities)
cit <- 'chic'
subset(cities, city == cit)
data <- readCity(cit)
data$yy <- substr(data$date,1,4)

# clean
png('reports/NMMAPSliteEgGAM-qc.png', res = 150, height = 1000, width = 1000)
par(mfrow=c(2,1))
with(subset(data, agecat == '75p'), plot(date, tmax))
with(subset(data, agecat == '75p'), plot(date, cvd, type ='l', col = 'grey'))
with(subset(data, agecat == '75p'), lines(lowess(date, cvd, f = 0.015)))
dev.off()

# do
numYears<-length(names(table(data$yy)))
df <- subset(data, agecat == '75p')
df$time <- as.numeric(df$date)
fit <- gam(cvd ~ s(tmax) + s(dptp) + s(time, k= 7*numYears, fx=T), data = df, family = poisson)

# report
png('reports/NMMAPSliteEgGAM-exposureResponse.png', res = 150, height = 1000, width = 1000)
par(mfrow=c(2,2))
plot(fit)
dev.off()

# archive
cache('df')
cache('numYears')

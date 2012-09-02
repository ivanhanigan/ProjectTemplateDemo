
################################################################
# name:fit
x<-rnorm(100,10,5)
y<-rnorm(100,20,15)
fit <- lm(y~x)
summary(fit)

################################################################
# name:plot
png('aPlot.png', res=200,width = 600, height = 600)
plot(x,y,main="Example Plot",xlab="X Variable",ylab="Y Variable")
abline(fit,col="Red")
dev.off()

################################################################
# name:points
png('pchopts.png')
par(mfrow=c(3,10), mar=c(0,0,2,0))
for(i in c(1:25)){
 plot(1,1,pch=i, axes=F, cex = 3, col = 'blue', bg = 'yellow')
 title(i)
 }
for(i in c("*", "?", ".", "X", "a")){
 plot(1,1,pch=i, axes=F, cex = 3, col = 'blue', bg = 'yellow')
 title(i)
 }
dev.off()

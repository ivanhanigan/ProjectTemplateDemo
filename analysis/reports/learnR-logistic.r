
################################################################
# name:logistic regression
#Load the data
#The following R code will construct the dataset
n.fail <- c(2, 0, 0, 1, 0, 0, 1, 0, 0, 1, 2, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
temp <- c(53, 66, 68, 70, 75, 78, 57, 67, 69, 70, 75, 79, 58, 67, 70, 72, 76, 81, 63, 67, 70,
73, 76)
# there were 6 o rings for each of 23 attempts
total <- rep(6,23)
# probability of fail
p.fail <- n.fail/total
# Response = resp column bind them together touching, # of failures & the # of those that didn't - ie -ve n
resp <- cbind(n.fail, total-n.fail)

################################################################
# name:learnR-logistic
png('pfail.png')
plot(temp, p.fail, pch=16, xlim=c(40,100), ylim=c(0,0.4))
title('A plot of the proportion failed by temperature')
dev.off()

###########################################################################
# newnode: linear
linear <- glm(resp ~ 1 + temp, family=binomial(link=logit))
summary(linear)
linearoutput <- summary(linear)
linearoutput

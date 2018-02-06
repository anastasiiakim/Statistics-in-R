seatbelts <- read.table("~/seatbelt.txt", header = TRUE)
# data structure
seatbelt.dat <- data.frame(Seatbelt=seatbelts$Seatbelt, Vehicle=as.factor(seatbelts$Vehicle),
           Location=as.factor(seatbelts$Location), Gender=as.factor(seatbelts$Gender))
summary(seatbelt.dat)
attach(seatbelts)

# pie charts
s.urb <- seatbelts[which(seatbelts$Location =="urban"),]
n <- nrow(s.urb)
urban <- nrow(s.urb[which(s.urb$Seatbelt =="yes"),])
# Pie Chart with Percentages
slices <- c(urban, n-urban) 
lbls <- c("Yes", "No")
pct <- round(slices/sum(slices)*100,2)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
cols <- c("grey90","grey40")
pie(slices,labels = lbls, col=cols, main="Location: Urban")


s.rural <- seatbelts[which(seatbelts$Location =="rural"),]
n <- nrow(s.rural)
rural <- nrow(s.rural[which(s.rural$Seatbelt =="yes"),])
# Pie Chart with Percentages
slices <- c(rural, n-rural) 
lbls <- c("Yes", "No")
pct <- round(slices/sum(slices)*100,2)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
cols <- c("grey90","grey40")
pie(slices,labels = lbls, col=cols, main="Location: Rural")


s.fem <- seatbelts[which(seatbelts$Gender =="female"),]
n <- nrow(s.fem)
s.female <- nrow(s.fem[which(s.fem$Seatbelt =="yes"),])
# Pie Chart with Percentages
slices <- c(s.female, n-s.female) 
lbls <- c("Yes", "No")
pct <- round(slices/sum(slices)*100,2)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
cols <- c("grey90","grey40")
pie(slices,labels = lbls, col=cols, main="Gender: Females")

s.m <- seatbelts[which(seatbelts$Gender =="male"),]
n <- nrow(s.m)
s.male <- nrow(s.m[which(s.m$Seatbelt =="yes"),])
# Pie Chart with Percentages
slices <- c(s.male, n-s.male) 
lbls <- c("Yes", "No")
pct <- round(slices/sum(slices)*100,2)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
cols <- c("grey90","grey40")
pie(slices,labels = lbls, col=cols, main="Gender: Males")

s.m <- seatbelts[which(seatbelts$Vehicle =="car"),]
n <- nrow(s.m)
s.male <- nrow(s.m[which(s.m$Seatbelt =="yes"),])
# Pie Chart with Percentages
slices <- c(s.male, n-s.male) 
lbls <- c("Yes", "No")
pct <- round(slices/sum(slices)*100,2)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
cols <- c("grey90","grey40")
pie(slices,labels = lbls, col=cols, main="Vehicle: Car")

s.m <- seatbelts[which(seatbelts$Vehicle =="van"),]
n <- nrow(s.m)
s.male <- nrow(s.m[which(s.m$Seatbelt =="yes"),])
# Pie Chart with Percentages
slices <- c(s.male, n-s.male) 
lbls <- c("Yes", "No")
pct <- round(slices/sum(slices)*100,2)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
cols <- c("grey90","grey40")
pie(slices,labels = lbls, col=cols, main="Vehicle: Van")

s.m <- seatbelts[which(seatbelts$Vehicle =="pickup"),]
n <- nrow(s.m)
s.male <- nrow(s.m[which(s.m$Seatbelt =="yes"),])
# Pie Chart with Percentages
slices <- c(s.male, n-s.male) 
lbls <- c("Yes", "No")
pct <- round(slices/sum(slices)*100,2)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
cols <- c("grey90","grey40")
pie(slices,labels = lbls, col=cols, main="Vehicle: Pick up")

s.m <- seatbelts[which(seatbelts$Vehicle =="SUV"),]
n <- nrow(s.m)
s.male <- nrow(s.m[which(s.m$Seatbelt =="yes"),])
# Pie Chart with Percentages
slices <- c(s.male, n-s.male) 
lbls <- c("Yes", "No")
pct <- round(slices/sum(slices)*100,2)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
cols <- c("grey90","grey40")
pie(slices,labels = lbls, col=cols, main="Vehicle: SUV")


# matching priors; prior, likelihood, posterior densities plot
library(epiR)
library(survival)

bsu <- epi.betabuster(0.2,0.95,F,0.5)
bsr <- epi.betabuster(0.35,0.95,F,0.6)

a = bsu$shape1 
b =  bsu$shape2 
s = 200-urban
f = urban

curve(dbeta(x,a+s,b+f), from=0, to=1, xlab="p",ylab="Density",lty=1,lwd=4)
curve(dbeta(x,s+1,f+1),lty=2,lwd=4)
curve(dbeta(x,a,b),add=TRUE,lty=3,lwd=4)
legend(.63,4,c("Prior","Likelihood","Posterior"),lty=c(3,2,1),lwd=c(3,3,3))


a = bsr$shape1 
b =  bsr$shape2 
s = 200-rural
f =  rural

curve(dbeta(x,a+s,b+f), from=0, to=1, xlab="p",ylab="Density",lty=1,lwd=4)
curve(dbeta(x,s+1,f+1),lty=2,lwd=4)
curve(dbeta(x,a,b),add=TRUE,lty=3,lwd=4)
legend(.63,4,c("Prior","Likelihood","Posterior"),lty=c(3,2,1),lwd=c(3,3,3))

#glm
m1 <- glm(Seatbelt ~ Vehicle + Location + Gender, family=binomial)
m2 <- glm(Seatbelt ~ Gender + Location, family=binomial)
m3 <- glm(Seatbelt ~ Location, family=binomial)

#summary(m1)
#summary(m2)
#summary(m3)
mod.reduced <- glm(Seatbelt ~ Location, family=binomial)
summary(mod.reduced)

#MCMC. Trace, autocorr and density plots for model 1
library(rjags)
library(coda)
sg <- as.numeric(Gender)
sl <- as.numeric(Location)
sv <- as.numeric(Vehicle)
ss <- as.numeric(Seatbelt)
sg[sg == '1'] <- 0
sg[sg == '2'] <- 1
sl[sl == '1'] <- 0
sl[sl == '2'] <- 1
ss[ss == '1'] <- 0
ss[ss == '2'] <- 1
sv[sv == '1'] <- 0
sv[sv == '2'] <- 1
sv[sv == '3'] <- 2
sv[sv == '4'] <- 3

# The model string written in the JAGS language
model.full <- "model{
 	for(i in 1:n)
 	{
 		logit(p[i]) <- b0 + b1*Vehicle[i] + b2*Location[i] + b3*Gender[i]
  	y[i] ~ dbin(p[i], 1)

 	}
 	b0 ~ dnorm(0, 0.0001)
 	b1 ~ dnorm(0, 0.0001)
 	b2 ~ dnorm(0, 0.0001) 
 	b3 ~ dnorm(0, 0.0001)
}"

data=list(y=ss, Vehicle=sv, Location=sl, Gender=sg, n=400) 
inits=list(b0=0, b1=1, b2=1, b3=1) # initial values
jags.m=jags.model(textConnection(model.full), data=data,n.chains=2,inits=inits)

zm=coda.samples(jags.m,variable.names=c("b1", "b2", "b3", "b0"), n.iter=30000, n.burnin=5000, n.thin=10)
m<-summary(zm)
mu_sd=m$stat[,1:2]  
q=m$quantile[,c(3,1,5)]  #make columns for median and CI
(table=cbind(mu_sd,q)) #make table
traceplot(zm)
densplot(zm)
gelman.diag(zm)
autocorr.plot(zm)

#inference about proportions
data=list(y1=200-urban, y2=200-rural, n=200) # the data
inits=list(theta1=0.25, theta2=0.4) # initial values
jags.m=jags.model(file="openbugs_model.txt", data=data,n.chains=2,inits=inits)
#out=jags.samples(jags.m,c("theta1", "theta2", "prob1", "prob2", "theta.ratio", "theta.diff"),n.iter=30000, n.burnin=5000,thin=10)
# histogram 
#hist(out$theta1,probability=T,xlab=expression(theta[1]), 
#     main=expression(paste("Histogram of ", theta[1])))
# trace plot
#plot(out$theta1,type='l',xlab="iteration",ylab=expression(theta[1]), 
#     main=expression(paste("Trace of ", theta[1])))
# acf plot
#acf(as.vector(out$theta1),lag.max=40, main=expression(paste("Acf of ", theta[1])))

# histogram 
#hist(out$theta2,probability=T,xlab=expression(theta[2]), 
#     main=expression(paste("Histogram of ", theta[2])))
# trace plot
#plot(out$theta2,type='l',xlab="iteration",ylab=expression(theta[2]), 
#     main=expression(paste("Trace of ", theta[2])))
# acf plot
#acf(as.vector(out$theta2),lag.max=40, main=expression(paste("Acf of ", theta[2])))

zm=coda.samples(jags.m,variable.names=c("theta1", "theta2", "prob1", "prob2", "theta.ratio", "theta.diff"),n.iter=30000, n.burnin=5000,thin=10)
m<-summary(zm)
mu_sd=m$stat[,1:2]
q=m$quantile[,c(1,5)]#make columns for CI
table=cbind(mu_sd,q)#make table
table


#model 4
model.reduced <- "model{
  for (i in 1:n) {
    logit(p[i]) <- b0 + b1*Location[i] 
    y[i] ~ dbin(p[i], 1)
  }
 	b0 ~ dnorm(0, 0.0001)
 	b1 ~ dnorm(0, 0.0001)
}"
data=list(y=ss, Location=sl, n=400) 
inits=list(b0=0, b1=1) # initial values
jags.m2=suppressMessages(jags.model(textConnection(model.reduced), data=data, n.chains=2, inits=inits))
zm=suppressMessages(coda.samples(jags.m2,variable.names=c("b1", "b0"), n.iter=30000, n.burnin=5000, n.thin=10))
m<-summary(zm)
mu_sd=m$stat[,1:4]
q=m$quantile[,c(1,5)] #make columns for CI
(table=cbind(mu_sd,q)) #make table
gelman.diag(zm)
traceplot(zm)
densplot(zm)
autocorr.plot(zm)

#Bayes factors
library(BayesFactor)
seatbelts$Gender <- as.numeric(Gender)
seatbelts$Location <- as.numeric(Location)
seatbelts$Vehicle <- as.numeric(Vehicle)
seatbelts$Seatbelt <- as.numeric(Seatbelt)
seatbelts[1:3,]


bf1 <- lmBF(Seatbelt ~ Vehicle + Location + Gender, data=seatbelts)
bf2 <- lmBF(Seatbelt ~ Location + Gender, data=seatbelts)
bf3 <- lmBF(Seatbelt ~ Vehicle + Location , data=seatbelts)
bf4 <- lmBF(Seatbelt ~ Vehicle + Gender, data=seatbelts)
bf5 <- lmBF(Seatbelt ~ Location , data=seatbelts)

bf5/bf1
bf5/bf2
bf5/bf3
bf5/bf4






#other models
model.reduced2 <- "model{
  for (i in 1:n) {
    logit(p[i]) <- b0 + b1*Vehicle[i] + b2*Location[i] 
    y[i] ~ dbin(p[i], 1)
  }
 	b0 ~ dnorm(0, 0.0001)
 	b1 ~ dnorm(0, 0.0001)
 	b2 ~ dnorm(0, 0.0001) 
}"
data=list(y=ss, Vehicle=sv, Location=sl, n=400) 
inits=list(b0=0, b1=1, b2=1) # initial values
jags.m3=jags.model(textConnection(model.reduced2), data=data, n.chains=2, inits=inits)

model.reduced3 <- "model{
  for (i in 1:n) {
    logit(p[i]) <- b0 + b1*Gender[i] + b2*Location[i] 
    y[i] ~ dbin(p[i], 1)
  }
 	b0 ~ dnorm(0, 0.0001)
 	b1 ~ dnorm(0, 0.0001)
 	b2 ~ dnorm(0, 0.0001) 
}"

model.reduced <- "model{
  for (i in 1:n) {
    logit(p[i]) <- b0 + b1*Location[i] 
    y[i] ~ dbin(p[i], 1)
  }
 	b0 ~ dnorm(0, 0.0001)
 	b1 ~ dnorm(0, 0.0001)
}"
data=list(y=ss, Gender=sg, Location=sl, n=400) 
inits=list(b0=0, b1=1, b2=1) # initial values
jags.m4=jags.model(textConnection(model.reduced3), data=data, n.chains=2, inits=inits)

data=list(y=ss, Location=sl, n=400) 
inits=list(b0=0, b1=1) # initial values
jags.m2=jags.model(textConnection(model.reduced), data=data, n.chains=2, inits=inits)

data=list(y=ss, Vehicle=sv, Location=sl, Gender=sg, n=400) 
inits=list(b0=0, b1=1, b2=1, b3=1) # initial values
jags.m=jags.model(textConnection(model.full), data=data,n.chains=2,inits=inits)

dic.samples(jags.m, n.iter = 1000, thin = 100) # DIC for the full model 1

dic.samples(jags.m3, n.iter = 1000, thin = 100) # DIC for the reduced model 2

dic.samples(jags.m4, n.iter = 1000, thin = 100) # DIC for the reduced model 3

dic.samples(jags.m2, n.iter = 1000, thin = 100) # DIC for the reduced model 4



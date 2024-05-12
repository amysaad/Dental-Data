
dental.dat <- read.table ("http://mkahn.webspace.wheatoncollege.edu/math151/Datasets/Prosthodontics.txt", 
                          header = T)
dental.dat

attach(dental.dat)

boxplot(Strength ~ Type)

Strength.aov<-aov(Strength~Type)
summary(Strength.aov)
summary(aov(Strength~Type))


by(Strength, Type, sd)

mu <- c(38.67, 39.6, 41.39, 42.55, 40.85)
sd <- c(15.81, 14.97, 18.1, 13.62, 15.51)
n <- c(121, 546, 97, 253, 155)
data_table <- data.frame (mu, sd, n)
n <- sum(data_table$n)
k <- length(data_table$mu)

# Finding degrees of freedom
df <- k - 1
dfResidual <- n - k

# Using the qf function on the Pr(>F) to get the F-statistic:

Prf <- 0.0682
F_statistic <- qf( 1 - Prf, df , dfResidual)
F_statistic
# F-statistic = MSG/MSE

MSG <- 501.54
MSE <- MSG / F_statistic

# MSG = 1 / df * SSG

SSG <- df * MSG
SSE <- 267382

# SST = SSG + SSE, and df_Total = df + dfResidual

SST <- SSG + SSE
dft <- df + dfResidual

meniscus <- read.table ("http://mkahn.webspace.wheatoncollege.edu/math151/Datasets/meniscus-repair.txt", 
                          header = T)

summary(meniscus)
summary(Load.at.Failure)
boxplot(Load.at.Failure)
attach(meniscus)
summary(Displacement)
summary(Stiffness)
boxplot(Load.at.Failure)
boxplot(Displacement)
boxplot(Stiffness)

p_value<- 0.00682
df<-4
res<-1167
F_value<- qt()
F_value




meniscus <- read.table ("http://mkahn.webspace.wheatoncollege.edu/math151/Datasets/meniscus-repair.txt", 
                        header = T)

load_failure <- c(97.3, 106.4, 118.2, 99.7, 106.5,84.2, 44.9, 46.1, 59.3, 35.5, 50.7, 56.8, 88.0, 119.8,65.8, 82.9, 
                     149.9, 117.1)
displaced <- c(16.9, 20.2, 20.1, 15.7, 13.9, 14.9,7.9, 12.5, 15.5, 10.2,  8.9, 13.3, 18.0, 18.5,  9.2, 18.8, 22.8, 17.5)
stiff  <- c(8.3, 7.2, 6.3, 7.3, 8.7, 8.7, 4.7, 6.1, 5.0, 5.8, 6.6, 8.4, 8.0, 8.3, 7.6, 6.4, 8.2, 7.7)

method <- c(load_failure, displaced, stiff)
Outcome <- rep(c("Load At Failure","Displacement","Stiffness"),c(18,18,18))

totalMethod <- c(method , method)
totalMeasure <- c(Outcome, rep("ZTotal",54))

boxplot(method ~ Outcome, ylab="Measurement")

boxplot(totalMethod ~ totalMeasure,
        names=c("Load At Failure","Displacement","Stiffness", "Total"),
        ylab="Measurement", xlab = "Outcome")

meniscusAOV.aov <- aov(method ~ Outcome)
summary(meniscusAOV.aov)

TukeyHSD(meniscusAOV.aov)

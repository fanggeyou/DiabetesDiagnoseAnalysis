# CS544 Project
# Author: Fangge You
###########################################################################################
setwd("D:/Graduate1/CS544/Project")
dir()

charges = read.csv("inpatientCharges.csv", header = TRUE)
charges

# analysis on categorical and numerical data
# check NA data
is.na(charges)

# diabetes subset
diabetes <- subset(charges, DRG.Code == 638, select = c(DRG.Code,Provider.State,Total.Discharges,
                                                       Average.Covered.Charges, Average.Total.Payments,
                                                       Average.Medicare.Payments))
diabetes

#save the data into R project
save(diabetes, file = "diabetes.RData")

###########################################################################################
# analysis for categorical and numerical data. Show appropriate	plots	for	your data
## State ##
state <- table(diabetes$Provider.State)
state

state.plot <- barplot(state, main = "Number of Hospitals for Each State", 
                      ylim =c(0,200), xlab = "State", ylab = "Count", las =2)

## Total Discharges ##
discharge <- table(diabetes$Total.Discharges)
discharge

discharge.plot <- barplot(discharge, main="Bar Chart of Total Discharges",
                           ylim = c(0,150),
                           xlab = "# of Discharges", ylab = "count")
# five numbers
a <- fivenum(diabetes$Total.Discharges)
a
# box plot 
boxplot(diabetes$Total.Discharges, xaxt = "n", horizontal = TRUE,
        col = "#98F5FF", main = "Total Discharges")
axis(side =1, at = a, labels = TRUE, las = 2)
text(a, rep(1.2,5), srt=90, adj=0, labels=c("Min","Lower Hinge","Median","Upper Hinge","Max"))
# outliers
c(a[2] - 1.5*(a[4] - a[2]),
  a[4] + 1.5*(a[4] - a[2]))
# average total discharges
round(mean(diabetes$Total.Discharges))

## Average Total Payments ##
payment <- table(as.numeric(diabetes$Average.Total.Payments))
payment

payment.plot <- barplot(as.numeric(diabetes$Average.Total.Payments), main="Bar Chart of Average Total Payments",
                        xlab = "Hospitals", ylab = "Average Total Payments", 
                        col = "#98F5FF", ylim = c(0,180000))

# five numbers
b <- fivenum(as.numeric(diabetes$Average.Total.Payments))
b
# box plot 
boxplot(as.numeric(diabetes$Average.Total.Payments), xaxt = "n", horizontal = TRUE,
        col = "#98F5FF", main = "Average Total Payments")
axis(side =1, at = b, labels = TRUE, las = 2)
text(b, rep(1.2,5), srt=90, adj=0, labels=c("Min","Lower Hinge","Median","Upper Hinge","Max"))
# outliers
c(b[2] - 1.5*(b[4] - b[2]),
  b[4] + 1.5*(b[4] - b[2]))
# average total payments
round(mean(as.numeric(diabetes$Average.Total.Payments)))


## Average Covered Charges ##
covered <- table(as.numeric(diabetes$Average.Covered.Charges))
covered

covered.plot <- barplot(as.numeric(diabetes$Average.Covered.Charges), main="Bar Chart of Average Covered Charges",
                        xlab = "Hospitals", ylab = "Average Covered Charges", 
                        col = "#98F5FF", ylim = c(0,180000))

# five numbers
d <- fivenum(as.numeric(diabetes$Average.Covered.Charges))
d
# box plot 
boxplot(as.numeric(diabetes$Average.Covered.Charges), xaxt = "n", horizontal = TRUE,
        col = "#98F5FF", main = "Average Covered Charges")
axis(side =1, at = d, labels = TRUE, las = 2)
text(d, rep(1.2,5), srt=90, adj=0, labels=c("Min","Lower Hinge","Median","Upper Hinge","Max"))
# outliers
c(d[2] - 1.5*(d[4] - d[2]),
  d[4] + 1.5*(d[4] - d[2]))
# average covered charges
round(mean(as.numeric(diabetes$Average.Covered.Charges)))

# plot the correlation between average total payments and averaged covered charges
tp <- as.numeric(diabetes$Average.Total.Payments)
cov <- as.numeric(diabetes$Average.Covered.Charges)
plot(cov, tp) 


## Average Medicare Payments ##
medicare <- table(as.numeric(diabetes$Average.Medicare.Payments))
medicare

medicare.plot <- barplot(as.numeric(diabetes$Average.Medicare.Payments), main="Bar Chart of Average Medicare Payments",
                        xlab = "Hospitals", ylab = "Average Medicare Payments", 
                        col = "#98F5FF", ylim = c(0,180000))

# five numbers
e <- fivenum(as.numeric(diabetes$Average.Medicare.Payments))
e
# box plot 
boxplot(as.numeric(diabetes$Average.Medicare.Payments), xaxt = "n", horizontal = TRUE,
        col = "#98F5FF", main = "Average Medicare Payments")
axis(side =1, at = e, labels = TRUE, las = 2)
text(e, rep(1.2,5), srt=90, adj=0, labels=c("Min","Lower Hinge","Median","Upper Hinge","Max"))
# outliers
c(e[2] - 1.5*(e[4] - e[2]),
  e[4] + 1.5*(e[4] - e[2]))
# average medicare payments
round(mean(as.numeric(diabetes$Average.Medicare.Payments)))

# plot correlation between average total payment and average medicare payment
# tp <- as.numeric(diabetes$Average.Total.Payments)
med <- as.numeric(diabetes$Average.Medicare.Payments)
plot(med, tp)

###########################################################################################
# one	variable with	numerical	data and examine the distribution	of the data
dist <- hist(diabetes$Total.Discharges, ylim = c(0,2000),
          main="Histogram of Total Discharges", 
          xlab="Discharges", col="#FF7F50", breaks=5)

dist$breaks
dist$counts


###########################################################################################
# Draw various random samples of the data and show the applicability of the Central Limit Theorem for this variable
# random sample of total discharge
par(mfrow = c(2,2))

set.seed(150)

samples <- 1000
sample.size <- c(10, 20, 30, 40)

xbar <- numeric(samples)

for (size in sample.size) {
  for (i in 1:samples) {
    xbar[i] <- mean(sample(discharge, size = size))
    
  }
  
  hist(xbar, prob = TRUE, 
       breaks = 15, col = "lightcoral", main = paste("Sample Size =", size))
  
  cat("Sample Size = ", size, " Mean = ", mean(xbar),
      " SD = ", sd(xbar), "\n")
}

par(mfrow = c(1,1))

##########################################################################################
# Show how various sampling	methods	can	be used on your data
library(sampling)

# srswr
#par(mfrow = c(1,2))
par(mfrow = c(1,1))
set.seed(153)
n <- 100

s <- srswr(n, nrow(diabetes))
s[s != 0]

rows <- (1:nrow(diabetes))[s!=0]
rows <- rep(rows, s[s != 0])
rows

sample.1 <- diabetes[rows, ]
sample.1

# the frequencies of each region
table(sample.1$Total.Discharges)

# graph
hist(sample.1$Total.Discharges, prob = TRUE,
     main="SRSWR", 
     ylim = c(0,0.02), xlab="Discharges", col="#6495ED", breaks=5)


# srswor
set.seed(153)

s <- srswor(n, nrow(diabetes))
s

rows <- (1:nrow(diabetes))[s!=0]
rows

sample.2 <- diabetes[rows, ]
sample.2

# the frequencies for each region
table(sample.2$Total.Discharges)

# graph
hist(sample.2$Total.Discharges, prob = TRUE,
     main="SRSWOR", 
     xlab="Discharges", col="#6495ED", breaks=5)

par(mfrow = c(1,1))

# systematic sampling
set.seed(153)

N <- nrow(diabetes)


k <- ceiling(N / n)
k

r <- sample(k, 1)
r
s <- seq(r, by = k, length = n)

sample.3 <- diabetes[s, ]
sample.3

#the frequencies for each region
table(sample.3$Total.Discharges)

# graph
hist(sample.3$Total.Discharges, prob = TRUE,
     main="Systematic Sampling",
     ylim = c(0,0.025), xlab="Discharges", col="#6495ED", breaks=5)

mean(sample.1$Total.Discharges)
mean(sample.2$Total.Discharges, na.rm = TRUE)
mean(sample.3$Total.Discharges, na.rm = TRUE)

##########################################################################################
# confidence levels of 80 and 90, show the confidence intervals of the mean of the numeric variable 
# for various samples and compare against the population mean
## population ##
sample.size <- 100
sample.data <- sample(diabetes$Total.Discharges, size = sample.size)
sample.data

n <- length(sample.data)
n

sd.sample.means <- sd(sample.data)/sqrt(n)
sd.sample.means

xbar <- mean(sample.data)
xbar

pop.mean <- mean(diabetes$Total.Discharges)
pop.mean

conf <- c(80,90)
conf

alpha <- 1 - conf/100
alpha

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), t: %.2f , %.2f",
                 100*(1-i), i, 
                 qt(i/2, df = n-1),
                 qt(1 - i/2, df = n-1))
  cat(str,"\n")
}

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f",
                 100*(1-i), i, 
                 xbar - qt(1 - i/2, df = n-1) * sd.sample.means,
                 xbar + qt(1 - i/2, df = n-1) * sd.sample.means)
  cat(str,"\n")
}

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), Precision = %.2f",
                 100*(1-i), i, 
                 2* qt(1-i/2, df = n-1) * sd.sample.means)
  cat(str,"\n")
}


## srswr ##
sample.size <- 100
sample.data <- sample(sample.1$Total.Discharges, size = sample.size)
sample.data

n <- length(sample.data)
n

sd.sample.means <- sd(sample.data)/sqrt(n)
sd.sample.means

xbar <- mean(sample.data)
xbar

pop.mean <- mean(sample.1$Total.Discharges)
pop.mean

conf <- c(80,90)
conf

alpha <- 1 - conf/100
alpha

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), t: %.2f , %.2f",
                 100*(1-i), i, 
                 qt(i/2, df = n-1),
                 qt(1 - i/2, df = n-1))
  cat(str,"\n")
}

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f",
                 100*(1-i), i, 
                 xbar - qt(1 - i/2, df = n-1) * sd.sample.means,
                 xbar + qt(1 - i/2, df = n-1) * sd.sample.means)
  cat(str,"\n")
}

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), Precision = %.2f",
                 100*(1-i), i, 
                 2* qt(1-i/2, df = n-1) * sd.sample.means)
  cat(str,"\n")
}


## srswor ##
sample.size <- 100
sample.data <- sample(sample.2$Total.Discharges, size = sample.size)
sample.data

n <- length(sample.data)
n

sd.sample.means <- sd(sample.data)/sqrt(n)
sd.sample.means

xbar <- mean(sample.data)
xbar

pop.mean <- mean(sample.2$Total.Discharges)
pop.mean

conf <- c(80,90)
conf

alpha <- 1 - conf/100
alpha

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), t: %.2f , %.2f",
                 100*(1-i), i, 
                 qt(i/2, df = n-1),
                 qt(1 - i/2, df = n-1))
  cat(str,"\n")
}

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f",
                 100*(1-i), i, 
                 xbar - qt(1 - i/2, df = n-1) * sd.sample.means,
                 xbar + qt(1 - i/2, df = n-1) * sd.sample.means)
  cat(str,"\n")
}

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), Precision = %.2f",
                 100*(1-i), i, 
                 2* qt(1-i/2, df = n-1) * sd.sample.means)
  cat(str,"\n")
}

## systematic ##
sample.size <- 100
sample.data <- sample(sample.3$Total.Discharges, size = sample.size)
sample.data <- na.omit(sample.data)
sample.data

n <- length(sample.data)
n

sd.sample.means <- sd(sample.data)/sqrt(n)
sd.sample.means

xbar <- mean(sample.data)
xbar

pop.mean <- mean(sample.3$Total.Discharges, na.rm = TRUE)
pop.mean

conf <- c(80,90)
conf

alpha <- 1 - conf/100
alpha

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), t: %.2f , %.2f",
                 100*(1-i), i, 
                 qt(i/2, df = n-1),
                 qt(1 - i/2, df = n-1))
  cat(str,"\n")
}

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f",
                 100*(1-i), i, 
                 xbar - qt(1 - i/2, df = n-1) * sd.sample.means,
                 xbar + qt(1 - i/2, df = n-1) * sd.sample.means)
  cat(str,"\n")
}

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), Precision = %.2f",
                 100*(1-i), i, 
                 2* qt(1-i/2, df = n-1) * sd.sample.means)
  cat(str,"\n")
}
########################################################################################





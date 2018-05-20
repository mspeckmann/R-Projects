#CS544 Final Project
#data source: https://www.kaggle.com/cdc/chronic-disease
#Melinda Speckmann

library(dplyr)
library(UsingR)
library(RColorBrewer)
library(sampling)

#load data
raw_data <- read.csv("Chronic_Disease.csv", header = TRUE)

#Cleaning and simplifying data

#choosing measures of interest
student_data <- raw_data[raw_data$Question %in% c('Healthy weight among high school students' #outcome metric
                         ,'Soda consumption among high school students'
                         ,'Meeting aerobic physical activity guidelines among high school students'
                         ,'Participation in daily school physical education classes among high school students'
                         ,'Overweight or obesity among high school students' #outcome metric
                         ,'Television viewing among high school students'
                         ,'Median daily frequency of fruit consumption among high school students'
                         ,'Median daily frequency of vegetable consumption among high school students'
                         ,'Secondary schools that allow students to purchase soda or fruit drinks'
                         ,'Secondary schools that allow students to purchase sports drinks'
                         ,'Secondary schools that offer less healthy foods as competitive foods'
                         ,'Secondary schools that allow community-sponsored use of physical activity facilities by youth outside of normal school hours'
                         ,'Computer use among high school students'
                         ,'Mean maternity practices in infant nutrition care (mPINC) score'
                         ,'Farmers markets that accept Supplemental Nutrition Assistance Program (SNAP) benefits'
                         ,'Number of farmers markets per 100,000 residents'
                         ,'Prevalence of high cholesterol among adults aged >= 18 years with diagnosed diabetes' #Diabetes, outcome
                         ,'Prevalence of diagnosed diabetes among adults aged >= 18 years' #Diabetes, outcome
                         ),]

#removing other categories like gender and race and only focusing on Overall metrics by state
student_data <- student_data[student_data$StratificationCategory1 == 'Overall',]

#choosing 2014 and 2015 data (most current) except for the metrics that do not have current data
student_data <- student_data[student_data$YearStart %in% c('2015','2014') | student_data$Question %in% c('Farmers markets that accept Supplemental Nutrition Assistance Program (SNAP) benefits'
                                                                                                         ,'Number of farmers markets per 100,000 residents'),]

#removing 2014 data as this measure has 2014 and 2015 data
student_data <- student_data[!(student_data$YearStart == '2014' & student_data$Question == 'Prevalence of diagnosed diabetes among adults aged >= 18 years'),]

#removing data value type as there were two types to represent the same metric
student_data <- student_data[!(student_data$DataValueType == 'Age-adjusted Prevalence' & student_data$Question == 'Prevalence of diagnosed diabetes among adults aged >= 18 years'),]
student_data <- student_data[!(student_data$DataValueType == 'Age-adjusted Prevalence' & student_data$Question == 'Prevalence of high cholesterol among adults aged >= 18 years with diagnosed diabetes'),]



#removing unnecessary columns
student_data[20:34] <- list(NULL)
student_data$DataSource <- NULL
student_data$Response <- NULL
student_data$DataValueType <- NULL
student_data$DataValueFootnoteSymbol <- NULL
student_data$DataValueFootnote <- NULL
student_data$Stratification1 <- NULL
student_data$StratificationCategory1 <- NULL
student_data$StratificationCategory2 <- NULL

#remove Guam, Virgin Islands, Puerto Rico and United States.

student_data <- student_data[student_data$LocationDesc != 'Guam',]
student_data <- student_data[student_data$LocationDesc != 'Puerto Rico',]
student_data <- student_data[student_data$LocationDesc != 'Virgin Islands',]
student_data <- student_data[student_data$LocationDesc != 'United States',]

#check to ensure only one year of data for each question
#student_data <- distinct(student_data, student_data$YearStart, student_data$Question)

#cleaned up data source
write.csv(student_data, "student_data.csv")




#list of Questions - 18 total, list of states (51 total with DC)
question <- distinct(student_data, student_data$Question)
location <- distinct(student_data, student_data$LocationDesc)



#What states do we not have data for for which metrics
#categorical data

missing_data <- student_data[student_data$DataValue == '',]

write.csv(missing_data, 'missing_data.csv')

missing_table <- table(missing_data$LocationAbbr)

mt <- missing_table[missing_table > 0]

barplot(mt, col = brewer.pal(n = 10, name = "Blues"), ylab = 'Frequency', xlab = 'States', main = '# of Questions missing (18 Total)', ylim = c(0,15))


# after taking inventory of missing question, now remove NA values
student_data <- student_data[!(is.na(student_data$DataValueAlt)),]

#Looking at outcome metric overweight percentage

om_ob <- student_data[student_data$Question == 'Overweight or obesity among high school students',]
#om_ob <- data.frame(om_ob$LocationAbbr, om_ob$DataValue)
om_ob$DataValue = as.numeric(as.character(om_ob$DataValue))
om_ob <- om_ob[order(-om_ob$DataValue),]

om_ob_10 <- head(om_ob, 10) #display top 10


xx <- barplot(om_ob_10$DataValue, names.arg = om_ob_10$LocationAbbr, col = brewer.pal(n=10, name = "OrRd"),
        ylim = c(0,40), ylab = '% of HS Population',
        cex.main = 2,
        cex.names = 2,
        main = 'Outcome Metric: % of Overweight HS students (Top 10)', xlab = "States")
text(x = xx, y = om_ob_10$DataValue, label = paste(om_ob_10$DataValue, "%", sep = ""), pos = 3, cex = 1.5)


#Looking at outome metric healthy weight
display.brewer.all()

om_h <- student_data[student_data$Question == 'Healthy weight among high school students',]
om_h$DataValue = as.numeric(as.character(om_h$DataValue))
om_h <- om_h[order(-om_h$DataValue),]
om_h_10 <- head(om_h, 10) #display top 10
hh <- barplot(om_h_10$DataValue, names.arg = om_h_10$LocationAbbr, col = brewer.pal(n=10, name = "BuGn"),
              ylim = c(0,75), ylab = '% of HS Population',
              cex.main = 2,
              cex.names = 2,
              main = 'Outcome Metric: % of Healthy weight HS students (Top 10)', xlab = "States")
text(x = hh, y = om_h_10$DataValue, label = paste(om_h_10$DataValue, "%", sep = ""), pos = 3, cex = 1.5)

#Looking at outcome metric of cholesterol
om_hc <- student_data[student_data$Question == 'Prevalence of high cholesterol among adults aged >= 18 years with diagnosed diabetes',]
om_hc$DataValue = as.numeric(as.character(om_hc$DataValue))
om_hc <- om_hc[order(-om_hc$DataValue),]
om_hc_10 <- head(om_hc, 10) #display top 10
hhc <- barplot(om_hc_10$DataValue, names.arg = om_hc_10$LocationAbbr, col = brewer.pal(n=10, name = "OrRd"),
              ylim = c(0,75), ylab = '% of Population', 
              cex.main = 2,
              cex.names = 2,
              main = 'Outcome Metric: % of Pop. with High Chol. (Top 10)', xlab = "States")
text(x = hhc, y = om_hc_10$DataValue, label = paste(om_hc_10$DataValue, "%", sep = ""), pos = 3, cex = 1.5)


#Looking at outcome metric of diabetes
om_d <- student_data[student_data$Question == 'Prevalence of diagnosed diabetes among adults aged >= 18 years',]
om_d$DataValue = as.numeric(as.character(om_d$DataValue))
om_d <- om_d[order(-om_d$DataValue),]
om_d_10 <- head(om_d, 10) #display top 10
hd <- barplot(om_d_10$DataValue, names.arg = om_d_10$LocationAbbr, col = brewer.pal(n=10, name = "OrRd"),
               ylim = c(0,25), ylab = '% of Population',
               cex.main = 2,
               cex.names = 2,
               main = 'Outcome Metric: % of Pop. with Diabetes (Top 10)', xlab = "States")
text(x = hd, y = om_d_10$DataValue, label = paste(om_d_10$DataValue, "%", sep = ""), pos = 3, cex = 1.5)

#boxplots

#Looking at outcome metric overweight percentage

boxplot(om_ob$DataValue, horizontal = TRUE, xaxt = 'n', col = 'rosybrown1'
        , xlab = '% Of Population', main = '% of Overweight HS Pop.', cex.main = 1.5)
axis(side = 1, at = fivenum(om_ob$DataValue), labels = TRUE, las = 2)
text(fivenum(om_ob$DataValue), rep(1.22,5), srt=90, adj=0, labels = c("Min", "Lower Hinge","Median"
                                                                     ,"Upper Hinge", "Max"))

#Looking at outome metric healthy weight
boxplot(om_h$DataValue, horizontal = TRUE, xaxt = 'n', col = 'seagreen3'
        , xlab = '% Of Population', main = '% of Healthy Weight HS Pop.', cex.main = 1.5)
axis(side = 1, at = fivenum(om_h$DataValue), labels = TRUE, las = 2)
text(fivenum(om_h$DataValue), rep(1.22,5), srt=90, adj=0, labels = c("Min", "Lower Hinge","Median"
                                                                      ,"Upper Hinge", "Max"))
#Looking at outcome metric of cholesterol
boxplot(om_hc$DataValue, horizontal = TRUE, xaxt = 'n', col = 'rosybrown1'
        , xlab = '% Of Population', main ='% of Pop. with High Chol.', cex.main = 1.5)
axis(side = 1, at = fivenum(om_hc$DataValue), labels = TRUE, las = 2)
text(fivenum(om_hc$DataValue), rep(1.22,5), srt=90, adj=0, labels = c("Min", "Lower Hinge","Median"
                                                                     ,"Upper Hinge", "Max"))
#Looking at outcome metric of diabetes
boxplot(om_d$DataValue, horizontal = TRUE, xaxt = 'n', col = 'rosybrown1'
        , xlab = '% Of Population', main ="% of Pop. with Diabetes", cex.main = 1.5)
axis(side = 1, at = fivenum(om_d$DataValue), labels = TRUE, las = 2)
text(fivenum(om_d$DataValue), rep(1.22,5), srt=90, adj=0, labels = c("Min", "Lower Hinge","Median"
                                                                      ,"Upper Hinge", "Max"))


#Scatter plots - two variable relationship

#correlation between Overweight outcome metric and soda consumption process metric
pm_sc <- student_data[student_data$Question == 'Soda consumption among high school students',]
pm_sc$DataValue = as.numeric(as.character(pm_sc$DataValue))

cor_sc <- merge(pm_sc, om_ob, by = 'LocationAbbr') #datavalue.y = outcome (overweight) and datavalue.x = process


plot(cor_sc$DataValue.x, cor_sc$DataValue.y, col = brewer.pal(n=4,name="Set1"), pch = 16,
     xlab = '% of Soda Consumption', ylab = '% of Overweight HS Students'
     , main = 'Overweight HS Pop. vs Soda Consumption', cex.main = 1.5)
text(cor_sc$DataValue.x, cor_sc$DataValue.y, labels = cor_sc$LocationAbbr, cex = 0.7, pos = 3)
abline(lm(cor_sc$DataValue.y ~ cor_sc$DataValue.x))


#correlation between Overwight outcome metric and TV viewing

pm_tv <- student_data[student_data$Question == 'Television viewing among high school students',]
pm_tv$DataValue = as.numeric(as.character(pm_tv$DataValue))

cor_tv <- merge(pm_tv, om_ob, by = 'LocationAbbr')

plot(cor_tv$DataValue.x, cor_tv$DataValue.y, col = brewer.pal(n=4, name='Set1'), pch = 16,
     xlab = '% of HS students watching TV', ylab = '% of Overweight HS students'
     , main = 'Overweight HS Pop. vs TV viewing', cex.main = 1.5)
text(cor_tv$DataValue.x, cor_tv$DataValue.y, labels = cor_tv$LocationAbbr, cex = 0.7, pos = 3)
abline(lm(cor_tv$DataValue.y ~ cor_tv$DataValue.x))


#correlation between Diabetes and Soda Consumption of HS students 
pm_sc2 <- student_data[student_data$Question == 'Soda consumption among high school students',]
pm_sc2$DataValue = as.numeric(as.character(pm_sc2$DataValue))

cor_sc2 <- merge(pm_sc2, om_d, by = 'LocationAbbr')

plot(cor_sc2$DataValue.x, cor_sc2$DataValue.y, col = brewer.pal(n=4, name='Set1'), pch = 16,
     xlab = '% of Soda Consumption', ylab = '% of Diabetic Pop.'
     , main = 'Diabetic Pop. vs Soda Consumption among HS Pop.', cex.main = 1.5)
text(cor_sc2$DataValue.x, cor_sc2$DataValue.y, labels = cor_sc2$LocationAbbr, cex = 0.7, pos = 2)
abline(lm(cor_sc2$DataValue.y ~ cor_sc2$DataValue.x))

#correlation between Healthy weight HS Pop and Veggie consumption
pm_vc <- student_data[student_data$Question == 'Median daily frequency of vegetable consumption among high school students',]
pm_vc$DataValue = as.numeric(as.character(pm_vc$DataValue))

cor_vc <- merge(pm_vc, om_h, by = 'LocationAbbr')

plot(cor_vc$DataValue.x, cor_vc$DataValue.y, col = brewer.pal(n=4, name='Set1'), pch = 16,
     xlab = 'Med. Freq. of Veggies Consumed by HS Pop.', ylab = '% of Healthy HS Pop.'
     , main = 'Healthy HS Pop. vs Veggie Consumption', cex.main = 1.5)
text(cor_vc$DataValue.x, cor_vc$DataValue.y, labels = cor_vc$LocationAbbr, cex = 0.7, pos = 2)
abline(lm(cor_vc$DataValue.y ~ cor_vc$DataValue.x))


#correlation between Pop. with High Cholesterol and Farmers markets that offer SNAP
pm_snap <- student_data[student_data$Question == 'Farmers markets that accept Supplemental Nutrition Assistance Program (SNAP) benefits',]
pm_snap$DataValue = as.numeric(as.character(pm_snap$DataValue))

cor_snap <- merge(pm_snap, om_hc, by = 'LocationAbbr')

plot(cor_snap$DataValue.x, cor_snap$DataValue.y, col = brewer.pal(n=4, name='Set1'), pch = 16,
     xlab = '% of Farmers markets with SNAP', ylab = '% of High Chol. Pop.'
     , main = 'High Chol. Pop. vs Farmers markets with SNAP', cex.main = 1.5)
text(cor_snap$DataValue.x, cor_snap$DataValue.y, labels = cor_snap$LocationAbbr, cex = 0.7, pos = 2)
abline(lm(cor_snap$DataValue.y ~ cor_snap$DataValue.x))


#distribution of outcome metrics: Overweight population and High Cholesterol 
#Central Limit Theorem

#outcome overweight HS population
x <- om_ob$DataValue
hist(x, prob = TRUE, main = 'Distribution of Overweight HS Population', xlab = '% of HS Population',
     col = brewer.pal(n = 10, name = "Blues"))
mean(x)
sd(x)

samples1 <- 1000
sample.size1 <- 5

xbar1 <- numeric(samples1)
for (i in 1: samples1) {
  xbar1[i] <- mean(sample(x, size = sample.size1, replace = TRUE))
}

hist(xbar1, prob = TRUE, main = 'Sample Size n = 5',col = brewer.pal(n=10, name='BuGn')
     , cex.main = 1.5, xlim = c(25,35), ylim = c(0,0.5), xlab = '% of HS Population')

mean(xbar1)
sd(xbar1)

samples2 <- 1000
sample.size2 <- 10

xbar2 <- numeric(samples2)
for (i in 1: samples2) {
  xbar2[i] <- mean(sample(x, size = sample.size2, replace = TRUE))
}

hist(xbar2, prob = TRUE, main = 'Sample Size n = 10',col = brewer.pal(n=10, name='BuGn')
     , cex.main = 1.5, xlim = c(25,35), ylim = c(0,0.5), xlab = '% of HS Population')

mean(xbar2)
sd(xbar2)

samples3 <- 1000
sample.size3 <- 20

xbar3 <- numeric(samples3)
for (i in 1: samples3) {
  xbar3[i] <- mean(sample(x, size = sample.size3, replace = TRUE))
}

hist(xbar3, prob = TRUE, main = 'Sample Size n = 20',col = brewer.pal(n=10, name='BuGn')
     , cex.main = 1.5, xlim = c(25,35), ylim = c(0,0.5), xlab = '% of HS Population')

mean(xbar3)
sd(xbar3)

#outcome metric high cholesterol population
y <- om_hc$DataValue
hist(y, prob = TRUE, main = 'Distribution of Pop. with High Chol.', xlab = '% of Pop. w/ High Chol.'
     ,col = brewer.pal(n = 10, name = "Blues"))
mean(y)
sd(y)

samples4 <- 1000
sample.size4 <- 5

ybar4 <- numeric(samples4)
for (i in 1: samples4) {
  ybar4[i] <- mean(sample(y, size = sample.size4, replace = TRUE))
}

hist(ybar4, prob = TRUE, main = 'Sample Size n = 5',col = brewer.pal(n=10, name='BuGn')
     , cex.main = 1.5, xlim = c(57,71), ylim = c(0,0.5), xlab = '% of Pop. w/ High Chol.')

mean(ybar4)
sd(ybar4)

samples5 <- 1000
sample.size5 <- 10

ybar5 <- numeric(samples5)
for (i in 1: samples5) {
  ybar5[i] <- mean(sample(y, size = sample.size5, replace = TRUE))
}

hist(ybar5, prob = TRUE, main = 'Sample Size n = 10',col = brewer.pal(n=10, name='BuGn')
     , cex.main = 1.5, xlim = c(57,71), ylim = c(0,0.5), xlab = '% of Pop. w/ High Chol.')

mean(ybar5)
sd(ybar5)

samples6 <- 1000
sample.size6 <- 20

ybar6 <- numeric(samples6)
for (i in 1: samples6) {
  ybar6[i] <- mean(sample(y, size = sample.size6, replace = TRUE))
}

hist(ybar6, prob = TRUE, main = 'Sample Size n = 20',col = brewer.pal(n=10, name='BuGn')
     , cex.main = 1.5, xlim = c(57,71), ylim = c(0,0.5), xlab = '% of Pop. w/ High Chol.')

mean(ybar6)
sd(ybar6)

#Various types of Sampling - outcome overweight HS population

samples7 <- 100
sample.size7 <- 5

#random sampling

xbar_ss <- numeric(samples7)
for (i in 1: samples7) {
  xbar_ss[i] <- mean(x[srswor(sample.size7, length(x)) != 0])
  }

hist(xbar_ss, prob = TRUE, main = 'SRSWOR n = 5', col = brewer.pal(n=10, name='BuPu'),
     cex.main = 2)
mean(xbar_ss)
sd(xbar_ss)

#systemic sampling
#tried to set sample size to 5 but put check in place that it doesn't NA

k <- ceiling(length(x)/sample.size7)
xbar_sys <- numeric(samples7)
for (i in 1: samples7) {
  r <- sample(k,1)
  sys <- seq(r, by = k, length = n)
  sys <- sys[!sys > length(x)]
  xbar_sys[i] <- mean(x[sys])
}

hist(xbar_sys, prob = TRUE, main = 'Systemic Sampling n <= 5', col = brewer.pal(n=10, name='BuPu'),
     cex.main = 2)
mean(xbar_sys)
sd(xbar_sys)

#Various types of Sampling - outcome metric high cholesterol population

#random sampling

ybar_ss <- numeric(samples7)
for (i in 1: samples7) {
  ybar_ss[i] <- mean(y[srswor(sample.size7, length(y)) != 0])
}

hist(ybar_ss, prob = TRUE, main = 'SRSWOR n = 5', col = brewer.pal(n=10, name='BuPu'),
     cex.main = 2)
mean(ybar_ss)
sd(ybar_ss)

#systemic sampling

k <- ceiling(length(y)/sample.size7)
ybar_sys <- numeric(samples7)
for (i in 1: samples7) {
  r <- sample(k,1)
  sys <- seq(r, by = k, length = n)
  sys <- sys[!sys > length(y)]
  ybar_sys[i] <- mean(y[sys])
}

hist(ybar_sys, prob = TRUE, main = 'Systemic Sampling n <= 5', col = brewer.pal(n=10, name='BuPu'),
     cex.main = 2)
mean(ybar_sys)
sd(ybar_sys)

#Confidence Intervals - outcome overweight HS population



conf <- c(80,90)
conf

alpha <- 1 - conf/100
alpha

qnorm(alpha/2)
qnorm(1 - alpha/2)

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), z: %.2f , %.2f",
                 100*(1-i), i, 
                 qnorm(i/2),
                 qnorm(1-i/2))
  cat(str,"\n")
}
x <- om_ob$DataValue
sample.size8 <- 5
pop.mean.x <- mean(x)
pop.sd.x <- sd(x)

sd.sample.means <- pop.sd.x/sqrt(sample.size8)
 
#80%
samples8 <- 20
xbar <- numeric(samples8) 
for (i in 1: samples8) {
  sample.data <- sample(x, size = sample.size8)
  xbar[i] <- mean(sample.data)
  str <- sprintf("%2d: xbar = %.2f, CI = %.2f - %.2f",
                 i, xbar[i], xbar[i] - qnorm(1-alpha[1]/2)*sd.sample.means,
                 xbar[i] + qnorm(1-alpha[1]/2)*sd.sample.means)
  cat(str,"\n")
}
xbar

matplot(rbind(xbar - qnorm(1-alpha[1]/2)*sd.sample.means, xbar + qnorm(1-alpha[1]/2)*sd.sample.means),
        rbind(1:samples8, 1:samples8), type = 'l', lty = 1)
abline(v = pop.mean.x)
points(xbar, 1:samples8, pch=19)

#90%
samples8 <- 20
xbar <- numeric(samples8) 
for (i in 1: samples8) {
  sample.data <- sample(x, size = sample.size8)
  xbar[i] <- mean(sample.data)
  str <- sprintf("%2d: xbar = %.2f, CI = %.2f - %.2f",
                 i, xbar[i], xbar[i] - qnorm(1-alpha[2]/2)*sd.sample.means,
                 xbar[i] + qnorm(1-alpha[2]/2)*sd.sample.means)
  cat(str,"\n")
}
xbar

matplot(rbind(xbar - qnorm(1-alpha[2]/2)*sd.sample.means, xbar + qnorm(1-alpha[2]/2)*sd.sample.means),
        rbind(1:samples8, 1:samples8), type = 'l', lty = 1)
abline(v = pop.mean.x)
points(xbar, 1:samples8, pch=19)

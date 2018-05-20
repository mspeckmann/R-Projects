library(UsingR)


# Part 1
# Use the primes (UsingR) dataset. Use the diff function to compute the
# differences between successive primes. Show the frequencies of these
# differences. Show the barplot of these differences.

primes
difp <- diff(primes) #computes differences
table(difp) #shows frequencies in a table
barplot(table(difp), main = "Prime Diff Frequency", col = "seagreen1", 
        xlab = 'Difference', ylab = 'Frequency', ylim = c(0,80))



# Part 2
# Use the coins (UsingR) dataset. Do not use explicit loops for any
# calculations. Do not hard code the denominations in the solution. The
# solution should work for any denominations.
# 
# a) How many coins are there of each denomination?


x <- table(coins$value)
x

# b) What is the total value of the coins for each denomination?


denom <- unique(coins$value)

for (i in denom) {
  a <- coins[coins$value == i,]
  b <- table(a$value)
  c <- b * i
  print(c)
  }


# c) What is the total value of all the coins?

sum(coins$value)

# d) Show the barplot for the number of coins by year.

coinsyr <- table(coins$year)

barplot(coinsyr, las = 2, xlab = 'Year', ylab = 'Frequency',
        main = 'Coins per Year', col = rainbow(2))


#Part 3
#Use the south (UsingR) dataset. 

#a) Show the stem plot of the data. What do you interpret from this plot?

stem(south)

#Most of the murder rates are between 10 and 14. 

# b) Show the five number summary of the data. Calculate the lower and
#upper ends of the outlier ranges. What are the outliers in the data?

#five number summary
s1 <- fivenum(south)
s1
#calculate the outliers
#iqr <- IQR(south)
#s1 <- quantile(south)
iqr <- s1[4] - s1[2]
ll <- s1[2]-(1.5*iqr) #lowerlimit
ul <- s1[4]+(1.5*iqr) #upperlimit
o1 <- which(south > ul)
o2 <- which(south < ll)
outliers_up <- south[o1] #o1 on its own just gives index
outliers_below <- south[o2] #o2 on its own just gives index
print('Outliers listed below:')
outliers_up
outliers_below

#c) Show the horizontal boxplot of the data along with the appropriate labels
#on the plot.

boxplot(south, horizontal = TRUE, xaxt = "n")
axis(side = 1, at = fivenum(south), labels = TRUE)
text(fivenum(south), rep(1.2,5), srt=90, adj=0, labels = c("Min","Lower Hinge","Median",
                                "Uppder Hinge", "Max"))

#Part 4
#Use the pi2000 (UsingR) dataset.

#a) How many times each of the digits 0 to 9 occur in this dataset?

table(pi2000)

#b) Show the percentages of their frequencies.

table(pi2000)/length(pi2000)

#c) Show the histogram of the data.

hist(pi2000, col = rainbow(10), xlab = "pi2000 digits")


#Part 5 
# Suppose that a football (NFL), basketball (NBA), and hockey (NHL)
# games are being shown at the same time. Consider the two-way
# summarized data shown below showing the preferences of men and
# women what sport they wish to watch.

#a) Using cbind, create the matrix for the above data.

x <- cbind(c(25,20), c(10,20), c(15,30))

#b) Set the row names for the data.

rownames(x) <- c("Men","Women")
x

#c) Set the column names for the data.

colnames(x) <- c("NFL","NBA","NHL")
x

#d) Now, add the dimension variables Gender and Sport to the data.
tmp1 = c("Men","Women")
tmp2 = c("NFL","NBA","NHL")

dimnames(x) <- list(Gender=tmp1, Sport=tmp2)
x

#e) Show the marginal distributions for the Gender and the Sport.

margin.table(x, 1) #Gender
margin.table(x, 2) #Sport

#f) Show the result of adding margins to the data.

addmargins(x)

#g) Show the proportional data separately for Gender and Sport. Interpret
#the results.

prop.table(x, 1)
#This computes the percentages with respect to the sum of the corresponding Gender (row).

prop.table(x, 2)
#This computes the percentages with respect to the sum of the corresponding Sport (column).


# h) Using appropriate colors, show the mosaic plot for the data. Also show
# the barplot for Gender and Sport separately with the bars side by side. Add
# legend to the plots.

mosaicplot(x, color=c("darkviolet","green","deeppink"))

barplot(x, beside=TRUE, legend.text=TRUE, xlab="Sport", col=c("purple","green"), 
        args.legend = list(x = "top"))

barplot(t(x), beside=TRUE, legend.text=TRUE, xlab="Gender", col=c("darkviolet","green","deeppink"),
        args.legend = list(x = "top"))


#Part 6
#Use the midsize (UsingR) dataset.

#a) Show the pair wise plots for all the variables.

pairs(midsize)

#b) Provide at least 4 interpretations of the results.

#There is a linear correlation between all of the variables. This can be
#interpreted in the following ways:

#Over time (as years increase) the value of Accords, Camrys and Taurus go up
#As the price of an Accord increases, the price of a Camry increases and vice versa.
#As the price of a Camry increase, the price of a Taurus increases and vice versa.
#As the price of an Accord increases, the price of a Taurus increases and vice versa. 

#Part 7
#Use the MLBattend (UsingR) dataset.

# a) Extract the wins for the teams BAL, BOS, DET, LA, PHI into the
# respective vectors.

bal_ <- MLBattend$wins[MLBattend$franchise == 'BAL']
bos_ <- MLBattend$wins[MLBattend$franchise == 'BOS']
det_ <- MLBattend$wins[MLBattend$franchise == 'DET']
la_ <-  MLBattend$wins[MLBattend$franchise == 'LA']
phi_ <- MLBattend$wins[MLBattend$franchise == 'PHI']

# b) Create a data frame of five columns using these vectors. Use the team
# names for the columns

z <- data.frame(bal_, bos_, det_, la_, phi_)
colnames(z) <- c("BAL", "BOS", "DET", "LA", "PHI")
z

#c) Show the boxplot of the data frame

boxplot(z, horizontal = TRUE, col=rainbow(5), ylab = "Teams", xlab = "Summary of Wins")

#d) Provide at least 5 interpretations of the results.


fivenum(phi_)
fivenum(la_)
fivenum(det_)
fivenum(bos_)
fivenum(bal_)


#Team BAL has the largest range of number of wins out of all the teams. 

#Team BOS had the smallest IQR and had the smallest range of upper and lower limits.
#They had two years of abnormally low wins (outliers) that deviated from their otherwise close range.

#Team PHI had the lowest median of wins compared to all the teams while team BAL had the highest.

#Team LA had one year of wins that was abnormally low (outlier) compared to their upper and lower limits. 

#Team BAL had the most wins in a year (109) out of all the teams.

#Part 8
# Use the following two CSV datasets showing the current US Senate and
# House members.

#a) How many members are there by party affiliation in the senate? How
#many members are there by party affiliation in the house?

senate <- read.csv("senate.csv", header = TRUE)
house <- read.csv("house.csv", header = TRUE)


table(senate$Party)
table(house$Party)


#b) Show the barplots of results from a). Use the appropriate colors for the
#respective parties (maybe, purple for independents)

barplot(table(senate$Party), main = 'Senate', xlab = "Party", ylab = 'Frequency', 
        col = c("blue","purple","red"))

barplot(table(house$Party), main = 'House', xlab = "Party", ylab = 'Frequency', 
        col = c("blue","red"))

# c) Who is the longest serving member in the senate? Who is the longest
# serving member in the house?

a <- fivenum(senate$Years_in_office)
b <- fivenum(house$Years_in_office)

senate$Name[senate$Years_in_office == a[5]]
house$Name[house$Years_in_office == b[5]]

#d) How many senators have their term ending in 2019?

nrow(senate[grep("2019", senate$Term_ends),])

#f) How many representatives are there from each state?

c <- table(house$State)

#g) Which state has the highest number of representatives?

d <- fivenum(table(house$State))
c[c == d[5]]


#h) Which states have the least number of representatives?
c[c == d[1]]



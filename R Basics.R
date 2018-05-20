#Part 1

#Load the data
data("rivers")
rivers

#a)	How many data points are there in the data set?  
length(rivers)

#b) Compute the mean, median, and mode.
mean(rivers)
median(rivers)

# calculate mode
names(table(rivers))[table(rivers)==max(table(rivers))]

#c) Compute the variance and the standard deviation.
var(rivers)
sd(rivers)

#d) Compute the five number summary, the interquartile range, and outliers, if any. 
x <- fivenum(rivers)
iqr <- x[4]-x[2]
lower <- x[2] - 1.5*iqr
lower
upper <- x[4] + 1.5*iqr
upper
i <- (rivers - upper) >= 0
rivers[i]

#e) Compute the standardized version (z-scores) of the above data.
z <- (rivers - mean(rivers))/sd(rivers)
z

#f) Create a matrix of size 2 x 30 using the first 60 data points in rivers. 
#The first 30 values belong to the first row of the matrix. Assign the result to the variable, rivers.60, and display the result.
rivers.60 <- matrix(rivers[1:60], nrow = 2, ncol = 30, byrow = TRUE)

#g) Without hardcoding, show the code for displaying the first and last columns of the matrix.
rivers.60[,1]
rivers.60[,ncol(rivers.60)]

#h) Assign row names for the rivers.60 as Row_1 and Row_2 and column names as Length_1, Length_2, ..Length_30. The code should not hard code the values of the numbers in the row and column names. 
row.names <- paste("Row", 1:nrow(rivers.60), sep="_")
col.names <- paste("Length", 1:ncol(rivers.60), sep="_")
dimnames(rivers.60) <- list(row.names, col.names)
rivers.60

#Part 2
#a) Read in the data from johnson.csv. 
setwd("C:\\Users\\guanglan\\Dropbox\\BUwork\\R\\CS544")
getwd()
johnson <- read.csv("johnson.csv", row.names=1)

#b)	Make a new data frame based on the data frame by removing the "Year" column from it while keeping data in columns "Qtr1", 'Qtr2", "Qtr3", and "Qtr4". Assign the data in the "Year" column to be the row names of the new data frame. 
summary(johnson)

#c)	Add a new column, Total, showing the earnings for the whole year (the sum of earnings for the 4 quarters). 
johnson$Yearly <- johnson$Qtr1+johnson$Qtr2+johnson$Qtr3+johnson$Qtr4
johnson

#d)	Which was the best performing year (in terms of highest earning) and worst performing year? 
best <- subset(johnson, johnson$Yearly == max(johnson$Yearly))
rownames(best)

worst <- subset(johnson, johnson$Yearly == min(johnson$Yearly))
rownames(worst)

#e)	Show all rows of the data frame whose "Yearly" is greater than 20.
subset(johnson, johnson$Yearly > 20)

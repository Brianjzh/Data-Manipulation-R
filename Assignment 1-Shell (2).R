#Programming 1 - Assignment 1
#Your name(s):

#The data of over 1000 eBay auctions is provided in the file eBayAcution.csv. 
#Use RStudio to study this marketplace. 
#(Source: The data is adapted from this book: https://www.dataminingbook.com/book/r-2nd-edition-2023)

#1) Load the file: "eBayAcution.csv" and save it as auctionData.
auctionData = read.csv("eBayAuctions.csv")


#2) Write a code that checks if the dataset has any missing values, 
#   a code that returns the number of auctions (i.e., rows), 
#   and one to return the number of variables (i.e., columns).
anyNA(auctionData)
sum(is.na(auctionData))
nrow(auctionData)
ncol(auctionData)


#3) What is the maximum auction duration? How many auctions were open for these many days? 
#   What is the average auction duration? What percentage of the auctions have an above average duration?
max_auction_Duration = max(auctionData$Duration)

print(max_auction_Duration)
sum(auctionData$Duration == 10)

avg_auction = mean(auctionData$Duration)
print(avg_auction)
above_avg_count = sum(auctionData$Duration > avg_auction)
total_auctions = length(auctionData$Duration)
(above_avg_count / total_auctions) * 100

#4) Create a new variable called Ratio that calculates the ratio of the closing price over the opening price 
#   for each auction and add this variable to the dataset as a new column. 
#   What's the average ratio of all auctions? What's the average ratio of 'Computer' auctions?
auctionData$Ratio = auctionData$ClosePrice / auctionData$OpenPrice
mean(auctionData$Ratio)
mean(auctionData$Ratio[auctionData$Category == "Computer"])

#5) Create an object named "catNames" that contains the names of unique auction categories, 
#   sorted in alphabetical order. Write a code to return the number of categories stored in this object.
catNames = unique(auctionData$Category)
sorted_unique = sort(catNames)
print(sorted_unique)
length(sorted_unique)

#6) Write a loop to go through "catNames" and calculate the number of auctions in each category. 
#   In so doing, save the results in a vector called "numAuctions". 
#   Write a code to return the values stored in this object.
numAuctions = c()
for (i in 1:length(sorted_unique)){
  x <- sorted_unique[i]
  numAuctions[i] = sum(auctionData$Category == x)
}
numAuctions



#7) Combine the two objects (catNames and numAuctions) into a new data frame called catInfo. 
# Write two different codes to return the fifth element of the second column in the catInfo dataframe.
catInfo = data.frame(cbind(sorted_unique, numAuctions))
catInfo[5,2]
catInfo$Auctions[5]

#8) Write a piece of code that prints the name of each category and the number of auctions in that category.
for (i in 1:nrow(catInfo)) {
  cat("Category: ", catInfo$Category[i], "- Number of Auctions: ", catInfo$Auctions[i], "\n")
}



#9) Create a function, called weekendTest, that checks whether a given day is a weekend (endDay of 'Sat' or 'Sun') 
#   or not and returns TRUE or FALSE (logical constants in R).
#   Then use this function to create a new variable (called Weekend) that shows if each auction had an endDay of the weekend or not. 
#   Add this variable to the dataset as a new column. How many auction ended on weekend? (Write a code that returns this value)
weekendTest = function(x){
  if(x== "Sat" | x=="Sun"){return(TRUE)}
  else {return(FALSE)}
}
Weekend = sapply(auctionData$endDay, weekendTest)
auctionData$Weekend = sapply(auctionData$endDay, weekendTest)
sum(auctionData$Weekend)



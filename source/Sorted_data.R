# Load the merged data file into a data frame named merged_Data
merged_Data <- read.csv("data/merged_Data.txt",header=T, sep = "|")
library(dplyr)

# Sort the Merged Data file in increasing order of GDP
sort_by_GDP <- arrange(merged_Data,GDP.in.Millions.USD)

#Verify the last country in the sorted data frame is United States
tail(sort_by_GDP$Country, n=1)

# Let's view the structure of the data
head(sort_by_GDP$Country)

# Find the 13th country in the sorted data set. 
sort_by_GDP[13,c("Country.Code","Country","GDP.in.Millions.USD","GDP.Ranking")]
# "St. Kitts and Nevis" is the 13 country in ascending order of GDP of the 189 countries in the merged data
# Its country code is KNA, GDP Rank is 178 and GDP is 767 Million USD.

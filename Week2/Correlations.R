mydata <- read.csv("Pearson_Spearman_Correlations.csv")

#Avoid getting results in scientific notation!
options(scipen = 999)

# View the first few rows of the data
head(mydata)

# Summarize each variable
summary(mydata)

plot(mydata$X, mydata$Y)

# Method can be pearson or spearman
cor(mydata$X, mydata$Y, method="pearson")

#The code below won't work if 'r' in rank is in lower case
cor(mydata$rankX, mydata$rankY, method="pearson")

#The code below will work, because 'R' in Rank is in upper case (as it is in the data set)
cor(mydata$RankX, mydata$RankY, method="pearson")

#Alternatively, do the following!
cor(mydata$X, mydata$Y, method="spearman")

#Correlation matrix for all variables in the data set mydata
cor(mydata)

#Hypothesis Tests
#Is the Pearson correlation between X and Y significant?
#p-value is 0.01142 - since it's significantly lower than 0.05, we reject
#the null hypothesis that rho = 0
cor.test(mydata$X, mydata$Y, method=("pearson"))

#Is the Spearman correlation between X and Y significant?
#p-value is infinitesimally small, so we reject the null hypothesis that
#rho = 0.
cor.test(mydata$X, mydata$Y, method=("spearman"))

#This yields an error
cor.test(mydata)



# An additional way of looking at correlations
install.packages("Hmisc")   # Why does this give an error?
library(Hmisc)
# Forces data frame to matrix - required for function to work
test <- rcorr(as.matrix(mydata), type="pearson")
test$r                    # correlation matrix (r's)
test$P                    # p-values for the correlation matrix

# In this example, the spearman correlations between all variables will be 1's and
# the p-values will all be approximately 0.s
test <- rcorr(as.matrix(mydata), type="spearman")
test$r                    # correlation matrix (r's)
test$P                    # p-values for the correlation matrix



#Let's look at some COVID data
covid <- read.csv("COVID Cases and Deaths - August 5, 2021 - NY Times.csv")
covid <- na.omit(covid) #Excluding observations with some missing values
head(covid) #Let's look at the first few rows in the data set

#Let's look at the distribution of the cases and deaths variables
hist(covid$cases, breaks = 20)
hist(covid$deaths, breaks = 20)

#We can go into the data set and sort cases in descending order; we see that
#LA county has a very high number of case (over 1,000,000)

#Because of the skewed histogram, we can look at Spearman correlations.
cor(covid$cases, covid$deaths, method="spearman")

#We can also exclude LA County from the analyses -- maybe the outlier has an effect
#on the correlation? We can see that it doesn't!
cor(covid$cases[covid$county!="Los Angeles"],
    covid$deaths[covid$county!="Los Angeles"], method="spearman")



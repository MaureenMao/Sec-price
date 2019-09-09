#Open the file
library(readxl)
monthly_sec_only <- read_excel("D:/JHU/career/1/monthly_sec_only.xlsx")
View(monthly_sec_only)

summary(monthly_sec_only)

##Q1
#Check the missing value
sum(is.na(c(monthly_sec_only$Partyname, monthly_sec_only$Bmunit)))
#Import functions
library(dplyr)
library(tidyr)
library(reshape2)
library(fBasics)
library(stats)
library(car)
#Kick out the duplicate data
t <- select(monthly_sec_only, Partyname, Bmunit)
t1 <- t[!duplicated(t),]
#Arrange and sort
arrange(t1, Partyname)
num <- table(t1$Partyname)
sort(num, decreasing=TRUE)
#Maximum is the first one in the list, "E.ON UK plc 21"

##Q2

p <- data.frame(monthly_sec_only$Month , monthly_sec_only$B_Sec, monthly_sec_only$Sec_Mwh, stringsAsFactors = FALSE)
p <- arrange(p, monthly_sec_only$Month)

#Check NAs
p0 <- na.omit(p)
dim(p)
dim(p0)
#They are 10 missing values
list <- which(rowSums(is.na(p)) !=0)
p_na <- p[list, ]
p_na
#in this case we simply wipe NAs out because the missing data has little influence on the result

#Define the montly price
names(p0)
p0 <- rename (p0, Month = "monthly_sec_only.Month", Bid = "monthly_sec_only.B_Sec", Volume = "monthly_sec_only.Sec_Mwh")
names(p0)
Month <- group_by(p0, Month)
s <- sum(p0[ ,2])
sec_price <- summarize(Month, secprice = sum(Bid * Volume / s))

#Create a time series data
price <- ts(sec_price$secprice, frequency = 12 , start = c(2005,11))
plot(price, xlab = 'Year', ylab = '(Secondary)Volume Weighted Average Price', main = 'Monthly Price Line', type = 'l')

##Q3
#Create a new chart of pence per KWh of coal, oil and gas from chart "Quarterly" and extract column A,B,D,F,G
#Impoer the refined data from another working sheet called "input price"
coal_gas_prices <- read_excel("D:/JHU/Career/1/coal_gas_prices.xls", sheet = "Input Price")
View(coal_gas_prices)
names(coal_gas_prices)

input_prices <- coal_gas_prices[ ,3:5]
head(input_prices)

#Merge the data to a dataset
#Modify the length of second price to the length of quarterly data in the sheet called "Sec"

sec <- read_excel("D:/JHU/Career/1/coal_gas_prices.xls", sheet = "Sec")

#Create join variable "qt"
sec$qt <- rep(1:16, each = 3)
input_prices$qt <- rownames(input_prices)

sec3 <- merge(sec, input_prices)
sec3 <- select(sec3,secprice:Gas)
sec3 <- sec3[-1, ]
#Check the merged dataset
head(sec3)
View(sec_price)

#Calling lm
fit <- lm (secprice ~ . , data=sec3)
summary (fit) $coefficient

##Interpretation
#R-square equals to 0.1195 and adjusted R-square equals to 0.05804, which is acceptable
#The probility tells us that the price of caol, oil and gas all have significant statistical impacts on the level of 5%
#Which means a $0.5 decrease in the coal price would lead to a $1 rise in the second price; a $0.03, $0.1 increase in the oil and gas price could lead to a $1 rise in the second price 

#Verify the multicollinearity
vif(fit)
#VIf less than 10, which means there is no multicollinearity

#From the analysis, we can conclude that although oil price has a little impact on the second price, caol price could have an adverse impact.
#So in order to analysis the second price of the plants, we should also pay close attention to the price of the coal market.





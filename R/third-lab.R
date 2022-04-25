##### The goal is to show the student #####
# The goal is to show the student
#   a. how to build the data and
#   b. How to run a simple OLS CAPM regression
#   c. How to expand the set of regressors
#   d. interpreting and sicussing the R2 and adj r2
################################################################
library(ggplot2)
library(lmtest)
library(scales)
library(lubridate)
library(stargazer)
# install.packages("*"): if want to install any(*) packages
################################################################
####################################################################2.3
##### a. #####
# Import data
library(readxl)
df <- read_excel("/home/rstudio/Return.xlsx")
df <- as.data.frame(df)

# Change date format
# library(lubridate)
###############################define dates
df$Date <- as.Date(paste(df$date, 1), "%Ym%m %d")


##### Calculate returns #####
for(i in c(2:nrow(df))){
  df$Return[i] <- (df$apple[i]-df$apple[i-1])/df$apple[i-1]*100
}

#################################################4.3.
##### Build the variables for the regression: excess returns, excess returns on the mkt #####
df$Excess_Returns <- df$Return - df$rf
# df$mktrf is excess returns on the mkt


##### Regress the basic model (no robust se) #####
# library(lmtest)
Model_2 <- lm(Excess_Returns ~
                mktrf + smb + hml
              , data=df
              , na.action=na.exclude)
coeftest(Model_2)
stargazer(Model_2, type="text")

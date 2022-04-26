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

######define date
df$Date <- as.Date(paste(df$date, 1), "%Ym%m %d")


##### Plot Apple prices #####
# library(ggplot2)
# library(scales): change date_format
ggplot(data = df) +
  geom_line(aes(x = Date, y = apple),
            color = "blue",
            size = 0.6) +
  labs(x = "Date",
       y = "Apple Price") +
  scale_x_date(date_breaks = "3 years", labels = date_format("%Y %b")) +
  theme_minimal()+
  theme_bw()+
  theme(panel.grid =element_blank())+
  theme(axis.title.x = element_text(size =12))+
  theme(axis.title.y = element_text(size =12))+
  theme(axis.text.x = element_text(size =12))+
  theme(axis.text.y = element_text(size =12))


##### Calculate returns #####
for(i in c(2:nrow(df))){
  df$Return[i] <- (df$apple[i]-df$apple[i-1])/df$apple[i-1]*100
}
##### Plot Returns
ggplot(data = df) +
  geom_line(aes(x = Date, y = Return),
            color = "blue",
            size = 0.6) +
  labs(x = "Date",
       y = "Apple Return") +
  scale_x_date(date_breaks = "3 years", labels = date_format("%Y %b")) +
  theme_minimal()+
  theme_bw()+
  theme(panel.grid =element_blank())+
  theme(axis.title.x = element_text(size =12))+
  theme(axis.title.y = element_text(size =12))+
  theme(axis.text.x = element_text(size =12))+
  theme(axis.text.y = element_text(size =12))


##### Calculate histogram of returns #####
Apple_Return <- df$Return
h <- hist(Apple_Return)
h
# h$breaks
# h$counts
# h$density
# h$mids
###################################################

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
library(readxl)
# install.packages("*"): if want to install any(*) packages
################################################################
####################################################################2.3
##### a. #####
# Import data



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

#################################################4.1.
##### Build the variables for the regression: excess returns, excess returns on the mkt #####
df$Excess_Returns <- df$Return - df$rf
# df$mktrf is excess returns on the mkt


##### Regress the basic model (no robust se) #####
# library(lmtest)
Model_1 <- lm(Excess_Returns ~
                mktrf
              , data=df
              , na.action=na.exclude)
coeftest(Model_1)
stargazer(Model_1, type="text")

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


# df$mktrf is excess returns on the mkt


##### Regress the basic model (no robust se) #####
# library(lmtest)
Model_1 <- lm(Excess_Returns ~
                mktrf
              , data=df
              , na.action=na.exclude)
coeftest(Model_1)
stargazer(Model_1, type="text")

##### Regress the basic model (no robust se) #####
# library(lmtest)
Model_2 <- lm(Excess_Returns ~
                mktrf + smb + hml
              , data=df
              , na.action=na.exclude)
coeftest(Model_2)
stargazer(Model_2, type="text")

##### Save residuals #####
df$e_1 <- residuals(Model_1)
df$e_2 <- residuals(Model_2)
##################################################

#################################################4.5.

resid1 <- df$e_1
h1 <- hist(resid1)
h1

resid2 <- df$e_2
h2 <- hist(resid2)
h2
###############################################

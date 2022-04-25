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

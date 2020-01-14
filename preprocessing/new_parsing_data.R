
# packages ----------------------------------------------------------------
library(readr)
library(lubridate)
library(dplyr)
library(xts)


# import and inspect data -------------------------------------------------

tides <- read.csv("../data/output/venezia.csv")
weather <- read.csv("../data/output/weatherData_2010-2018.csv")


head(tides)
tail(tides)
str(tides)
nrow(tides)

# estraiamo da inizio 2010 a fine 2018
ind <- as.numeric(rownames(tides[tides$datetime=="2010-01-01 00:00:00",]))
tides <- tides[ind:(nrow(tides)-1),]
nrow(tides)
head(tides)
tail(tides)

nrow(weather)
head(weather)
tail(weather)


# create a unique dataset -------------------------------------------------
df <- merge(tides, weather, by.x = "datetime", by.y = "all_data", all = T)
nrow(df)
head(df)
tail(df)


as.numeric(count(df[is.na(df),]))



# define a strategy to impute missing data --------------------------------
library(mice)
library(Hmisc)
describe(df)

df_miss <- subset(df, select = -c(datetime))

md.pattern(df_miss)

impute_arg <- aregImpute(~ level + vel_wind + dir_wind + rain, data = df, n.impute = 10)

impute_arg
names(impute_arg)
impute_arg$imputed$rain

completeData <- impute.transcan(impute_arg, imputation = 1, data = df, list.out = TRUE, pr = FALSE, check = FALSE) 

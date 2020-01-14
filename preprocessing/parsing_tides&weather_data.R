
# packages ----------------------------------------------------------------

library(readr)
library(lubridate)
library(dplyr)
library(xts)
library(mice)
library(Hmisc)


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

describe(df)

md.pattern(df)

impute_arg <- aregImpute(~ level + vel_wind + dir_wind + rain, data = df, n.impute = 15)

impute_arg
names(impute_arg)

completeData <- impute.transcan(impute_arg, imputation = 1, data = df,
                                list.out = T, pr = F, check = F) 


# imputation --------------------------------------------------------------

complete_df <- as.data.frame(df$datetime)
complete_df <- as.data.frame(cbind(complete_df, completeData$level, completeData$rain,
                                   completeData$vel_wind, completeData$dir_wind))

names(complete_df) <- c("datetime", "level", "rain", "vel_wind", "dir_wind")
head(complete_df)
tail(complete_df)



# check means and stds ----------------------------------------------------

mean(df$rain, na.rm = T)
mean(complete_df$rain)

mean(df$dir_wind, na.rm = T)
mean(complete_df$dir_wind)

mean(df$vel_wind, na.rm = T)
mean(complete_df$vel_wind)

mean(df$level)
mean(complete_df$level)
sd

means_df <- c(mean(df$level), mean(df$rain, na.rm = T), mean(df$dir_wind, na.rm = T), mean(df$vel_wind, na.rm = T))
means_df <- round(means_df, digits = 3)

means_completedf <- c(mean(complete_df$level), mean(complete_df$rain, na.rm = T),
                      mean(complete_df$dir_wind, na.rm = T), mean(complete_df$vel_wind, na.rm = T))
means_completedf <- round(means_completedf, digits = 3)

std_df <- c(sd(df$level), sd(df$rain, na.rm = T), sd(df$dir_wind, na.rm = T), sd(df$vel_wind, na.rm = T))
std_df <- round(std_df, digits = 3)

std_completedf <- c(sd(complete_df$level), sd(complete_df$rain, na.rm = T),
                      sd(complete_df$dir_wind, na.rm = T), sd(complete_df$vel_wind, na.rm = T))
std_completedf <- round(std_completedf, digits = 3)

matrix(c(means_df, means_completedf, std_df, std_completedf), nrow = 4, ncol = 4,
       dimnames = list(c("level", "rain", "dir_wind", "vel_wind"),
                       c("True means", "Imputed means", "True std", "Imputed std")))


# saves data --------------------------------------------------------------

write.csv(complete_df, "../data/output/df_final2010-2018.csv", row.names = F)


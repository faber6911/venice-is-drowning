rm(list = ls()) # clean environment

# packages ----------------------------------------------------------------

library(xts)
library(lubridate)
library(dplyr)
library(tidyverse)
library(ggplot2)


# import data --------------------------------------------------------------

data <- read_delim("../data/output/venezia_xts.csv",
                   delim = " ",
                   col_types = cols_only(col_datetime(), col_number()))



# inspections -------------------------------------------------------------

head(data)

data_ts <- xts(order.by = data$datetime,
               x = data$level,
               frequency = 24)

plot(data_ts, main = "Venice tides")


# check for stationarity in variance ----------------------------------------

data %>%
  add_column(date = date(data$datetime)) %>%
  group_by(date) %>% 
  mutate(mean_day = mean(level), sd_day = sd(level)) %>%
  select(date, mean_day, sd_day) %>% 
  distinct(date, mean_day, sd_day) %>%
  ggplot(aes(mean_day, sd_day)) + 
  geom_point() + 
  ggtitle("Mean per day vs Std per day") +
  labs(x = "Day mean",
       y = "Day std") +
  geom_smooth(method = "lm", se = FALSE)+
  theme_b()


# Osservando questo plot sembrerebbe che la serie sia stazionaria in varianza.
# Ciò potrebbe avere senso considerando che si tratta di un fenomeno naturale e non di un evento umano soggetto
# alla crescita economica o demografica

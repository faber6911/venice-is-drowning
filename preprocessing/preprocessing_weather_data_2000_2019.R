

# packages ----------------------------------------------------------------

library(readr)
library(lubridate)

# import data -------------------------------------------------------------

data<- read_delim("D:/GIT/venice-is-drowning/new_data/dati_meteo/dati_meteo_2000_2019.txt", 
                                   ";", escape_double = FALSE, trim_ws = TRUE)


# manipulation ------------------------------------------------------------

head(data)

View(data)

# troviamo i punti in  cui il dataset ricomincia
print(which(data$DATA == "01/03/2000" & data$ORA == "01"))

# tagliamo i dati relativi alla direzione del vento in formato stringa
data <- data[163255:nrow(data),]
head(data)

# creiamo 3 dataset distinti per ogni feature, pioggia, direzione vento ed intensità vento
dir_wind <- data[1:163254,]
head(dir_wind)
head(dir_wind)

rain <- data[163255:333835,]
vel_wind <- data[333836:nrow(data),]

View(dir_wind)

# realizziamo colonna datetime
dir_wind$datetime <- as.Date(dir_wind$DATA, format = "%d/%m/%Y") + hours(as.numeric(dir_wind$ORA))
rain$datetime <- as.Date(rain$DATA, format = "%d/%m/%Y") + hours(as.numeric(rain$ORA))
vel_wind$datetime <- as.Date(vel_wind$DATA, format = "%d/%m/%Y") + hours(as.numeric(vel_wind$ORA))

# eliminiamo features inutili
dir_wind$STAZIONE <- NULL
dir_wind$SENSORE <- NULL
dir_wind$DATA <- NULL
dir_wind$ORA <- NULL


vel_wind$STAZIONE <- NULL
vel_wind$SENSORE <- NULL
vel_wind$DATA <- NULL
vel_wind$ORA <- NULL


rain$STAZIONE <- NULL
rain$SENSORE <- NULL
rain$DATA <- NULL
rain$ORA <- NULL

# aggiorniamo colnames
colnames(dir_wind) <- c("dir_wind", "datetime")
colnames(vel_wind) <- c("vel_wind", "datetime")
colnames(rain) <- c("rain", "datetime")

head(dir_wind,2)
head(vel_wind,2)
head(rain,2)

# convertiamo datetime in character per evitare problemi di merge
dir_wind$datetime <- as.character(dir_wind$datetime)
vel_wind$datetime <- as.character(vel_wind$datetime)
rain$datetime <- as.character(rain$datetime)

length(seq(from = rain$datetime[1], to = rain$datetime[nrow(rain)], by = "hour"))

# nessuno dei dati sembrerebbe completo, mancano delle osservazioni

# effettuiamo il merge dei tre dataset
tmp <- merge(vel_wind, dir_wind, by = "datetime")
final <- merge(tmp, rain, by = "datetime")

tail(final)

print(which(final$datetime == "2019-01-01 00:00:00"))
final[153202,]

# eliminiamo la parte di 2019 che non ci serve poichè non avremmo comunque i relativi dati per le maree
final <- final[1:153202,]

# dal 24/12/2002 al 14/01/2003 compresi sono dati mancanti

tail(grep("2002-", final$datetime))
tail(final)

# scartiamo i dati dal 2000 a fine 2002 poichè già presenti negli altri dataset
final <- final[21875:nrow(final),]
rownames(final) <- NULL

head(final)
tail(final)

# conteggio totale dati mancanti
print(paste0("Mancano ",
      length(seq(from = as.POSIXct("2003-01-01 00:00:00"), to = as.POSIXct("2019-01-01 00:00:00"), by = "hour")) - nrow(final)
      ," osservazioni"))

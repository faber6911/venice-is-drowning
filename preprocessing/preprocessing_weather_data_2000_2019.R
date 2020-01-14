

# packages ----------------------------------------------------------------

library(readr)
library(lubridate)

# import data -------------------------------------------------------------

data<- read_delim("D:/GIT/venice-is-drowning/new_data/dati_meteo/dati_meteo_2000_2019.txt", 
                                   ";", escape_double = FALSE, trim_ws = TRUE)


# manipulation ------------------------------------------------------------

head(data)

#View(data)

# troviamo i punti in  cui il dataset ricomincia
print(which(data$DATA == "01/03/2000" & data$ORA == "01"))

326509-163255
# tagliamo i dati relativi alla direzione del vento in formato stringa
data <- data[163255:nrow(data),]
head(data)
nrow(data)
print(which(data$DATA == "01/03/2000" & data$ORA == "01"))

# creiamo 3 dataset distinti per ogni feature, pioggia, direzione vento ed intensità vento
dir_wind <- data[1:163254,]
head(dir_wind)
tail(dir_wind)

rain <- data[163255:333835,]
head(rain)
tail(rain)

vel_wind <- data[333836:nrow(data),]
head(vel_wind)
tail(vel_wind)

# realizziamo colonna datetime
dir_wind$datetime <- as.Date(dir_wind$DATA, format = "%d/%m/%Y") + hours(as.numeric(dir_wind$ORA))
rain$datetime <- as.Date(rain$DATA, format = "%d/%m/%Y") + hours(as.numeric(rain$ORA))
vel_wind$datetime <- as.Date(vel_wind$DATA, format = "%d/%m/%Y") + hours(as.numeric(vel_wind$ORA))

head(dir_wind)
tail(dir_wind)

head(rain)
tail(rain)

head(vel_wind)
tail(vel_wind)

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

head(dir_wind)
tail(dir_wind)
head(vel_wind)
tail(vel_wind)
head(rain)
tail(rain)

# convertiamo datetime in character per evitare problemi di merge
dir_wind <- as.data.frame(dir_wind)
vel_wind <- as.data.frame(vel_wind)
rain <- as.data.frame(rain)

dir_wind$datetime <- as.character(dir_wind$datetime)
vel_wind$datetime <- as.character(vel_wind$datetime)
rain$datetime <- as.character(rain$datetime)

# occorre verificare come approcciare il problema dei missing values
# innanzitutto occorre disporre di tutte le date, ed effettuare il merge dei data.frame
# riempiendo i valori mancanti con NA

nrow(rain) == length(seq(from = as.POSIXct(rain$datetime[1]), to = as.POSIXct(rain$datetime[nrow(rain)]), by = "hour"))
# Vediamo appunto che il numero di date non corrisponde a quello che dovrebbe essere

# estraiamo tutte le date
all_data = seq(from = as.POSIXlt(rain$datetime[1], tz = "UTC"), to = as.POSIXct("2019-11-13 00:00:00", tz = "UTC"), by = "hour")
head(as.data.frame(all_data))
all_data <- strftime(all_data, tz = "UTC") #rimuoviamo la timezone che da solo noia
head(as.data.frame(all_data))
tail(as.data.frame(all_data))

# cominciamo realizzando un dataframe che contenga tutte le date
yy <- as.data.frame(all_data)
head(yy)
tail(yy)

# Merging datasets and define data interval -------------------------------

# a questo punto possiamo cominciare ad effettuare il merge specificando all = TRUE proprio per riempire le
# righe vuote con NA values
yy <- merge(yy, rain, by.x = "all_data", by.y = "datetime", all = TRUE)
yy <- merge(yy, vel_wind, by.x = "all_data", by.y = "datetime", all = TRUE)
yy <- merge(yy, dir_wind, by.x = "all_data", by.y = "datetime", all = TRUE)

# verifichiamo ancora una volta che tutto sia andato nel modo previsto
head(yy)
tail(yy)

# A questo punto estraiamo i nostri dati di interesse, ovvero eliminiamo tutte le istanze 2019, poichè non possediamo
# i dati delle maree per quell'anno e partiamo dalla prima osservazione del 2015, 3 anni dovrebbero essere sufficienti
yy[yy$all_data=="2019-01-01 00:00:00",]
yy <- yy[1:165119, ]
tail(yy)
# il 2019 è stato rimosso

yy[yy$all_data=="2010-01-01 00:00:00",]
yy <- yy[86232:nrow(yy),]
head(yy)
tail(yy)
# adesso i dati partono da inizio 2010 e finiscono a fine 2018

# Verifichiamo che i numeri effettivamente coincidano
nrow(yy) #78888
length(seq(from = as.POSIXct("2010-01-01 00:00:00"), to = as.POSIXct("2018-12-31 23:00:00"), by = 'hour'))

# nessuno dei dati, rain, vel_wind e dir_wind sembrerebbe completo, occorre scegliere una strategia per l'handling
# dei missing values

sum(is.na(yy$rain)) # 500
sum(is.na(yy$dir_wind)) # 397
sum(is.na(yy$vel_wind)) # 215

# molto probabilmente velocità e direzione del vento sono rilevati dallo stesso sensore quindi i dati mancanti di
# queste due misure quasi sicuramente coincidono, la pioggia invece è a sè stante

yy[is.na(yy$rain),] # un po più sparse rispetto a quelle mancanti del vento ma comunque risolvibile
yy[is.na(yy$dir_wind),] # 3 giorni e mezzo a giugno 2016


# Salviamo i dati meteo
write.csv(yy, "../data/output/weatherData_2010-2018.csv", col.names = T, row.names = F)
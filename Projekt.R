# Ustawienie sciezki
setwd("E:/STUDIA/SEMESTR VI/Inżynierski projekt dyplomowy")

# zwraca aktualny katalog roboczy
getwd() 

# Wczytanie danych
dane <- head(read.csv("Timeseries.csv", header = TRUE, sep = ",", skip = 10), -7)

# Usuniecie niepotrzebych kolumn
dane <- dane[,-3:-7]

# Zaladowanie pakietu dplyr i tidyr 
library(dplyr)
library(tidyr)

# Rozdzielenie kolumny time na Data i Godzina
dane <-  separate(dane, time, c('Data', 'Godzina'))

# Zmiana nazw kolumn
colnames(dane)[3] = "Moc"

# Formatowanie daty
library(lubridate)
dane$Data <- ymd(dane$Data)

# Formatowanie godziny
dane$Godzina <- substr(dane$Godzina, start = 1, stop = 2)
dane$Godzina <- sprintf("%02d", as.numeric(dane$Godzina))
dane$Godzina[dane$Godzina == "00"] <- "24"

# Konwersja typow zakowych kolumn
# dane$Godzina <- as.integer(dane$Godzina)
# dane$Moc <- as.double(dane$Moc)

str(dane)

#Srednia, najgorszy i najlepszy rok


#------------------------------------------------------------------#

setwd("E:/STUDIA/SEMESTR VI/Inżynierski projekt dyplomowy/ceny")

library(lubridate)

for (ceny in list.files()){
  
  # Zlaczenie danych jezeli istnieja
  if (exists("RCE")){
    tempory <-read.csv(ceny, sep = ";", header=TRUE, fileEncoding = "Windows-1250", stringsAsFactors = FALSE)
    tempory$Data <- ymd(tempory$Data) # konwersja daty na format R
    RCE <-rbind(RCE, tempory)
    rm(tempory)
  }
  
  # Utworzenie danych, jesli nie istnieja
  if (!exists("RCE")){
    RCE <- read.csv(ceny, sep = ";", header=TRUE, fileEncoding = "Windows-1250", stringsAsFactors = FALSE)
    RCE$Data <- ymd(RCE$Data) # konwersja daty na format R
  }
}

RCE$RCE[41008]

#Usuwanie twardej spacji
RCE$RCE <- gsub("\u00A0", "", RCE$RCE)
#RCE$RCE <- gsub("<a0>", "", RCE$RCE)
RCE$RCE <- as.double(gsub(",", ".", RCE$RCE)) # Konwersja z chr na num RCE

# Zmiany czasu
library(tidyr)
RCE$Czas <- as.POSIXct(paste0(RCE$Data, " ", RCE$Godzina),format = "%Y-%m-%d %H", tz = "Europe/Warsaw")
attr(RCE$Czas, "tzone") <- "UTC"

# Usuwanie starej daty i godziny
RCE <- RCE[,-1:-2]

# Sprawdzenie, czy rozdzielenie przebiegło pomyślnie
head(RCE)

# Utworzenie poprawnej daty i godziny
RCE$Data <- as.Date(RCE$Czas_kopia)
RCE$Godzina <- format(RCE$Czas_kopia, "%H")

# Utworzenie poprawnej daty i godziny
RCE$Czas_kopia <- RCE$Czas
RCE <- separate(RCE, Czas_kopia, into = c("Data", "Godzina"), sep = " ", na.rm = TRUE)

# Formatowanie godziny
RCE$Godzina <- substr(RCE$Godzina, start = 1, stop = 2)
RCE$Godzina[RCE$Godzina == "00"] <- "24"

# Zmiana godziny na integer
# RCE$Godzina <- as.integer(RCE$Godzina)

str(RCE)
RCE$Godzina[1993:1994]
RCE$Czas[1993:1994]

#------------------------------------------------------------------#

# Obliczenie średniego dziennego zużycia energii
roczne_zuzycie <- 2300
dni_w_roku <- 365
srednie_dzienne_zuzycie <- roczne_zuzycie / dni_w_roku

# Utworzenie sekwencji dat na podstawie roku
start_date <- as.Date("2023-01-01")
end_date <- as.Date("2023-12-31")
dates <- seq(start_date, end_date, by = "days")

# Utworzenie pustej ramki danych z datami i godzinami
hourly_data <- expand.grid(Data = dates, Godzina = sprintf("%02d", 1:24))

# Funkcja generująca godzinowe zużycie energii dla każdego dnia
generate_hourly_consumption <- function(daily_consumption) {
  hourly_distribution <- runif(24, min = 0, max = 1)
  hourly_consumption <- daily_consumption * hourly_distribution / sum(hourly_distribution)
  return(hourly_consumption)
}

# Wypełnienie ramki danych godzinowym zużyciem energii
hourly_data$Zuzycie <- c()

for (date in unique(hourly_data$Data)) {
  daily_consumption <- runif(1, min = 4, max = 8)
  hourly_consumption <- generate_hourly_consumption(daily_consumption)
  
  # Wybór wierszy dla danego dnia i przypisanie wartości godzinowego zużycia energii
  hourly_data$Zuzycie[hourly_data$Data == date] <- hourly_consumption
}

# Sortowanie ramki danych według daty i godziny
hourly_data <- hourly_data[order(hourly_data$Data, hourly_data$Godzina), ]

# Resetowanie indeksu wiersza
rownames(hourly_data) <- seq_len(nrow(hourly_data))

# Weryfikacja wyników
head(hourly_data)

#------------------------------------------------------------------#
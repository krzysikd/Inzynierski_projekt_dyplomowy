#Wplyw pojemnosci magazynu energii elektrycznej w hybrydowych systemach PV na koszt zakupu energii brakujacej.

#------------------------------------------------------------------#

# W tym skrypcie zamierzamy pobrac godzinowa moc generowana przez panele fotowoltaiczne (PV)
# ze strony: https://re.jrc.ec.europa.eu/pvg_tools/en/

# Ustalanie katalogu roboczego
setwd("E:/STUDIA/SEMESTR VI/Inżynierski projekt dyplomowy")

# Wczytywanie pakietów
library(dplyr)
library(tidyr)
library(lubridate)

# Wczytywanie danych
godz_moc_PV <- read.csv("Timeseries.csv", header = TRUE, sep = ",", skip = 10) %>%
  head(-7) %>%
  select(-c(3:7)) %>%
  separate(time, c('Data', 'Godzina')) %>%
  rename(Moc = P_MAX) %>%
  mutate(Data = ymd(Data),
         Godzina = sprintf("%02d", as.numeric(substr(Godzina, 1, 2))),
         Godzina = if_else(Godzina == "00", "24", Godzina),
         Moc = as.double(Moc) / 1000,
         Rok = year(Data))

# Analiza roku średniego, najgorszego i najlepszego
podsumowanie_roczne <- godz_moc_PV %>%
  group_by(Rok) %>%
  summarize(CalkowitaMoc = sum(Moc, na.rm = TRUE), .groups = 'drop')

rok_sredni <- mean(podsumowanie_roczne$CalkowitaMoc)
rok_najgorszy <- min(podsumowanie_roczne$CalkowitaMoc)
rok_najlepszy <- max(podsumowanie_roczne$CalkowitaMoc)

dane_najgorszego_roku <- filter(podsumowanie_roczne, CalkowitaMoc == rok_najgorszy)
dane_najlepszego_roku <- filter(podsumowanie_roczne, CalkowitaMoc == rok_najlepszy)

cat("Rok średni:", rok_sredni, "kW mocy\n")
cat("Najgorszy rok to", dane_najgorszego_roku$Rok, "z", rok_najgorszy, "kW mocy\n")
cat("Najlepszy rok to", dane_najlepszego_roku$Rok, "z", rok_najlepszy, "kW mocy\n")

str(godz_moc_PV)
plot(godz_moc_PV$Data, godz_moc_PV$Moc, type = "l", xlab = "Data", ylab = "Moc kW")




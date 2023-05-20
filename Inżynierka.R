#Wplyw pojemnosci magazynu energii elektrycznej w hybrydowych systemach PV na koszt zakupu energii brakujacej.

#------------------------------------------------------------------#

# W tym skrypcie zamierzamy pobrac godzinowa moc generowana przez panele fotowoltaiczne (PV)
# ze strony: https://re.jrc.ec.europa.eu/pvg_tools/en/

# Ustalanie katalogu roboczego
setwd("E:/STUDIA/SEMESTR VI/Inżynierski projekt dyplomowy")

# Pobieranie bieżącego katalogu roboczego
getwd() 

# Wczytywanie danych
godz_moc_PV <- head(read.csv("Timeseries.csv", header = TRUE, sep = ",", skip = 10), -7)

# Usuwanie niepotrzebnych kolumn
godz_moc_PV <- godz_moc_PV[,-3:-7]

# Wczytywanie pakietow dplyr i tidyr
library(dplyr)
library(tidyr)

# Rozdzielenie kolumny 'time' na 'Data' i 'Godzina'
godz_moc_PV <-  separate(godz_moc_PV, time, c('Data', 'Godzina'))

# Zmiana nazw kolumn
colnames(godz_moc_PV)[3] = "Moc"

# Formatowanie daty
library(lubridate)
godz_moc_PV$Data <- ymd(godz_moc_PV$Data)

# Formatowanie godziny
godz_moc_PV$Godzina <- substr(godz_moc_PV$Godzina, start = 1, stop = 2)
godz_moc_PV$Godzina <- sprintf("%02d", as.numeric(godz_moc_PV$Godzina))
godz_moc_PV$Godzina[godz_moc_PV$Godzina == "00"] <- "24"

# Konwersja typow danych kolumn i przeliczanie Mocy z W na kW
#godz_moc_PV$Godzina <- as.integer(godz_moc_PV$Godzina)
godz_moc_PV$Moc <- as.double(godz_moc_PV$Moc) / 1000

# Analiza roku sredniego, najgorszego i najlepszego
godz_moc_PV <- godz_moc_PV %>% mutate(Rok = year(Data))
podsumowanie_roczne <- godz_moc_PV %>%
  group_by(Rok) %>%
  summarize(CalkowitaMoc = sum(Moc, na.rm = TRUE))

rok_sredni <- mean(podsumowanie_roczne$CalkowitaMoc)
rok_najgorszy <- min(podsumowanie_roczne$CalkowitaMoc)
rok_najlepszy <- max(podsumowanie_roczne$CalkowitaMoc)

dane_najgorszego_roku <- filter(podsumowanie_roczne, CalkowitaMoc == rok_najgorszy)
dane_najlepszego_roku <- filter(podsumowanie_roczne, CalkowitaMoc == rok_najlepszy)

cat("Rok średni:", rok_sredni, "kW mocy\n")
cat("Najgorszy rok to", dane_najgorszego_roku$Rok, "z", rok_najgorszy, "kW mocy\n")
cat("Najlepszy rok to", dane_najlepszego_roku$Rok, "z", rok_najlepszy, "kW mocy\n")

str(godz_moc_PV)

#------------------------------------------------------------------#

# W tym skrypcie załadujemy dane o godzinowej rynkowej cenie energii elektrycznej (RCE)
# ze strony:
# https://www.pse.pl/dane-systemowe/funkcjonowanie-rb/raporty-dobowe-z-funkcjonowania-rb/podstawowe-wskazniki-cenowe-i-kosztowe/rynkowa-cena-energii-elektrycznej-rce


# Ustawiamy katalog roboczy
setwd("E:/STUDIA/SEMESTR VI/Inżynierski projekt dyplomowy/ceny")

# Pobieramy bieżący katalog roboczy
getwd() 

# Ladujemy pakiet lubridate
library(lubridate)

for (ceny in list.files()){
  
  # Laczymy dane, jesli istnieja
  if (exists("RCE")){
    tymczasowe <-read.csv(ceny, sep = ";", header=TRUE, fileEncoding = "Windows-1250", stringsAsFactors = FALSE)
    tymczasowe$Data <- ymd(tymczasowe$Data) # Konwersja daty do formatu R
    RCE <-rbind(RCE, tymczasowe)
    rm(tymczasowe)
  }
  
  # Tworzymy dane, jeśli nie istnieją
  if (!exists("RCE")){
    RCE <- read.csv(ceny, sep = ";", header=TRUE, fileEncoding = "Windows-1250", stringsAsFactors = FALSE)
    RCE$Data <- ymd(RCE$Data) # Konwersja daty do formatu R
  }
}

# Zmieniamy nazwy kolumn
colnames(RCE) <- c("Data", "Godzina", "RCE")

# Zmieniamy nazwy ramek danych
godzinowa_cena_energii <- RCE
rm(RCE)

godzinowa_cena_energii$RCE[41008]

# Usuwamy twarde spacje
godzinowa_cena_energii$RCE <- gsub("\u00A0", "", godzinowa_cena_energii$RCE)
godzinowa_cena_energii$RCE <- as.double(gsub(",", ".", godzinowa_cena_energii$RCE)) # Konwersja RCE z chr na num

godzinowa_cena_energii$RCE[41008]

# Dostosowujemy czas
godzinowa_cena_energii$Czas <- as.POSIXct(paste0(godzinowa_cena_energii$Data, " ", godzinowa_cena_energii$Godzina), format = "%Y-%m-%d %H", tz = "Europe/Warsaw")
attr(godzinowa_cena_energii$Czas, "tzone") <- "UTC"

# Usuwamy stare kolumny daty i godziny
godzinowa_cena_energii <- godzinowa_cena_energii[,-1:-2]

# Tworzymy poprawne kolumny daty i godziny
godzinowa_cena_energii$Kopia_czasu <- godzinowa_cena_energii$Czas
godzinowa_cena_energii$Data <- as.Date(godzinowa_cena_energii$Kopia_czasu)
godzinowa_cena_energii$Godzina <- format(godzinowa_cena_energii$Kopia_czasu, "%H")

# Formatujemy godzine
godzinowa_cena_energii$Godzina <- substr(godzinowa_cena_energii$Godzina, start = 1, stop = 2)
godzinowa_cena_energii$Godzina[godzinowa_cena_energii$Godzina == "00"] <- "24"

# Zmieniamy godzine na liczby całkowite
#godzinowa_cena_energii$Godzina <- as.integer(godzinowa_cena_energii$Godzina)

# Usuwamy godzinowa_cena_energii$Kopia_czasu
godzinowa_cena_energii <- godzinowa_cena_energii[,-3]

# Konwersja z PLN/MWh na PLN/kWh
godzinowa_cena_energii$RCE <- godzinowa_cena_energii$RCE / 1000

str(godzinowa_cena_energii)
godzinowa_cena_energii$Godzina[1993:1994]
godzinowa_cena_energii$Czas[1993:1994]

#------------------------------------------------------------------#

# Obliczanie sredniego dziennego zużycia energii dla 4-osobowej rodziny

roczne_zuzycie <- 2300
dni_w_roku <- 365
srednie_dzienne_zuzycie <- roczne_zuzycie / dni_w_roku

# Tworzymy sekwencje dat na podstawie roku
data_poczatkowa <- as.Date("2018-01-01")
data_koncowa <- as.Date("2022-12-31")
daty <- seq(data_poczatkowa, data_koncowa, by = "days")

# Tworzymy pusta ramke danych z datami i godzinami
godzinowe_dane_zuzycia <- expand.grid(Data = daty, Godzina = sprintf("%02d", c(24, 1:23)))

# Funkcja generujaca godzinowe zuzycie energii dla każdego dnia
generuj_godzinowe_zuzycie <- function(dzienne_zuzycie) {
  godzinowy_rozklad <- runif(24, min = 0, max = 1)
  godzinowe_zuzycie <- dzienne_zuzycie * godzinowy_rozklad / sum(godzinowy_rozklad)
  return(godzinowe_zuzycie)
}

# Wypelniamy ramke danych godzinowym zuzyciem energii
godzinowe_dane_zuzycia$Zuzycie <- c()

for (data in unique(godzinowe_dane_zuzycia$Data)) {
  dzienne_zuzycie <- runif(1, min = 4, max = 8)
  godzinowe_zuzycie <- generuj_godzinowe_zuzycie(dzienne_zuzycie)
  
  # Wybieramy wiersze dla konkretnego dnia i przypisujemy wartosci godzinowego zuzycia energii
  godzinowe_dane_zuzycia$Zuzycie[godzinowe_dane_zuzycia$Data == data] <- godzinowe_zuzycie
}

# Sortujemy ramke danych wedlug daty i godziny
godzinowe_dane_zuzycia <- godzinowe_dane_zuzycia[order(godzinowe_dane_zuzycia$Data, godzinowe_dane_zuzycia$Godzina), ]

# Resetujemy indeks wierszy
rownames(godzinowe_dane_zuzycia) <- seq_len(nrow(godzinowe_dane_zuzycia))

# Ladujemy potrzebne biblioteki
library(dplyr)
library(lubridate)

# Grupujemy dane wedlug dnia i obliczamy dziennie zuzycie energii
dzienna_dane <- godzinowe_dane_zuzycia %>%
  group_by(Data) %>%
  summarise(DzienneZuzycie = sum(Zuzycie))

# Dodajemy kolumnę z rokiem
godzinowe_dane_zuzycia$Rok <- year(godzinowe_dane_zuzycia$Data)

# Grupujemy dane według roku i miesiąca, a następnie obliczamy miesięczne zużycie energii
miesieczne_dane <- godzinowe_dane_zuzycia %>%
  mutate(Miesiac = month(Data)) %>%
  group_by(Rok, Miesiac) %>%
  summarise(MiesieczneZuzycie = sum(Zuzycie))

# Grupujemy dane wedlug roku i obliczamy roczne zuzycie energii
roczne_zuzycie <- godzinowe_dane_zuzycia %>%
  mutate(Rok = year(Data)) %>%
  group_by(Rok) %>%
  summarise(RoczneZuzycie = sum(Zuzycie))

# Sprawdzamy wyniki
head(godzinowe_dane_zuzycia)
head(dzienna_dane)
head(miesieczne_dane)
head(roczne_zuzycie)

godzinowe_dane_zuzycia$Godzina <- as.character(godzinowe_dane_zuzycia$Godzina)
str(godzinowe_dane_zuzycia)


#------------------------------------------------------------------#

# analiza
# 1. bez paneli PV

# Wczytane dane
godzinowe_dane_zuzycia
godzinowa_cena_energii 

# Połącz dane
dane <- merge(godzinowe_dane_zuzycia, godzinowa_cena_energii, by=c("Data","Godzina"))

# Usuwanie niepotrzebnej kolumny
dane <- dane[,-6]

# Obliczanie kosztu energii dla kazdej godziny
dane$Koszt <- dane$Zuzycie * dane$RCE

# Wyswietlanie danych
head(dane)

# Podsumowanie
sumaryczny_koszt <- sum(dane$Koszt, na.rm=TRUE)  # Oblicz całkowity koszt energii dla całego roku

# Obliczamy dzienny koszt dla każdej daty
dzienny_koszt <- dane %>%
  group_by(Data) %>%
  summarise(DziennyKoszt = sum(Koszt, na.rm = TRUE))

# Obliczamy średni dzienny koszt
sredni_dzienny_koszt <- mean(dzienny_koszt$DziennyKoszt, na.rm = TRUE)

# 2. z panelami PV
# 3. kupujac energie
# 4. z magazynem
# 5. sprzedajac



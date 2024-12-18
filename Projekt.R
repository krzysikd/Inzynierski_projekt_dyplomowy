# Wplyw pojemnosci magazynu energii elektrycznej w hybrydowych systemach PV na koszt zakupu energii brakujacej

# ================================
# Pakiety
# ================================

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(highcharter)
library(ggthemes)
library(scales)
library(RColorBrewer)
library(webshot)

# ================================
# 1. Pobranie i przetwarzanie danych o produkcji energii przez panele PV
# ================================

# W tym skrypcie zamierzamy pobrac godzinowa moc generowana przez panele fotowoltaiczne (PV)
# ze strony: https://re.jrc.ec.europa.eu/pvg_tools/en/

# Ustalanie katalogu roboczego
setwd("E:/STUDIA/SEMESTR VI/Inżynierski projekt dyplomowy")

# Pobieranie bieżącego katalogu roboczego
getwd() 

#alternatywa 
#plik <- file.choose()
#godz_moc_PV <- head(read.csv(plik, header = TRUE, sep = ",", skip = 10), -7)

# Wczytywanie danych
godz_moc_PV <- head(read.csv("Timeseries.csv", header = TRUE, sep = ",", skip = 10), -7)

# Usuniecie niepotrzebnych wierszy (chcemy miec od 2018)
godz_moc_PV <- godz_moc_PV[-(1:113952), ]

# Usuwanie niepotrzebnych kolumn
godz_moc_PV <- godz_moc_PV[,-3:-7]

# Rozdzielenie kolumny 'time' na 'Data' i 'Godzina'
godz_moc_PV <-  separate(godz_moc_PV, time, c('Data', 'Godzina'))

# Zmiana nazw kolumn
colnames(godz_moc_PV)[3] = "Moc"

# Formatowanie daty
godz_moc_PV$Data <- ymd(godz_moc_PV$Data)

# Formatowanie godziny
godz_moc_PV$Godzina <- substr(godz_moc_PV$Godzina, start = 1, stop = 2)
godz_moc_PV$Godzina <- sprintf("%02d", as.numeric(godz_moc_PV$Godzina))
godz_moc_PV$Godzina[godz_moc_PV$Godzina == "00"] <- "24"

# Konwersja typow danych kolumn i przeliczanie Mocy z W na kW
#godz_moc_PV$Godzina <- as.integer(godz_moc_PV$Godzina)
godz_moc_PV$Moc <- as.double(godz_moc_PV$Moc) / 1000

# Utworzenie 2021 i 2022 roku
 
# Obliczenie średniej mocy dla każdej godziny z lat 2018-2020
srednia_godzinowa <- godz_moc_PV %>%
  mutate(DzienRoku = yday(Data)) %>%
  group_by(DzienRoku, Godzina) %>%
  summarize(SredniaMoc = mean(Moc, na.rm = TRUE), .groups = 'drop')

# Tworzenie pełnej sekwencji dat i godzin dla lat 2021 i 2022
daty_2021_2022 <- seq(as.Date("2021-01-01"), as.Date("2022-12-31"), by = "day")
godziny <- sprintf("%02d", c(24,1:23))  

# Rozszerzamy grid do pełnej ramki dat i godzin
godz_moc_2021_2022 <- expand.grid(Data = daty_2021_2022, Godzina = godziny)

# Usuwamy 29 lutego dla roku 2021, który nie jest przestępny
godz_moc_2021_2022 <- godz_moc_2021_2022[!((month(godz_moc_2021_2022$Data) == 2) & (day(godz_moc_2021_2022$Data) == 29)),]

# Dodanie identyfikatora dnia roku
godz_moc_2021_2022$DzienRoku <- yday(godz_moc_2021_2022$Data)

# Dołączanie średnich wartości mocy
godz_moc_2021_2022 <- left_join(godz_moc_2021_2022, srednia_godzinowa, by = c("DzienRoku", "Godzina"))

# Usuwamy kolumnę DzienRoku, która już nie jest potrzebna
godz_moc_2021_2022 <- select(godz_moc_2021_2022, Data, Godzina, SredniaMoc)

# Zmieniamy nazwę kolumny z średnią mocą na Moc
colnames(godz_moc_2021_2022)[3] <- "Moc"

# Sortujemy ramkę danych godz_moc_2021_2022
godz_moc_2021_2022 <- godz_moc_2021_2022 %>% 
  arrange(Data, Godzina)

# Dołączamy nowe dane do istniejącej ramki danych
godz_moc_PV <- bind_rows(godz_moc_PV, godz_moc_2021_2022)

# Upewniamy się, że wszystkie kolumny są odpowiedniego typu
#godz_moc_PV$Data <- as.Date(godz_moc_PV$Data)
#godz_moc_PV$Godzina <- as.character(godz_moc_PV$Godzina)
#godz_moc_PV$Moc <- as.numeric(godz_moc_PV$Moc)

# Analiza roku sredniego, najgorszego i najlepszego
godz_moc_PV <- godz_moc_PV %>% mutate(Rok = year(Data))
podsumowanie_roczne <- godz_moc_PV %>%
  group_by(Rok) %>%
  summarize(CalkowitaMoc = sum(Moc, na.rm = TRUE))

rok_sredni <- mean(podsumowanie_roczne$CalkowitaMoc)
rok_najgorszy <- min(podsumowanie_roczne$CalkowitaMoc)
rok_najlepszy <- max(podsumowanie_roczne$CalkowitaMoc)

# ================================
# Wizualiacja 1
# ================================

# Wykres liniowy produkcji energii na przestrzeni czasu:
# Pokazuje, jak produkcja energii zmieniała się z czasem.

ggplot(godz_moc_PV, aes(x = Data, y = Moc)) +
  geom_line(aes(color = as.factor(Godzina)), linewidth = 1) +
  facet_wrap(~ Godzina, ncol = 3) +
  labs(title = "Produkcja energii przez panele PV na przestrzeni czasu",
       x = "Data", y = "Moc (kW)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "#f7f7f7"),
    panel.background = element_rect(fill = "#f7f7f7"),
    plot.title = element_text(face = "bold", size = 16, color = "#333333", hjust = 0.5),
    axis.text = element_text(color = "#333333"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#e1e1e1", linewidth = 0.5),
    legend.position = "none"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%y") +
  scale_y_continuous(labels = scales::comma)

ggsave("1.1.png", width = 6.3, height = 10, dpi = 300)


# ================================
# Wizualiacja 2
# ================================

# Dzienne wartości produkcji energii w najgorszym/najlepszym roku 
dane_najgorszego_roku <- filter(podsumowanie_roczne, CalkowitaMoc == rok_najgorszy)
dane_najlepszego_roku <- filter(podsumowanie_roczne, CalkowitaMoc == rok_najlepszy)

godz_moc_dla_najgorszego_roku <- filter(godz_moc_PV, Rok == dane_najgorszego_roku$Rok)
godz_moc_dla_najlepszego_roku <- filter(godz_moc_PV, Rok == dane_najlepszego_roku$Rok)

# Połączenie danych dla obu lat
oba_lata <- rbind(
  mutate(godz_moc_dla_najgorszego_roku, Typ = "Najgorszy rok"),
  mutate(godz_moc_dla_najlepszego_roku, Typ = "Najlepszy rok")
)

# Konwertujemy datę na format "miesiąc-dzień", żeby dane obu lat były nakładane na siebie.
oba_lata$MiesiacDzien <- format(oba_lata$Data, "%m-%d")

# Filtrujemy dane dla najgorszego i najlepszego roku
najgorszy_rok_data <- oba_lata %>% filter(Typ == "Najgorszy rok")
najlepszy_rok_data <- oba_lata %>% filter(Typ == "Najlepszy rok")

#?webshot::install_phantomjs(force = TRUE)

hc1.2 <- highchart() %>% 
  hc_title(text = "Porównanie produkcji energii: Najlepszy vs. Najgorszy rok") %>%
  hc_xAxis(categories = najgorszy_rok_data$MiesiacDzien, tickInterval = 30) %>%
  hc_yAxis(title = list(text = "Moc [kW]")) %>%
  hc_add_series(name = "Najgorszy rok", data = najgorszy_rok_data$Moc, type = "area", color = "#E63946", fillColor = "rgba(230, 57, 70, 0.3)") %>%
  hc_add_series(name = "Najlepszy rok", data = najlepszy_rok_data$Moc, type = "area", color = "#2A9D8F", fillColor = "rgba(42, 157, 143, 0.3)") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, valueDecimals = 2) %>%
  hc_legend(title = list(text = NULL), 
            layout = "vertical", 
            align = "right", 
            verticalAlign = "top", 
            floating = TRUE,
            borderWidth = 1,
            itemStyle = list(color = "#000000"),
            itemHoverStyle = list(color = "#333333"),
            symbolRadius = 0)

htmlwidgets::saveWidget(hc1.2, "1.2.html", selfcontained = TRUE)

webshot::webshot("1.2.html", "1.2.png", delay = 5)

# ================================
# Wizualiacja 3
# ================================

# Średnia godzinowa produkcja energii w ciągu dnia dla całego zbioru danych:
srednia_godzinowa <- godz_moc_PV %>%
  group_by(Godzina) %>%
  summarize(SredniaMoc = mean(Moc, na.rm = TRUE))

srednia_godzinowa$Godzina <- as.numeric(srednia_godzinowa$Godzina)

ggplot(srednia_godzinowa, aes(x = Godzina, y = SredniaMoc)) +
  # Linia z wypełnieniem
  geom_line(color = "#2A9D8F", size = 1.2) +
  geom_point(aes(y = SredniaMoc), color = "#E63946", size = 3, shape = 21, fill = "#E63946") +
  geom_area(aes(y = SredniaMoc), fill = "#2A9D8F", alpha = 0.3) +
  
  # Etykiety i tytuły
  labs(
    title = "Średnia godzinowa produkcja energii w ciągu dnia", 
    x = "Godzina", 
    y = "Średnia moc [kW]",
    subtitle = "Analiza bazująca na całym zbiorze danych",
  ) +
  
  # Stylizacja osi
  scale_x_continuous(breaks = seq(0, 24, 1), limits = c(0, 24)) +
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(color = "grey60"),
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

ggsave("1.3.png", width = 6.3, height = 10, dpi = 300)

# ================================
# Wizualiacja 4
# ================================

# Histogram rozkładu mocy:
# Pokazuje, jakie wartości mocy są najczęstsze.
wybrane_dane <- filter(godz_moc_PV, Moc > 0)

ggplot(wybrane_dane, aes(x = Moc)) +
  # Histogram pokazuje, jak często pojawiają się różne wartości mocy.
  geom_histogram(fill = "#2A9D8F", color = "#264653", binwidth = 0.1, alpha = 0.7) +
  
  # Gładzenie - Krzywa gęstości (dodana za pomocą geom_density) pokazuje, gdzie w tych danych jest największa koncentracja wartości mocy.
  geom_density(aes(y = ..scaled.. * max(..count..)), color = "#E63946", size = 1.2) +
  
  # Etykiety i tytuły
  labs(
    title = "Rozkład produkcji energii przez panele PV",
    x = "Moc (kW)",
    y = "Częstotliwość",
    subtitle = "Analiza bazująca na przefiltrowanych danych"
  ) +
  
  # Stylizacja wykresu
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(color = "grey60"),
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

ggsave("1.4.png", width = 6, height = 6.5, dpi = 300)

# ================================
# Wizualiacja 5
# ================================

# Interaktywny wykres przy użyciu highcharter przedstawiający roczną produkcję energii:
# Pokazuje, jak roczna produkcja energii zmieniała się na przestrzeni lat.

hc1.5 <- hchart(podsumowanie_roczne, "column", hcaes(x = Rok, y = CalkowitaMoc)) %>%
  hc_title(text = "Roczna produkcja energii przez panele PV", margin = 20, style = list(fontSize = "18px")) %>%
  hc_xAxis(title = list(text = "Rok"), lineColor = '#999999', lineWidth = 1, tickLength = 5, gridLineWidth = 0.5) %>%
  hc_yAxis(title = list(text = "Całkowita Moc (kW)"), lineColor = '#999999', lineWidth = 1, gridLineWidth = 0.5) %>%
  hc_colors(c("#5B8FF9", "#5AD8A6", "#5D7092", "#F6BD16", "#E8684A", "#6DC8EC", "#9270CA", "#FF9D4D", "#269A99", "#FF99C3")) %>%
  hc_chart(backgroundColor = "#FAFAFA", borderColor = "#E1E1E1", borderWidth = 1, borderRadius = 3, plotBackgroundColor = "#FFFFFF") %>%
  hc_plotOptions(
    column = list(
      dataLabels = list(enabled = TRUE, color = '#333333'),
      borderWidth = 0,
      borderColor = "#666666",
      colorByPoint = TRUE,
      states = list(
        hover = list(
          brightness = -0.1,
          shadow = list(
            color = 'gray',
            offsetX = 0,
            offsetY = 0,
            opacity = 0.6,
            width = 8
          )
        )
      )
    )
  )
htmlwidgets::saveWidget(hc1.5, "1.5.html", selfcontained = TRUE)

webshot::webshot("1.5.html", "1.5.png", delay = 5)

# ================================
# 2. Pobranie i przetwarzanie danych o rynkowej cenie energii elektrycznej
# ================================

# W tym skrypcie załadujemy dane o godzinowej rynkowej cenie energii elektrycznej (RCE)
# ze strony:
# https://www.pse.pl/dane-systemowe/funkcjonowanie-rb/raporty-dobowe-z-funkcjonowania-rb/podstawowe-wskazniki-cenowe-i-kosztowe/rynkowa-cena-energii-elektrycznej-rce


# Ustawiamy katalog roboczy
setwd("E:/STUDIA/SEMESTR VI/Inżynierski projekt dyplomowy/ceny")

# Pobieramy bieżący katalog roboczy
getwd() 

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

# ================================
# Wizualiacja 1
# ================================

# Wykres Ceny w Czasie: Wyświetla zmienność ceny energii elektrycznej na przestrzeni czasu.
# krzywa loess - Jest to linia trendu, wygładzona metoda loess, która wskazuje ogólny trend w danych. 
# Dzięki tej krzywej można zobaczyć, czy ceny mają tendencję do wzrostu, spadku, czy 
# też są stabilne w dłuższej perspektywie czasowej.
ggplot(godzinowa_cena_energii, aes(x = Data, y = RCE)) +
  # Rzeczywiste dane
  geom_line(aes(color = "Rzeczywiste dane"), size = 1) +
  
  # Krzywa trendu Loess
  geom_smooth(method = "loess", aes(color = "Trend (loess)"), se = FALSE, size = 1.5) +
  
  # Tytuły i etykiety osi
  labs(
    title = "Trend cen energii z krzywą loess",
    subtitle = "Krzywa loess wskazuje ogólny trend cenowy w czasie",
    x = "Data",
    y = "Cena [PLN/kWh]",
    color = "Legenda"
  ) +
  
  # Manualne kolory dla linii
  scale_color_manual(values = c("Rzeczywiste dane" = "blue", "Trend (loess)" = "red")) +
  
  theme_minimal() +
  
  # Dodatkowe modyfikacje
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  )

ggsave("2.1.png", width = 6.3, height = 10, dpi = 300)

# ================================
# Wizualiacja 2
# ================================

# Histogram Ceny Energii: Pozwala ocenić, jakie ceny są najbardziej powszechne.

# Obliczenie średniej wartości
srednia_cena <- mean(godzinowa_cena_energii$RCE, na.rm = TRUE)

ggplot(godzinowa_cena_energii, aes(x = RCE)) +
  # Histogram
  geom_histogram(fill = "green", color = "black", binwidth = 0.001) +
  
  # Linia średniej wartości
  geom_vline(aes(xintercept = srednia_cena), linetype = "dashed", size = 1, color = "red") +
  
  # Etykieta średniej wartości
  annotate("text", x = srednia_cena, y = Inf, label = paste("Średnia:", round(srednia_cena, 4)),
           vjust = 2, hjust = 0.5, color = "red", size = 4) +
  
  # Tytuły i etykiety osi
  labs(
    title = "Rozkład ceny energii elektrycznej",
    x = "Cena [PLN/kWh]",
    y = "Częstotliwość"
  ) +
  
  theme_minimal() +
  
  # Dodatkowe modyfikacje
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

ggsave("2.2.png", width = 6.3, height = 8, dpi = 300)


# ================================
# Wizualiacja 3
# ================================

# Wykres średniej ceny energii w ciągu miesiąca:
# Pozwala zrozumieć, czy są jakieś sezony, w których ceny są wyższe lub niższe.
godzinowa_cena_energii$Rok <- year(godzinowa_cena_energii$Data)

# Zamieniamy numeryczne wartości miesiąca na nazwy w języku polskim
nazwy_miesiecy <- c("styczeń", "luty", "marzec", "kwiecień", "maj", "czerwiec", "lipiec", "sierpień", "wrzesień", "październik", "listopad", "grudzień")
godzinowa_cena_energii$NazwaMiesiaca <- nazwy_miesiecy[month(godzinowa_cena_energii$Data)]

# Grupowanie i agregacja
srednia_miesieczna <- godzinowa_cena_energii %>%
  group_by(Rok, NazwaMiesiaca) %>%
  summarize(SredniaCena = mean(RCE, na.rm = TRUE))

# Tworzenie wykresu
ggplot(srednia_miesieczna, aes(x = NazwaMiesiaca, y = SredniaCena, color = as.factor(Rok), group = as.factor(Rok))) +
  geom_line() +
  scale_color_discrete(name = "Rok") +
  labs(title = "Średnia cena energii w ciągu miesiąca według roku",
       x = "Miesiąc", y = "Średnia cena (PLN/kWh)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limits = nazwy_miesiecy)  # Upewniamy się, że kolejność miesięcy jest zachowana

ggsave("2.3.png", width = 6.5, height = 10, dpi = 300)

# ================================
# 3. Analiza średniego dziennego zużycia energii
# ================================

#https://www.pse.pl/home

pora_roku <- function(data) {
  if(!inherits(data, "Date")) stop("Argument 'data' musi być typu Date.")

  # Pobieranie miesiąca z daty
  miesiac <- month(data)
  if(miesiac %in% c(12, 1, 2)) return("zima")
  if(miesiac %in% 3:5) return("wiosna")
  if(miesiac %in% 6:8) return("lato")
  if(miesiac %in% 9:11) return("jesien")
}

# Współczynniki dla różnych por roku, które odzwierciedlają względne zużycie energii
# Zimą zużycie jest wyższe, latem niższe itp.
wspolczynniki_por_roku <- c(zima = 1.2, wiosna = 1.0, lato = 0.8, jesien = 1.1)

# Funkcja generująca godzinowy rozkład zużycia energii w zależności od pory roku
godzinowy_rozklad_zuzycia <- function(pora) {
  if(pora == "zima") {
    return(c(rep(0.3, 5), 0.5, 0.7, rep(1.2, 3), rep(1.0, 4), rep(1.5, 5), 1.3, 1.0, rep(0.6, 2), 0.4))
  }
  if(pora == "wiosna") {
    return(c(rep(0.2, 5), 0.4, 0.6, rep(1.0, 3), rep(0.9, 4), rep(1.4, 5), 1.2, 0.9, rep(0.5, 2), 0.3))
  }
  if(pora == "lato") {
    return(c(rep(0.1, 5), 0.2, 0.4, rep(0.8, 3), rep(0.7, 4), rep(1.2, 5), 0.9, 0.7, rep(0.3, 2), 0.2))
  }
  if(pora == "jesien") {
    return(c(rep(0.25, 5), 0.45, 0.65, rep(1.1, 3), rep(1.0, 4), rep(1.3, 5), 1.1, 0.8, rep(0.55, 2), 0.35))
  }
}

# Tworzymy sekwencje dat na podstawie roku
data_poczatkowa <- as.Date("2018-01-01")
data_koncowa <- as.Date("2022-12-31")
daty <- seq(data_poczatkowa, data_koncowa, by = "days")

# Tworzymy pusta ramke danych z datami i godzinami
godzinowe_dane_zuzycia <- expand.grid(Data = daty, Godzina = sprintf("%02d", c(24, 1:23)))

# Wypelniamy ramke danych godzinowym zuzyciem energii
godzinowe_dane_zuzycia$Zuzycie <- numeric(nrow(godzinowe_dane_zuzycia))

for (data in unique(godzinowe_dane_zuzycia$Data)) {
  pora <- pora_roku(as.Date(data))
  dzienne_zuzycie <- runif(1, min = 5, max = 9) * wspolczynniki_por_roku[pora]
  godzinowy_rozklad <- godzinowy_rozklad_zuzycia(pora)
  godzinowe_zuzycie <- dzienne_zuzycie * godzinowy_rozklad / sum(godzinowy_rozklad)
  
  # Wybieramy wiersze dla konkretnego dnia i przypisujemy wartosci godzinowego zuzycia energii
  godzinowe_dane_zuzycia$Zuzycie[godzinowe_dane_zuzycia$Data == data] <- godzinowe_zuzycie
}

# Sortujemy ramke danych wedlug daty i godziny
godzinowe_dane_zuzycia <- godzinowe_dane_zuzycia[order(godzinowe_dane_zuzycia$Data, godzinowe_dane_zuzycia$Godzina), ]

# Resetujemy indeks wierszy
rownames(godzinowe_dane_zuzycia) <- seq_len(nrow(godzinowe_dane_zuzycia))

# Grupujemy dane wedlug dnia i obliczamy dziennie zuzycie energii
dzienna_dane <- godzinowe_dane_zuzycia %>%
  group_by(Data) %>%
  summarise(DzienneZuzycie = sum(Zuzycie))

# Grupujemy dane według roku i miesiąca, a następnie obliczamy miesięczne zużycie energii
miesieczne_dane <- godzinowe_dane_zuzycia %>%
  mutate(Miesiac = month(Data), Rok = year(Data)) %>%
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

# ================================
# Wizualiacja 1
# ================================

# Miesięczne Zestawienie Zużycia:
# Wykres słupkowy przedstawiający średnie miesięczne zużycie energii dla każdego roku. 
# Umożliwi zrozumienie, w których miesiącach zużycie jest największe.

# Ustawienie kolorów
kolory <- brewer.pal(5, "Set3")

# Tworzenie wektora z polskimi nazwami miesięcy
polskie_miesiace <- c("Sty", "Lut", "Mar", "Kwi", "Maj", "Cze", "Lip", "Sie", "Wrz", "Paź", "Lis", "Gru")

# Zamiana numerów miesięcy na ich polskie nazwy
miesieczne_dane$Miesiac <- factor(polskie_miesiace[miesieczne_dane$Miesiac], levels = polskie_miesiace)

# Wykres
ggplot(miesieczne_dane, aes(x = Miesiac, y = MiesieczneZuzycie, fill = as.factor(Rok))) +
  geom_bar(stat = "identity", position = "dodge", show.legend = TRUE, width = 0.7) +
  geom_text(aes(label=sprintf("%.0f", round(MiesieczneZuzycie, digits = 0))), vjust=1.6, color="black", position = position_dodge(0.9), size=3.5) +
  scale_fill_manual(values = kolory) +
  labs(title = "Średnie Miesięczne Zużycie Energii Elektrycznej w Poszczególnych Latach", x = "Miesiąc", y = "Miesięczne Zużycie [kWh]") +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.position = "right")

ggsave("3.1.png", width = 6.5, height = 10, dpi = 300)

# ================================
# Wizualiacja 2
# ================================

# Średnie dzienne godzinowe zużycie energii
# Słupki wykresu ilustrują, w jakich godzinach zużycie energii jest największe i najmniejsze.
# !Obecnie wykres jest niezbyt uzyteczny gdyz nie mamy rzeczywistych danych.

# Obliczanie średniego godzinowego zużycia energii
srednie_godzinowe_zuzycie <- godzinowe_dane_zuzycia %>%
  group_by(Godzina) %>%
  summarise(SrednieZuzycie = mean(Zuzycie, na.rm = TRUE))

# Tworzenie wykresu
ggplot(srednie_godzinowe_zuzycie, aes(x = Godzina, y = SrednieZuzycie, fill = SrednieZuzycie)) +
  geom_col(show.legend = FALSE, color = "black", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", SrednieZuzycie)), vjust = -0.5, size = 3) +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  labs(title = "Średnie Dzienne Godzinowe Zużycie Energii",
       subtitle = "Zużycie energii w ciągu dnia",
       x = "Godzina Dnia",
       y = "Średnie Zużycie Energii [kWh]") +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("3.2.png", width = 6.5, height = 10, dpi = 300)

# ================================
# 4. Analiza kosztów i korzyści z zastosowania paneli PV:
# ================================

# ================================
# 4.1 Analiza sytuacji bez paneli PV: obliczenie kosztu zakupu energii przy braku paneli
# ================================

# Wczytane dane
godzinowe_dane_zuzycia
godzinowa_cena_energii 

# Połączenie danych
dane_zuzycia_i_kosztu <- merge(godzinowe_dane_zuzycia, godzinowa_cena_energii, by=c("Data","Godzina"))

# Usuwanie niepotrzebnej kolumny
dane_zuzycia_i_kosztu <- dane_zuzycia_i_kosztu[,-6]

# Obliczanie kosztu energii dla kazdej godziny
dane_zuzycia_i_kosztu$Koszt <- dane_zuzycia_i_kosztu$Zuzycie * dane_zuzycia_i_kosztu$RCE

# Dodanie kolumny Rok do dalszej analizy
dane_zuzycia_i_kosztu$Rok <- format(as.Date(dane_zuzycia_i_kosztu$Data), "%Y")

# Wyswietlanie danych
head(dane_zuzycia_i_kosztu)

# Podsumowanie
sumaryczny_koszt <- sum(dane_zuzycia_i_kosztu$Koszt, na.rm=TRUE)  # całkowity koszt energii dla całego roku

# Obliczamy dzienny koszt dla każdej daty
koszt_dzienny <- dane_zuzycia_i_kosztu %>%
  group_by(Data) %>%
  summarise(KosztDzienny = sum(Koszt, na.rm = TRUE))

# Obliczamy średni dzienny koszt
sredni_koszt_dzienny <- mean(koszt_dzienny$KosztDzienny, na.rm = TRUE)

# Analiza sezonowości - koszty miesięczne przez lata
dane_zuzycia_i_kosztu <- dane_zuzycia_i_kosztu %>%
  mutate(Rok = year(Data), Miesiac = month(Data))

# ================================
# Wizualiacja 1
# ================================

# Wykres kosztów miesięcznych przez lata

# Polskie nazwy miesięcy
polskie_miesiace <- c("Sty", "Lut", "Mar", "Kwi", "Maj", "Cze", "Lip", "Sie", "Wrz", "Paź", "Lis", "Gru")

# Grupowanie i obliczanie kosztów miesięcznych
koszt_miesieczny <- dane_zuzycia_i_kosztu %>%
  group_by(Rok, Miesiac) %>%
  summarise(KosztMiesieczny = sum(Koszt, na.rm=TRUE))

# Zamiana numeru miesiąca na polską nazwę
koszt_miesieczny$Miesiac <- polskie_miesiace[koszt_miesieczny$Miesiac]

# Ustalenie kolejności miesięcy w ramce danych
koszt_miesieczny <- koszt_miesieczny %>%
  mutate(Miesiac = factor(Miesiac, levels = polskie_miesiace))

# Ustalenie palety kolorów
kolory <- brewer.pal(9, "Set1")

# Wykres
ggplot(koszt_miesieczny, aes(x = Miesiac, y = KosztMiesieczny, color = as.factor(Rok), group = Rok)) +
  geom_line(size = 1) +
  geom_point(size = 3, shape = 21, fill = "white") +
  scale_color_manual(values = kolory) +
  labs(title = "Miesięczne Koszty Energii przez Lata",
       subtitle = "Analiza sezonowości kosztów energii",
       x = "Miesiąc",
       y = "Koszt Miesięczny [PLN]",
       color = "Rok") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "right", 
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("4.1a.png", width = 6.5, height = 10, dpi = 300)

# ================================
# Wizualiacja 2
# ================================

# Obliczamy roczne koszty
koszt_roczny <- dane_zuzycia_i_kosztu %>%
  group_by(Rok) %>%
  summarise(RocznyKoszt = sum(Koszt, na.rm = TRUE))

# Dodajemy skumulowany koszt do danych o rocznych kosztach
koszt_roczny <- koszt_roczny %>%
  arrange(Rok) %>%
  mutate(SkumulowanyKoszt = cumsum(RocznyKoszt))

# Ustawienie kolorów
kolory <- brewer.pal(9, "Set1")

# Wykres skumulowanych rocznych kosztów energii
hc4.2a<- hchart(koszt_roczny, "line", hcaes(x = Rok, y = SkumulowanyKoszt)) %>%
  hc_title(text = "Skumulowane roczne koszty energii") %>%
  hc_subtitle(text = "Analiza kosztów energii na przestrzeni lat") %>%
  hc_xAxis(title = list(text = "Rok"), categories = koszt_roczny$Rok, crosshair = TRUE) %>%
  hc_yAxis(title = list(text = "Skumulowany koszt [PLN]"), opposite = FALSE, crosshair = TRUE) %>%
  hc_add_series(koszt_roczny, "point", hcaes(x = Rok, y = SkumulowanyKoszt), name = "Roczny Koszt",
                dataLabels = list(enabled = TRUE, format = '{point.y:,.0f} zł', style = list(fontSize = '10px'))) %>%
  hc_tooltip(headerFormat = "<b>Rok {point.x}</b><br/>",
             pointFormat = "Skumulowany koszt: <b>{point.y:.2f} zł</b>") %>%
  hc_colors(kolory) %>%
  hc_chart(zoomType = 'xy') %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = TRUE)

htmlwidgets::saveWidget(hc4.2a, "4.2a.html", selfcontained = TRUE)

webshot::webshot("4.2a.html", "4.2a.png", delay = 5)

# ================================
# 4.2 Analiza sytuacji z panelami PV bez magazynu: w tym scenariuszu nadmiar energii generowany przez panele jest marnowany
# ================================

# Dołączenie danych o produkcji energii przez panele PV
dane_z_panelami <- merge(dane_zuzycia_i_kosztu, godz_moc_PV, by=c("Data","Godzina"))

dane_z_panelami$Rok <- dane_z_panelami$Rok.x  # nadpisanie kolumny 'Rok' wartościami z kolumny 'Rok.x'
dane_z_panelami$Rok.x <- NULL  # usunięcie kolumny 'Rok.x'
dane_z_panelami$Rok.y <- NULL  # usunięcie kolumny 'Rok.y'

# Obliczenie różnicy pomiędzy generowaną mocą a zużyciem
dane_z_panelami$Roznica <- dane_z_panelami$Moc - dane_z_panelami$Zuzycie

# Obliczenie, ile energii jest marnowane i ile musi być dokupione
dane_z_panelami$Marnowane <- pmax(0, dane_z_panelami$Roznica)  # jeśli różnica jest dodatnia, marnujemy energię
dane_z_panelami$Dokupione <- pmax(0, -dane_z_panelami$Roznica) # jeśli różnica jest ujemna, musimy dokupić energię

# Obliczenie kosztu dokupionej energii
dane_z_panelami$KosztDokupionej <- dane_z_panelami$Dokupione * dane_z_panelami$RCE

# Podsumowanie
koszt_dokupionej <- sum(dane_z_panelami$KosztDokupionej, na.rm=TRUE)

# ================================
# Wizualiacja 1
# ================================

# Wykres kosztów miesięcznych dla energii dokupionej przez lata

# Polskie nazwy miesięcy
polskie_miesiace <- c("Sty", "Lut", "Mar", "Kwi", "Maj", "Cze", "Lip", "Sie", "Wrz", "Paź", "Lis", "Gru")

# Grupowanie i obliczanie kosztów miesięcznych dla energii dokupionej
koszt_miesieczny <- dane_z_panelami %>%
  group_by(Rok, Miesiac) %>%
  summarise(KosztMiesieczny = sum(KosztDokupionej, na.rm=TRUE))

# Zamiana numeru miesiąca na polską nazwę
koszt_miesieczny$Miesiac <- polskie_miesiace[koszt_miesieczny$Miesiac]

# Ustalenie kolejności miesięcy w ramce danych
koszt_miesieczny <- koszt_miesieczny %>%
  mutate(Miesiac = factor(Miesiac, levels = polskie_miesiace))

# Ustalenie palety kolorów
kolory <- brewer.pal(9, "Set1")

# Wykres
ggplot(koszt_miesieczny, aes(x = Miesiac, y = KosztMiesieczny, color = as.factor(Rok), group = Rok)) +
  geom_line(size = 1) +
  geom_point(size = 3, shape = 21, fill = "white") +
  scale_color_manual(values = kolory) +
  labs(title = "Miesięczne koszty dokupionej energii przez lata",
       subtitle = "Analiza sezonowości kosztów energii dokupionej",
       x = "Miesiąc",
       y = "Koszt Miesięczny [PLN]",
       color = "Rok") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "right", 
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("4.1b.png", width = 6.5, height = 10, dpi = 300)

# ================================
# Wizualiacja 2
# ================================

# Obliczamy roczne koszty na podstawie danych_z_panelami
roczne_koszty_paneli <- dane_z_panelami %>%
  group_by(Rok) %>%
  summarise(RoczneWydatkiZakupionejEnergii = sum(KosztDokupionej, na.rm = TRUE))

# Dodajemy skumulowany koszt do danych o rocznych kosztach
roczne_koszty_paneli <- roczne_koszty_paneli %>%
  arrange(Rok) %>%
  mutate(SkumulowaneWydatkiZakupionejEnergii = cumsum(RoczneWydatkiZakupionejEnergii))

# Ustawienie kolorów
kolory <- brewer.pal(9, "Set1")

# Wykres skumulowanych rocznych kosztów energii
hc4.2b <- hchart(roczne_koszty_paneli, "line", hcaes(x = Rok, y = SkumulowaneWydatkiZakupionejEnergii)) %>%
  hc_title(text = "Skumulowane roczne koszty Energii przy użyciu paneli PV (Bez Magazynu)") %>%
  hc_subtitle(text = "Analiza kosztów energii na przestrzeni lat (nadmiar energii z PV jest marnowany)") %>%
  hc_xAxis(title = list(text = "Rok"), categories = roczne_koszty_paneli$Rok, crosshair = TRUE) %>%
  hc_yAxis(title = list(text = "Skumulowany Koszt [PLN]"), opposite = FALSE, crosshair = TRUE) %>%
  hc_add_series(roczne_koszty_paneli, "point", hcaes(x = Rok, y = SkumulowaneWydatkiZakupionejEnergii), name = "Koszt Zakupionej Energii",
                dataLabels = list(enabled = TRUE, format = '{point.y:,.0f} zł', style = list(fontSize = '10px'))) %>%
  hc_tooltip(headerFormat = "<b>Rok {point.x}</b><br/>",
             pointFormat = "Skumulowany Koszt: <b>{point.y:.2f} zł</b>") %>%
  hc_colors(kolory) %>%
  hc_chart(zoomType = 'xy') %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = TRUE)

htmlwidgets::saveWidget(hc4.2b, "4.2b.html", selfcontained = TRUE)

webshot::webshot("4.2b.html", "4.2b.png", delay = 5)

# ================================
# 4.3 Analiza sytuacji z panelami PV bez magazynu: w tym scenariuszu nadmiar energii generowany przez panele jest natychmiast sprzedawany do sieci
# ================================

# Używając wcześniejszych danych z "dane_z_panelami"
dane_z_panelami$Sprzedane <- pmax(0, dane_z_panelami$Roznica)  # jeśli różnica jest dodatnia, sprzedajemy energię do sieci

# Teraz uwzględniamy, że cena sprzedaży energii do sieci jest niższa o 4% od jej kosztu zakupu
dane_z_panelami$DochodZeSprzedazy <- dane_z_panelami$Sprzedane * dane_z_panelami$RCE * 0.96

# Obliczenie, ile energii musi być dokupione (jeśli różnica jest ujemna)
dane_z_panelami$Dokupione <- pmax(0, -dane_z_panelami$Roznica)

# Obliczenie kosztu dokupionej energii
dane_z_panelami$KosztDokupionej <- dane_z_panelami$Dokupione * dane_z_panelami$RCE

# Podsumowanie
koszt_dokupionej <- sum(dane_z_panelami$KosztDokupionej, na.rm=TRUE)
dochod_ze_sprzedazy <- sum(dane_z_panelami$DochodZeSprzedazy, na.rm=TRUE)

# ================================
# Wizualizacja 1
# ================================

# Obliczamy miesięczne dochody ze sprzedaży energii oraz miesięczny koszt zakupionej energii
analiza_miesieczna <- dane_z_panelami %>%
  group_by(Rok, Miesiac) %>%
  summarise(DochodMiesieczny = sum(DochodZeSprzedazy, na.rm=TRUE),
            KosztMiesieczny = sum(KosztDokupionej, na.rm=TRUE))

# Obliczamy miesięczny zysk netto
analiza_miesieczna <- analiza_miesieczna %>%
  mutate(ZyskMiesieczny = DochodMiesieczny - KosztMiesieczny)

# Zamiana numeru miesiąca na polską nazwę
analiza_miesieczna$Miesiac <- polskie_miesiace[analiza_miesieczna$Miesiac]

# Ustalenie kolejności miesięcy w ramce danych
analiza_miesieczna <- analiza_miesieczna %>%
  mutate(Miesiac = factor(Miesiac, levels = polskie_miesiace))

# Wykres
ggplot(analiza_miesieczna, aes(x = Miesiac, y = ZyskMiesieczny, color = as.factor(Rok), group = Rok)) +
  geom_line(size = 1) +
  geom_point(size = 3, shape = 21, fill = "white") +
  scale_color_manual(values = kolory) +
  labs(title = "Miesięczny zysk netto z energii przez lata",
       subtitle = "Analiza sezonowości zysku netto",
       x = "Miesiąc",
       y = "Zysk Netto Miesięczny [PLN]",
       color = "Rok") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "right", 
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("4.1c.png", width = 6.5, height = 10, dpi = 300)

# ================================
# Wizualizacja 2
# ================================

# Obliczamy roczne koszty i dochody na podstawie danych_z_panelami
roczne_analiza <- dane_z_panelami %>%
  group_by(Rok) %>%
  summarise(RoczneDochody = sum(DochodZeSprzedazy, na.rm = TRUE),
            RoczneWydatkiZakupionejEnergii = sum(KosztDokupionej, na.rm = TRUE))

# Obliczamy roczny zysk netto oraz skumulowany zysk netto
roczne_analiza <- roczne_analiza %>%
  arrange(Rok) %>%
  mutate(RocznyZyskNetto = RoczneDochody - RoczneWydatkiZakupionejEnergii,
         SkumulowanyZyskNetto = cumsum(RocznyZyskNetto))

# Wykres skumulowanego rocznego zysku netto
hc4.2c <- hchart(roczne_analiza, "line", hcaes(x = Rok, y = SkumulowanyZyskNetto)) %>%
  hc_title(text = "Skumulowany roczny zysk netto przy użyciu paneli PV (Bez Magazynu)") %>%
  hc_subtitle(text = "Analiza zysku netto na przestrzeni lat (nadmiar energii z PV sprzedawany do sieci)") %>%
  hc_xAxis(title = list(text = "Rok"), categories = roczne_analiza$Rok, crosshair = TRUE) %>%
  hc_yAxis(title = list(text = "Skumulowany Zysk Netto [PLN]"), opposite = FALSE, crosshair = TRUE) %>%
  hc_add_series(roczne_analiza, "point", hcaes(x = Rok, y = SkumulowanyZyskNetto), name = "Zysk Netto",
                dataLabels = list(enabled = TRUE, format = '{point.y:,.0f} zł', style = list(fontSize = '10px'))) %>%
  hc_tooltip(headerFormat = "<b>Rok {point.x}</b><br/>",
             pointFormat = "Skumulowany Koszt: <b>{point.y:.2f} zł</b>") %>%
  hc_colors(kolory) %>%
  hc_chart(zoomType = 'xy') %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = TRUE)

htmlwidgets::saveWidget(hc4.2c, "4.2c.html", selfcontained = TRUE)

webshot::webshot("4.2c.html", "4.2c.png", delay = 5)
             
# ================================
# 4.4 Analiza sytuacji z magazynem energii: Nadmiar energii jest marnowany.
# ================================

# Wstępne parametry magazynu
pojemnosc_magazynu <- 2
sprawnosc_magazynowania <- 0.90 
stan_magazynu <- 0 

# Tworzenie kopii danych z panelami i dodanie nowych kolumn
dane_z_magazynem <- dane_z_panelami
dane_z_magazynem$PobranoZMagazynu <- 0
dane_z_magazynem$DokupioneZSieci <- 0
dane_z_magazynem$WlozonoDoMagazynu <- 0

# Iteracja przez każdy wiersz (czyli każdą godzinę)
for(i in 1:nrow(dane_z_magazynem)) {
  
  # Pobieranie danych dla bieżącej godziny
  Moc <- dane_z_magazynem$Moc[i]
  Zuzycie <- dane_z_magazynem$Zuzycie[i]
  
  # Jeśli moc z paneli jest większa niż zużycie
  if(Moc > Zuzycie) {
    # Obliczanie nadmiaru energii
    nadmiar <- Moc - Zuzycie
    # Sprawdzanie, ile miejsca jest dostępne w magazynie
    miejsce_w_magazynie <- pojemnosc_magazynu - stan_magazynu
    # Obliczanie ile energii możemy wprowadzić do magazynu
    do_magazynu <- min(nadmiar, miejsce_w_magazynie)
    dane_z_magazynem$WlozonoDoMagazynu[i] <- do_magazynu
    # Aktualizacja stanu magazynu
    stan_magazynu <- stan_magazynu + do_magazynu
    
    # Jeśli moc z paneli jest mniejsza niż zużycie
  } else {
    # Obliczanie, ile energii brakuje
    brakuje <- Zuzycie - Moc
    # Obliczanie ile energii jest dostępne w magazynie uwzględniając sprawność magazynowania
    dostepne_w_magazynie <- stan_magazynu * sprawnosc_magazynowania
    # Obliczanie ile energii możemy pobrać z magazynu
    z_magazynu <- min(brakuje, dostepne_w_magazynie)
    dane_z_magazynem$PobranoZMagazynu[i] <- z_magazynu
    # Aktualizacja stanu magazynu po pobraniu energii
    stan_magazynu <- stan_magazynu - z_magazynu/sprawnosc_magazynowania
    # Obliczanie ile energii jeszcze brakuje po wykorzystaniu magazynu
    brakuje <- brakuje - z_magazynu
    # Dodawanie informacji o dokupionej energii
    dane_z_magazynem$DokupioneZSieci[i] <- brakuje
  }
}

# Podsumowanie kosztów zakupionej energii z wykorzystaniem magazynu
roczne_koszty_z_magazynem <- dane_z_magazynem %>%
  group_by(Rok) %>%
  summarise(RoczneWydatkiZakupionejEnergii = sum(DokupioneZSieci * RCE, na.rm = TRUE))

# ================================
# Wizualiacja 1
# ================================

# Grupowanie i obliczanie kosztów miesięcznych dla energii dokupionej z magazynem
koszt_miesieczny_magazyn <- dane_z_magazynem %>%
  group_by(Rok, Miesiac) %>%
  summarise(KosztMiesiecznyMagazyn = sum(DokupioneZSieci * RCE, na.rm=TRUE)) 

# Zamiana numeru miesiąca na polską nazwę
koszt_miesieczny_magazyn$Miesiac <- polskie_miesiace[koszt_miesieczny_magazyn$Miesiac]

# Ustalenie kolejności miesięcy w ramce danych
koszt_miesieczny_magazyn <- koszt_miesieczny_magazyn %>%
  mutate(Miesiac = factor(Miesiac, levels = polskie_miesiace))

# Wykres
ggplot(koszt_miesieczny_magazyn, aes(x = Miesiac, y = KosztMiesiecznyMagazyn, color = as.factor(Rok), group = Rok)) +
  geom_line(size = 1) +
  geom_point(size = 3, shape = 21, fill = "white") +
  scale_color_manual(values = kolory) +
  labs(title = "Miesięczne koszty dokupionej energii z magazynem przez lata",
       subtitle = "Analiza sezonowości kosztów energii dokupionej z wykorzystaniem magazynu",
       x = "Miesiąc",
       y = "Koszt Miesięczny z Magazynem [PLN]",
       color = "Rok") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "right", 
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("4.1d.png", width = 6.5, height = 10, dpi = 300)

# ================================
# Wizualiacja 2
# ================================

# Obliczamy roczne koszty na podstawie danych_z_magazynem
roczne_koszty_magazyn <- dane_z_magazynem %>%
  group_by(Rok) %>%
  summarise(RoczneWydatkiZakupionejEnergii = sum(DokupioneZSieci * RCE, na.rm = TRUE))

# Dodajemy skumulowany koszt do danych o rocznych kosztach
roczne_koszty_magazyn <- roczne_koszty_magazyn %>%
  arrange(Rok) %>%
  mutate(SkumulowaneWydatkiZakupionejEnergii = cumsum(RoczneWydatkiZakupionejEnergii))

# Ustawienie kolorów
kolory <- brewer.pal(9, "Set1")

# Wykres skumulowanych rocznych kosztów energii z wykorzystaniem magazynu
hc4.2d <- hchart(roczne_koszty_magazyn, "line", hcaes(x = Rok, y = SkumulowaneWydatkiZakupionejEnergii)) %>%
  hc_title(text = "Skumulowane roczne koszty Energii przy użyciu Magazynu Energii") %>%
  hc_subtitle(text = "Analiza kosztów energii na przestrzeni lat z uwzględnieniem magazynowania nadmiarów energii z PV") %>%
  hc_xAxis(title = list(text = "Rok"), categories = roczne_koszty_magazyn$Rok, crosshair = TRUE) %>%
  hc_yAxis(title = list(text = "Skumulowany Koszt [PLN]"), opposite = FALSE, crosshair = TRUE) %>%
  hc_add_series(roczne_koszty_magazyn, "point", hcaes(x = Rok, y = SkumulowaneWydatkiZakupionejEnergii), name = "Koszt Zakupionej Energii",
                dataLabels = list(enabled = TRUE, format = '{point.y:,.0f} zł', style = list(fontSize = '10px'))) %>%
  hc_tooltip(headerFormat = "<b>Rok {point.x}</b><br/>",
             pointFormat = "Skumulowany Koszt: <b>{point.y:.2f} zł</b>") %>%
  hc_colors(kolory) %>%
  hc_chart(zoomType = 'xy') %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = TRUE)

htmlwidgets::saveWidget(hc4.2d, "4.2d.html", selfcontained = TRUE)

webshot::webshot("4.2d.html", "4.2d.png", delay = 5)

# ================================
# 4.5 Analiza sytuacji z różnymi pojemnościami magazynu i marnowaniem nadmiaru: Gdy magazyn jest pełny, nadmiar energii jest marnowany.
# ================================

# Definicje różnych pojemności magazynu
pojemnosci_magazynu <- c(2, 5, 7, 10, 13, 15, 17)

# Zbieranie danych do analizy
wyniki_rozne_pojemnosci <- data.frame()

# Pętla dla każdej pojemności magazynu
for(pojemnosc in pojemnosci_magazynu) {
  pojemnosc_magazynu <- pojemnosc
  sprawnosc_magazynowania <- 0.90
  stan_magazynu <- 0
  
  # Przetwarzanie danych dla każdej pojemności
  dane_z_magazynem <- dane_z_panelami
  # Dodawanie nowych kolumn
  dane_z_magazynem$PobranoZMagazynu <- 0
  dane_z_magazynem$DokupioneZSieci <- 0
  dane_z_magazynem$WlozonoDoMagazynu <- 0
  
  for(i in 1:nrow(dane_z_magazynem)) {
    Moc <- dane_z_magazynem$Moc[i]
    Zuzycie <- dane_z_magazynem$Zuzycie[i]
    
    if(Moc > Zuzycie) {
      nadmiar <- Moc - Zuzycie
      miejsce_w_magazynie <- pojemnosc_magazynu - stan_magazynu
      do_magazynu <- min(nadmiar, miejsce_w_magazynie)
      dane_z_magazynem$WlozonoDoMagazynu[i] <- do_magazynu
      stan_magazynu <- min(stan_magazynu + do_magazynu, pojemnosc_magazynu)
    } else {
      brakuje <- Zuzycie - Moc
      dostepne_w_magazynie <- stan_magazynu * sprawnosc_magazynowania
      z_magazynu <- min(brakuje, dostepne_w_magazynie)
      dane_z_magazynem$PobranoZMagazynu[i] <- z_magazynu
      stan_magazynu <- max(stan_magazynu - z_magazynu/sprawnosc_magazynowania, 0)
      brakuje <- brakuje - z_magazynu
      dane_z_magazynem$DokupioneZSieci[i] <- brakuje
    }
  }
  
  # Podsumowanie kosztów dla każdej pojemności
  roczne_koszty_magazyn <- dane_z_magazynem %>%
    group_by(Rok) %>%
    summarise(RoczneWydatkiZakupionejEnergii = sum(DokupioneZSieci * RCE, na.rm = TRUE))
  
  wyniki_rozne_pojemnosci <- rbind(wyniki_rozne_pojemnosci, transform(roczne_koszty_magazyn, PojemnoscMagazynu = pojemnosc))
}

# ================================
# Wizualiacja 1
# ================================

# Wykres Słupkowy Porównujący Roczne Koszty Zakupionej Energii dla Różnych Pojemności Magazynu
ggplot(wyniki_rozne_pojemnosci, aes(x = factor(Rok), y = RoczneWydatkiZakupionejEnergii, fill = factor(PojemnoscMagazynu))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Set3") +  
  theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), 
    plot.subtitle = element_text(hjust = 0.5, size = 12), 
    axis.title.x = element_text(size = 12, face = "bold"), 
    axis.title.y = element_text(size = 12, face = "bold"), 
    legend.title = element_text(face = "bold"), 
    legend.position = "bottom" 
  ) +
  labs(
    title = "Roczne Koszty Zakupionej Energii w Zależności od Pojemności Magazynu",
    x = "Rok",
    y = "Koszt Zakupionej Energii [PLN]",
    fill = "Pojemność Magazynu"
  ) +
  geom_text(aes(label=sprintf("%.2f", RoczneWydatkiZakupionejEnergii)), vjust=-0.3, position = position_dodge(0.9), size=3)

ggsave("hc4.2v1.png", width = 11.5, height = 11, dpi = 300)

# ================================
# Wizualiacja 2
# ================================

# Obliczanie skumulowanych wydatków
wynik_finalowy <- wyniki_rozne_pojemnosci %>%
  arrange(Rok, PojemnoscMagazynu) %>%
  group_by(PojemnoscMagazynu) %>%
  mutate(SkumulowaneWydatki = cumsum(RoczneWydatkiZakupionejEnergii)) %>%
  ungroup()

# Tworzenie wykresu
hc4.2v2 <- hchart(wynik_finalowy, "line", hcaes(x = Rok, y = SkumulowaneWydatki, group = PojemnoscMagazynu)) %>%
  hc_title(text = "Skumulowane Roczne Wydatki na Energię przy Użyciu Magazynu Energii") %>%
  hc_subtitle(text = "Analiza wydatków na energię na przestrzeni lat z uwzględnieniem różnych pojemności magazynów") %>%
  hc_xAxis(title = list(text = "Rok"), tickInterval = 1) %>%
  hc_yAxis(title = list(text = "Skumulowane Wydatki [PLN]")) %>%
  hc_colors(c("#7cb5ec", "#434348", "#90ed7d", "#f7a35c", "#8085e9")) %>%
  hc_legend(title = list(text = "Pojemność Magazynu"), enabled = TRUE, layout = "vertical", align = "right", verticalAlign = "middle", useHTML = TRUE, labelFormatter = JS("function() { return this.name + ' <span style=\"color: ' + this.color + '\">&#9679;</span>'; }")) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_tooltip(headerFormat = "<b>Rok {point.key}</b><br/>", 
             pointFormat = "<span style='color:{series.color}'>\u25CF</span> {series.name}: {point.y:,.2f} zł<br/>", 
             shared = TRUE, 
             crosshairs = TRUE)

# Zapis do pliku HTML i konwersja na obraz PNG
htmlwidgets::saveWidget(hc4.2v2, "hc4.2v2.html", selfcontained = TRUE)
webshot::webshot("hc4.2v2.html", "hc4.2v2.png", delay = 5)

# ================================
# 4.6 Analiza sytuacji z magazynem i sprzedażą nadmiaru: Gdy magazyn jest pełny, nadmiar energii jest sprzedawany.
# ================================

# Dodanie kolumny dla sprzedanej energii oraz dochodu z tej sprzedaży
dane_z_magazynem$SprzedanoEnergie <- 0
dane_z_magazynem$DochodZeSprzedazy <- 0

# Iteracja przez każdy wiersz (czyli każdą godzinę)
for(i in 1:nrow(dane_z_magazynem)) {
  
  # Pobieranie danych dla bieżącej godziny
  Moc <- dane_z_magazynem$Moc[i]
  Zuzycie <- dane_z_magazynem$Zuzycie[i]
  
  # Sprawdzenie czy w danej godzinie energia produkowana przez panele przekracza zużycie.
  if(Moc > Zuzycie) {
    nadmiar <- Moc - Zuzycie
    miejsce_w_magazynie <- pojemnosc_magazynu - stan_magazynu
    do_magazynu <- min(nadmiar, miejsce_w_magazynie)
    
    # Aktualizacja wartości energii dodanej do magazynu.
    dane_z_magazynem$WlozonoDoMagazynu[i] <- do_magazynu
    stan_magazynu <- stan_magazynu + do_magazynu
    
    # Jeśli nadmiar energii przekracza pojemność magazynu, nadmiar jest sprzedawany
    if(nadmiar > miejsce_w_magazynie) {
      sprzedane <- nadmiar - miejsce_w_magazynie
      dane_z_magazynem$SprzedanoEnergie[i] <- sprzedane
      
      # Obliczenie dochodu ze sprzedaży energii. Zakładamy, że dochód wynosi 96% wartości sprzedanej energii pomnożonej przez RCE.
      dane_z_magazynem$DochodZeSprzedazy[i] <- sprzedane * dane_z_magazynem$RCE[i] * 0.96
    }
    
  } else {
    # Jeśli moc jest mniejsza niż zużycie, konieczne jest pobranie energii z magazynu lub zakupienie jej
    brakuje <- Zuzycie - Moc
    dostepne_w_magazynie <- stan_magazynu * sprawnosc_magazynowania
    z_magazynu <- min(brakuje, dostepne_w_magazynie)
    dane_z_magazynu <- min(brakuje, dostepne_w_magazynie)
    dane_z_magazynem$PobranoZMagazynu[i] <- z_magazynu
    # Aktualizacja stanu magazynu po pobraniu potrzebnej ilości energii.
    stan_magazynu <- stan_magazynu - z_magazynu/sprawnosc_magazynowania
    brakuje <- brakuje - z_magazynu
    # Jeśli brakuje więcej energii niż jest dostępne w magazynie, resztę musimy zakupić z sieci.
    dane_z_magazynem$DokupioneZSieci[i] <- brakuje
  }
}

# Podsumowanie kosztów zakupionej energii i dochodów ze sprzedaży nadmiaru
rezultaty_analizy <- dane_z_magazynem %>%
  group_by(Rok) %>%
  summarise(
    RoczneWydatkiZakupionejEnergii = sum(DokupioneZSieci * RCE, na.rm = TRUE),
    RocznyDochodZeSprzedazy = sum(DochodZeSprzedazy, na.rm = TRUE)
)

# Dodawanie kolumny z rocznym zyskiem netto do tabeli rezultaty_analizy
rezultaty_analizy <- rezultaty_analizy %>%
  mutate(RocznyZyskNetto = RocznyDochodZeSprzedazy - RoczneWydatkiZakupionejEnergii)

# ================================
# Wizualiacja 1
# ================================

# Grupowanie i obliczanie miesięcznego zysku netto z energii sprzedanej z magazynem
zysk_miesieczny_magazyn <- dane_z_magazynem %>%
  group_by(Rok, Miesiac) %>%
  summarise(ZyskMiesiecznyMagazyn = sum(DochodZeSprzedazy, na.rm=TRUE) - sum(DokupioneZSieci * RCE, na.rm=TRUE))

# Zamiana numeru miesiąca na polską nazwę
zysk_miesieczny_magazyn$Miesiac <- polskie_miesiace[zysk_miesieczny_magazyn$Miesiac]

# Ustalenie kolejności miesięcy w ramce danych
zysk_miesieczny_magazyn <- zysk_miesieczny_magazyn %>%
  mutate(Miesiac = factor(Miesiac, levels = polskie_miesiace))

# Wykres
ggplot(zysk_miesieczny_magazyn, aes(x = Miesiac, y = ZyskMiesiecznyMagazyn, color = as.factor(Rok), group = Rok)) +
  geom_line(size = 1) +
  geom_point(size = 3, shape = 21, fill = "white") +
  scale_color_manual(values = kolory) +
  labs(title = "Miesięczny zysk netto ze sprzedaży energii z magazynem przez lata",
       subtitle = "Analiza sezonowości zysku netto z energii sprzedanej z wykorzystaniem magazynu",
       x = "Miesiąc",
       y = "Zysk Miesięczny z Magazynem [PLN]",
       color = "Rok") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "right", 
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("4.1e.png", width = 6.5, height = 10, dpi = 300)

# ================================
# Wizualiacja 2
# ================================

# Obliczamy roczne zyski netto na podstawie danych_z_magazynem
roczne_zyski_netto_magazyn <- dane_z_magazynem %>%
  group_by(Rok) %>%
  summarise(RoczneZyskiNetto = sum(DochodZeSprzedazy, na.rm = TRUE) - sum(DokupioneZSieci * RCE, na.rm=TRUE))

# Dodajemy skumulowany zysk netto do danych o rocznych zyskach
roczne_zyski_netto_magazyn <- roczne_zyski_netto_magazyn %>%
  arrange(Rok) %>%
  mutate(SkumulowaneZyskiNetto = cumsum(RoczneZyskiNetto))

# Ustawienie kolorów
kolory <- brewer.pal(9, "Set1")

# Wykres skumulowanych rocznych zysków netto z energii sprzedanej z wykorzystaniem magazynu
hc4.2e <- hchart(roczne_zyski_netto_magazyn, "line", hcaes(x = Rok, y = SkumulowaneZyskiNetto)) %>%
  hc_title(text = "Skumulowane roczne zyski netto ze Sprzedaży Energii przy użyciu Magazynu Energii") %>%
  hc_subtitle(text = "Analiza zysków netto ze sprzedaży energii na przestrzeni lat z uwzględnieniem magazynowania nadmiarów energii z PV") %>%
  hc_xAxis(title = list(text = "Rok"), categories = roczne_zyski_netto_magazyn$Rok, crosshair = TRUE) %>%
  hc_yAxis(title = list(text = "Skumulowany Zysk Netto [PLN]"), opposite = FALSE, crosshair = TRUE) %>%
  hc_add_series(roczne_zyski_netto_magazyn, "point", hcaes(x = Rok, y = SkumulowaneZyskiNetto), name = "Zysk Netto ze Sprzedaży Energii",
                dataLabels = list(enabled = TRUE, format = '{point.y:,.0f} zł', style = list(fontSize = '10px'))) %>%
  hc_tooltip(headerFormat = "<b>Rok {point.x}</b><br/>",
             pointFormat = "Skumulowany Zysk Netto: <b>{point.y:.2f} zł</b>") %>%
  hc_colors(kolory) %>%
  hc_chart(zoomType = 'xy') %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = TRUE)

htmlwidgets::saveWidget(hc4.2e, "4.2e.html", selfcontained = TRUE)

webshot::webshot("4.2e.html", "4.2e.png", delay = 5)

# ================================
# 4.7 Analiza sytuacji z różnymi pojemnościami magazynu i sprzedażą nadmiaru: Gdy magazyn jest pełny, nadmiar energii jest sprzedawany.
# ================================

# Definicje różnych pojemności magazynu
pojemnosci_magazynu <- c(7, 8, 9, 10) 

# Zbieranie danych do wykresu
wyniki <- data.frame()

# Pętla dla każdej pojemności magazynu
for(pojemnosc in pojemnosci_magazynu) {
  
  # Ustawienie początkowych parametrów magazynu
  pojemnosc_magazynu <- pojemnosc
  sprawnosc_magazynowania <- 0.90 
  stan_magazynu <- 0 
  rezultaty_analizy_pojemnosc <- data.frame()
  
  # Tworzenie kopii danych z panelami i dodanie nowych kolumn
  dane_z_magazynem <- dane_z_panelami
  dane_z_magazynem$PobranoZMagazynu <- 0
  dane_z_magazynem$DokupioneZSieci <- 0
  dane_z_magazynem$WlozonoDoMagazynu <- 0
  dane_z_magazynem$SprzedanoEnergie <- 0
  dane_z_magazynem$DochodZeSprzedazy <- 0
  
  # Iteracja przez każdy wiersz (czyli każdą godzinę)
  for(i in 1:nrow(dane_z_magazynem)) {
    
    # Pobieranie danych dla bieżącej godziny
    Moc <- dane_z_magazynem$Moc[i]
    Zuzycie <- dane_z_magazynem$Zuzycie[i]
    
    # Sprawdzenie czy w danej godzinie energia produkowana przez panele przekracza zużycie.
    if(Moc > Zuzycie) {
      nadmiar <- Moc - Zuzycie
      miejsce_w_magazynie <- pojemnosc_magazynu - stan_magazynu
      do_magazynu <- min(nadmiar, miejsce_w_magazynie)
      
      # Aktualizacja wartości energii dodanej do magazynu.
      dane_z_magazynem$WlozonoDoMagazynu[i] <- do_magazynu
      stan_magazynu <- stan_magazynu + do_magazynu
      
      # Jeśli nadmiar energii przekracza pojemność magazynu, nadmiar jest sprzedawany
      if(nadmiar > miejsce_w_magazynie) {
        sprzedane <- nadmiar - miejsce_w_magazynie
        dane_z_magazynem$SprzedanoEnergie[i] <- sprzedane
        
        # Obliczenie dochodu ze sprzedaży energii. Zakładamy, że dochód wynosi 96% wartości sprzedanej energii pomnożonej przez RCE.
        dane_z_magazynem$DochodZeSprzedazy[i] <- sprzedane * dane_z_magazynem$RCE[i] * 0.96
      }
      
    } else {
      # Jeśli moc jest mniejsza niż zużycie, konieczne jest pobranie energii z magazynu lub zakupienie jej
      brakuje <- Zuzycie - Moc
      dostepne_w_magazynie <- stan_magazynu * sprawnosc_magazynowania
      z_magazynu <- min(brakuje, dostepne_w_magazynie)
      dane_z_magazynu <- min(brakuje, dostepne_w_magazynie)
      dane_z_magazynem$PobranoZMagazynu[i] <- z_magazynu
      # Aktualizacja stanu magazynu po pobraniu potrzebnej ilości energii.
      stan_magazynu <- stan_magazynu - z_magazynu/sprawnosc_magazynowania
      brakuje <- brakuje - z_magazynu
      # Jeśli brakuje więcej energii niż jest dostępne w magazynie, resztę musimy zakupić z sieci.
      dane_z_magazynem$DokupioneZSieci[i] <- brakuje
    }
  }
  
  # Agregacja wyników dla każdej pojemności magazynu
  rezultaty_analizy_pojemnosc <- dane_z_magazynem %>%
    group_by(Rok) %>%
    summarise(
      RocznyZyskNetto = sum(DochodZeSprzedazy, na.rm = TRUE) - sum(DokupioneZSieci * RCE, na.rm = TRUE)
    ) %>%
    mutate(PojemnoscMagazynu = pojemnosc)
  
  wyniki <- rbind(wyniki, rezultaty_analizy_pojemnosc)
}

# ================================
# Wizualiacja 1
# ================================

# Obliczanie skumulowanego zysku netto
wynik_finalowy <- wyniki %>%
  arrange(Rok, PojemnoscMagazynu) %>%
  group_by(PojemnoscMagazynu) %>%
  mutate(SkumulowanyZyskNetto = cumsum(RocznyZyskNetto)) %>%
  ungroup()

hc4.1f <- hchart(wynik_finalowy, "line", hcaes(x = Rok, y = SkumulowanyZyskNetto, group = PojemnoscMagazynu)) %>%
  hc_title(text = "Skumulowane Roczne Zyski Netto ze Sprzedaży Energii przy Użyciu Magazynu Energii") %>%
  hc_subtitle(text = "Analiza zysków netto ze sprzedaży energii na przestrzeni lat z uwzględnieniem różnych pojemności magazynów") %>%
  hc_xAxis(title = list(text = "Rok"), tickInterval = 1) %>%
  hc_yAxis(title = list(text = "Skumulowany Zysk Netto [PLN]")) %>%
  hc_colors(c("#7cb5ec", "#434348", "#90ed7d", "#f7a35c", "#8085e9")) %>%
  hc_legend(title = list(text = "Pojemność Magazynu"), enabled = TRUE, layout = "vertical", align = "right", verticalAlign = "middle", useHTML = TRUE, labelFormatter = JS("function() { return this.name + ' <span style=\"color: ' + this.color + '\">&#9679;</span>'; }")) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_tooltip(headerFormat = "<b>Rok {point.key}</b><br/>", 
             pointFormat = "<span style='color:{series.color}'>\u25CF</span> {series.name}: {point.y:,.2f} zł<br/>", 
             shared = TRUE, 
             crosshairs = TRUE)

htmlwidgets::saveWidget(hc4.1f, "4.1f.html", selfcontained = TRUE)

webshot::webshot("4.1f.html", "4.1f.png", delay = 5)

# ================================
# Wizualiacja 2
# ================================

#Wykres Słupkowy Porównujący Roczne Zyski Netto dla Różnych Pojemności Magazynu
ggplot(wyniki, aes(x = factor(Rok), y = RocznyZyskNetto, fill = factor(PojemnoscMagazynu))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Roczny Zysk Netto w Zależności od Pojemności Magazynu",
       x = "Rok", y = "Zysk Netto [PLN]",
       fill = "Pojemność Magazynu")

ggplot(wyniki, aes(x = factor(Rok), y = RocznyZyskNetto, fill = factor(PojemnoscMagazynu))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Set3") +  
  theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), 
    plot.subtitle = element_text(hjust = 0.5, size = 12), 
    axis.title.x = element_text(size = 12, face = "bold"), 
    axis.title.y = element_text(size = 12, face = "bold"), 
    legend.title = element_text(face = "bold"), 
    legend.position = "bottom" 
  ) +
  labs(
    title = "Roczny Zysk Netto w Zależności od Pojemności Magazynu",
    x = "Rok",
    y = "Zysk Netto [PLN]",
    fill = "Pojemność Magazynu"
  ) +
  geom_text(aes(label=sprintf("%.2f", RocznyZyskNetto)), vjust=-0.3, position = position_dodge(0.9), size=3)

ggsave("4.2f.png", width = 11.5, height = 11, dpi = 300)

###################################################################################
# ANALITYKA GOSPODARCZA - PROGNOZOWANIE GOSPODARCZE - R.A. 2024/2025              #
# LABORATORIUM XI                                                                 #
#                                                                                 #
# BADANIE WYSTĘPOWANIA ZJAWISKA KONWERGENCJI  - zbieżność lub powstawanie         #
# zbieżności, np. powstawanie podobnych wytworów kulturowych u różnych ludów      #
#                                                                                 #
# PROGNOZOWANIE LICZBY LAT POTRZEBNYCH DO ZREDUKOWANIA OBECNYCH RÓŻNIC O POŁOWĘ   #
###################################################################################

# konwergencja typu beta zachodzi, jeśli obszary
# o początkowo mniejszej wartości badanej cechy (np. PKB per capita) wykazują
# szybsze tempo wzrostu niż obszary o początkowo jej wyższej wartości
# - doganianie początkowo bogatszych obszarów przez biedniejsze
# - regresja (patrzeć jakie równanie jest zastosowane w pakiecie)

# "Konwergencja gospodarcza w polsce i jej znaczenie w osiąganiu celów polityki spójności" - Ewa Kusideł

# często pierwszy okres jest oznaczany 0 zamiast 1


rm(list=ls())
# znak oddzielający część dziesiętną
options(OutDec=",")

library(tidyverse)

# pobranie danych
dane <- read_csv2(file = 'Dane 5_2024.csv', col_types = "ffffnnnnnnnnnnnnnnnnnn") # f - faktor
summary(dane)

# R_... - PKB na mieszkańca
# badamy 18 lat, ale przejść z roku na rok jest 17

###########################################################
## WIZUALIZACJA DANYCH

# wizualizacja danych - histogram
dane %>%
  select(-c("Region","Nazwa","UE_do_2004","UE_od_2004")) %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot() +
  geom_histogram(mapping = aes(x = value, fill = key), color = "black", bins = 16) +
  facet_wrap(~ key, scales = "free") +
  ylim(0, 60) +     # standaryzacja skali
  labs(title = "HISTOGRAMY ZMIENNYCH", y = "Liczba", x = "Wartość") +
  labs(fill = "PKB per capita") +
  theme_minimal()

# zmiana układu danych do wykresu pudełkowego
library(reshape2)
dane_ciag <- melt(dane[,c(3,5:22)], id = "UE_do_2004", variable.name = "Zmienne", 
                  value.name = "Wartość")   
head(dane_ciag)

# wizualizacja danych - wykres pudełkowy
ggplot(dane_ciag, aes(x = Zmienne, y = Wartość, color = UE_do_2004)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "WYKRESY PUDEŁKOWE") +
  labs(color = "Państwa UE") +
  scale_colour_discrete(labels = c("UE_do_2004", "UE_od_2004")) +
  geom_boxplot()

###################################################################
# UWAGA: Proszę spróbować przygotować kod umożliwiający wykonanie #
# poprzedniego rysunku (z histogramami, w. 22-33) w podziale      #
# na "UE_do_2004" i "UE_od_2004".                                 #
###################################################################

# zmienić kod poniżej tak żeby był podział na OD 2004 i DO 2004:

dane %>%
  select(-c("Region","Nazwa","UE_do_2004","UE_od_2004")) %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot() +
  geom_histogram(mapping = aes(x = value, fill = key), color = "black", bins = 16) +
  facet_wrap(~ key, scales = "free") +
  ylim(0, 60) +     # standaryzacja skali
  labs(title = "HISTOGRAMY ZMIENNYCH", y = "Liczba", x = "Wartość") +
  labs(fill = "PKB per capita") +
  theme_minimal()


###########################################################
## BADANIE KONWERGENCJI TYPU BETA W WERSJI ABSOLUTNEJ

### ustalenie T i zmiennych
T <- dim(dane[,5:22])[2]-1
T

# zmienna niezależna
dane$niezalezna <- log(dane$R_2004) # log = logarytm naturalny ln

# zmienna zależna
dane$zalezna <- (log(dane$R_2021)-log(dane$R_2004))/T

### prosta regresji dla Ogółem
model_ogolem <- lm(data = dane, zalezna ~ niezalezna)
summary(model_ogolem)
# statystycznie istotny bo p-value < 2,2e-16; występuje konwergencja

model_ogolem$coefficients
summary(model_ogolem)[[4]][,4] # samo p-value

### współczynnik regresji
a1_ogolem <- model_ogolem$coefficients[[2]]
a1_ogolem # wartość oceny parametru

a1_p_ogolem <- summary(model_ogolem)[[4]][2,4]
a1_p_ogolem # p-value

### przyjęcie poziomu istotności, obliczenie współczynników ze wzorów
alfa <- 0.05
if (is.na(a1_ogolem) != TRUE & is.na(a1_p_ogolem) != TRUE){
  if (a1_p_ogolem < alfa){
    if (a1_ogolem < 0){
      cat("Współczynnik regresji:", round(a1_ogolem, 5), "\n")
      # współczynnik zbieżności
      beta_ogolem <- -log(1+a1_ogolem*T)/T
      cat("Współczynnik zbieżności:", round(beta_ogolem, 5), "\n")
      # współczynnik połowicznej zbieżności
      T_polow.zb_ogolem <- log(2)/beta_ogolem
      cat("Współczynnik połowicznej zbieżności:", round(T_polow.zb_ogolem, 5), "\n")
    } else {
       cat("Współczynnik regresji jest nieujemny.")
      } 
  } else {
      cat("Na poziomie istotności",alfa, "nie ma podstaw do odrzucenia hipotezy zerowej głoszącej, 
      że parametr kierunkowy prostej regresji jest statystycznie nieistotny.")
    }
} else {
    cat("Nie udało się oszacować prostej regresji.")
}

# INTERPRETACJA: w badanym okresie regiony zmniejszały odległość przeciętnie o 2,3% od punktu równowagi (Współczynnik zbieżności: 0,02302)
# PROGNOZA: regiony potrzebują co najmniej 31 lat aby pokonać połowę drogi do punktu zbieżności (Współczynnik połowicznej zbieżności: 30,10611)

### prosta regresji dla UE_stare (UE_do_2004)
model_stare <- lm(data = dane, zalezna ~ niezalezna, subset = (UE_do_2004 == 1))
summary(model_stare)

model_stare$coefficients
summary(model_stare)[[4]][,4]

# p-value, parametr statystycznie nieistotny

# współczynnik regresji
a1_stare <- model_stare$coefficients[[2]]
a1_stare
a1_p_stare <- summary(model_stare)[[4]][2,4]
a1_p_stare
alfa <- 0.05
if (is.na(a1_stare) != TRUE & is.na(a1_p_stare) != TRUE){
  if (a1_p_stare < alfa){
    if (a1_stare < 0){
      cat("Współczynnik regresji:", round(a1_stare, 5), "\n")
      # współczynnik zbieżności
      beta_stare <- -log(1+a1_stare*T)/T
      cat("Współczynnik zbieżności:", round(beta_stare, 5), "\n")
      # współczynnik połowicznej zbieżności
      T_polow.zb_stare <- log(2)/beta_stare
      cat("Współczynnik połowicznej zbieżności:", round(T_polow.zb_stare, 5), "\n")
    } else {
      cat("Współczynnik regresji jest nieujemny.")
    } 
  } else {
    cat("Na poziomie istotności",alfa, "nie ma podstaw do odrzucenia hipotezy zerowej głoszącej, 
    że parametr kierunkowy prostej regresji jest statystycznie nieistotny.")
  }
} else {
  cat("Nie udało się oszacować prostej regresji.")
}

# INTERPRETACJA: Na poziomie istotności 0,05 nie ma podstaw do odrzucenia hipotezy zerowej głoszącej, 
# że parametr kierunkowy prostej regresji jest statystycznie nieistotny.

### prosta regresji dla UE_nowe (UE_od_2004)
model_nowe <- lm(data = dane, zalezna ~ niezalezna, subset = (UE_od_2004 == 1))
summary(model_nowe)

# parametr statystycznie istotny -> występuje konwergencja

model_nowe$coefficients
summary(model_nowe)[[4]][,4]

# współczynnik regresji
a1_nowe <- model_nowe$coefficients[[2]]
a1_nowe
a1_p_nowe <- summary(model_nowe)[[4]][2,4]
a1_p_nowe
alfa <- 0.05
if (is.na(a1_nowe) != TRUE & is.na(a1_p_nowe) != TRUE){
  if (a1_p_nowe < alfa){
    if (a1_nowe < 0){
      cat("Współczynnik regresji:", round(a1_nowe, 5), "\n")
      # współczynnik zbieżności
      beta_nowe <- -log(1+a1_nowe*T)/T
      cat("Współczynnik zbieżności:", round(beta_nowe, 5), "\n")
      # współczynnik połowicznej zbieżności
      T_polow.zb_nowe <- log(2)/beta_nowe
      cat("Współczynnik połowicznej zbieżności:", round(T_polow.zb_nowe, 5), "\n")
    } else {
      cat("Współczynnik regresji jest nieujemny.")
    } 
  } else {
    cat("Na poziomie istotności",alfa, "nie ma podstaw do odrzucenia hipotezy zerowej głoszącej, 
    że parametr kierunkowy prostej regresji jest statystycznie nieistotny.")
  }
} else {
  cat("Nie udało się oszacować prostej regresji.")
}

# INTERPRETACJA: Regiony przeciętnie co roku pokonują ok. 1,8%
# PROGNOZA: Potrzebują co najmniej 39 lat aby pokonać połowę drogi do punktu zbieżności


###########################################################
## WIZUALIZACJA MODELI REGRESJI

# diagram korelacyjny
ggplot(dane, aes(x = niezalezna, y = zalezna)) + 
  labs(title = "DIAGRAM KORELACYJNY") +
  geom_point()

# diagram korelacyjny + podział na państwa UE_stare i UE_nowe
ggplot(dane, aes(x = niezalezna, y = zalezna, color = dane$UE_do_2004)) + 
  labs(title = "DIAGRAM KORELACYJNY") +
  labs(color = "Państwa UE") +
  scale_colour_discrete(labels = c("UE_do_2004", "UE_od_2004")) +
  geom_point()

# diagram korelacyjny + prosta regresji dla Ogółem + podział na państwa UE_stare i UE_nowe
ggplot(model_ogolem, aes_string(x = names(model_ogolem$model)[2], y = names(model_ogolem$model)[1], 
                                color = dane$UE_do_2004)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(title = paste("MODEL OGÓŁEM: Skor.R^2 = ",signif(summary(model_ogolem)$adj.r.squared, 3),
                     "; Wyraz wolny =",signif(model_ogolem$coef[[1]], 3),
                     "; Wyraz kierunkowy =",signif(model_ogolem$coef[[2]], 3),
                     "; p-value =",signif(summary(model_ogolem)$coef[2,4], 3))) +
  labs(color = "Państwa UE") +
  scale_colour_discrete(labels = c("UE_do_2004", "UE_od_2004"))
  
# diagram korelacyjny + proste regresji dla UE_stare i UE_nowe
ggplot(model_ogolem, aes_string(x = names(model_ogolem$model)[2], y = names(model_ogolem$model)[1], 
                                fill = dane$UE_do_2004)) + 
  geom_point() + 
  stat_smooth(method = "lm", col = "red") +
  labs(title = paste("MODEL UE_STARE: Skor.R^2 = ",signif(summary(model_stare)$adj.r.squared, 2),
                     "; Wyraz wolny =",signif(model_stare$coef[[1]], 2),
                     "; Wyraz kierunkowy = ",signif(model_stare$coef[[2]], 1),
                     "; p-value =",signif(summary(model_stare)$coef[2,4], 3))) +
  labs(subtitle = paste("MODEL UE_NOWE:  Skor.R^2 = ",signif(summary(model_nowe)$adj.r.squared, 3),
                     "; Wyraz wolny = ",signif(model_nowe$coef[[1]], 3),
                     "; Wyraz kierunkowy =",signif(model_nowe$coef[[2]], 2),
                     "; p-value =",signif(summary(model_nowe)$coef[2,4], 3))) +
  labs(fill = "Państwa UE") + 
  scale_fill_discrete(labels = c("UE_do_2004", "UE_od_2004")) +
  theme(plot.title = element_text(size = 12), plot.subtitle = element_text(size = 12))


###########################################################
### ANALIZA SKUPIEŃ - NIENADZOROWANE UCZENIE STATYSTYCZNE

# technika analizy wielowymiarowej

### metoda hierarchiczna - metoda Warda

# standaryzacja zmiennych
dane_stand <- dane
for (j in 5:22){
    dane_stand[,j] <- scale(dane[,j])
  }
summary(dane_stand)

# miara odległości
odl_Euklidesa <- dist(dane_stand[,5:22], method = "euclidean")

# metoda Warda z kwadratem odległości Euklidesa
dane_Ward <- hclust(odl_Euklidesa, method = 'ward.D2')

# drzewo połączeń nr 1
plot(dane_Ward, labels = NULL, cex = 0.6, 
     main = "Dendrogram", sub = NULL,
     xlab = "Regiony", ylab = "Wysokość")

# przeskalowanie "Wysokości"
dane_Ward$height <- 100*dane_Ward$height/max(dane_Ward$height)

# drzewo połączeń nr 2
plot(dane_Ward, labels = NULL, cex = 0.6, 
     main = "Dendrogram", sub = NULL,
     xlab = "Regiony", ylab = "Wysokość")

# drzewo połączen nr 3
plot(dane_Ward, labels = NULL, cex = 0.6, hang = -1, 
     main = "Dendrogram", sub = NULL,
     xlab = "Regiony", ylab = "Wysokość")

# podział regionów na 2 skupienia
rect.hclust(dane_Ward, k=2, border="black")
# podział regionów na 3 skupienia
rect.hclust(dane_Ward, k=3, border="red")
# podział regionów na 4 skupienia
rect.hclust(dane_Ward, k=4, border="blue")
# podział regionów na 5 skupień
rect.hclust(dane_Ward, k=5, border="green")
# podział regionów na 7 skupień
rect.hclust(dane_Ward, k=7, border="yellow")

## metoda optymalizacyjna - metoda k-średnich

# metoda k-średnich k = 3

# wynik dla iteracji nr 1
set.seed(1)
dane_k_srednich_3_i1 <- kmeans(x = dane_stand[,5:22], centers = 3, nstart = 1)
dane_k_srednich_3_i1

# wizualizacja podziału na 3 skupienia - iteracja nr 1
library(useful)
plot.kmeans(dane_k_srednich_3_i1, data = dane_stand[,5:22],
            title = "Podział regionów na 3 skupienia metodą k-średnich - iteracja nr 1",
            xlab = "Główna składowa nr 1", ylab = "Główna składowa nr 2") 

# wynik iteracji nr 5
set.seed(1)
dane_k_srednich_3_i5 <- kmeans(x = dane_stand[,5:22], centers = 3, nstart = 5)

# wizualizacja podziału na 3 skupienia - itracja nr 5
plot.kmeans(dane_k_srednich_3_i5, data = dane_stand[,5:22],
            title = "Podział regionów na 3 skupienia metodą k-średnich - iteracja nr 5",
            xlab = "Główna składowa nr 1", ylab = "Główna składowa nr 2") 

# wynik iteracji nr 10
set.seed(1)
dane_k_srednich_3_i10 <- kmeans(x = dane_stand[,5:22], centers = 3, nstart = 10)

# wizualizacja podziału na 3 skupienia - itracja nr 10
plot.kmeans(dane_k_srednich_3_i10, data = dane_stand[,5:22],
            title = "Podział regionów na 3 skupienia metodą k-średnich - iteracja nr 10",
            xlab = "Główna składowa nr 1", ylab = "Główna składowa nr 2") 

# wynik iteracji nr 20
set.seed(1)
dane_k_srednich_3_i20 <- kmeans(x = dane_stand[,5:22], centers = 3, nstart = 20)

# wizualizacja podziału na 3 skupienia - itracja nr 20
plot.kmeans(dane_k_srednich_3_i20, data = dane_stand[,5:22],
            title = "Podział regionów na 3 skupienia metodą k-średnich - iteracja nr 20",
            xlab = "Główna składowa nr 1", ylab = "Główna składowa nr 2") 

# wynik iteracji nr 50
set.seed(1)
dane_k_srednich_3_i50 <- kmeans(x = dane_stand[,5:22], centers = 3, nstart = 50)

# wizualizacja podziału na 3 skupienia - itracja nr 50
plot.kmeans(dane_k_srednich_3_i50, data = dane_stand[,5:22],
            title = "Podział regionów na 3 skupienia metodą k-średnich - iteracja nr 50",
            xlab = "Główna składowa nr 1", ylab = "Główna składowa nr 2") 

# porównanie liczebności skupień w iteracji nr 1, iteracji nr 5, iteracji nr 10, iteracji nr 20 i iteracji nr 50
dane_k_srednich_3_i1$size
dane_k_srednich_3_i5$size
dane_k_srednich_3_i10$size
dane_k_srednich_3_i20$size
dane_k_srednich_3_i50$size

# przynależność do skupień w iteracji nr 1
dane_k_srednich_3_i1$cluster

## charakterystyka skupień - wykres średnich wartości zmiennych w 3 skupieniach

# dla zmiennych standaryzowanych
mean_class_stand <- t(dane_k_srednich_3_i1$centers)
colnames(mean_class_stand) <- c("Grupa 1","Grupa 2","Grupa 3")
mean_class_stand_ciag <- melt(mean_class_stand) 
colnames(mean_class_stand_ciag) <- c("Zmienna_standaryzowana","Grupa","Średnia_wartość")
head(mean_class_stand_ciag)
ggplot(data=mean_class_stand_ciag, aes(x=Grupa, y=Średnia_wartość, fill=Zmienna_standaryzowana)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(title = "WYKRES ŚREDNICH WARTOŚCI ZMIENNYCH STANDARYZOWANYCH W SKUPIENIACH")

# dla zmiennych oryginalnych
dane$Grupy_3 <- as.factor(dane_k_srednich_3_i1$cluster)
mean_class <- aggregate(dane[,5:22], list(dane$Grupy_3), FUN=mean)
mean_class <- t(mean_class[,-1])
colnames(mean_class) <- c("Grupa 1","Grupa 2","Grupa 3")
mean_class_ciag <- melt(mean_class) 
colnames(mean_class_ciag) <- c("Zmienna_oryginalna","Grupa","Średnia_wartość")
head(mean_class_ciag)
ggplot(data=mean_class_ciag, aes(x=Grupa, y=Średnia_wartość, fill=Zmienna_oryginalna)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(title = "WYKRES ŚREDNICH WARTOŚCI ZMIENNYCH ORYGINALNYCH W SKUPIENIACH")


###########################################################
## BADANIE KONWERGENCJI W GRUPACH REGIONÓW - podział na 3 skupienia

# dla 3 skupień - metoda k-średnich dla k = 3, wynik iteracji nr 1
# dodanie zmiennej z numerami skupień
dane$Grupy_3 <- as.factor(dane_k_srednich_3_i1$cluster)

# zmiana nazw kategorii zmiennej dane$Grupy_3
library(dplyr)
dane <- dane%>%
  mutate(Grupy_3 = recode_factor(Grupy_3, "1"="Grupa 1", "2"="Grupa 2", "3"="Grupa 3"))

# składy skupień
dane[which(dane$Grupy_3 == "Grupa 1"),"Region"] %>%
  print(n = dim(dane[which(dane$Grupy_3 == "Grupa 1"),])[1])
#dane[which(dane$Region == "PL91"), "Nazwa"]
dane[which(dane$Grupy_3 == "Grupa 2"),"Region"] %>%
  print(n = dim(dane[which(dane$Grupy_3 == "Grupa 2"),])[1])
dane[which(dane$Grupy_3 == "Grupa 3"),"Region"] %>%
  print(n = dim(dane[which(dane$Grupy_3 == "Grupa 3"),])[1])


#########################################################################################
#########################################################################################
## ZADANIA DOMOWE
# Zad.D13 - Proszę napisać kod umożliwiający sprawdzenie, czy występuje zjawisko 
#           konwergencji w wyodrębnionych 3 grupach państw.
# Zad.D14 - Proszę powtórzyć analizę przeprowadzoną w czasie zajęć dla k = 2 
#          (tj. dla podziału regionów na 2 skupienia).
# Zad.D15 - Proszę powtórzyć analizę przeprowadzoną w czasie zajęć dla k = 4 
#          (tj. dla podziału regionów na 4 skupienia).
# Zad.D16 - Proszę powtórzyć analizę przeprowadzoną w czasie zajęć 
#           dla lat 2010-2021.
#
# Zad.D17.EKSTRA - Proszę dokonać wizualizacji składów 3 skupień rozważanych 
#                  w czasie zajęć na mapie regionów NUTS2 państw członkowskich UE.
#########################################################################################
#########################################################################################
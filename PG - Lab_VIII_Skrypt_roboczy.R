###################################################################################
# ANALITYKA GOSPODARCZA - PROGNOZOWANIE GOSPODARCZE - R.A. 2024/2025              #
# LABORATORIUM VIII                                                               #
# DYNAMICZNY LINIOWY MODEL ZGODNY                                                 #
###################################################################################

rm(list=ls())
install.packages("readxl")
library(readxl)
install.packages("tseries")
library(tseries)
install.packages("forecast")
library(forecast)


### Pobranie danych
dane1 <- read_excel("Dane 3_2024.xls", sheet = "Dane 3", range = cell_cols("B:D"))
dane2 <- ts(dane1,start=c(2010,1),freq=12)


### Prezentacja graficzna szeregów czasowych

par(mar=c(4,4,2,1)+0.1, mgp=c(3,0.6,0),bg="lightgoldenrodyellow",las=1,mfrow=c(3,1))

plot(1,axes=F,xlab="",ylab="",main="Dane 3_2024", col="white")
u <- par("usr")
rect(u[1], u[3], u[2], u[4], col="white")
par(new=plot)
plot(dane2[,1],xlab="Czas",ylab="Y",col="SteelBlue",lwd=2,las=1,type="b")

plot(1,axes=F,xlab="",ylab="",main="", col="white")
u <- par("usr")
rect(u[1], u[3], u[2], u[4], col="white")
par(new=plot)
plot(dane2[,2],xlab="Czas",ylab="X1",col="SteelBlue",lwd=2,las=1,type="b")

# Proszę uzupełnić kod:
plot(1,axes=F,xlab="",ylab="",main="", col="white")
u <- par("usr")
rect(u[1], u[3], u[2], u[4], col="white")
par(new=plot)
plot(dane2[,3],xlab="Czas",ylab="X2",col="SteelBlue",lwd=2,las=1,type="b")

#-----------------------------------------------------------------------
### Analiza struktury wewnętrzenej procesu - Y

## Dodanie nowych zmiennych - analiza trendu
# Zmienna t
t1 <- ts(1:length(dane2[,1]),start=c(2010,1),freq=12)
# Zmienna t do kwadratu
t2 <- ts(t1^2,start=c(2010,1),freq=12)
# Zmienna t do potęgi trzeciej
# Proszę uzupełnić kod:
t3 <- ts(t1^3,start=c(2010,1),freq=12)

## Modele trendu (2010:1-2022:9)
zbior_uczacy_1 <- as.data.frame(cbind(dane2[1:153,],t1[1:153],t2[1:153],t3[1:153]))
colnames(zbior_uczacy_1) <- c("Y","X1","X2","t1","t2","t3")
m_0 <- lm(Y ~ 1, data = zbior_uczacy_1)                   # tylko wyraz wolny
m_1 <- lm(Y ~ 1 + t1, data = zbior_uczacy_1)              # wielomian stopnia pierwszego
m_2 <- lm(Y ~ 1 + t1 + t2, data = zbior_uczacy_1)         # wielomian stopnia drugiego
m_2_bis <- lm(Y ~ 1 + t2, data = zbior_uczacy_1)          # wielomian stopnia drugiego - zredukowany/okrojony
# Proszę uzupełnić kod:
m_3 <- lm(Y ~ 1 + t1 + t2 + t3, data = zbior_uczacy_1)    # wielomian stopnia trzeciego
m_3_bis <- lm(Y ~ 1 + t3, data = zbior_uczacy_1)          # wielomian st. trzeciego - zredukowany

# Podsumowanie modeli trendu
summary(m_0)
summary(m_1)
summary(m_2)
summary(m_2_bis)
# Proszę uzupełnić kod:
summary(m_3)
summary(m_3_bis)


## Porównanie modeli trendu

# 1) Test dla dwóch wariancji
# Proszę zaproponować kod - Zadanie domowe

# 2) Analiza wariancji - można jeśli na tych samych danych, najlepiej jak to modele zagnieżdżone
anova(m_0,m_1)
# Proszę wybrać model do dalszej analizy
anova(m_1,m_2)
# Proszę wybrać model do dalszej analizy
anova(m_1,m_3) #bo m_1 "wygrał" z m_2
#Wniosek: Wybieramy model m_1.

# 3) Kryteria informacyjne (zredukowane modele też zrobić), np.:
AIC(m_0,m_1,m_2,m_3, m_2_bis, m_3_bis)
# Wniosek: Wskazanie na model m_3_bis.

## Dodanie nowych zmiennych - analiza sezonowości
# Zmienne identyfikujące fazy w cyklu - zmienne zero-jedynkowe
# Proszę uzupełnić kod:
month <- ts(seasonaldummy(dane2[,1]),start=c(2010,1),freq=12) #dane miesięczne
month

# Zmienne identyfikujące fazy w cyklu - zmienne z trzema wariantami (-1, 0 , 1)
for (i in 1:14){
  month[12+(i-1)*12,] <- c(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1)
}
month
# Proszę zaproponować inne rozwiązanie - Zadanie domowe - tak żeby kod rozpoznał sam grudzień i tam wstawił "-1"

## Nowy zbiór uczący
zbior_uczacy_2 <- as.data.frame(cbind(dane2[1:153,],t1[1:153],t2[1:153],t3[1:153],month[1:153,]))
colnames(zbior_uczacy_2) <- c("Y","X1","X2","t1","t2","t3","M1","M2","M3","M4","M5","M6","M7","M8","M9","M10","M11")

## Modele: trend + sezonowość (2010:1-2022:9)
ms_0 <- lm(Y ~ 1 + M1 + M2 + M3 + M4 + M5 + M6 + M7 + M8 + M9 + M10 + M11, data = zbior_uczacy_2)
ms_1 <- lm(Y ~ 1 + t1 + M1 + M2 + M3 + M4 + M5 + M6 + M7 + M8 + M9 + M10 + M11, data = zbior_uczacy_2)
ms_2 <- lm(Y ~ 1 + t1 + t2 + M1 + M2 + M3 + M4 + M5 + M6 + M7 + M8 + M9 + M10 + M11, data = zbior_uczacy_2)
ms_2_bis <- lm(Y ~ 1 + t2 + M1 + M2 + M3 + M4 + M5 + M6 + M7 + M8 + M9 + M10 + M11, data = zbior_uczacy_2)
# Proszę uzupełnić kod:
ms_3 <- lm(Y ~ 1 +t1 + t2 + t3 + M1 + M2 + M3 + M4 + M5 + M6 + M7 + M8 + M9 + M10 + M11, data = zbior_uczacy_2)
ms_3_bis <- lm(Y ~ 1 + t3 + M1 + M2 + M3 + M4 + M5 + M6 + M7 + M8 + M9 + M10 + M11, data = zbior_uczacy_2)

# Podsumowanie modeli z trendem i sezonowością
summary(ms_0)
summary(ms_1)
summary(ms_2)
# Proszę uzupełnić kod:
summary(ms_3)
summary(ms_2_bis)
summary(ms_3_bis) #chyba najlepszy

## Porównanie modeli z trendem i sezonowością

# 1) Test dla dwóch wariancji
# Proszę zaproponować kod - Zadanie domowe

# 2) Analiza wariancji
anova(ms_0,ms_1)
# Proszę wybrać model do dalszej analizy
anova(ms_1,ms_2)
# Proszę wybrać model do dalszej analizy
anova(ms_2,ms_3)
# Wniosek: Wybieramy model ms_2.

# 2) Kryteria informacyjne, np.
AIC(ms_0,ms_1,ms_2,ms_3, ms_2_bis, ms_3_bis)
# Wniosek: Wskazanie na model ms_2.

## Analiza autokorelacji

# Generowanie reszt
r_ms_0 <- ts(resid(ms_0),start=c(2010,1),freq=12)
r_ms_1 <- ts(resid(ms_1),start=c(2010,1),freq=12)
r_ms_2 <- ts(resid(ms_2),start=c(2010,1),freq=12)
# Proszę uzupełnić kod:
r_ms_3 <- ts(resid(ms_3),start=c(2010,1),freq=12)

## Współczynnik PACF - wykresy

par(mar=c(4,4,2,1)+0.1, mgp=c(3,0.6,0),bg="lightgoldenrodyellow",las=1,mfrow=c(2,2))

plot(1,axes=F,xlab="",ylab="",main="Model: tylko wyraz wolny", col="white")
u <- par("usr")
rect(u[1], u[3], u[2], u[4], col="white")
par(new=plot)
pacf(r_ms_0[1:length(r_ms_0)], lag.max = 13, plot = TRUE, ylab = "PACF", xlab = "Opóźnienie", main = "")

plot(1,axes=F,xlab="",ylab="",main="Model: wielomian stopnia pierwszego", col="white")
u <- par("usr")
rect(u[1], u[3], u[2], u[4], col="white")
par(new=plot)
pacf(r_ms_1[1:length(r_ms_1)], lag.max = 13, plot = TRUE, ylab = "PACF", xlab = "Opóźnienie", main = "")

plot(1,axes=F,xlab="",ylab="",main="Model: wielomian stopnia drugiego", col="white")
u <- par("usr")
rect(u[1], u[3], u[2], u[4], col="white")
par(new=plot)
pacf(r_ms_2[1:length(r_ms_2)], lag.max = 13, plot = TRUE, ylab = "PACF", xlab = "Opóźnienie", main = "")

# Proszę uzupełnić kod:
plot(1,axes=F,xlab="",ylab="",main="Model: wielomian stopnia trzeciego", col="white")
u <- par("usr")
rect(u[1], u[3], u[2], u[4], col="white")
par(new=plot)
pacf(r_ms_3[1:length(r_ms_3)], lag.max = 13, plot = TRUE, ylab = "PACF", xlab = "Opóźnienie", main = "")

# Współczynnik PACF - wartości + p-value
# Proszę zaproponować kod - Zadanie domowe


#-----------------------------------------------------------------------
### Analiza struktury wewnętrzenej procesu - X1
# Proszę przeprowadzić analizę samodzielnie

#-----------------------------------------------------------------------
### Analiza struktury wewnętrzenej procesu - X2
# Proszę przeprowadzić analizę samodzielnie

#-----------------------------------------------------------------------
### Budowa dynamicznego liniowego modelu zgodnego

## Estymacja modelu pełnego

# Wersja na podstawie analizy wykonanej w programie GRETL
m_pelny_G <- lm(Y ~ 1 + t1 + t2 + t3 + M1 + M2 + M3 + M4 + M5 + M6 + M7 + M8 + M9 + M10 + M11 + 
                X1 + lag(X1,-1) + X2 + lag(X2,-1) + lag(X2,-3) +
                lag(Y,-1) + lag(Y,-2) + lag(Y,-3), data = zbior_uczacy_2)

### DO UZUPEŁNIENIA
# Wersja na podstawie analizy wykonanej w R
m_pelny_R <- 

# Podsumowanie modelu pełnego
summary(m_pelny_R)

## Redukcja modelu pełnego
# przykład kodu
m_red_R <- step(m_pelny_R, direction = "backward", test = "F")
summary(m_red_R)
# Proszę zaproponować inne rozwiązanie - Zadanie domowe

# Wartości teoretyczne
war_teor_R <- predict(m_red_R)

## Graficzna prezentacja wartości empirycznych i teoretycznych uzyskanych z modelu zredukowanego
par(mar=c(4,4,2,1)+0.1, mgp=c(3,0.6,0),bg="lightgoldenrodyellow",las=1)
plot(1,axes=F,xlab="",ylab="",main="", col="white")
u <- par("usr")
rect(u[1], u[3], u[2], u[4], col="white")
par(new=plot)
plot(zbior_uczacy_2[,1],type='l',lwd=2,col='SteelBlue')
lines(war_teor_R,lwd=2,col='violetred3')
legend("topright",bg='white',bty="n",lty=1,lwd=c(4,2),
       c('empiryczna','teoretyczna'),col=c('SteelBlue','violetred3'))

### Analiza reszt modelu zredukowanego

# Generowanie reszt z modelu zredukowanego
r_m_red_R <- ts(resid(m_red_R),start=c(2010,1),freq=12)

## Prezentacja graficzna reszt modelu zredukowanego
par(mar=c(4,4,2,1)+0.1, mgp=c(3,0.6,0),bg="lightgoldenrodyellow",las=1)
plot(1,axes=F,xlab="",ylab="",main="Model zredukowany", col="white")
u <- par("usr")
rect(u[1], u[3], u[2], u[4], col="white")
par(new=plot)
plot(r_m_red_R,type="b",col="SteelBlue", xlab="Czas", ylab="Reszty")
abline(h=0, col="red")

## Badanie normalności rozkładu składnika losowego
# Proszę zaproponować kod - Zadanie domowe

## Badanie homoskedastyczności rozkładu składnika losowego
# Proszę zaproponować kod - Zadanie domowe

## Badanie autokorelacji składnika losowego
# Proszę zaproponować kod - Zadanie domowe

### Prognozowanie i ocena dokładności prognoz
# Proszę zaproponować kod - Zadanie domowe

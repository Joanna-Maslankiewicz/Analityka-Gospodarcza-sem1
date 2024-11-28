################### Diagnostyka w regresji ##############

###########################################################
############# dane gala ###################################
###########################################################

library(faraway)
data(gala)
head(gala)
summary(gala)

# Wykres par (bez drugiej kolumny)
pairs(gala[,-2])


# Model regresji dla Species
gg <- lm(Species ~ Area + Elevation + Scruz + Nearest + Adjacent, data = gala)
summary(gg)

# Wykres diagnostyczny dla modelu gg
par(mfrow = c(2, 2))
plot(gg)

# Stabilizacja wariancji reszt przez transformację pierwiastkową
gg1 <- lm(sqrt(Species) ~ Area + Elevation + Scruz + Nearest + Adjacent, data = gala)
summary(gg1)

# Wykres diagnostyczny dla modelu gg1
plot(gg1)

# Wykres reszt vs wartości dopasowane dla modelu gg1
plot(gg1$fitted.values, gg1$residuals, 
     xlab = "Dopasowane wartości", 
     ylab = "Reszty", 
     main = "Wykres reszt vs dopasowane wartości (transformacja)")



# Usunięcie dwóch wysp ("Isabela", "Fernandina") i ponowna analiza
gala_1 <- gala[!rownames(gala) %in% c("Isabela", "Fernandina"), ]

# Model regresji na zaktualizowanych danych (gala_1)
gg2 <- lm(sqrt(Species) ~ Area + Elevation + Scruz + Nearest + Adjacent, data = gala_1)
summary(gg2)
plot(gg2)






###################################################################
######### dane savings ############################################
###################################################################

# Przykład 2: Analiza oszczędności
data(savings)
head(savings)

# Model regresji dla oszczędności
g <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = savings)
summary(g)

# Wykres diagnostyczny dla modelu g
plot(g)


############ korelacja predyktorów #########

library(car)
vif(g) #variance inflation factor

########car################################################
############# dane mtcars #################################
###########################################################

# Załaduj zbiór danych
data(mtcars)
summary(mtcars)

# Tworzenie wykresów par wybranych zmiennych
pairs(mtcars[, c("mpg", "wt", "hp", "drat")])

# Krok 1: Dopasowanie początkowego modelu
model_1 <- lm(mpg ~ wt + hp + drat, data = mtcars)
summary(model_1)

# Krok 2: Wykresy diagnostyczne dla początkowego modelu
par(mfrow = c(2, 2))
plot(model_1)

# Krok 3: Dodanie kwadratu hp do obsługi nieliniowości
model_2 <- lm(mpg ~ wt + hp + I(hp^2) + drat, data = mtcars)
summary(model_2)

# Wykresy diagnostyczne dla modelu z kroku 3
plot(model_2)

# Krok 4: Transformacja logarytmiczna 
log_model <- lm(log(mpg) ~ wt + hp + drat, data = mtcars)
summary(log_model)

# Wykresy diagnostyczne dla modelu z transformacją logarytmiczną
par(mfrow = c(2, 2))
plot(log_model)

# Wykluczenie samochodu "Chrysler Imperial" z danych
mtcars_filtered <- mtcars[rownames(mtcars) != "Chrysler Imperial", ]

# Dopasowanie modelu logarytmicznego bez "Chrysler Imperial"
log_model <- lm(log(mpg) ~ wt + hp + drat, data = mtcars_filtered)

# Wyświetlenie podsumowania modelu
summary(log_model)

# Wykresy diagnostyczne dla modelu bez "Chrysler Imperial"
par(mfrow = c(2, 2))
plot(log_model)

# Krok 5: Porównanie modeli
AIC(model_1, model_2, log_model) #nie zadziała bo różne zmienne


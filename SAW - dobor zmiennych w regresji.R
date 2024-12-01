#####################
## Przykład 1 #####

# Generujemy dane i definiujemy model powstawania y
set.seed(212)

# Tworzenie zmiennych losowych
x1 <- rnorm(50, 0, 1)
x2 <- rnorm(50, 0, 1)
x3 <- rnorm(50, 0, 1)
x4 <- rnorm(50, 0, 1)
x5 <- rnorm(50, 0, 1)
# Definiujemy model generujący y (Uwaga: w modelu nie ma zmiennej x4 oraz x5)
y <- 3 + 2 * x1 - 1.2 * x2 + 0.4 * x3 + rnorm(50, 0, 1)

# Estymacja parametrów modelu liniowego
fit <- lm(y ~ x1 + x2 + x3 + x4 + x5)
summary(fit)

# Wykres diagnostyczny dla modelu liniowego
plot(fit)

##################################################
# Regresja krokowa (Stepwise Regression) - 1. sposób

library(MASS)
step <- stepAIC(fit, direction = "both") # selekcja zmiennych na podstawie AIC
fit1 <- lm(y ~ x1 + x2 + x3)
summary(fit1)
plot(fit1)


############################################
# Wybór najlepszego podzbioru predyktorów - 2. sposób

library(leaps)
dane <- data.frame(y, x1, x2, x3, x4, x5)
leaps <- regsubsets(y ~ x1 + x2 + x3 + x4 + x5, data = dane, nbest = 2)
summary(leaps)

# Wykres dla metryk R² i skorygowanego R²
par(mfrow = c(1, 2))
plot(leaps, scale = "r2")
plot(leaps, scale = "adjr2")

fit1 <- lm(y ~ x1 + x2 + x3)

############## Lasso ######################## - 3. sposób

library(glmnet)

# Skalowanie zmiennych, aby poprawić wyniki lasso
x_scaled <- scale(as.matrix(dane[ , -1])) # skalowanie zmiennych niezależnych
y_scaled <- scale(dane$y, center = TRUE, scale = FALSE) # centrowanie y

# Estymacja modelu lasso
fit <- glmnet(x_scaled, y_scaled)
plot(fit, label = TRUE)

############ Walidacja krzyżowa dla lasso
cvfit <- cv.glmnet(x_scaled, y_scaled)
plot(cvfit)

# Parametry lambda
cvfit$lambda.min
cvfit$lambda.1se


# Współczynniki dla wybranych wartości lambda
coef(cvfit, s = "lambda.min")
coef(cvfit, s = "lambda.1se")

########################################################
########### savings ####################################

library(faraway)
data(savings)

cor(savings)
m1 = lm(sr ~ ., data = savings)
summary(m1)

# Wybór najlepszego podzbioru predyktorów
best_sub <- regsubsets(sr ~ ., data = savings, nbest = 1)
plot(best_sub, scale = "adjr2")

# Model pełny z regresją krokową
fit <- lm(sr ~ ., data = savings)
summary(fit)
stepAIC(fit)

# Skalowanie zmiennych dla lasso
x_savings <- scale(as.matrix(savings[ , -1]))
y_savings <- scale(savings$sr, center = TRUE, scale = FALSE)

# Model lasso na danych savings - lasso jest do wybrania modelu dobrego do prognozowania
fit <- glmnet(x_savings, y_savings)
plot(fit, label = TRUE)

cvfit <- cv.glmnet(x_savings, y_savings)
plot(cvfit)

# Parametry lambda
cvfit$lambda.min
cvfit$lambda.1se

# Współczynniki dla wybranych wartości lambda
coef(cvfit, s = "lambda.min")
coef(cvfit, s = "lambda.1se")

###########################################################
# Gdy liczba predyktorów jest większa niż liczba obserwacji

set.seed(1324)
X <- scale(matrix(rnorm(25 * 30, 6, 2), 25, 30)) # skalowanie X
dim(X)
beta <- rnorm(31, 10, 1)
beta


#generujemy wektor y używając tylko trzech x, i trzech bet
y <- 7 + X[, c(1, 4, 10)] %*% beta[c(1, 4, 10)] + rnorm(25, 0.2)

#estymacja MNK nie jest możliwa

model_mnk <- lm(y~X)
summary(model_mnk)

# Regresja lasso
fit <- glmnet(X, y)
plot(fit, label = TRUE)

cvfit <- cv.glmnet(X, y)
plot(cvfit)

# Parametry lambda
cvfit$lambda.min
cvfit$lambda.1se

coef(cvfit, s = "lambda.min")
coef(cvfit, s = "lambda.1se")

#prawdziwe parametry
beta[c(1,4,10)]


######################################################################################
# Analiza wpływu zmiennych na śmiertelność noworodków w kantonach Szwajcarii w 1888 roku
########################################################################################
data(swiss)
round(cor(swiss), 2)

# a) podać optymalny zestaw zmiennych za pomocą funkcji regsubsets
# b) podać optymalny zestaw zmiennychza pomocą funkcji stepAIC
# c) podać optymalny zestaw zmiennych za pomocą lasso


# Model liniowy i regresja krokowa
m1 <- lm(Infant.Mortality ~ ., data = swiss)

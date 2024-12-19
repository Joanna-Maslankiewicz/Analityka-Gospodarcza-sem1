# ================================================
# Ładowanie bibliotek
# ================================================
library(MASS)  # LDA, QDA
library(class) # KNN
install.packages("klaR")
library(klaR)  # Wizualizacje dla LDA/QDA
library(tidyverse) # Obsługa danych
set.seed(1001)

# ================================================
# 1. Klasyfikacja danych "earthquake"
# ================================================

# Wczytanie danych
earthquake <- read.table('earthquake.txt', header = TRUE)

# Wizualizacja danych
kolor <- c(rep('black', 20), rep('red', 9))  # Kolory dla klas
symbole <- c(rep('Q', 20), rep('X', 9))      # Symbole dla klas

plot(earthquake$body, earthquake$surface, type = 'n', xlab = 'body', ylab = 'surface')
text(earthquake$body, earthquake$surface, symbole, col = kolor)

# LDA
equake.lda <- lda(popn ~ ., data = earthquake, prior = c(0.5, 0.5)) #zmienna objaśniana, dane, p-stwo a priori
print(equake.lda)
plot(equake.lda)


# Ocena klasyfikacji na zbiorze uczącym
equake.pred <- predict(equake.lda, newdata = earthquake) #predykcja
table(earthquake$popn, equake.pred$class)

# Predykcja dla nowych przypadków
# Zaobserowano pomiar: body = 5.03, surface = 4.05
nowe.przypadki <- data.frame(matrix(c(5.03, 4.05), ncol = 2))
colnames(nowe.przypadki) <- c('body', 'surface')
predict(equake.lda, newdata = nowe.przypadki)

# KNN - K najbliższych sąsiadów
train <- earthquake[, 2:3] #zbiór uczący
test <- matrix(c(5.03, 4.05, 5.66, 4.2), 2, 2, byrow = TRUE) #zbiór testowy
cl <- earthquake[, 1]

knn_result <- knn(train, test, cl, k = 3, prob = TRUE) #czy w najbliższym otoczeniu przewagę mają trzęsienia ziemi czy eksplozje jądrowe
attributes(knn_result)

# Wizualizacja decyzji KNN
xp <- seq(4.5, 6.5, length = 100)
yp <- seq(3.5, 6.5, length = 100)
test_grid <- expand.grid(body = xp, surface = yp)
test_knn <- knn(train, test_grid, cl, k = 3, prob = FALSE)

plot(earthquake$body, earthquake$surface, type = "n", xlab = "body", ylab = "surface")
text(earthquake$body, earthquake$surface, symbole, cex = 0.8)
contour(xp, yp, matrix(as.numeric(test_knn == "equake"), length(xp)), levels = 0.5, add = TRUE)
points(matrix(c(5.03, 4.05, 5.66, 4.2), 2, 2, byrow = TRUE), col = 3, cex = 1)

# ================================================
# 2. Klasyfikacja danych "Tibet"
# ================================================

# Wczytanie danych
Tibet <- source("http://www.york.ac.uk/depts/maths/data/everitt/chap7tibetskull.dat")$value
Tibet$Type <- as.factor(Tibet[, 6])

# LDA
disk_l <- lda(Type ~ Length + Breadth + Height + Fheight + Fbreadth, data = Tibet)
print(disk_l)

# Klasyfikacja na zbiorze uczącym
Tibet.pred <- predict(disk_l, newdata = Tibet[, -6])
table(Tibet$Type, Tibet.pred$class)
#popełniamy 6 błędów

# Zmiana rozkładu a priori
disk_l_prior <- lda(Type ~ Length + Breadth + Height + Fheight + Fbreadth, data = Tibet, prior = c(0.1, 0.9))
print(disk_l_prior)

Tibet.pred <- predict(disk_l_prior, newdata = Tibet[, -6])
table(Tibet$Type, Tibet.pred$class)

# QDA
dis_q <- qda(Tibet[, -6], Tibet$Type)
qda_pred <- predict(dis_q, Tibet[, -6])$class
table(qda_pred, Tibet[, 6]) #wiersz, kolumna
#ten model lepszy bo mniej błędów

# ================================================
# 3. Klasyfikacja danych "iris"
# ================================================

# Przygotowanie danych treningowych i testowych
set.seed(1234)
training_sample <- sample(c(TRUE, FALSE), nrow(iris), replace = TRUE, prob = c(0.6, 0.4)) #replace - ze zwracaniem
train <- iris[training_sample, ]
test <- iris[!training_sample, ]

# Wizualizacja rozkładów
par(mfrow = c(2, 2))
boxplot(Sepal.Length ~ Species, data = train)
boxplot(Sepal.Width ~ Species, data = train)
boxplot(Petal.Length ~ Species, data = train)
boxplot(Petal.Width ~ Species, data = train)
par(mfrow = c(1, 1))

# LDA
lda.iris <- lda(Species ~ ., data = train)
print(lda.iris)

# Wizualizacja wartości LD1, LD2
plot(lda.iris, col = as.integer(train$Species))
plot(lda.iris, dimen = 1, type = "b")
pairs(lda.iris, col = as.integer(train$Species))

# Wyniki klasyfikacji na zbiorze treningowym i testowym
lda.train <- predict(lda.iris)
train$lda <- lda.train$class
table(train$lda, train$Species) #prognoza, dane  rzeczywiste

lda.test <- predict(lda.iris, newdata = test)
test$lda <- lda.test$class
table(test$lda, test$Species)

# Zmiana rozkładu a priori
lda.iris_prior <- lda(Species ~ ., data = train[,-6], prior = c(0.2, 0.7, 0.1))
print(lda.iris_prior)

# Wizualizacja dla zmienionych priors
plot(lda.iris_prior, col = as.integer(train$Species))
pairs(lda.iris_prior, col = as.integer(train$Species))

# Wyniki klasyfikacji dla nowych priors
lda.train_prior <- predict(lda.iris_prior)
train$lda_prior <- lda.train_prior$class
table(train$lda_prior, train$Species)

lda.test_prior <- predict(lda.iris_prior, newdata = test)
test$lda_prior <- lda.test_prior$class
table(test$lda_prior, test$Species)


##############################################
#klasyfikator chetnie przyznaje kategorie versicolor 
#############################
# QDA - zastosowac klasyfikator qda dla tej samej formuly co poprzednio
# wykorzystac zbior uczacy
qda.iris <- qda(Species ~ ., data = train)
qda.iris #show results


# wykorzystac poprzednia formule 
# typ metody "qda"
partimat(Species ~., data=train, method="qda")



# predykcja na zbiorze uczacym,
qda.train <- predict(qda.iris)

train$qda <- qda.train$class

# macierz trafnosci na zbiorze uczacym:
# uzupelnic prawdziwe etykiety ze zbioru uczacego
table(train$qda,train$Species)


# predykcja na zbiorze tesowym
# wybrac zbior testowy
qda.test <- predict(qda.iris,---)
test$qda <- qda.test$class

# zapisac macierz trafnosci na zbiorze tesowym
table(test$qda,test$Species)


############### knn #################
library(class)


train <- iris[training_sample, ]
test <- iris[!training_sample, ]
train_knn = train[,-5]
test_knn  = test[,-5]
etykiety = train$Species #etykiety sa argmentem w funkcji knn

# zbudowac klasyfikator knn przyjmujac k= 3 (sasiadow)
knn_iris = knn(train_knn, test_knn, cl= etykiety, k = 3, prob=TRUE)
table(knn_iris,test$Species)

sum(knn_iris == test$Species)/dim(test)[1]


### czy trafnosc zalezy od liczby sasiadow?
trafnosc  = sapply(1:15, function(nf)
{
  knn_iris = knn(train_knn, test_knn, cl= etykiety, k = nf)
  traf = (sum(knn_iris == test$Species)/dim(test)[1])
  return(traf)
}
)
par(mfrow= c(1,1))
plot(1:15,trafnosc, type="l")

#ktora procedura okazala sie nalepiej prognozowac na zbiorze testowym? - są porównywalne

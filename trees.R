#######################################################################
### drzewa decyzyjne ##################################################
#######################################################################


require(ISLR)
install.packages("tree")
library("tree")
attach(Carseats)
?Carseats
hist(Sales)
dim(Carseats)
head(Carseats)
summary(Carseats)

#utworzymy zmienna binarna
High=factor(ifelse(Sales<=8,"No","Yes"))
table(High)
236/(236+164)

#dodamy zmienna High do ramki danych
Carseats = data.frame(Carseats[,1:11],High)

#budujemy drzewo
tree.carseats=tree(High~.-Sales,Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)

#drzewo mozna zobaczyc w formie niegraficznej
tree.carseats

### dzielimy zbior na czesc uczaca i testowa
set.seed(2)
train=sample(1:nrow(Carseats), 200)
Carseats.test=Carseats[-train,]
High.test=High[-train]

### hodujemy drzewo na probce uczacej
tree.carseats=tree(High~.-Sales,Carseats,subset=train)
plot(tree.carseats)
text(tree.carseats,pretty=1)

tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)

#procent trafnych klasyfikacji
sum(diag(table(tree.pred,High.test)))/sum(table(tree.pred,High.test))

#walidacja krzyzowa (cross-validation)
set.seed(1001)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
names(cv.carseats)

#wykres wskazujacy jakosc klasyfikacji w zaleznosci od wielkosci drzewa
par(mfrow=c(1,1))
plot(cv.carseats$size,cv.carseats$dev,type="b") 


# opcja best wskazuje na liczbe lisci drzewa, tutaj = 8
prune.carseats=prune.tree(tree.carseats,best=8)
plot(prune.carseats) #mamy braki danych...
text(prune.carseats,pretty=0)

#sprawdzimy jakosc klasyfikacji
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
sum(diag(table(tree.pred,High.test)))/sum(table(tree.pred,High.test))


# czy wieksze drzewo lepiej klasyfikuje?
prune.carseats=prune.misclass(tree.carseats,best=14)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
sum(diag(table(tree.pred,High.test)))/sum(table(tree.pred,High.test))




########################################################################
######### drzewa regresyjne ############################################
########################################################################

library(MASS)
set.seed(1)
?Boston
dim(Boston)

#bedziemy prognozowac mediane wartosci domow medv

#tworzymy probe uczaca
train = sample(1:nrow(Boston), nrow(Boston)/2)

tree.boston=tree(medv~.,Boston,subset=train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston,pretty=0) #liczby na samym dole drzewa to mediany

#jaka wielkosc drzewa jest optymalna?
set.seed(12345)
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')

prune.boston=prune.tree(tree.boston,best=6)
plot(prune.boston)
text(prune.boston,pretty=0)

######### obliczymy bledy prognoz ##############
yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"] #medv = mediana wartości
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2) #blad sredniokwadratowy

### Random Forests
# lasy losowe; 
install.packages("randomForest")
library("randomForest")
set.seed(1)

#jedyny parametr w przypadku lasow to mtry - liczba losowanych predyktorow
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=5,importance=TRUE) #mtry=5 = 5 predyktorów; hiperparametr #małe drzewa ale dużo
bag.boston
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2) #wyrazne zmniejszenie bledu predykcji

# czy wylosowanie kilkudzisiciu np.25 drzew zamiast 500 jest wystarczajace?
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25) #mało drzew ale rozrośnięte
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2) #bledy prognoz nie sa duzo wieksze

#czy zmniejszenie liczby losowanych predyktorow poprawi bledy?
#czy model z duza liczba predyktorow nbedzie lepszy?

set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=12,importance=TRUE)
yhat.rf = predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)
importance(rf.boston)
#^ rm - liczba pokoi; IncMSE - Increase MSE; IncMSE dla rm = 50.12 => dodatnie czyli lepiej

########################################################
# no to ile predyktorow wziac
# zmienna mtry odpowiada za liczbe predyktorow w modelach

train.err = double(13) #blad w probce uczacej dla obserwacji
test.err = double(13)
for (mtry in 1:13)
{
  fit = randomForest(medv~., data = Boston, subset = train, mtry = mtry, ntree = 300)
  train.err[mtry] = fit$mse[300]
  pred = predict(fit, Boston[-train,])
  test.err[mtry]=with(Boston[-train,], mean((medv-pred)^2))
  cat(mtry," ") #zwraca mtry - bedziemy widzieli jak szybko dziala algorytm
}

## zrobimy wykresy
matplot(1:mtry, cbind(test.err, train.err), pch =19 , col = c("red", "blue"), type="b",
        ylab = "MSE") #niebieski to uczący, czerwony to testowy

################################################################################
# Bagging - losowanie ze zwracaniem ze zbioru uczącego (?) a następnie uśrednianie

library(ipred)
library(caret)
set.seed(111)
bag.boston=bagging(medv~.,data=Boston,subset=train, nbagg = 150, coob = TRUE) #nbagg - liczba losowań bootstrapowych
bag.boston   
#Out-of-bag estimate of root mean squared error: 3.5262 ==> wiersze których nie ma bo niektóre elementy pojawiły się kilkukrotnie

#trafnosc na probie testowej
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
mean((yhat.bag-Boston[-train,]$medv)^2)

################################################################################
# Boosting - wzmacnianie

install.packages("gbm")
library(gbm)
set.seed(1)
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=500,interaction.depth=4)
summary(boost.boston) #ważność zmiennych; rm i lstat na pewno wpływają ale nie wiemy w jaki sposób

par(mfrow=c(1,2))
plot(boost.boston,i="rm") # w jaki sposob rm wplywa na zmienna odpowiedzi medv; patrzymy na generalny kierunek. 7 pokoi różnicuje w drastyczny sposób cenę
plot(boost.boston,i="lstat")# w jaki sposob lstat wplywa na zmienna odpowiedzi medv
plot(boost.boston,i="crim") #ujemna zależność

#wartosci prognozowane, proba testowa
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=500)
mean((yhat.boost-boston.test)^2) #błędy średniokwadratowe. Ten model okazał się najlepiej prognozować

#################################################################################
#### svm - support vector machine

library(e1071)

summary(Boston)

svm_model <-svm(medv~., data = Boston[train,], kernel= "radial")
yhat.svm <- predict(svm_model, newdata = Boston[-train,])
mean((yhat.svm-boston.test)^2)

lm_model <-lm(medv~., data = Boston[train,])
yhat.lm <- predict(lm_model, newdata = Boston[-train,])
mean((yhat.lm-boston.test)^2) #lepsza predykcja liniowego modelu niż w drzewie, ale gorsza niż w lasach losowych i zwykłych drzewach
#model liniowy działa jak słaby las losowy (jeśli chodzi o predykcję)

################################################
## Klasyfikacja - dane iris

data(iris)

set.seed(212)
train = sample(1:nrow(iris), nrow(iris)/2)
train.iris = iris[train,]

# lasy losowe
# zbudowac klasfikator lasow losowych na zbiorze danych uczacych train.iris
# uuzupelnic formule 
set.seed(71)
iris.rf <- randomForest(---, data=---, importance=TRUE,
                        proximity=TRUE)
print(iris.rf)

# prognozoac etykiete na zbiorez testowym iris[-train,]:
# uzupelnic: model i zbior testowy 
pred.rf <- predict(---, ---)

summary(iris.pred)

### ocena klasyfikacji
table(iris[-train,5], pred.rf)

## Waznosc predyktorow
round(importance(iris.rf), 2)


# svm 

#uzueplnic model svm przyjmujac kernel = "linear"


svm.iris <- svm(Species ~ ., data = train.iris, kernel= ---)

# zaprognozowac etykiety w zbiorze testowym
# uzupelnic: model i zbior testowy 
pred.smv <- predict(---, ---)


### ocena klasyfikacji
table(iris[-train,5], pred.svm)



## boostig

set.seed(1)

# wybrac typ rozkladu "multinomial"
boost.iris=gbm(Species~.,data=iris[-train,],distribution=---,n.trees=100,interaction.depth=3)

# dokonac predykcji na zbiorze testowym iris[-train,]
pred.gbm <- predict(boost.iris, newdata = ---, type = "response")
#otrzymujemy prawd przynaleznosci do klasy
pred.gbm


# zmieniamy prawd na etykiety
gbm.classes <- colnames(pred.gbm)[apply(pred.gbm, 1, which.max)]

### ocena klasyfikacji
table(iris[-train,5], gbm.classes)

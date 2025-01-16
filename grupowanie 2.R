# biblioteki:
#install.packages("fpc")
#install.packages("mclust")
#install.packages("dbscan") 
#install.packages("cluster") 

#---------------------------------------
########################################
### model based clustering #############

library(mclust)
library(factoextra) #wizualizacja

#### Old Faithful Geyser Data
head(faithful)

mc <- Mclust(faithful) #przeprowadzamy mbc
summary(mc)
plot(mc) #wyniki grupowania
#uncertainty - im większa kropka tym większa niepewność
#density - poziomice gęstości

mc$modelName                # Optymalny model --> "EEE"
mc$G                        # Optymalna liczba grup --> 3
head(mc$z, 30)              # Prawdopodobieństwo przynależnści do grup 
head(mc$classification, 30) # Klasyfikacja obiektów do grup

#wizualizacja 
library(factoextra)
# kryterium BIC wg modeli i liczby grup
fviz_mclust(mc, "BIC", palette = "jco")
# Klasyfikacja: 
fviz_mclust(mc, "classification", geom = "point", 
            pointsize = 1.5, palette = "jco")
# Niepewnosc grupowania - im wiekszy "punkt" tym bardziej niepewna klasyfikacja
fviz_mclust(mc, "uncertainty", palette = "jco")

#####################################################
#### dane irys ######################################

data(iris)
head(iris)
dim(iris)
iris[,-5] #usuwamy etykiety "species", bo sprawdzamy czy dobrze zaetykietuje

mc <- Mclust(iris[,-5]) 
summary(mc) #model VEV --> elipsy, różne zmienności, w różnych kierunkach; dwie grupy, a chcemy trzy --> można to narzucić
plot(mc) #wyniki grupowania


mc$modelName                # Optymalny model ==> "VEV"
mc$G                        # Optymalna liczba grup => 2
head(mc$z, 30)              # Prawd. przynależności do grup 
head(mc$classification, 30) # Klasyfikacja obiektów do grup


library(factoextra)
# BIC wg modeli i liczby grup
fviz_mclust(mc, "BIC", palette = "jco")
# Klasyfikacja: 
fviz_mclust(mc, "classification", geom = "point", 
            pointsize = 1.5, palette = "jco")
# Niepewnosc
fviz_mclust(mc, "uncertainty", palette = "jco")


###################################################
### jesli wiemy jaka jest liczba grup #############
### mozemy ta informacje narzuci      #############

mod2 <- Mclust(iris[,1:4], G = 3)
summary(mod2)


# Klasyfikacja: 
fviz_mclust(mod2, "classification", geom = "point", 
            pointsize = 1.5, palette = "jco")
# Niepewnosc 
fviz_mclust(mod2, "uncertainty", palette = "jco")


##### czy klasyfikacja z mbc pokrywa sie z etykietami?##

table(iris[,5],mod2$classification)

#------------------------------------------------------
#######################################################
########density based clustering ######################

#bardziej ciekawostka niż praktyczne zastosowanie

library(dbscan)

# kod pokazuje dzialanie algorytmu dbc dla sztucznego zbioru danych

# ladujemy dane
data("multishapes", package = "factoextra")
df <- multishapes[, 1:2]
plot(df) #mamy różne kształty --> stosujemy algorytm iteracyjny

# stosujemy algorytm dbc z pakietu fpc 
library("fpc")
set.seed(123)
#trzeba okreslić dwa parametry modelu: eps (promień) i MinPts (minimalna liczba punktów leżąca wewnątrz koła o promieniu eps, aby utworzył się nowy cluster)

db <- fpc::dbscan(df, eps = 0.4, MinPts = 5) 
# ilustrujmey wyniki grupowania 
library("factoextra")
fviz_cluster(db, data = df, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())

### zmienimy wartosc eps


set.seed(123)
db <- fpc::dbscan(df, eps = 0.12, MinPts = 5) 
# ilustrujmey wyniki grupowania 
fviz_cluster(db, data = df, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())



#### wyniki zaleza od eps
### jak wybrac eps?

#rysujemy k-distance plot, ktory pokazuje przecietna odleglosc
# kazdego punktu do jego k- najblizszych sasiadow
# punkty na wykresie sa posortowane ze wzgl. na odleglosc
# mamy znalesc miejsce, gdzie wykres zaczyna stromo rosnac
# Wartosc k bedzie odpowiadac MinPts
dbscan::kNNdistplot(df, k = 5) #5 najblizyszych sasiadow
abline(h = c(0.1,0.15,0.2), lty = 2, col = 1:3)

#---------------------------------------------------
#################### grupowanie rozmyte ############

library(cluster) #zawiera funkcja fanny
head(USArrests)
df <- scale(USArrests) # standaryzacja zmiennych
res.fanny <- fanny(df, 2) # stosujemy funkcje fanny wskazujac dwa skupiska
res.fanny
plot(res.fanny)


head(res.fanny$membership, 5) # Stopien przynaleznosci do skupisk
head(res.fanny$clustering) # Przynaleznosc do grup
res.fanny$coeff # Wspolczynnik podzialu Dunn

### wizualizacja w wersji kolorowej
library(factoextra)
fviz_cluster(res.fanny, ellipse.type = "norm", repel = TRUE,
             palette = "jco", ggtheme = theme_minimal(),
             legend = "right")



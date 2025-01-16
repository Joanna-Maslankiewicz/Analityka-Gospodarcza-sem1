#Clustering czesc I
#metody hierarchiczne, metody podzialu
#ocena jakosci grupowania; miary wewnetrzne; miary zewnetrzne

#####################DYSTANS#############################

data(USArrests) 
summary(USArrests) #rozna zmiennosc - koniecznosc doprowadzenia do porownywalnosci


df.scaled <- scale(USArrests) # standaryzujemy zmienne
?dist
head(df.scaled)
#dystans euklidesa
dist.eucl <- dist(df.scaled, method = "euclidean")
# Podglad 3 pierwszych kolumn i wierszy macierzy
round(as.matrix(dist.eucl)[1:3, 1:3], 1)
#dystans manhattan
dist.manh <- dist(df.scaled, method = "manhattan")
# Podgl?d 3 pierwszych kolumn i wierszy macierzy
round(as.matrix(dist.manh)[1:3, 1:3], 1)

class(dist.eucl)
dist.eucl.mat <- as.matrix(dist.eucl)

heatmap(dist.eucl.mat, col = heat.colors(256),
        main = "Macierz Dystansu")

##### w bibliotece factoextra
library(factoextra)
fviz_dist(dist.eucl)

#### a co jesli mamy zmienne mierzone na roznych skalach?

library(cluster) #bibliteka do grupowania
# dane
data(flower)
dim(flower)
head(flower, 3)
# struktura danych
str(flower)
#dane maja zmienne na roznych skalach (nominalna, porzadkowa,ilorazowa)
dd <- daisy(flower) #funkcja daisy zwraca odleglosc dla wektorow zmiennych dla roznych skal
round(as.matrix(dd)[1:3, 1:3], 2)

###### wizualizcja macierzy odleglosci
library(factoextra)
fviz_dist(dd)




###############################################################################
############# METODY GRUPOWANIA ###############################################
############# grupowanie hierarchiczne ###########
############# k-srednich, pam ###################
#generujemy dwa skupiska
library(mvtnorm)
set.seed(1222)
x1<-rmvnorm(n=10, mean=c(0,1), sigma = diag(c(1,1)))
x2<-rmvnorm(n=15, mean=c(4,5), sigma = diag(c(1,1)))

dane=data.frame(rbind(x1,x2))
clust = c(rep(1,10),rep(2,15))
dane$clust = clust
head(dane)

plot(dane$X1, dane$X2, col = dane$clust, xlab="x1", ylab = "x2",
     pch = dane$clust)

#grupowanie hierarchiczne
hc<-hclust(dist(dane[,1:2]),method='ward.D2')
plot(hc)

#przycinanie drzewa


hc_cl = cutree(hc,2)
table(dane$clust, hc_cl)

#to samo, ale zblizamy skupiska

set.seed(1222)
x1<-rmvnorm(n=10, mean=c(0,1), sigma = diag(c(1,1)))
x2<-rmvnorm(n=15, mean=c(1,2), sigma = diag(c(1,1)))

dane=data.frame(rbind(x1,x2))
clust = c(rep(1,10),rep(2,15))
dane$clust = clust
head(dane)

plot(dane$X1, dane$X2, col = dane$clust, xlab="x1", ylab = "x2",
     pch = dane$clust)

hc2<-hclust(dist(dane[,1:2]),method='ward.D2')
plot(hc2)

hc_cl2 = cutree(hc2,2)
table(dane$clust, hc_cl2)
## porownanie dendrogramow
par(mfrow=c(1,2))
plot(hc)
plot(hc2)

#gdzie sa wieksze odleglosci miedzy grupami?

#################################################
#generujemy 4 skupienia z rozkladu jednostajnego
set.seed(123)
x1<-matrix(c(runif(20),runif(20)),ncol=2)
x2<-matrix(c(runif(20)+1,runif(20)+1),ncol=2)
x3<-matrix(c(runif(20)+2,runif(20)+2),ncol=2)
x4<-matrix(c(runif(20)+3,runif(20)+3),ncol=2)

dane<-rbind(x1,x2,x3,x4)
par(mfrow=c(1,1))
plot(dane)

# grupowanie metodami hierarchicznymi

hc1<-hclust(dist(dane),method='single') #method wskazuje na sposob laczenia grup

plot(hc1,xlab=,ylab=,las=TRUE)
print(table(skupienia1<-cutree(hc1,k=4))) #cutree daje podzial na dana licze grup

hc2<-hclust(dist(dane),method='complete')
print(table(skupienia2<-cutree(hc2,k=4)))
hc3<-hclust(dist(dane),method='centroid')
print(table(skupienia3<-cutree(hc3,k=4)))
hc4<-hclust(dist(dane),method='ward.D2')
print(table(skupienia4<-cutree(hc4,k=4)))


par(mfrow=c(2,2)) #por?wnanie r??nych metod grupowania
plot(dane,pch=(20+skupienia1),col=skupienia1)
plot(hc1)
plot(dane,pch=(20+skupienia2),col=skupienia2)
plot(hc2)
plot(dane,pch=(20+skupienia3),col=skupienia3)
plot(hc3)
plot(dane,pch=(20+skupienia4),col=skupienia4)
plot(hc4)


#########################################
## dendrogramy - inna forma graficzna
data(swiss)
head(swiss)
summary(swiss)
swiss_s<-scale(swiss)



hc<-hclust(dist(swiss_s),method='ward.D2')
plot(hc)
hcd <- as.dendrogram(hc)
par(mfrow=c(1,1))
plot(hcd, type = "triangle", ylab = "Height")
plot(hcd, xlim = c(1, 20), ylim = c(1,8))#fragment 
plot(hcd,  xlab = "Height", horiz = TRUE)#w poziomie
#jeszcze inaczej
library(ape)
plot(as.phylo(hc), cex = 0.6, label.offset = 0.5)
plot(as.phylo(hc), type = "cladogram", cex = 0.6, 
     label.offset = 0.5)
plot(as.phylo(hc), type = "unrooted", cex = 0.6,
     no.margin = TRUE)
plot(as.phylo(hc), type = "fan")


# obcinamy dendrogram do trzech grup
colors = c("red", "blue", "green")
clus4 = cutree(hc, 3)
plot(as.phylo(hc), type = "unrooted", tip.color = colors[clus4],
     label.offset = 1, cex = 0.7)

############# profile grup - czym charakteryzuja sie grupy? #####


# Przypisanie obserwacji do grup
# Przykład: podział na 3 grupy
clusters <- cutree(hc, k = 3)

# Dodanie grup do danych
swiss$cluster <- as.factor(clusters)

# Obliczenie średnich dla każdej grupy
group_profiles <- aggregate(. ~ cluster, data = swiss, FUN = median)

print(group_profiles)

par(mfrow=c(2,3))
boxplot(Fertility~cluster, swiss)
boxplot(Agriculture~cluster, swiss)
boxplot(Examination~cluster, swiss)
boxplot(Education~cluster, swiss)
boxplot(Catholic~cluster, swiss)
boxplot(Catholic~cluster, swiss)


## jakie regiony sa kolejnych grupach?
rownames(swiss)
rownames(swiss[swiss$cluster==1,])
rownames(swiss[swiss$cluster==2,])
rownames(swiss[swiss$cluster==3,])

# jakie pytania mozna zadac?


######################################
#####################################
# grupowanie metoda k-means --> centroidy



grupy_km <- kmeans(swiss_s, 3,nstart=200) # podzial na 3 skupiska; nstart --> liczba losowań
grupy_km
grupy_km$centers #centra skupisk; profile grup
grupy_km$withinss #zmiennosc wewnetrz grupowa

#################################################
## ile grup? Zbadamy zmiany sumy zmiennosc wewnetrz-grupowej

swiss_s <- scale(swiss)

# Funkcja do obliczania WCSS
wcss <- sapply(1:10, function(k) {
  kmeans(swiss_s, centers = k, nstart = 25)$tot.withinss
})

# Wykres osypiska
plot(1:10, wcss, type = "b", pch = 19, frame = FALSE,
     xlab = "Liczba grup (k)", ylab = "WCSS",
     main = "Wykres osypiska")

####################################################
### zalozmy, ze mamy 3 grup ########################



# Grupowanie k-means
kmeans_result <- kmeans(swiss_s, centers = 3, nstart = 25)

# Dodanie grup do danych
swiss$cluster_kmeans <- as.factor(kmeans_result$cluster)

# Wyświetlenie wyników
print(swiss[, c("cluster_kmeans")], row.names = TRUE)


# PCA dla redukcji wymiarów
pca_result <- prcomp(swiss_s)
summary(pca_result)
pca_result$rotation
pca_result$x
#PC_ --> składowe główne

# Przygotowanie danych do wizualizacji
plot_data <- data.frame(PC1 = pca_result$x[, 1], 
                        PC2 = pca_result$x[, 2], 
                        cluster = swiss$cluster_kmeans)

# Wykres
library(ggplot2)
ggplot(plot_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "Klasteryzacja k-means (wizualizacja PCA)",
       x = "Pierwsza składowa główna (PC1)",
       y = "Druga składowa główna (PC2)") +
  theme_minimal()


#######################################################
########## ile grup -  przecietna sylwetka ############
library(cluster)
dissE<-daisy(swiss_s) #wyznaczenie macierzy odlegsci,

sk <- silhouette(kmeans_result$cluster, dissE)  #wywolanie silhouette; cluster --> do którego został przypisany; neighbor --> do którego pasuje; sil_width --> chcemy dodatnią
plot(sk)


###########################################################
################ metoda k-medoidow ########################

#grupowanie metoda k-medoidow
grup2<-pam(swiss_s, 3) #3 grupy
grup2$medoids #medoidy - profile grup

grup2$id.med  #numery medoidow
swiss[grup2$id.med, ]
grup2$clustering #numery grup dla kolejnych obiektow
table(grup2$clustering)

## ile grup sylwetka
si <- silhouette(grup2)
summary(si)
plot(si)


#####################################################
# wizualizacja facotextra - wybor liczby grup

# Wykres osypiska
fviz_nbclust(swiss_s, kmeans, method = "wss") +
  labs(title = "Wykres osypiska (metoda WSS)")


# Metoda sylwetki
fviz_nbclust(swiss_s, kmeans, method = "silhouette") +
  labs(title = "Optymalna liczba grup (metoda sylwetki)") #dzielimy na tyle grup gdzie jest kreska



# Wizualizacja grup
set.seed(123)
kmeans_result <- kmeans(swiss_s, centers = 3, nstart = 25)

# Wizualizacja grup w ukladzie skladowych glownych

fviz_cluster(kmeans_result, data = swiss_s,
             geom = "point",
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal(),
             main = "Wizualizacja grup k-means")


# to samo dla pam
pam_results <- pam(swiss_s, 3)

fviz_cluster(pam_results, data = swiss_s,
             geom = "point",
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal(),
             main = "Wizualizacja grup pam")

#wyglada na to, ze mamy niemal te same grupy

################################################
#### sylwetka 

# Obliczanie współczynnika sylwetki
silhouette_kmeans <- silhouette(kmeans_result$cluster, dist(swiss_s))

# Wizualizacja sylwetki
fviz_silhouette(silhouette_kmeans) +
  labs(title = "Wykres sylwetki dla klasteryzacji k-means") 

###### pam 
silhouette_pam <- silhouette(pam_results)
fviz_silhouette(silhouette_pam) +
  labs(title = "Wykres sylwetki dla klasteryzacji k-means") 


# dendrogram w facotextra

# metoda Warda 
hc <- hclust(dist(swiss_s), method = "ward.D2")


# Wizualizacja dendrogramu
fviz_dend(hc, main = "Dendrogram (Ward.D2)")

# dodajemy opcje

fviz_dend(hc, k = 3, # Liczba grup
          rect = TRUE, # Dodaj prostokąty wokół grup
          show_labels = TRUE, # Pokaż etykiety
          rect_fill = TRUE, # Wypełnij prostokąty kolorami
          rect_border = "jco", # Kolory krawędzi
          main = "Dendrogram z podziałem na 3 grupy")

######################################################################
########### zgodnosc z typologia - indeks randa ######################

install.packages("cluster")
library("cluster")
library(fpc)
# potrzebna biblioteka

# podajemy dwie typologie

hc_results <- cutree(hc,3)               # trzy grupy - metod Warda
km_results <- kmeans_result$cluster      # grupy - k means


clust_stats <- cluster.stats(d = dist(swiss_s),
                             hc_results , km_results  )
clust_stats #bardzo duzo statystyk!
# Corrected Rand index
clust_stats$corrected.rand


#################################################################
############## mtcars 
# przeprowdzic grupowanie metoda warda oraz najblizszego sasiedztwa (single)
# narysowac dendrogramy
# podzielic zbior na 4 grupy, zgodnie z wynikami grupowania
# ocenic zgodnosc grupowania
# Dane
data(mtcars)

# Skalowanie danych
mtcars_s <- scale(mtcars)

# Obliczanie macierzy odległości
dist_matrix <- dist(mtcars_s)

# Hierarchiczne grupowanie
# metoda Warda
hc_w <- hclust(dist_matrix, method = "ward.D2")
# metoda average
hc_a <- hclust(dist_matrix, method = "average")

# Wizualizacja dendrogramu zpodziałem na 3 grupy
fviz_dend(hc_w, k = 3, # Liczba grup
          rect = TRUE, # Dodanie prostokątów wokół grup
          rect_fill = TRUE, # Wypełnienie prostokątów kolorami
          main = "Dendrogram dla mtcars")


fviz_dend(hc_a, k = 3, # Liczba grup
          rect = TRUE, # Dodanie prostokątów wokół grup
          rect_fill = TRUE, # Wypełnienie prostokątów kolorami
          main = "Dendrogram dla mtcars")

#### obcinamy dendrogramy
hc_w_gr <- cutree(hc_w,3)
hc_a_gr <- cutree(hc_a,3)

#### porownujemy dwa grupowania
clust_stats <- cluster.stats(d = dist(mtcars_s),
                             hc_w_gr, hc_a_gr)
clust_stats$corrected.rand

###############################################################
### pogrupowac samochody metoda pam
### sprawdzic za pomoca sylwetki jaka jest optymalna liczba grup
### porownac profile grup
### przedstwic grupy w ukladzie PC


# Wykres sylwetki
fviz_nbclust(mtcars_s, pam, method = "silhouette") +
  labs(title = "Współczynnik sylwetki dla PAM")


# zalozmy, ze 4 grupy to optymalny wynik
# Grupowanie PAM
pam_result <- pam(mtcars_s, k = 4)
pam_result$medoids

# Wizualizacja grup
fviz_cluster(pam_result, geom = "point", ellipse.type = "convex",
             main = "Grupowanie PAM") +
  theme_minimal()


###############################################################
#### Przyklad - ostrzezenie ####################################
# Script 6 - ostrzezenie. Grupowanie zawsze prowadzi do jakiegos wyniku
# porownamy wyniki grupowania dla zbiru irys
# oraz dla danych wylosowanych z jednej populacji, ktorych wymiar jest taki jak w danych irys
# Dane irys
df <- iris[, -5]
summary(df)
# przypisanie losowych wartosci zmiennym
random_df <- apply(df, 2,
                   function(x){runif(length(x), min(x), (max(x)))})
random_df <- as.data.frame(random_df)
head(random_df)
# Standaryzacja obu zbiorow
df <- iris.scaled <- scale(df)
random_df <- scale(random_df)

#wizualizacja
library("factoextra")
#Dane na tle skladowych - oryginalny zbior
fviz_pca_ind(prcomp(df), title = "PCA - Iris data",
             habillage = iris$Species, palette = "jco",
             geom = "point", ggtheme = theme_classic(),
             legend = "bottom")
# Dane na tle skladowych - zbior losowych punktow
fviz_pca_ind(prcomp(random_df), title = "PCA - Random data",
             habillage = iris$Species, palette = "jco",
             geom = "point", ggtheme = theme_classic())



##################
# jakie sa wyniki grupowania obu zbiorow

set.seed(123)
# K-means na df
km.res1 <- kmeans(df, 3)
fviz_cluster(list(data = df, cluster = km.res1$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())

# K-means na losowym zbiorze
km.res2 <- kmeans(random_df, 3)
fviz_cluster(list(data = random_df, cluster = km.res2$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())

# Grupowanie hierarchiczne iris
fviz_dend(hclust(dist(df), method = "ward.D2"), k = 3, k_colors = "jco",
          as.ggplot = TRUE, show_labels = FALSE)


# Grupowanie hierarchiczne na zbiorze losowym
fviz_dend(hclust(dist(random_df), method = "ward.D2"), k = 3, k_colors = "jco",
          as.ggplot = TRUE, show_labels = FALSE)





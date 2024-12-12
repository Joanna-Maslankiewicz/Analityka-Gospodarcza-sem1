####################################################
## dane stock zawieraja stopy zwrotu (tygdniowe) 5 spolek
## trzy spolki chemiczne, dwie paliwowe
## przeprowadzic analize czynnikowa
## 
stocks <- read.csv("stock.csv", header=T)
stocks<-stocks[,-1]
names(stocks)=c("Allied","Dupont","Carbide", "Exxon","Texaco")
head(stocks)
#korelacja 
cor(stocks)
summary(stocks)
#wykresy: 
pairs(stocks,labels=c("Allied","Dupont","Carbide", "Exxon",
                      "Texaco"), panel=function(x,y){panel.smooth(x,y) 
                        abline(lsfit(x,y),lty=2) })

#wyznaczenie skladowych glownych
eigen(cor(stocks))
l1<-eigen(cor(stocks))$values[1] 
l2<-eigen(cor(stocks))$values[2]
e1<-eigen(cor(stocks))$vectors[,1] 
e2<-eigen(cor(stocks))$vectors[,2]
e2
sqrt(l1)*e1 #ladunki czynnikowe I czynnika
sqrt(l2)*e2 #ladunki czynnikowe II czynnika

#czesc wariancji wyjasniona przez czynniki wspolne
(sqrt(l1)*e1)^2 #I czynnik
(sqrt(l2)*e2)^2 #II czynnik

h<-(sqrt(l1)*e1)^2+(sqrt(l2)*e2)^2 # I i II czynnik ??cznie
h
#zmiennosc specyficzna dla poszczegolnych spolek
1-h #zmiennosc nie wyjasnione przez dwa pierwsze czynniki wsplne
#czesc wariancji objasniona przez 1 i 2 skladowa
l1/5
l2/5
##############################################
# ocena odwzorowania macierzy korelacji
# obliczymy roznice miedzy macierzami korelacji i modelem czynnikowym
# L*t(L)+psi ~ Corr?

L<-matrix(c(sqrt(l1)*e1,sqrt(l2)*e2),5,2)
L%*%t(L) #macierz ladunkow czynnikowych
psi<-cor(stocks)-L%*%t(L)
psi<-diag(diag(psi))  #zmiennosc specyficzna, niewyjasniona przez czynniki wspolne
psi
cor(stocks)-(L%*%t(L)+psi) #macierz reszt modelu. pokazuje ile korelacji nie zostalo wyjasnione przez model


######### czynniki wyznaczone za pomoca ML ##########################################################

stocks.fac <- factanal(stocks, rotation="varimax",scores='regression',factors=2,method='mle', scale=T)
## mozemy wskazac rotacje tutaj 'varimax', wyznaczyc wartosci czynnikow (scores...)
stocks.fac

## Pytania, ktore mozna zadac:
# Zmiennosc ktorej spolki jest najlepiej (najgorzej) objasniona przez model czynnikowy?
# Czy model czynnikowy dostatecznie objasnia macierz korealcji (kowariancji)?
# Jaka czesc calkowiej zmiennosci spolek objasnia model czynnikowy?
# Jaka jest interpretacja czynnikow?
# Ktore czynniki, otrzymane z PCA, czy FA, opisuja wiecej zmiennosci? Dlaczego?



### ocena odwzorowania macierzy korelacji
# obliczymy roznie miedzy macierza korelacji i modelem czynnikowym

Lambda <- stocks.fac$loadings
Psi <- diag(stocks.fac$uniquenesses)
S <- stocks.fac$correlation
Sigma <- Lambda %*% t(Lambda) + Psi

S - Sigma

### jaka jest interpretacja obu czynnikow?


### wartosci czynnikow
stocks.fac$scores #wartosci czynnikow

#
plot(stocks.fac$scores[,1], type = "l")
lines(stocks.fac$scores[,2], col = "red")
legend("topright",legend=c("Koniunktura Chemia","Koniunktura Paliwa"), col = c(1,2), lty=1 )
######################################################
## dane life opisuja oczekiwana dlugosc zycia kobiet i mezczyzn w wieku 25, 50, 75 oraz w momencie urodzenia
## Zbudujemy model czynnikowy


life=read.csv('life.csv',header=TRUE,sep=';' )
life
class(life)
row.names(life) = life[,1]

life_fa=life[,-c(1:2)]


model=factanal(scale(life_fa),factors=4,
               method='mle', rotation = "none") #standaryzacja danych

model

model_var=factanal(scale(life_fa),factors=4,
               method='mle', rotation = "varimax")

model_var
# interpretacje: sila zyciowa w momencie urodzenia 
#               sila zyciowa osob starszych ...

# psi (czynniki swoiste) się nie zmienia niezależnie od rotacji, rotując zmieniamy tylko ładunki czynnikowe. Takie samo p-value

head(life)
scores <- factanal(life[,-c(1,2)], factors = 3, method = "mle",scores = "regression")$scores
scores

#domyślnie jest rotacja varimax, więc jak nie chcemy to musimy zapisać

plot(scores[,1],scores[,2], type = "n", xlab="sila przy urodzeniu", ylab = "sila zyciowa starszych")
text(scores[,1],scores[,2], labels = row.names(life), cex=0.7)

#Quiz life

#####################
# rekordy krajowe w konkurencjach biegowych 
# zbudowa? model czynnikowy 

wrecord<-read.table('wrecord.txt',header=TRUE)
wrecord

scale(wrecord)
cor(wrecord)



wrecord.pc<-princomp(~.,cor=TRUE,data=wrecord)
print(summary(wrecord.pc))
wrecord.pc$loadings
wrecord.pc$scores[,1:3]

sort(wrecord.pc$scores[,2])
## jaka czsc zmiennosci poszczegolnych zmiennych jest objasniana przez model
##str(wrecord.pc)
## 
ll=wrecord.pc$sdev[1:3]
pll=sqrt(ll)
egg=wrecord.pc$loadings[,1:3]

L=cbind(pll[1]*egg[,1],pll[2]*egg[,2],pll[3]*egg[,3])
L%*%t(L) #macierz korelacji odtworzona przez uklad czynnikow
psi<-cor(wrecord)-L%*%t(L) 
psi

psi<-diag(diag(psi))  #zmiennosc specyficzna, nie wyjasniona przez czynniki wspolne
psi

ll[1]*egg[,1]
L<-matrix(c(sqrt(l1)*e1,sqrt(l2)*e2),5,2)
L%*%t(L) #macierz korelacji odtworzona przez uklad czynnikow
psi<-cor(stocks)-L%*%t(L) 
psi<-diag(diag(psi))  #zmiennosc specyficzna, nie wyjasniona przez czynniki wspolne
psi
##zmienne 5 i 6 sa najgorzej opisane w modelu czynnikowym

#jeden czynnik?
factanal(scale(wrecord),factors=1,rotation="varimax")
#Mo?e dwa czynniki?
factanal(scale(wrecord),factors=2,rotation="varimax")
# No to mo?e trzy czynniki?
factanal(scale(wrecord),factors=3, rotation="varimax")
# 3 czynniki powinny wystarczy? p-value =0.201
# jaka sensowna interpretacji zaproponowac?

scores <- factanal(scale(wrecord),factors=3, method = "mle",scores = "regression")$scores
scores

#####################################################
### macierz korelacji dla wynikow 220 chlopcow 
### z szesciu szkol, z przedmiotow: Francuski, Angielski,
## historia, arytmetyka, algebra, geometria

r = matrix(c(1,0.44,0.41,0.29,0.33,0.25,0,1,0.35,0.35,0.32,0.33,0,0,1,0.16,0.19,0.18,0,0,0,1,0.59,0.47,0,0,0,0,1,0.46,0,0,0,0,0,1), nrow=6)
rr = r+t(r) - diag(rep(1,6))

colnames(rr)=c("franc","ang","hist","artym", "algebra", "geom")
row.names(rr) = colnames(rr)
rr
### przeprowadzic analize czynnikowa dla dwoch czynnikow
### zaproponowac sensowna rotacje i podac interpretacje wynikow

fa_n = factanal(rr, covmat=rr, 2,rotation="none", n.obs = 220)
fa_n
fa_v = factanal(rr, covmat=rr, 2,rotation="varimax" )
fa_v

#Factor 1 = probably "zdolności ogólne"
#Factor 2 = probably "zdolności humanistyczne"

fa_pr = factanal(rr, covmat=rr, 2,rotation="promax" )
fa_pr

#drugi komponent to prawie na pewno uzdolnienia "humanistyczne"

#Uwaga. Jesli nie mamy danych, a jedynie macierz korelacji, wowczas nie mozemy oszacowac wartosci czynnikow.


est <- tcrossprod(fa_n$loadings) + diag(fa_n$uniquenesses)
est

ret <- round(rr - est, 3)
colnames(ret) <- rownames(ret) <- 
  abbreviate(rownames(ret), 3)

ret


###################################################
### dane student druguse
### macierz korelacji obliczona dla 1634 obserwacji
###################################################
d <-
  c(0.447,          
    0.422, 0.619,       
    0.435, 0.604, 0.583,        
    0.114, 0.068, 0.053, 0.115,        
    0.203, 0.146, 0.139, 0.258, 0.349,   
    0.091, 0.103, 0.110, 0.122, 0.209, 0.221,
    0.082, 0.063, 0.066, 0.097, 0.321, 0.355, 0.201,
    0.513, 0.445, 0.365, 0.482, 0.186, 0.315, 0.150, 0.154,
    0.304, 0.318, 0.240, 0.368, 0.303, 0.377, 0.163, 0.219, 0.534,
    0.245, 0.203, 0.183, 0.255, 0.272, 0.323, 0.310, 0.288, 0.301, 0.302,
    0.101, 0.088, 0.074, 0.139, 0.279, 0.367, 0.232, 0.320, 0.204, 0.368, 0.340,
    0.245, 0.199, 0.184, 0.293, 0.278, 0.545, 0.232, 0.314, 0.394, 0.467, 0.392, 0.511)

druguse <- diag(13) / 2
druguse[upper.tri(druguse)] <- d
druguse <- druguse + t(druguse)

rownames(druguse) <- colnames(druguse) <- c("cigarettes", "beer", "wine", "liquor", "cocaine",
                                            "tranquillizers", "drug store medication", "heroin",
                                            "marijuana", "hashish", "inhalants", "hallucinogenics", "amphetamine")



##### ile czynnikow 

sapply(1:6, function(nf)
  factanal(covmat = druguse, factors = nf,
           method = "mle", n.obs = 1634)$PVAL)

############################
#### model fa dla 6 czynnikow

(factanal(covmat = druguse, factors = 6,
          method = "mle", n.obs = 1634))

#### podać interpretację 4 pierwszych czynników wspólnych


###################################
#### macierz resztkowej kowariacji
#### sprawdzimy roznice dla liczby czynnikow rownej 4 oraz 6
fa <- factanal(covmat = druguse, factors = 4, 
               method = "mle", n.obs = 1634)
est <- tcrossprod(fa$loadings) + diag(fa$uniquenesses)
est
#### roznica miedzy macierza z proby i kowariancja z modelu czynnikowego
ret <- round(druguse - est, 3)
colnames(ret) <- rownames(ret) <- 
  abbreviate(rownames(ret), 3)

ret

#######################################


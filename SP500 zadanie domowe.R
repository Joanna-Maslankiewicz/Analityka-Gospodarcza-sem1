########## SP500 - zadanie #######################################

require(ISLR)
names(Smarket) #szereg czasowy, dane dzienne; lagX - zbiór predyktorów, opóźnione wartości
head(Smarket)
summary(Smarket)
?Smarket

pairs(Smarket, col=Smarket$Direction)
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
summary(glm.fit)

glm.probs = predict(glm.fit, type="response")
glm.probs[1:5]
glm.pred=ifelse(glm.probs>0.5,"Up","Down")
glm.pred[1:4]

attach(Smarket)
table(glm.pred,Direction)
mean(glm.pred==Direction)

#zbudujemy zbior uczacy
train=Year<2005
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, 
            data=Smarket, family=binomial, subset=train)
glm.probs = predict(glm.fit,newdata=Smarket[!train,], type="response")
glm.pred=ifelse(glm.probs>0.5,"Up","Down")
glm.pred[1:4]

Direction.2005 = Smarket$Direction[!train]
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)

#predykcja jest kiepska
#moze mamy przeuczony model?

#sprobujmy mniejszy model
glm.fit=glm(Direction~Lag1+Lag2, 
            data=Smarket, family=binomial, subset=train)
glm.probs = predict(glm.fit,newdata=Smarket[!train,], type="response")
glm.pred=ifelse(glm.probs>0.5,"Up","Down")
glm.pred[1:4]

Direction.2005 = Smarket$Direction[!train]
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)

###########################################################
# zadanie - porownac co najmniej 4 rozne klasyfikatory #### #drzewa, lasy losowe, boosting, bagging, ...
# ocenic trafnosc prognoz na zbiorze testowym          ####
###########################################################
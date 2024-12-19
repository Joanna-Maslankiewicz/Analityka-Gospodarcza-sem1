library(MASS)
attrition <- read.csv('attrition.csv', sep=",")
names(attrition)
dim(attrition)

#zmienna "attrition" to wypalenie zawodowe

################################################################################
## zanim oszacujemy parametry modelu, musimy przygotowac dane ##################
#
# w modelu logitowym zmienna odpowiedzi ma byc numeryczna {0,1}
table(attrition$Attrition)
# zamieniamy "Yes" na 1, "No" na 0

attrition$Attrition[attrition$Attrition=="Yes"]=1
attrition$Attrition[attrition$Attrition=="No"]=0
attrition$Attrition=as.numeric(attrition$Attrition) #liczby

#czesc zmiennych jest na skali nominalnej - Business Travel, Department, Education, Education Field, Gender, Job role, Marital Status, Over Time
head(attrition)
attrition[,c(3,5,7,8,12,16,18,23)] = lapply(attrition[,c(3,5,7,8,12,16,18,23)],as.factor) #wybieramy zmienne czynnikowe
attrition$Over18[attrition$Over18=="Y"]=1
attrition$Over18=as.numeric(attrition$Over18)

## podzial na probki czesc i testowa

set.seed(123)
ranuni=sample(x=c("Training","Testing"),size=nrow(attrition),replace=T,prob=c(0.7,0.3))
train_d=attrition[ranuni =="Training",] #zbior uczacy
test_d=attrition[ranuni =="Testing",]   #zbior testowy
nrow(train_d)
nrow(test_d)



names(attrition)
# przygotowanie formuly, sporo zmiennych - automatyzujemy
independentvariables=colnames(attrition[,-2])
independentvariables
Model=paste(independentvariables,collapse="+")
Model
Model_1=paste("Attrition~",Model)
Model_1
class(Model_1)
formula=as.formula(Model_1)
formula
summary(train_d)
#budujemy model full, tj. ze wszystkimi zmiennymi
model_full=glm(formula=formula,data=train_d,family="binomial")

summary(model_full)

#krokowa selekcja zmiennych
model_step=step(object = model_full,direction = "both")
summary(model_step)

#ocena trafnosci w probie uczacej
trpred=ifelse(test=model_step$fitted.values>0.5,yes = 1,no=0)
table(model_step$y,trpred)
#ile poprawnych?
sum(diag(table(model_step$y,trpred)))/sum(table(model_step$y,trpred))

#ocena trafnosci w probie testowej
testpred=predict.glm(object=model_step,newdata=test_d,type = "response")
testpred


testpred=ifelse(test=testpred>0.5,yes=1,no=0)
table(test_d$Attrition,testpred)
sum(diag(table(test_d$Attrition,testpred)))/sum(table(test_d$Attrition,testpred))


##########################################################################333
#### porownajmy z lda 

### w formule Model_1 usuwamy zmienne, bez zmiennosci

Model_2 = Attrition~ Age+BusinessTravel+DailyRate+Department+DistanceFromHome+Education+EducationField+EmployeeNumber+EnvironmentSatisfaction+Gender+HourlyRate+JobInvolvement+JobLevel+JobRole+JobSatisfaction+
  MaritalStatus+MonthlyIncome+MonthlyRate+NumCompaniesWorked+OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+StockOptionLevel+TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+
  YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager
Model_2 = as.formula(Model_2)

atrr_lda = lda(Model_2, train_d)
atrr_lda

lda_test = predict(atrr_lda, test_d)

table(test_d$Attrition,lda_test$class)

sum(diag(table(test_d$Attrition,lda_test$class)))/sum(table(test_d$Attrition,lda_test$class))


##########################################################################333
#### porownajmy z qda 

### w formule usuwamy zmienne, bez zmiennosci (Model_2)
### budujemy model na zbiorze uczacym 

atrr_qda = qda(---, ---)
atrr_qda

#### prognozujemy etykiety: wykorzystujemy model qda, oraz zbior testowy
qda_test = predict(---, ---)

table(test_d$Attrition,lda_test$class)

#### wyznaczamy trafnosc predykcji




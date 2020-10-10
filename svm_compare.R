data(cats, package = "MASS")
setwd("~/Downloads")
data(cats, package = "MASS")
library(e1071)
library(caret)
library(lattice)
library(ROCR)
#install.packages("caret")
#install.packages("ROCR")
#install.packages("lattice")
summary(cats)
dim(cats)
pairs(cats)
# Typy zmiennych wystepujace w zbiorze to: zmienne kategoryzujace (binarne)odnoszace sie do plci oraz zmienne rzeczywiste Bwt i Hwt
## Zmienne Hwt i Bwt pozwalaja na skuteczna klasyfikacje plci dla wysokich parametrow Hwt i Bwt(rozdzielenie danych ze wzgledu na plec)


mdl = glm(Sex~., data = cats, family = binomial)
mdlP = predict(mdl, cats)
slope <- coef(mdl)[3]/(-coef(mdl)[2])
intercept <- coef(mdl)[1]/(-coef(mdl)[2])
xyplot( Bwt ~ Hwt , data = cats, groups = cats$Sex,
        panel=function(...){
          panel.xyplot(...)
          panel.abline(intercept , slope)
          panel.grid(...)
        })


?svm
m1 <- svm(Sex~., data = cats, kernel="linear", cost=1)
plot(m1, cats)





m1 <- svm(Sex~., data = cats, kernel="linear", cost=50)
plot(m1, cats)
m2 <- svm(Sex~., data = cats, kernel="linear", cost=100)
plot(m2, cats)
m3<- svm(Sex~., data = cats, kernel="linear", cost=10e6)
plot (m3, cats)
m1 <- svm(Sex~., data = cats, kernel="linear", cost=1)
predict(m1)-> p1
table(p1)
confusionMatrix(cats$Sex, p1)


m2 <- svm(Sex~., data = cats, kernel="linear", cost=100)
predict(m1)-> p2
table(p2)
confusionMatrix(cats$Sex, p2)


m3 <- svm(Sex~., data = cats, kernel="linear", cost=2)
predict(m3)-> p3
table(p3)
confusionMatrix(cats$Sex, p3)
predict(m2)-> p2
confusionMatrix(cats$Sex, p2)
# a) Accuracy spada znaczaco dla modelu m3, czyli znacznie wiekszego C#
confusionMatrix(cats$Sex, p3)
# b) Balnced Acucurancy dla m3 jest w pryzblizeniu rowne Accuracy, a dla m1 i m2 znaczaco wyzsza wartosc ma Accuracy, co pokazuje, ze lepszym parametrem dla tego zbioru jest Balanced Accuracy
predict(m1)-> p1
confusionMatrix(cats$Sex, p1)
# c) Sensitivity naksymlizuje poprawnego wkrycia kota TP i FN, Specify naksymalizuje  poprawno?? nie uznania kota za samaca
# PPV to procent przewidzanych przypdk?w pozytywnych  do Do prawdziwe pozytwnych i analogicznie w 2 przypadku,
#  Sensitivity Specificity to chyba co? na zaszadzie prawodpodobie?st pope?nienia b?edu 1 i 2 rodzaju

#d) dla modelu m1 prawwdopodobienstwo wykrycia prawdziwych przypadkow (wykryte prawdziwe wsrod wysztkich prawdziwych)wynosi ok. 0.7, co oznacza, ze wsrod 100 naprawde
# chorych na raka wykrylibysmy 70; prawdopodobienstwo oceny, ze ktos jest zdrowy (nie ma raka)jest na poziomie 80%
m1r <- svm(Sex~., data = cats, kernel="radial", cost=1)
predict(m1r)-> p1r
confusionMatrix(cats$Sex, p1r)
m3r <- svm(Sex~., data = cats, cost=10e6)
predict(m3r)-> p3r
confusionMatrix(cats$Sex, p3r)
m3 <- svm(Sex~., data = cats, kernel="linear", cost=10e6)
predict(m3)-> p3
confusionMatrix(cats$Sex, p3)
# model radialny dla m3 pozwala zwiekszyc prawidlowa 
#wykrywalnosc zarowno przypadku prawdziwych (chorych na raka), jak i falszywych (zdrowych)
library(e1071)
library(caret)
library(lattice)
library(ROCR)
#Zad 6

numberOfF <- length(which(cats$Sex == "F"))
numberOfM <-  length(which(cats$Sex == "M"))

wF=1
wM=numberOfF/numberOfM
wMg =  wM + 0.1

# w populacji jest dw?krotna przewaga samc?w  
# class.weights	 parametr okre?la propoecje wyst?powania klas w populacji 
# pozwala sterowa? tym aby licznio?? wyst?powaia klasy mia?a mniejszy wp?yw na wynik

m1 <- svm(Sex~., data = cats, kernel="linear", cost=1)
predict(m1)-> p1
table(p1)
confusionMatrix(cats$Sex, p1)
#Sensitivity : 0.7021          
#Specificity : 0.8557
#Accuracy : 0.8056  
             #Reference
#Prediction  F  M
#F          33 14
#M          14 83

m1cw <- svm(Sex~., data = cats, kernel="linear", cost=1, class.weights=c("F" = wF,"M" = wM ))
predict(m1cw)-> p1cw
summary(m1cw)
table(p1cw)
confusionMatrix(cats$Sex, p1cw)

#Sensitivity : 0.5571          
#Specificity : 0.8919          
#Pos Pred Value : 0.8298          
#Neg Pred Value : 0.6804
# po dodaniu paramertu class weights poprawi?a si? Accurency z 70 , 72
#spad?o Sensitivity a wzrozs?o  Specificity zmniejszy?o si? przweidzenie samic jako samc?w
#oraaz zwi?kszy?o samc?w jako samice

             #Reference
#Prediction  F  M
          #F 39  8
          #M 31 66



m1cw <- svm(Sex~., data = cats, kernel="linear", cost=1, class.weights=c("F" = wF,"M" = wMg ))
predict(m1cw)-> p1cw
summary(m1cw)
table(p1cw)
confusionMatrix(cats$Sex, p1cw)

#Sensitivity : 0.5781          
#Specificity : 0.8750          
#Pos Pred Value : 0.7872          
#Neg Pred Value : 0.7216  

#            Reference
  #Prediction  F  M
#           F 37 10
#           M 27 70

# widzmy wzrost wykrywalnosci samców kosztem spadku wykrywalności prawdziwych samic ale również spadek rozpoznania samic jako samców  oraz zwiększenie rozponawania samców o 4
# oraz wzrost wykrycia samic jako samców
#Wzrost sensivity oraz spadrek Specificity


#Zad 7



m1cw <- svm(Sex~., data = cats, cost=1, gamma=0.1)
predict(m1cw)-> p1cw
summary(m1cw)
table(p1cw)
confusionMatrix(cats$Sex, p1cw)

#Reference
#Prediction  F  M
#         F 34 13
#         M 17 80

"
     Accuracy : 0.7917          
                 95% CI : (0.7161, 0.8548)
    No Information Rate : 0.6458          
    P-Value [Acc > NIR] : 0.000103        
Mcnemar's Test P-Value : 0.583882        
                                          
            Sensitivity : 0.6667          
            Specificity : 0.8602          
         Pos Pred Value : 0.7234          
         Neg Pred Value : 0.8247          
             Prevalence : 0.3542          
         Detection Rate : 0.2361          
   Detection Prevalence : 0.3264          
      Balanced Accuracy : 0.7634          
                                          
       'Positive' Class : F               
"

m1cw <- svm(Sex~., data = cats, cost=1, gamma=1)
predict(m1cw)-> p1cw
summary(m1cw)
table(p1cw)
confusionMatrix(cats$Sex, p1cw)

"
          Reference
Prediction  F  M
         F 32 15
         M 14 83
                                          
               Accuracy : 0.7986          
                 95% CI : (0.7237, 0.8608)
    No Information Rate : 0.6806          
    P-Value [Acc > NIR] : 0.001119        
                                          
                  Kappa : 0.5395          
                                          
 Mcnemar's Test P-Value : 1.000000        
                                          
            Sensitivity : 0.6957          
            Specificity : 0.8469          
         Pos Pred Value : 0.6809          
         Neg Pred Value : 0.8557          
             Prevalence : 0.3194          
         Detection Rate : 0.2222          
   Detection Prevalence : 0.3264          
      Balanced Accuracy : 0.7713          
                                          
       'Positive' Class : F               
                               "
m1cw <- svm(Sex~., data = cats, cost=1, gamma=5)
predict(m1cw)-> p1cw
summary(m1cw)
table(p1cw)
confusionMatrix(cats$Sex, p1cw)

"1cw
  F   M 
 44 100 
> confusionMatrix(cats$Sex, p1cw)
Confusion Matrix and Statistics

          Reference
Prediction  F  M
         F 32 15
         M 12 85
                                         
               Accuracy : 0.8125         
                 95% CI : (0.739, 0.8727)
    No Information Rate : 0.6944         
    P-Value [Acc > NIR] : 0.0009493      
                                         
                  Kappa : 0.5665         
                                         
 Mcnemar's Test P-Value : 0.7003114      
                                         
            Sensitivity : 0.7273         
            Specificity : 0.8500         
         Pos Pred Value : 0.6809         
         Neg Pred Value : 0.8763         
             Prevalence : 0.3056         
         Detection Rate : 0.2222         
   Detection Prevalence : 0.3264         
      Balanced Accuracy : 0.7886         
                                         
       'Positive' Class : F              
                                         "

m1cw <- svm(Sex~., data = cats, cost=1, gamma=50)
predict(m1cw)-> p1cw
summary(m1cw)
table(p1cw)
confusionMatrix(cats$Sex, p1cw)
"
p1cw
  F   M 
 39 105 
> confusionMatrix(cats$Sex, p1cw)
Confusion Matrix and Statistics

          Reference
Prediction  F  M
         F 37 10
         M  2 95
                                         
               Accuracy : 0.9167         
                 95% CI : (0.859, 0.9562)
    No Information Rate : 0.7292         
    P-Value [Acc > NIR] : 1.652e-08      
                                         
                  Kappa : 0.8018         
                                         
 Mcnemar's Test P-Value : 0.04331        
                                         
            Sensitivity : 0.9487         
            Specificity : 0.9048         
         Pos Pred Value : 0.7872         
         Neg Pred Value : 0.9794         
             Prevalence : 0.2708         
         Detection Rate : 0.2569         
   Detection Prevalence : 0.3264         
      Balanced Accuracy : 0.9267         
                                         
       'Positive' Class : F   
"
"Wybrałbym model dlatego że ma największe Acurrency i maksymalizuje Sensitivity i Specificity oraz jak pokazuje tabela predykcji to jest bnajblizej realnego podziału"


#Zad 9

nmbOfCats = dim(cats)[1] 
trIndx = sample(nmbOfCats, nmbOfCats*0.7)
catsTr = cats[trIndx, ]
catsTst = cats[-trIndx, ]

#a) 
model <- svm(Sex~., data = catsTr, cost=1, gamma=50)

#b)
predict(model, newdata=catsTst) -> prediction
summary(model)
table(prediction)
confusionMatrix(catsTst$Sex, prediction)
"         Reference
Prediction  F  M
         F  5 12
         M  4 23
                                          
               Accuracy : 0.6364          
                 95% CI : (0.4777, 0.7759)
    No Information Rate : 0.7955          
    P-Value [Acc > NIR] : 0.99568         
                                          
                  Kappa : 0.1599          
                                          
 Mcnemar's Test P-Value : 0.08012         
                                          
            Sensitivity : 0.5556          
            Specificity : 0.6571          
         Pos Pred Value : 0.2941          
         Neg Pred Value : 0.8519          
             Prevalence : 0.2045          
         Detection Rate : 0.1136          
   Detection Prevalence : 0.3864          
      Balanced Accuracy : 0.6063          
                                          
       'Positive' Class : F               "
"Model dał duzo gorsze  wyniki zatówno Accuracy jak Balanced Accuracy są niższe oraz tak jak statystyki dla  Sensitivity Specificity 
 Wydaje mi się , że tutaj wpływ miało to że losowaliśmy zbiór treningowy wcześniej pracowaliśmy na 
 jednym zbiorze danych. Wybraliśmy te dane które nie pojawiają się w zbiorze treningowym co spowodowało, że preducja stała sie bardziej ralna bo nie weryfukacja nie następowała w obrębie tych samych danych co uczenie
Zbiór testowy zwiera trż inna propocję danych class F / M"


?tune
          
data(iris)
## tune `svm' for classification with RBF-kernel (default in svm),
## using one split for training/validation set

data(iris)

obj <- tune.svm(Sex~., data = cats, gamma = 2^(-1:1), cost = 2^(2:4))

summary(obj)
plot(obj, type="contour")
"
arameter tuning of ‘svm’:

- sampling method: 10-fold cross validation 

- best parameters:
 gamma cost
   2    8
Dla funckcji tunde zostaly dobrane prametry gamma 2 cost 8 jako generujace najmniejszy 
odsetek bledow


Zad 11


"
def = read.csv("./defaults.csv", sep = ";", header = TRUE)
smpl = def[sample(nrow(def),2000),]
summary(smpl)
names(smpl)
apply(smpl, 2, function(x) any(is.na(x)))

kredytM <- tune.svm(default.payment.next.month~SEX+EDUCATION+MARRIAGE+AGE+LIMIT_BAL, data = smpl, gamma = 2^(-1:1), cost = 2^(2:4))
summary(kredytM)
plot(kredytM)

modelKredyt <- svm(default.payment.next.month~SEX+EDUCATION+MARRIAGE+AGE+LIMIT_BAL, data = smpl, cost=4, gamma=0.5)
predict(modelKredyt) -> kp
summary(kp)
kp <- ifelse(kp >= 0.04150,1,0)
length(kp[kp==0])
kp <- as.integer(kp)
confusionMatrix(smpl$default.payment.next.month, kp)
smpl$default.payment.next.month



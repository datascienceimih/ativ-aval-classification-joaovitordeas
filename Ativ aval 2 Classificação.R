##Atividade avaliativa 2 - Classificação.
## ISLR pag 171.
## Exercicios 10(- G,I), 11(-G), 13(-KNN)
## João Vitor de Andrade Santos.
install.packages("ISLR")
library(ISLR)

## 10 - a)
summary(Weekly)
cor(Weekly[, -9])
attach(Weekly)
plot(Volume)

## A unica relação que pude observar foi entre as variaveis year e volume. 
## Tendo em vista o plot(Volume) é possivel observar o aumento com o passar do tempo.

## 10 - b)
fit.glm = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
              data = Weekly, family = binomial)
summary(fit.glm)
## De acordo com os resultados somente Lag2 é statisticamente significante
## por ser a única com p valor menor que 0,05.

## 10 - c)
probs = predict(fit.glm, type = "response")
pred.glm = rep("Down", length(probs))
pred.glm[probs > 0.5] = "Up"
table(pred.glm, Direction)
## Os resultados certos de treino, de acordo com a tabela são aproximadamente 56%,
## isto é 44% de erro, o que é uma proporção ruim.

## 10 - d)
train = (Year < 2009)
Weekly.20092010 = Weekly[!train, ]
Direction.20092010 = Direction[!train]
fit.glm2 <- glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
summary(fit.glm2)

## Nesse caso os dados de teste acertaram 62% e errou 38%.

## 10 - e)
library(MASS)
fit.lda = lda(Direction ~ Lag2, data = Weekly, subset = train)
fit.lda

## Com o LDA o teste obteve 62.5% de acerto e 37.5% de erro.

## 10 - f)
fit.qda = qda(Direction ~ Lag2, data = Weekly, subset = train)
fit.qda
## Com o QDA o teste obteve 58% de acerto e 42% de erro.

##10 - h)
## Levando em considerassão o erro do teste, o melhor modelo foi usando o LDA
################################################################################

##11 - a)
attach(Auto)
mpg01 = rep(0, length(mpg))
mpg01[mpg > median(mpg)] = 1
Auto = data.frame(Auto, mpg01)

## 11 - b)
cor(Auto[, -9])
pairs(Auto)
boxplot(cylinders ~ mpg01, data = Auto, main = "Cylinders vs mpg01")

boxplot(displacement ~ mpg01, data = Auto, main = "Displacement vs mpg01")

boxplot(horsepower ~ mpg01, data = Auto, main = "Horsepower vs mpg01")
## Conclui que existem associações entre as variaveis ""mpg01""= e "cylinders",
## "weight", “displacement” e “horsepower”.

## 11 - c)
train <- (year %% 2 == 0)
Auto.train = Auto[train, ]
Auto.test = Auto[!train, ]
mpg01.test = mpg01[!train]

## 11 - d)
fit.lda = lda(mpg01 ~ cylinders + weight + displacement + horsepower, 
              data = Auto, subset = train)
fit.lda

pred.lda = predict(fit.lda, Auto.test)
table(pred.lda$class, mpg01.test)

mean(pred.lda$class != mpg01.test)
## A taxa de erro do teste com LDA foi de aprox 12%.

## 11 - e)
fit.qda = qda(mpg01 ~ cylinders + weight + displacement + horsepower, 
               data = Auto, subset = train)
fit.qda
pred.qda = predict(fit.qda, Auto.test)
table(pred.qda$class, mpg01.test)

mean(pred.qda$class != mpg01.test)
## O taxa de erro no teste com QDA foi de aprox 13%

fit.glm = glm(mpg01 ~ cylinders + weight + displacement + horsepower, 
              data = Auto, family = binomial, subset = train)
summary(fit.glm)

probs = predict(fit.glm, Auto.test, type = "response")
pred.glm = rep(0, length(probs))
pred.glm[probs > 0.5] = 1
table(pred.glm, mpg01.test)

mean(pred.glm != mpg01.test)
## A taxa de erro do teste foi de aprox 12%

## 13 
library(MASS)
attach(Boston)
crim01 = rep(0, length(crim))
crim01[crim > median(crim)] = 1
Boston = data.frame(Boston, crim01)

train = 1:(length(crim) / 2)
test = (length(crim) / 2 + 1):length(crim)
Boston.train = Boston[train, ]
Boston.test = Boston[test, ]
crim01.test = crim01[test]

fit.glm = glm(crim01 ~ . - crim01 - crim, 
               data = Boston, family = binomial, subset = train)

probs = predict(fit.glm, Boston.test, type = "response")
pred.glm = rep(0, length(probs))
pred.glm[probs > 0.5] = 1
table(pred.glm, crim01.test)

mean(pred.glm != crim01.test)
## O erro da regressão logistica foi de 18%

fit.glm = glm(crim01 ~ . - crim01 - crim - chas - nox, 
               data = Boston, family = binomial, subset = train)

probs = predict(fit.glm, Boston.test, type = "response")
pred.glm = rep(0, length(probs))
pred.glm[probs > 0.5] = 1
table(pred.glm, crim01.test)

mean(pred.glm != crim01.test)
## O Erro dessa regressão logistica foi de aprox 15%, um erro maior que a anterior.

fit.lda = lda(crim01 ~ . - crim01 - crim, data = Boston, subset = train)
pred.lda = predict(fit.lda, Boston.test)
table(pred.lda$class, crim01.test)

mean(pred.lda$class != crim01.test)
## O erro dessa LDA foi de aprox 13%

fit.lda <- lda(crim01 ~ . - crim01 - crim - chas - nox, 
               data = Boston, subset = train)
pred.lda <- predict(fit.lda, Boston.test)
table(pred.lda$class, crim01.test)

mean(pred.lda$class != crim01.test)
## O Erro dessa LDA foi de aprox 15%, um aumento comparado com a anterior.
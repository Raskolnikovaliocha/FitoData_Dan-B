

library(tinytex)
library(tidyverse)
library(knitr)
library(nortest)
library(gt)
library(gtsummary)
library(car)
library(gridExtra)

#Transforming the data

dir()
data_1 <- read.table("adavis.txt", h = TRUE)
data_1
attach(data_1)
head(data_1)
data_2 <-data_1 %>% transmute(
  Adjuvant = as.factor(Adjuvante),
  Taxa = as_factor(Taxa), 
  Bloco = as.factor(Bloco), Sub = as.factor(Sub),tresA )





data_2
attach(data_2)
data_2
str(data_2)


#mediana em torno de zero 
data_3 <- data_2 %>%  group_by(Adjuvant, Taxa) %>% 
  summarise(
    Mean = mean(tresA, na.rm = TRUE), 
    Variance = var(tresA, na.rm = TRUE), 
    Median = median(tresA, na.rm = TRUE))
data_3
summary(data_2)
var(tresA, na.rm = TRUE)
mean(tresA, na.rm = TRUE)


# Observação da relação entre as variáveis explicativas e variáveis respostas 
# os dados se concentram em torno de zero igual a uma curva de poisson 
# não há uma relação linear entre asvariáveis, se isso não ocorre então a relação é 
# naõ linear e o melhor modelo é o de poisson 

p1 <- ggplot(data_2, aes(x= Taxa, y = tresA)) + geom_point() + ylim(0,9)
p2 <- ggplot(data_2, aes( x = Adjuvant, y = tresA)) + geom_boxplot()
p3 <- ggplot(data_2, aes(Y = tresA, x= Bloco)) + geom_boxplot()
grid.arrange(p1, p2, p3,p4,  nrow = 2, ncol = 2)

#Histogram
p4 <-ggplot(data = data_2, aes(x =  tresA)) + 
  geom_histogram(binwidth = 0.20) 


names(data_2)
model1 <- glm( tresA~1, poisson, data_2)
model1
model2<- glm( tresA~Taxa*Bloco, poisson, data = data_2)
model1 <- glm(tresA ~Taxa*Adjuvant*Bloco, poisson, data = data_2)
summary(model1)
anova(model1, test = "Chisq")
names(data_2)





min(data_2$Taxa)
summary(data_2)


summary(model2)
# Distribution poisson:
model1 <- glm(tresA~1, poisson)
model1
modelo_poisson <- glm(tresA ~ Adjuvant * Taxa*Bloco, family = poisson(link = "log"), data = data_2, data_2_clean)
modelo_poisson


summary(modelo_poisson)
anova(model1, test = "Chisq")



# Construindo modelos:
names(data_2)
mnulo <- glm(tresA~1)
summary(mnulo)
modelo2  <- glm(tresA ~ Adjuvant * Taxa*Bloco, family = poisson, data = data_2)

anova(modelo2)
summary(modelo2)# como os resíduos do resíduo foram maior do que os graus de liberadade, então utilizo distribuição de quase poisson
# Pressuposto para utilização de poisson é que a média seja igual à variância. e a variável dependente seja discreta, o, 1 etc
# Se a distribuição for de poisson, então utilizo qui quadrado, se ela for de quase poisson então teste F
mnulo <- glm(tresA~1 )
summary(mnulo)

modelo3  <- glm(tresA ~  Taxa*Adjuvant*Bloco, family = quasipoisson, data = data_2)
anova(mnulo,modelo3,test = "F" )
summary(modelo3) 
anova(modelo3, test = "F" )
anova(mnulo, modelo3, test = "F")
summary(modelo3)

# Ficaremos com o modelo complexo comparando com o modeli nula, pois ele explica mais
modelo4 <- update(modelo3 , ~. -AdjuvantSilwet:TaxaOchima:Bloco4)

anova(model3, modelo4)
summary(modelo4)
rm(modelo4) 
vif(modelo3)# Há interação entre os termos, um é igual ao outro
alias(modelo3)
cor(data_2[,c("Adjuvant", "Taxa", "Bloco")])

correlation <- cor("adjuvant", "taxa", data_2)
print(correlation)
cor(data_2)

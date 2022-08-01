
# Pacotes necessários
library(rsample) # for data splitting
library(caret) # for logistic regression modeling
library(vip) # variable importance
library(sjPlot) # table


# Carregando a base de dados para modelagem criada anteriormente (esse passo não é necessário vamos importar
# base de teste e treinamento a seguir)
#df <- data.table::fread('data/Base_modelagem.txt',sep2 = " ")


# removendo as colunas ID_CONTA, DT_ACORDO
# df <- df %>%
#  dplyr::select(-c(ID_CONTA,DT_ACORDO))

# Separando a amostra em treinamento e teste (lembrando que a semente aqui, pode ser diferente
# se a versão do R for diferente, estou rodando na versão 4.1.1)

# set.seed(123) # for reproducibility
# acordo_split <- initial_split(df, prop = .7)
# acordo_train <- training(acordo_split)
# acordo_test <- testing(acordo_split)

# para garantir reprodutibilidade em outras versões do R,
# salvamos a base teste e treino nas pasta base esse código foi rodado na versão
# 4.1.1 se tiver a mesma versão pode comentar a passo de importação a seguir
# e descomentar o anterior que vai funcionar

# salvando base de teste e treinamento
# data.table::fwrite(acordo_test, 'data/Base_teste.txt')
# data.table::fwrite(acordo_train, 'data/Base_treino.txt')

# importando bases de teste e treinamento
acordo_test <- data.table::fread('data/Base_teste.txt',sep2 = " ")
acordo_train <- data.table::fread('data/Base_treino.txt',sep2 = " ")


# Ajustando o modelo de regressão logistica para os dados de treinamento
logit <- glm(RESPOSTA ~., family = binomial(link="logit"),
             data = acordo_train)

# identificando as variáveis que tem efeito significativo ao nível de significância de 5%
summary(logit)

# Ajustando o modelo com as variáveis que são significativas para os dados de treino
logit_final <- glm(RESPOSTA ~QTD_CPC_6M+
                     QTD_CP_6M+
                     QTD_ACIONAMENTO_10D+
                     QTD_ACIONAMENTO_1M+
                     QTD_ACIONAMENTO_6M+
                     NU_DIAS_ATRASO+
                     VALOR_CRELIQ+
                     QTD_PARCELAMENTO_12M+
                     LIMITE+
                     #QTD_FX0_6M+
                     QTD_FX1_6M, family = binomial(link="logit"),
                   data = acordo_train)


# logit_final <- glm(RESPOSTA ~NU_DIAS_ATRASO+
#                      VALOR_CRELIQ+
#                      QTD_PARCELAMENTO_12M+
#                      LIMITE+
#                      QTD_FX1_6M+
#                      QTD_CPC_1M+
#                      QTD_CPC_6M+
#                      QTD_ACIONAMENTO_10D+
#                      QTD_ACIONAMENTO_1M+
#                      QTD_ACIONAMENTO_6M, family = binomial(link="logit"),
#                    data = acordo_train)

# Avaliando o efeito das variáveis após o novo ajuste
summary(logit_final)


# obtendo as probabilidade de acordo para os dados de treinamento
pred <- predict(logit_final, acordo_train,type = "response")


# implementando a função para identificar a probabilidade de 
# corte que será usada na definição da regra de predição que maximiza
# sensibilidade + especificidade

perform_fn <- function(cutoff) 
{
  predicted_churn <- factor(ifelse(pred >= cutoff, "1", "0"))
  conf <- confusionMatrix(table(predicted_churn, acordo_train$RESPOSTA), positive="1")
  accuray <- conf$overall[1]
  sensitivity <- conf$byClass[1]
  specificity <- conf$byClass[2]
  out <- t(as.matrix(c(sensitivity, specificity, accuray))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


options(repr.plot.width =8, repr.plot.height =6)
summary(pred)
s = seq(0.01,0.80,length=100)
OUT = matrix(0,100,3)

try(for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
},
TRUE)

OUT2 <- OUT[1:56,]


# obtendo o valor da probabilidade que maximiza a sensibilidade+especificidade
cutoff <- s[which(abs(OUT2[,1]-OUT2[,2]) == min(abs(OUT2[,1]-OUT2[,2])))]
cutoff


# plotando as curvas de sensibildade, espeficidade e acurácia vs os valores
# das probabilidade de corte.

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),
     type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend("bottom",col=c(2,"darkgreen",4,"darkred"),text.font =3,inset = 0.02,
       box.lty=0,cex = 0.8, 
       lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
abline(v = cutoff, col="red", lwd=1, lty=2)
axis(1, at = seq(0.1, 1, by = 0.1))


# classificando os usuários usando a regra de classificação
# com o ponto de corte selecionado anteriormente
pred_treinamento_logistica <- ifelse(pred > cutoff, "1", "0")

# risco do modelo
mean(pred_treinamento_logistica != acordo_train$RESPOSTA, na.rm = TRUE)

# matriz de confusão e métricas para os dados de treinamento
confusionMatrix(table(pred_treinamento_logistica, acordo_train$RESPOSTA), positive="1")


# obtendo as probabilidade das novas observações do conjunto de teste
pred <- predict(logit_final, acordo_test,type = "response")

# classificando utilizando a  regra de predição definida com os dados de teste 
classe_log <- ifelse(pred > cutoff, "1", "0")

# obtendo o risco
mean(classe_log != acordo_test$RESPOSTA, na.rm = TRUE)

# calculando a matriz de confusão e as métricas associadas
confusionMatrix(table(classe_log, acordo_test$RESPOSTA), positive="1")


# obtendo o efeito das variáveis do modelo em termos percentuais
(exp(coef(logit_final))-1)/1*100

# apresentando as 10 variáveis que mais contribuiram nas predições do modelo
vip(logit_final, num_features = 10)


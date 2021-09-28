# Prevision the result of the S&P index (The Standard & Poor's 500) of the
# American stock market index (NYSE or NASDAQ)
# https://rdrr.io/cran/ISLR/man/Smarket.html

# Imports library
library(ISLR)
library(caret)
library(e1071)

# Split train and test
set.seed(300)
indxTrain <- createDataPartition(y = Smarket$Direction, p = 0.75, list = FALSE)
dados_treino <- Smarket[indxTrain,]
dados_teste <- Smarket[-indxTrain,]

# Normalization Function
scale.features <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- scale(df[[variable]], center = T, scale = T)
  }
  return(df)
}

# Aplied normalization in train and test
numeric.vars_treino <- colnames(treinoX <- dados_treino[,names(dados_treino) != "Direction"])
numeric.vars_teste <- colnames(testeX <- dados_teste[,names(dados_teste) != "Direction"])

dados_treino_scaled <- scale.features(dados_treino, numeric.vars_treino)
dados_teste_scaled <- scale.features(dados_teste, numeric.vars_teste)


# KNN V1
set.seed(400)
ctrl <- trainControl(method = "repeatedcv", repeats = 3) 
knn_v1 <- train(Direction ~ ., 
                data = dados_treino_scaled, 
                method = "knn", 
                trControl = ctrl, 
                tuneLength = 20)

# Model V1
knn_v1

# Plot
plot(knn_v1)

# Make Prevision
knnPredict <- predict(knn_v1, newdata = dados_teste_scaled)

# Make a Confusion Matrix
confusionMatrix(knnPredict, dados_teste$Direction)


# KNN V2
ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 3, 
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

knn_v2 <- train(Direction ~ ., 
                data = dados_treino_scaled, 
                method = "knn", 
                trControl = ctrl, 
                metric = "ROC",
                tuneLength = 20)

# Model V2
knn_v2

# Plot
plot(knn_v2, print.thres = 0.5, type="S")

# Make Prevision
knnPredict <- predict(knn_v2, newdata = dados_teste_scaled)

# Make a Confusion Matrix
confusionMatrix(knnPredict, dados_teste$Direction)

# Make prevision

Year = c(2006, 2007, 2008)
Lag1 = c(1.31, 0.09, -0.654)
Lag2 = c(1.484, -0.198, 0.589)
Lag3 = c(-0.346, 0.029, 0.690)
Lag4 = c(1.397, 0.104, 1.483)
Lag5 = c(0.215, 0.105, 0.589)
Volume = c(1.36891, 1.09876, 1.231233)
Today = c(0.279, -0.497, 1.649)
novos_dados = data.frame(Year, Lag1, Lag2, Lag3, Lag4, Lag5, Volume, Today)
nomes_variaveis <- colnames(novos_dados)
novos_dados_scaled <- scale.features(novos_dados, nomes_variaveis)

knnPredict <- predict(knn_v2, newdata = novos_dados_scaled)
cat(sprintf("\n Prevision of \"%s\" is \"%s\"\n", novos_dados$Year, knnPredict))







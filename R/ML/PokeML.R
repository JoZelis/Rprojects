# Title     : Legendary ML analyser
# Objective : Getting familiar with ML
# Created by: Jo
# Created on: 24-4-2020
library(tidyverse)
library(gplots)
library(rpart)
library(ROCR)
library(rpart.plot)
#from https://towardsdatascience.com/introduction-to-machine-learning-with-pokemon-ccb7c9d1351b
pkdf <- read.csv("Pokemon.csv")
head(pkdf)
# test different linear models
pk_logit_model_1 <- glm(Legendary ~ Attack + Defense + Sp_Atk + Sp_Def + HP + Speed, data = pkdf, family = "binomial")
summary(pk_logit_model_1)

pk_logit_model_2 <- glm(Legendary ~ Attack + Sp_Atk + Sp_Def + HP + Speed, data = pkdf, family = "binomial")
summary(pk_logit_model_2)
# median absolute deviation is lower than the other models (median from output)
pk_logit_model_3 <- glm(Legendary ~ Attack + Sp_Atk + Sp_Def + HP, data = pkdf, family = "binomial")
summary(pk_logit_model_3)
# make a tree model to compare the linear model with
pk_tree_model <- rpart(Legendary ~  Attack + Sp_Atk + Sp_Def + HP, data = pkdf, method = "class")
rpart.plot(pk_tree_model, type = 1, extra=1, box.palette =c("pink", "green"), branch.lty=3, shadow.col = "gray")
# see if the models can predict the outcome
pk_predict_logit <- predict(pk_logit_model_3, pkdf, type="response")
pk_predict_tree <- predict(pk_tree_model, pkdf, type="prob")
# predict the outcome of ID#59
pk_predict_logit[59] #logit is correct that its not legendary
pk_predict_tree[59]
# see how well it predicts legendaries
pk_logit_prediction <- prediction(pk_predict_logit, pkdf$Legendary)
pk_tree_prediction <- prediction(pk_predict_tree[,2], pkdf$Legendary)

pk_performance_logit <- performance(pk_logit_prediction,"tpr","fpr")
pk_performance_tree <- performance(pk_tree_prediction,"tpr","fpr")
# plot ROC curve
plot(pk_performance_logit,col="blue",lty=3, lwd=3) #logit performs best
plot(pk_performance_tree,col="black",lty=3, lwd=3, add=TRUE)
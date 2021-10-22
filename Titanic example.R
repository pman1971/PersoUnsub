# Fit using DataRobot

library(httr)
library(knitr)
library(data.table)
library(pROC)
library(dplyr)
library(lattice)

library(datarobot)
ConnectToDataRobot(endpoint = getOption("endpoint"), token = getOption("apiToken"))

# Load Titanic Data
trainDat= read.csv('https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv')


target <- "Survived"
projectName <- "Titanic"

project <- StartProject(dataSource = trainDat, 
                        projectName = projectName,
                        target = target,
                        workerCount = "max",
                        wait = TRUE)

results <- as.data.frame(ListModels(project))
kable(head(results), longtable = TRUE, booktabs = TRUE, row.names = TRUE)

# Generating Model Predictions
bestModel <- GetRecommendedModel(project, type = RecommendedModelType$RecommendedForDeployment)

bestPredictions <- Predict(bestModel, trainDat, type = "probability")

testDat= cbind(trainDat, bestPredictions)

roc(testDat$Survived, testDat$bestPredictions,
    smoothed = TRUE,
    # arguments for ci
    ci=TRUE, ci.alpha=0.9, stratified=FALSE,
    # arguments for plot
    plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
    print.auc=TRUE, show.thres=TRUE)

testDatDeciles= 
  testDat %>%
  mutate(decile = ntile(bestPredictions, 10))

testDatDeciles$decile= as.factor(testDatDeciles$decile)

# Summary stats by decile
calibrationDeciles=
  testDatDeciles %>%
  group_by(decile) %>%
  summarise(totalRows= n(),
            posResp= sum(testDat$Survived),
            avgPred= mean(bestPredictions)) %>%
  mutate(actualRate= posResp/totalRows)

xyplot(avgPred + actualRate~  decile, data= calibrationDeciles, type = "l", pch=20,
       as.table= TRUE,
       main = "Calibration plot", ylab = "%", xlab = "Decile",
       auto.key=list(space="top", columns= 2))

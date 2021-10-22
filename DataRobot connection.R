library(httr)
library(knitr)
library(data.table)

library(datarobot)
endpoint <- "https://datarobot.edr.qantasloyalty.net/api/v2"
apiToken <- "NjE2ODliODg5YjdhOTZkMjAzMjE0OTk0OldYRjVaN2VZZUR0cWJ4d1o1M3FKVXhMcExyR0VHMG5rbWUvY1YxbUUyekk9"
ConnectToDataRobot(endpoint = endpoint, token = apiToken)

?datarobot

testSet= fread("unsubTestingDS.csv")
nrow(testSet)


# Requesting prediction
projectId <- "6168e3e1a48d2583ecb446d9"
modelID= '61690f728b9680420063041f'

project= GetProject(projectId)

model <- GetRecommendedModel(project)

predict_job_id <- RequestPredictions(project, modelId = modelID, datasetId = testSet$id)



GetProject(projectId)

results <- as.data.frame(ListModels(project))
results

# DataRobot best model
projectId <- "6168e3e1a48d2583ecb446d9"
modelId <- "61690f728b9680420063041f"
GetModel(projectId, modelId)

#bestModel <- GetRecommendedModel(project)
bestModel <- GetModel(projectId, modelId)


bestPredictions <- Predict(bestModel, testSet, type = "probability")

boxplot(bestPredictions)

dataRobotPreds= data.frame(bestPredictions)


str(dataRobotPreds)
names(dataRobotPreds)= 'Prediction'

# Merge with preds
unsubTestPredsDF= cbind(dataRobotPreds, testSet)
library(pROC)
roc(unsubTestPredsDF$Unsubscribe, unsubTestPredsDF$Prediction,
    smoothed = TRUE,
    # arguments for ci
    ci=TRUE, ci.alpha=0.9, stratified=FALSE,
    # arguments for plot
    plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
    print.auc=TRUE, show.thres=TRUE)





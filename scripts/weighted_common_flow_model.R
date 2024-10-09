AppPermPairModel = read.csv("AppPermPairModel.csv")
AppPermPairTest = read.csv("AppPermPairTest.csv")
Topics = read.csv("Topics.csv")
DominantTopics = read.csv("DominantTopics.csv")
PermPairMap = read.csv("PermPairMap.csv")

TopicsFDroid = read.csv("TopicsFDroid.csv")

# 1. Creating the model, with topics as rows and pairs as columns
pairs.length = nrow(PermPairMap)
topics.length = 32

flows.for.weighted.topic.model = data.frame(matrix(data=0, nrow=topics.length, ncol = pairs.length))
row.names(flows.for.weighted.topic.model) = paste("X", row.names(flows.for.weighted.topic.model), sep="")

#topic = unique(Topics$TopicID)[1]
for (topic in unique(Topics$TopicID) ){
  apps = Topics$AppName[Topics$TopicID==topic & Topics$TopicPos==1]
 
  # an.app = apps[1]
  for (an.app in apps){
    an.app.tring = as.character(an.app)
    pairs = AppPermPairModel$PermPairID [AppPermPairModel$App == an.app.tring]
    for (a.pair in pairs){
      old = flows.for.weighted.topic.model[as.character(topic), a.pair]
      flows.for.weighted.topic.model[as.character(topic), a.pair] = old +1
      rm(old)
    }
    rm(a.pair)
    rm(pairs, an.app.tring)
    }
  rm(an.app,apps)
  TOT = sum(flows.for.weighted.topic.model[as.character(topic),])
  old = flows.for.weighted.topic.model[as.character(topic),]
  old = old/TOT
  flows.for.weighted.topic.model[as.character(topic),] = old
  rm(TOT,old)
}
rm(topic)

# 2. Using the mode for classifying AUT
AnomalousApp.app = c()
AnomalousApp.flow = c()
AnomalousApp.model = c()
for (app in unique(TopicsFDroid$AppName)){
  aut = as.character(app)
    # extract the topics probabilties TP.AUT from the AUT
    TP.AUT = subset(TopicsFDroid, subset=(TopicsFDroid$AppName==aut))
    TP.AUT = TP.AUT[, c("TopicID","TopicProbabiity")]
    
    # extract the flows FLOWS.AUT from the AUT
    if (length(grep(aut, AppPermPairTest$App))==0) {
      next
    }
    aut.flows = AppPermPairTest$PermPairID[grep(aut, AppPermPairTest$App)]
    
    
    # extract the lines MODELS from the model, with the flows for topics TP.AUT
    # cumpute the weighetd MODEL = SUM(TP.AUT[i]*MODEL[i])
    model = rep(0, ncol(flows.for.weighted.topic.model))
    for(topic in TP.AUT$TopicID){
      topic.str = as.character(topic)
      a.line = flows.for.weighted.topic.model[topic.str,]
      weight = TP.AUT$TopicProbabiity[TP.AUT$TopicID==topic.str]
      contribution = a.line * weight
      model = model + contribution
      rm(topic.str, a.line, weight, contribution)
    }
    
    # compute the Threashold TR for the MODEL line
    threshold = 0
    vals = model
    vals = vals[vals>0]
    if (length(vals)>0){
      b= boxplot(vals, plot=FALSE)
      threshold = b$stats[1]
      rm(b)  
    }
    rm(vals)
  
    # if for an index pair P we have that 
    #        FLOWS.AUT[P]=1 AND MODEL[P]=0  OR
    #        FLOWS.AUT[P]=1 AND MODEL[P]=X  AND X<TR
    # than there is an anomaly
    
    for(i in aut.flows){
      if (model[i] == 0) {
        AnomalousApp.app = c(AnomalousApp.app, aut)
        AnomalousApp.flow = c(AnomalousApp.flow, i)
        AnomalousApp.model = c(AnomalousApp.model, model[i])
      }
      else {
        if (model[i]<threshold){
          AnomalousApp.app = c(AnomalousApp.app, aut)
          AnomalousApp.flow = c(AnomalousApp.flow, i)
          AnomalousApp.model = c(AnomalousApp.model, model[i])
        }
      }
    }
}

AnomalousApp = data.frame( cbind(AppName = AnomalousApp.app, PermPairID = AnomalousApp.flow, Model=AnomalousApp.model) )
rm(AnomalousApp.app, AnomalousApp.flow, AnomalousApp.model)

AnomalousAppMatrix = as.matrix(AnomalousApp)
write.csv(AnomalousAppMatrix, file="AnomalousApp_weighted_flow_model.csv", row.names=FALSE)

##########################


AppPermPairModel = read.csv("AppPermPairModel.csv")
AppPermPairTest = read.csv("AppPermPairTest.csv")
Topics = read.csv("Topics.csv")
DominantTopics = read.csv("DominantTopics.csv")
PermPairMap = read.csv("PermPairMap.csv")

TopicsFDroid = read.csv("TopicsFDroid.csv")

# 1. Creating the model flows.for.dominant.topic.model, with topics as rows and pairs as columns
pairs.length = nrow(PermPairMap)
topics.length = 32

flows.for.dominant.topic.model = data.frame(matrix(data=0, nrow=topics.length, ncol = pairs.length))
row.names(flows.for.dominant.topic.model) = paste("X", row.names(flows.for.dominant.topic.model), sep="")

for (topic in unique(Topics$TopicID) ){
  apps = Topics$AppName[Topics$TopicID==topic & Topics$TopicPos==1]
  
  for (an.app in apps){
    an.app.tring = as.character(an.app)
    pairs = AppPermPairModel$PermPairID [AppPermPairModel$App == an.app.tring]
    flows.for.dominant.topic.model[as.character(topic), pairs] = 1
    rm(pairs, an.app.tring)
  }
  rm(an.app,apps)
}
rm(topic)

# 2. Using the mode for classifying AUT

AnomalousApp.app = c()
AnomalousApp.flow = c()
for (app in unique(TopicsFDroid$AppName)) {
  # extract the dominant topic DT.AUT from the AUT
  aut = as.character(app)
  aut.dt = as.character(TopicsFDroid$TopicID[TopicsFDroid$AppName==aut & TopicsFDroid$TopicPos==1])
  
  # extract the flows FLOWS.AUT from the AUT
  #aut.flows = AppPermPairTest$PermPairID[AppPermPairTest$App == aut]
  aut.flows = AppPermPairTest$PermPairID[grep(aut, AppPermPairTest$App)]
  
  # extract the line MODEL from the model, with the flows for topic DT.AUT
  model = flows.for.dominant.topic.model[aut.dt, ]
  
  # if for an index pair P we have that 
  #        FLOWS.AUT[P]=1 AND MODEL[P]=0   than there is an anomaly
  for(i in aut.flows){
    if (model[i] == 0) {
      AnomalousApp.app = c(AnomalousApp.app, aut)
      AnomalousApp.flow = c(AnomalousApp.flow, i)
    }
  }
  rm(i)
  rm(aut, aut.dt, aut.flows)
}

AnomalousApp = data.frame( cbind(AppName = AnomalousApp.app, PermPairID = AnomalousApp.flow) )
rm(AnomalousApp.app, AnomalousApp.flow)

write.csv(AnomalousApp, file="AnomalousApp_new_flow_model.csv", row.names=FALSE)

##########################


#res = rep(0, nrow(flows.for.dominant.topic.model))
#for(row in 1:nrow(flows.for.dominant.topic.model)){
#  res[row] = sum(flows.for.dominant.topic.model[row,])
#}  

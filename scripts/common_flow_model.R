AppPermPairModel = read.csv("AppPermPairModel.csv")
AppPermPairTest = read.csv("AppPermPairTest.csv")
Topics = read.csv("Topics.csv")
DominantTopics = read.csv("DominantTopics.csv")
PermPairMap = read.csv("PermPairMap.csv")

TopicsFDroid = read.csv("TopicsFDroid.csv")

# 1. Creating the model, with topics as rows and pairs as columns
pairs.length = nrow(PermPairMap)
topics.length = 32

flows.for.common.topic.model = data.frame(matrix(data=0, nrow=topics.length, ncol = pairs.length))
row.names(flows.for.common.topic.model) = paste("X", row.names(flows.for.common.topic.model), sep="")

#topic = unique(Topics$TopicID)[1]
for (topic in unique(Topics$TopicID) ){
  apps = Topics$AppName[Topics$TopicID==topic & Topics$TopicPos==1]
 
  # an.app = apps[1]
  for (an.app in apps){
    an.app.tring = as.character(an.app)
    pairs = AppPermPairModel$PermPairID [AppPermPairModel$App == an.app.tring]
    for (a.pair in pairs){
      old = flows.for.common.topic.model[as.character(topic), a.pair]
      flows.for.common.topic.model[as.character(topic), a.pair] = old +1
      rm(old)
    }
    rm(a.pair)
    rm(pairs, an.app.tring)
  }
  rm(an.app,apps)
}
rm(topic)

model.threshold = rep(NA,length(row.names(flows.for.common.topic.model)))
names(model.threshold) = row.names(flows.for.common.topic.model)

for (name in row.names(flows.for.common.topic.model)) {
  vals = as.numeric(flows.for.common.topic.model[name,])
  vals = vals[vals>0]
  if (length(vals)==0){
    model.threshold[name] = 0
  }
  else{
    b= boxplot(vals, plot=FALSE)
    model.threshold[name] = b$stats[1]
    rm(b)  
  }
  rm(vals)
}
rm(name)

# 2. Using the mode for classifying AUT

  # extract the dominant topic DT.AUT from the AUT

  # extract the flows FLOWS.AUT from the AUT
  #aut.flows = AppPermPairTest$PermPairID[AppPermPairTest$App == aut]

  # extract the line MODEL from the model, with the flows for topic DT.AUT
  # use the Threashold TR for the MODEL line

  # if for an index pair P we have that 
  #        FLOWS.AUT[P]=1 AND MODEL[P]=0  OR
  #        FLOWS.AUT[P]=1 AND MODEL[P]=X  AND X<TR
  # than there is an anomaly

##########################


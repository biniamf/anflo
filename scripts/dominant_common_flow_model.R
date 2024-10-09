AppPermPairModel = read.csv("AppPermPairModel.csv")
AppPermPairMalware = read.csv("AppPermPairMalware.csv")
AppPermPairTest = read.csv("AppPermPairTest.csv")
Topics = read.csv("Topics.csv")
DominantTopics = read.csv("DominantTopics.csv")
PermPairMap = read.csv("PermPairMap.csv")

MalwareTopics = read.csv("MalwareTopics.csv")
TopicsFDroid = read.csv("TopicsFDroid.csv")
GeneralTopics = read.csv("GeneralTopics.csv")

## for malware analysis
TopicsFDroid = MalwareTopics
AppPermPairTest = AppPermPairMalware
####

# 1. Creating the model, with topics as rows and pairs as columns
pairs.length = nrow(PermPairMap)
topics.length = 32

flows.for.dominant.topic.model = data.frame(matrix(data=0, nrow=topics.length, ncol = pairs.length))
row.names(flows.for.dominant.topic.model) = paste("X", row.names(flows.for.dominant.topic.model), sep="")

for (topic in unique(Topics$TopicID) ){
  apps = Topics$AppName[Topics$TopicID==topic & Topics$TopicPos==1]
  
  # an.app = apps[1]
  for (an.app in apps){
    an.app.tring = as.character(an.app)
    pairs = AppPermPairModel$PermPairID [AppPermPairModel$App == an.app.tring]
    for (a.pair in pairs){
      old = flows.for.dominant.topic.model[as.character(topic), a.pair]
      flows.for.dominant.topic.model[as.character(topic), a.pair] = old +1
      rm(old)
    }
    rm(a.pair)
    rm(pairs, an.app.tring)
  }
  rm(an.app,apps)
}
rm(topic)

# 2. Using the mode for classifying AUT

AnomalousApp.app = c()
AnomalousApp.flow = c()
AnomalousApp.model = c()
# app = unique(TopicsFDroid$AppName)[2]
for (app in unique(TopicsFDroid$AppName)) {
  # extract the dominant topic DT.AUT from the AUT
  aut = as.character(app)
  aut.dt = as.character(TopicsFDroid$TopicID[TopicsFDroid$AppName==aut & TopicsFDroid$TopicPos==1])
  
  # extract the flows FLOWS.AUT from the AUT
  #aut.flows = AppPermPairTest$PermPairID[AppPermPairTest$App == aut]
  aut.flows = AppPermPairTest$PermPairID[grep(aut, AppPermPairTest$App)]
  
  if (length(aut.flows) == 0) { 
    next
  }
  
  # extract the line MODEL from the model, with the flows for topic DT.AUT
  model = flows.for.dominant.topic.model[aut.dt, ]
  
  threshold = 0
  vals = model
  vals = vals[vals>0]
  if (length(vals)>0){
    b= boxplot(vals, plot=FALSE)
    threshold = b$stats[1]
    rm(b)  
  }
  rm(vals)
  
  for(i in aut.flows) {
      if (model[i]<threshold) {
            AnomalousApp.app = c(AnomalousApp.app, aut)
            AnomalousApp.flow = c(AnomalousApp.flow, i)
            AnomalousApp.model = c(AnomalousApp.model, model[i])
      }
  }
  
}

AnomalousApp = data.frame( cbind(AppName = AnomalousApp.app, PermPairID = AnomalousApp.flow) )
rm(AnomalousApp.app, AnomalousApp.flow, AnomalousApp.model, model, app, aut, aut.dt, aut.flows,i,pairs.length,topics.length,threshold)

App = c()
AppTopic = c()
Source = c()
Sink = c()
Status = c()
# i = 1
for (i in 1:nrow(AnomalousApp)) {
  
  anom.app = AnomalousApp[i,]
  
  App = c(App, as.character(anom.app$AppName))
  
  fdroid.dominant.topic = as.character(TopicsFDroid$TopicID[as.character(TopicsFDroid$AppName) == as.character(anom.app$AppName) & TopicsFDroid$TopicPos == 1])
  
  topic = as.character(GeneralTopics$topics[GeneralTopics$colm == fdroid.dominant.topic])
  
  if (topic == "-"){
    topic = fdroid.dominant.topic
  }
    
  AppTopic = c(AppTopic, topic)
  
  source = as.character(PermPairMap$SourcePerm[PermPairMap$PermPairID == anom.app$PermPairID])
  source = gsub("android.permission.","",source)
  Source = c(Source, gsub("POSSIBLE_","",source))
  
  sink = as.character(PermPairMap$SinkPerm[PermPairMap$PermPairID == anom.app$PermPairID])
  sink = gsub("android.permission.","", sink)
  Sink = c(Sink, gsub("POSSIBLE_","", sink))
  
  Status = c(Status, "")
}

AnomAppPermFlow = data.frame(cbind(AppName = App, Topic = AppTopic, SourcePerm = Source, SinkPerm = Sink, AVName=Status))
rm(i,App,source, sink,Source,Sink,Status, anom.app,AppTopic)
library(xtable)
anom.tbl<-xtable(AnomAppPermFlow, caption="Malware anomalous information flows based on Dominant Topic model")
align(anom.tbl) <- "llllll"
tbl=print(anom.tbl, include.rownames = FALSE, floating = TRUE, floating.environment = "table*")

write(tbl,file="data/malwaredominantflow.tex")


AnomalousAppMatrix = as.matrix(AnomalousApp)
write.csv(AnomalousAppMatrix, file="AnomalousApp_dominant_threshold_flow_model.csv", row.names=FALSE)
  
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


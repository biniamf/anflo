## for the entire apps in the model


AppPermPairModel = read.csv("AppPermPairModel.csv")
AppPermPairTest = read.csv("AppPermPairTest.csv")

DominantTopics = read.csv("DominantTopics.csv")
PermPairMap = read.csv("PermPairMap.csv")

TopicsFDroid = read.csv("TopicsFDroid.csv")
GeneralTopics = read.csv("GeneralTopics.csv")

pairs.length = nrow(PermPairMap)
all.length = length(unique(AppPermPairModel$App))

flows.for.all.model = data.frame(matrix(data=0, nrow=all.length, ncol = pairs.length))
row.names(flows.for.all.model) = paste("App", row.names(flows.for.all.model), sep="")

Lookup <- data.frame(unique(AppPermPairModel$App))
row.names(Lookup) = paste("App", row.names(Lookup), sep="")
colnames(Lookup) = c("App")

apps = unique(AppPermPairModel$App)
  
# an.app = apps[1]
for (an.app in apps){
  an.app.string = as.character(an.app)
  pairs = AppPermPairModel$PermPairID [AppPermPairModel$App == an.app.string]
  for (a.pair in pairs){
    old = flows.for.all.model [rownames(Lookup)[Lookup$App == an.app.string], a.pair]
    flows.for.all.model [rownames(Lookup)[Lookup$App == an.app.string], a.pair] = old +1
    rm(old)
  }
  rm(a.pair)
  rm(pairs, an.app.string)
}
rm(an.app,apps)

model = data.frame(matrix(data=0, 1, ncol = pairs.length))
for (i in 1:pairs.length) {
  TOT = sum( flows.for.all.model[,i])
  model[i] = TOT
}

threshold = 0
vals = model
vals = vals[vals>0]
if (length(vals)>0){
  b= boxplot(vals, plot=FALSE)
  threshold = b$stats[1]
  rm(b)  
}
rm(vals)
## 2

AnomalousApp.app = c()
AnomalousApp.flow = c()
AnomalousApp.model = c()
app = unique(AppPermPairTest$App)[1]
for (app in unique(AppPermPairTest$App)) {
  # extract the dominant topic DT.AUT from the AUT
  aut = as.character(app)
  
  aut.flows = AppPermPairTest$PermPairID[grep(aut, AppPermPairTest$App)]
  
  for(i in aut.flows) {
    if (model[i]<threshold) {
      AnomalousApp.app = c(AnomalousApp.app, aut)
      AnomalousApp.flow = c(AnomalousApp.flow, i)
      AnomalousApp.model = c(AnomalousApp.model, model[i])
    }
  }
  
}

AnomalousApp = data.frame( cbind(AppName = AnomalousApp.app, PermPairID = AnomalousApp.flow, Model=AnomalousApp.model) )
rm(AnomalousApp.app, AnomalousApp.flow, AnomalousApp.model)

App = c()
AppTopic = c()
Source = c()
Sink = c()
Status = c()
#i = 2
for (i in 1:nrow(AnomalousApp)) {
 
  anom.app = AnomalousApp[i,]
 
  indexOf_ = which(strsplit(as.character(anom.app$AppName), "")[[1]]=="_")[1]
  
  appName = substr(as.character(anom.app$AppName), 1, indexOf_-1)
  appName = as.character(TopicsFDroid$AppName[grep(appName, TopicsFDroid$AppName)[1]])
  
  if (is.na(appName)) {
    next
  }
  
  App = c(App, appName)
  
  fdroid.dominant.topic = as.character(TopicsFDroid$TopicID[as.character(TopicsFDroid$AppName) == appName & TopicsFDroid$TopicPos == 1])
  
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
  
  # Source = c(Source, as.character(PermPairMap$SourcePerm[PermPairMap$PermPairID == AnomalousApp[i,]$PermPairID]))
  # Sink = c(Sink, as.character(PermPairMap$SinkPerm[PermPairMap$PermPairID == AnomalousApp[i,]$PermPairID]))
  Status = c(Status, "")
}

AnomAppPermFlow = data.frame(cbind(AppName = App, Topic = AppTopic, SourcePerm = Source, SinkPerm = Sink, AVName=Status))
rm(i,App,source, sink,Source,Sink,Status, anom.app,AppTopic,appName,indexOf_)

anom.tbl<-xtable(AnomAppPermFlow, caption="Malware anomalous information flows without considering topics")
align(anom.tbl) <- "llllll"
tbl=print(anom.tbl, include.rownames = FALSE, floating = TRUE, floating.environment = "table*")
write(tbl,file="data/malwarewholeappmodel.tex")


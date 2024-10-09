AppPermPairModel = read.csv("AppPermPairModel.csv")
AppPermPairTest = read.csv("AppPermPairTest.csv")
DominantTopics = read.csv("DominantTopics.csv")
PermPairMap = read.csv("PermPairMap.csv")

googleplay_app_new_cat = read.csv("googleplay_cats_model.csv")
fdroid_cats_test = read.csv("fdroid_app_old_cat.csv")

# 1. Creating the model flows.for.dominant.topic.model, with topics as rows and pairs as columns
pairs.length = nrow(PermPairMap)
topics.length = length(unique(googleplay_app_new_cat$CAT))

flows.for.same.category.model = data.frame(matrix(data=0, nrow=topics.length, ncol = pairs.length))
row.names(flows.for.same.category.model) = unique(googleplay_app_new_cat$CAT)

rm(pairs.length,topics.length)

# category = unique(googleplay_cats_model$CAT)[1]
for (category in unique(googleplay_app_new_cat$CAT) ){
  apps = googleplay_app_new_cat$APP[googleplay_app_new_cat$CAT==category]
  
  # an.app = apps[1]
  for (an.app in apps){
    an.app.string = as.character(an.app)
    pairs = AppPermPairModel$PermPairID [AppPermPairModel$App == an.app.string]
    for (a.pair in pairs){
      old =  flows.for.same.category.model[as.character(category), pairs]
      flows.for.same.category.model[as.character(category), pairs] = old + 1
      rm(old)
    }
    rm(a.pair,pairs, an.app.string)
  }     
  rm(an.app,apps)
}
rm(category)

# 2. Using the mode for classifying AUT

AnomalousApp.app = c()
AnomalousApp.flow = c()
AnomalousApp.cat = c()
AnomalousApp.model = c()
# app = unique(fdroid_cats_test$APP)[1]
for (app in unique(fdroid_cats_test$APP)) {
  # extract the dominant topic DT.AUT from the AUT
  aut = as.character(app)
  aut.dt = as.character(fdroid_cats_test$CAT[fdroid_cats_test$APP==aut])
  
  # extract the flows FLOWS.AUT from the AUT
  #aut.flows = AppPermPairTest$PermPairID[AppPermPairTest$App == aut]
  aut.flows = AppPermPairTest$PermPairID[grep(aut, AppPermPairTest$App)]
  
  # extract the line MODEL from the model, with the flows for topic DT.AUT
  model = flows.for.same.category.model[aut.dt, ]
  print(aut.dt)
  
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
  #        FLOWS.AUT[P]=1 AND MODEL[P]=0   than there is an anomaly
  for(i in aut.flows){
    if (model[i] == 0) {
      AnomalousApp.app = c(AnomalousApp.app, aut)
      AnomalousApp.flow = c(AnomalousApp.flow, i)
      AnomalousApp.model = c(AnomalousApp.model, model[i])
      AnomalousApp.cat = c(AnomalousApp.cat, aut.dt)
    }
  }
  rm(i)
  rm(app,threshold, aut, aut.dt, aut.flows)
}

AnomalousApp = data.frame( cbind(AppName = AnomalousApp.app, PermPairID = AnomalousApp.flow, Category =  AnomalousApp.cat, Model=AnomalousApp.model) )
rm(AnomalousApp.app, AnomalousApp.flow,AnomalousApp.cat,AnomalousApp.model)

App = c()
AppTopic = c()
Source = c()
Sink = c()
Status = c()
for (i in 1:nrow(AnomalousApp)) {
  App = c(App, AnomalousApp[i,]$AppName)
  
  AppTopic = c(AppTopic, as.character(fdroid_cats_test$CAT[fdroid_cats_test$APP == AnomalousApp[i,]$AppName]))
  
  source =  as.character(PermPairMap$SourcePerm[PermPairMap$PermPairID == AnomalousApp[i,]$PermPairID])
  Source = c(Source, gsub("android.permission.","",source))
  
  sink = as.character(PermPairMap$SinkPerm[PermPairMap$PermPairID == AnomalousApp[i,]$PermPairID])
  Sink = c(Sink, gsub("android.permission.","",sink))
  
  Status = c(Status, "FP")
}

AnomAppPermFlow = data.frame(cbind(AppName = App, Category = AppTopic, SourcePerm = Source, SinkPerm = Sink, Status=Status))
rm(i,App,Source,Sink,Status, AppTopic)

anom.tbl<-xtable(AnomAppPermFlow, caption="Anomalous information flows based on Google Play categories")
align(anom.tbl) <- "llllll"
tbl=print(anom.tbl, include.rownames = FALSE, floating = TRUE, floating.environment = "table*")
write(tbl,file="data/googleplaycatflow.tex")


write.csv(AnomalousApp, file="AnomalousApp_same_cat_model.csv", row.names=FALSE)

##########################


#res = rep(0, nrow(flows.for.dominant.topic.model))
#for(row in 1:nrow(flows.for.dominant.topic.model)){
#  res[row] = sum(flows.for.dominant.topic.model[row,])
#}  

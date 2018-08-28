week8 = read.csv("bds/exerciseData/week08.csv")

week8 = week8[-(which(week8$id == "average")), ]

clusterData = week8 %>% 
  dplyr::select(id, starts_with("job"), salary, tenure, age)

png(filename = "bds/clusterImages/cluster2.png", width = 960, height = 960)
kmeans(x = scale(na.omit(clusterData[,-grepl("id", names(clusterData))])), centers = 2) %>% 
  fviz_cluster(., scale(na.omit(clusterData[,-grepl("id", names(clusterData))])))
dev.off()  

png(filename = "bds/clusterImages/cluster3.png", width = 960, height = 960)
kmeans(x = scale(na.omit(clusterData[,-grepl("id", names(clusterData))])), centers = 3) %>% 
  fviz_cluster(., scale(na.omit(clusterData[,-grepl("id", names(clusterData))])))
dev.off()

png(filename = "bds/clusterImages/cluster4.png", width = 960, height = 960)
kmeans(x = scale(na.omit(clusterData[,-grepl("id", names(clusterData))])), centers = 4) %>% 
  fviz_cluster(., scale(na.omit(clusterData[,-grepl("id", names(clusterData))])))
dev.off()

png(filename = "bds/clusterImages/cluster5.png", width = 960, height = 960)
kmeans(x = scale(na.omit(clusterData[,-grepl("id", names(clusterData))])), centers = 5) %>% 
  fviz_cluster(., scale(na.omit(clusterData[,-grepl("id", names(clusterData))])))
dev.off()

png(filename = "bds/clusterImages/cluster6.png", width = 960, height = 960)
kmeans(x = scale(na.omit(clusterData[,-grepl("id", names(clusterData))])), centers = 6) %>% 
  fviz_cluster(., scale(na.omit(clusterData[,-grepl("id", names(clusterData))])))
dev.off()
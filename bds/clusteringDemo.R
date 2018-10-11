library(fpc)

library(dplyr)

library(cluster)

library(factoextra)

library(mclust)


tobacco = read.csv("bds/brfss-prevalence-and-trends-data-tobacco-use-four-level-smoking-data-for-2011.csv")

tobacco = tobacco %>% 
  select(Smoke.everyday, Smoke.some.days) %>%
  mutate_all(scale)

athleteData = read.csv("bds/athlete_events.csv")

athleteData = athleteData %>% 
  filter(Sex == "M", Season == "Summer")

medalCount = athleteData %>% 
  mutate(medalCount = ifelse(is.na(Medal), 0, 1)) %>% 
  group_by(Name) %>% 
  summarize(total = sum(medalCount))

athleteClusterData = athleteData %>% 
  select(Name, Age, Weight, Height) %>% 
  distinct() %>%
  left_join(., medalCount, by = "Name") %>% 
  group_by(Name) %>% 
  arrange(Age) %>% 
  slice(n()) %>% 
  ungroup() %>% 
  select(-Name) %>% 
  mutate_at(vars("Age", "Height", "Weight", "total"), scale) %>%
  na.omit()

fviz_nbclust(tobacco, clara, method = "silhouette", 
             k.max = 5)

## Clara

claraTest = clara(tobacco, k = 4)

summary(claraTest)

par(mfrow = c(2, 2))

fviz_cluster(claraTest, choose.vars = c("Age", "Weight"))

fviz_cluster(claraTest, choose.vars = c("Age", "Height"))

fviz_cluster(claraTest, choose.vars = c("Height", "Weight"))

fviz_cluster(claraTest, choose.vars = c("total", "Age"))

fviz_cluster(claraTest, choose.vars = c("total", "Height"))

fviz_cluster(claraTest, choose.vars = c("total", "Weight"))

fviz_cluster(claraTest, stand = FALSE)

## Kmeans

kmeansTest = kmeans(tobacco, centers = 4)

kmeansTest

fviz_cluster(kmeansTest, data = athleteClusterData, choose.vars = c("Age", "Weight"))

fviz_cluster(kmeansTest, data = athleteClusterData, choose.vars = c("Age", "Height"))

fviz_cluster(kmeansTest, data = athleteClusterData, choose.vars = c("Height", "Weight"))

fviz_cluster(kmeansTest, data = athleteClusterData, choose.vars = c("total", "Age"))

fviz_cluster(kmeansTest, data = athleteClusterData, choose.vars = c("total", "Height"))

fviz_cluster(kmeansTest, data = athleteClusterData, choose.vars = c("total", "Weight"))

fviz_cluster(kmeansTest, data = tobacco)

## Model-based

bicTest = mclustBIC(tobacco)

bicTest

mclustTest = Mclust(athleteClusterData, G = 5, modelNames = "EEV")

summary(mclustTest, parameters = TRUE, classification = TRUE)

fviz_cluster(mclustTest, data = athleteClusterData, choose.vars = c("Age", "Weight"))

fviz_cluster(mclustTest, data = athleteClusterData, choose.vars = c("Age", "Height"))

fviz_cluster(mclustTest, data = athleteClusterData, choose.vars = c("Height", "Weight"))

fviz_cluster(mclustTest, data = athleteClusterData, choose.vars = c("total", "Age"))

fviz_cluster(mclustTest, data = athleteClusterData, choose.vars = c("total", "Height"))

fviz_cluster(mclustTest, data = athleteClusterData, choose.vars = c("total", "Weight"))

fviz_cluster(mclustTest, data = athleteClusterData)

## dbscan

dbscanTest = fpc::dbscan(data.matrix(tobacco), eps = .5, MinPts = 5)

fviz_cluster(dbscanTest, data = tobacco, geom = "point", shape = NULL)

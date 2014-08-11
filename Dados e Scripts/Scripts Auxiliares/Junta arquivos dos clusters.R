library(plyr)
clusters = rbind(clusters,cluster)


cluster = read.csv("7 _cluster.csv")
mean(cluster$count_sim.y)
mean(cluster$count_pop.y)
mean(cluster$media_pop.y)

cluster$cluster = 4
cluster$nome = "Especializados em seu estilo"

clusters = rename(clusters, c("count_sim.x"="fam_AT", "count_pop.x"="pop_AT","ecletic.x" = "ecletic","media_pop.x" = "media_pop",
                   "propNov.x" = "propNov"))

clusters = rename(clusters, c("count_sim.y"="fam_AT_norm", "count_pop.y"="pop_AT_norm","ecletic.y" = "ecletic_norm","media_pop.y" = "media_norm",
                              "propNov.y" = "propNov_norm"))
clusters$Row.names = NULL
write.csv(clusters,file="aspectos_ouvintes.csv")
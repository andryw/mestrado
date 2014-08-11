library(reshape)
centroides <- read.csv(file="centroides_comparacao.csv",head=TRUE,sep=",",)
colnames(centroides) <- c("familiaridade","popularidade")
centroides$cluster = row.names(centroides)


centroidesModificado <- melt(centroides, id=c("cluster"),row.names=FALSE)
centroidesModificado <- centroidesModificado[with(centroidesModificado, order(cluster)), ] 
centroidesModificado$row.names <- NULL
write.csv(centroidesModificado,"centroides_mod.csv")

centroidesModificado <- read.csv(file="centroides_mod.csv",head=TRUE,sep=",")
centroidesModificado$X <- NULL

centroidesModificado <- centroidesModificado[with(centroidesModificado, order(cluster)), ] 


centroidesModificado[,"nome_cluster"] <- NA
centroidesModificado[centroidesModificado$cluster == 1,]$nome_cluster = "Grupo 1"
centroidesModificado[centroidesModificado$cluster == 2,]$nome_cluster = "Grupo 6"
centroidesModificado[centroidesModificado$cluster == 3,]$nome_cluster = "Grupo 3"
centroidesModificado[centroidesModificado$cluster == 4,]$nome_cluster = "Grupo 4"
centroidesModificado[centroidesModificado$cluster == 5,]$nome_cluster = "Grupo 5"
centroidesModificado[centroidesModificado$cluster == 6,]$nome_cluster = "Grupo 2"

centroidesModificado <- centroidesModificado[with(centroidesModificado, order(cluster)), ] 



p <- ggplot(centroidesModificado, aes(variable,value)) + 
  geom_bar(stat="identity") + facet_wrap(~ nome_cluster,ncol = 3) + 
  xlab("") + ylab ("Diferença de correlações") + ylim(-0.4, 0.4) + coord_flip()
p

pdf("clusters_diferenca.pdf",width = 10, height = 3)

print(p)

dev.off()



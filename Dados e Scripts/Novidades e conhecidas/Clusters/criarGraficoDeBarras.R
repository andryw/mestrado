library(reshape)
library(ggplot2)
setwd("C:/Users/Andryw/Dropbox/Mestrado/Novos dados/mestrado/Dados e Scripts/Novidades e conhecidas")
centroides1 <- read.csv(file="Clusters/dados/centroides_conhecidas10.csv",head=TRUE,sep=",")
centroides1 = rename(centroides1, c("count_pop.x"="cor(pop, AT) - nov","count_pop.y" = "cor(pop, AT) - conhec",
                                    "count_sim.x"="cor(fam, AT) - nov", "count_sim.y"="cor(fam, AT) - conhec"))

centroidesModificado <- melt(centroides1, id=c("cluster"))


dado = subset(centroidesModificado,cluster >= 0)
dado$nome_cluster <- 
  factor(dado$nome_cluster, levels = 
           c(string1, string5, string8, string2, string6, string9, string3
             , string7, string10, string4))

old <- theme_set(theme_bw())

p <- ggplot(dado, aes(variable,value)) + geom_bar(stat="identity") +
  facet_wrap(~ nome_cluster ,ncol = 3) + xlab("") + 
  ylab ("Variáveis normalizadas (Z-Score)") + ylim(-2, 2) + coord_flip()
p

pdf("Clusters/pdfs/clusters_conhec.pdf",width = 9, height = 6)

print(p)

dev.off()

string1 = "1 - Averso a artistas diferentes"
string2 = "2 - Apreciador de surpresas"
string3 = "3 - Descobridores do mainstream"
string4 = "4 - Especializados em seu estilo"
string5 = "5 - Altamente eclético"
string6 = "6 - Exploradores I"
string7 = "7 - Exploradores II"
string8 = "8 - Underground"
string9 = "9 - Averso a artistas \n conhecidos diferentes"
string10 = "10 - Apreciador de artistas \n conhecidos não-usuais"

centroidesModificado[,"nome_cluster"] <- NA
centroidesModificado[centroidesModificado$cluster == 1,]$nome_cluster = string6
centroidesModificado[centroidesModificado$cluster == 2,]$nome_cluster = string8
centroidesModificado[centroidesModificado$cluster == 3,]$nome_cluster = string2
centroidesModificado[centroidesModificado$cluster == 4,]$nome_cluster = string5
centroidesModificado[centroidesModificado$cluster == 5,]$nome_cluster = string10
centroidesModificado[centroidesModificado$cluster == 6,]$nome_cluster = string9
centroidesModificado[centroidesModificado$cluster == 7,]$nome_cluster = string4
centroidesModificado[centroidesModificado$cluster == 8,]$nome_cluster = string7
centroidesModificado[centroidesModificado$cluster == 9,]$nome_cluster = string3
centroidesModificado[centroidesModificado$cluster == 10,]$nome_cluster = string1

library(reshape)
library(ggplot2)
path = ""
centroides1 <- read.csv(file="C:\\Users\\Andryw\\Dropbox\\Mestrado\\Novos dados\\Dados\\Criar Gráfico Bonito\\centroides1.csv",head=TRUE,sep=";")

centroidesModificado <- melt(centroides1, id=c("cluster"))
centroidesModificado <- centroidesModificado[with(centroidesModificado, order(cluster)), ] 

#write.csv(centroidesModificado,"centroides_mod.csv")

centroidesModificado <- read.csv(file="C:\\Users\\Andryw\\Dropbox\\Mestrado\\Novos dados\\Dados\\centroides_mod.csv",head=TRUE,sep=";")
centroidesModificado <- centroidesModificado[with(centroidesModificado, order(cluster,-variable)), ] 


dado = subset(centroidesModificado,cluster >= 0)
dado$nome_cluster <- 
  factor(dado$nome_cluster, levels = c(string1, string5, string2, string6, string3, string7, string4))
old <- theme_set(theme_bw())

p <- ggplot(dado, aes(variable,value)) + geom_bar(stat="identity") +
  facet_wrap(~ nome_cluster ,nrow = 4) + xlab("") + 
  ylab ("Variáveis normalizadas (Z-Score)") + ylim(-2, 2) + coord_flip()
p

pdf("C:\\Users\\Andryw\\Dropbox\\Mestrado\\1_Dissertação\\Modelos\\Modelo_LateX_Copin\\figs\\clusterstodos1.pdf",width = 7, height = 6)

print(p)

dev.off()

string1 = "1 - Averso a artistas diferentes"
string2 = "2 - Apreciador de surpresas"
string3 = "3 - Descobridores do mainstream"
string4 = "4 - Especializados em seu estilo"
string5 = "5 - Altamente eclético"
string6 = "6 - Exploradores"
string7 = "7 - Underground"

centroidesModificado[,"nome_cluster"] <- NA
centroidesModificado[centroidesModificado$cluster == 1,]$nome_cluster = string1
centroidesModificado[centroidesModificado$cluster == 2,]$nome_cluster = string2
centroidesModificado[centroidesModificado$cluster == 3,]$nome_cluster = string3
centroidesModificado[centroidesModificado$cluster == 4,]$nome_cluster = string4
centroidesModificado[centroidesModificado$cluster == 5,]$nome_cluster = string5
centroidesModificado[centroidesModificado$cluster == 6,]$nome_cluster = string6
centroidesModificado[centroidesModificado$cluster == 7,]$nome_cluster = string7



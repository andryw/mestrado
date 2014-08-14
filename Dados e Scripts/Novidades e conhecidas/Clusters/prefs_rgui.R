setwd("~/Dropbox/Mestrado/Novos dados/Dados/")#library(clv)
setwd("C:/Users/Andryw/Dropbox/Mestrado/Novos dados/Dados/")
library(plyr)
library(GGally)
library(moments)

library(fastcluster)
library(ggplot2)

clust.centroid = function(i, dat, clusters) {
  ind = (clusters == i)
  colMeans(dat[ind,])
}
clust.sd = function(i, dat, clusters) {
  ind = (clusters == i)
  apply(dat[ind,],2,sd)
}
plot_ward = function(data){
  d <- dist(data, method = "euclidean") 
  hc <- hclust(d^2, method="ward") 
  plot(hc)
  list(hc = hc)
}

plot_height = function(hc){
  hc_atual = hc
  treeSize=length(hc_atual$height)
  height=numeric(length=treeSize)
  
  for (i in 1:treeSize)
    height[i]=sqrt(hc_atual$height[treeSize-(i-1)])
  
  dadosx<-data.frame(x=2:20, y=height[2:20], group=1)
  p <- ggplot(dadosx, aes(x,y)) + geom_line(size=1)
  p <- p + xlab('\nNúmero de grupos') + ylab('Average within') 
  p <- p + scale_x_continuous(breaks=seq(2, 20, 1))
  p
}

tudo_junto = read.csv("tudo_junto.csv",head=TRUE,sep=",")
tudo_junto = tudo_junto[complete.cases(tudo_junto),]
COLUNAS = tudo_junto[c("count_sim.x","count_sim.y",  "count_pop.x","count_pop.y")]
COLUNAS_N = scale(COLUNAS)

COLUNAS_DIF = data.frame(count_sim = tudo_junto$count_sim.x -  tudo_junto$count_sim.y,
                         count_pop = tudo_junto$count_pop.x -  tudo_junto$count_pop.y)

hc = plot_ward(COLUNAS_DIF)$hc
plot(hc)
plot_height(hc)
hei = data.frame(hei = hc$height)
k_ = 6

for (k_ in 1:10){
  clusters <- cutree(hc, k = k_)
  
  
  centroides = sapply(unique(clusters), clust.centroid, COLUNAS_DIF, clusters)
  
  centroides = t(centroides)
  pdf(paste("RESULTADOS/prefs_2/2_bruto_",k_,".pdf",sep=""))
  par(mfrow=c(2,5))
  for (i in 1:k_) barplot(ylim = c(-0.4,0.4),centroides[i,][1:4],main=length(COLUNAS_DIF[(clusters == i),][,1]))
  dev.off()
}

  for (i in 1:6)length(COLUNAS_DIF[(clusters == i),][,1])


mydata = COLUNAS_DIF

centroidesFormatados <- function(hc,mydata,k_){
clusters <- cutree(hc, k = k_)
centroides = sapply(unique(clusters ), clust.centroid, mydata, clusters )
colnames(centroides ) <- list(paste("Centro", 1:k_))[[1]]
centroides = t(centroides )
colnames(centroides ) <- c("TA_fam","TA_pop")
centroides = round(centroides , digits=2)
centroides 

}

a = centroidesFormatados (hc,mydata,5)
b = centroidesFormatados (hc,mydata,6)
c = centroidesFormatados (hc,mydata,7)

write.csv(unique(rbind(b,c)),paste("centroides_old.csv"))

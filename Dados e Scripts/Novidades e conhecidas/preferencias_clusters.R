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
install.packages("fastcluster")
library(fastcluster)
library(ggplot2)
setwd("C:/Users/Andryw/Dropbox/Mestrado/Novos dados/Dados/")
setwd("~/mestrado/Dados e Scripts")
plot_height = function(hc){
  hc_atual = hc
  treeSize=length(hc_atual$height)
  height=numeric(length=treeSize)
  
  for (i in 1:treeSize)
    height[i]=sqrt(hc_atual$height[treeSize-(i-1)])
  
  dadosx<-data.frame(x=2:20, y=height[2:20], group=1)
  p <- ggplot(dadosx, aes(x,y)) + geom_line(size=1)
  p <- p + xlab('\nN?mero de grupos') + ylab('Dist?ncia m?dia dentro dos grupos') 
  p <- p + scale_x_continuous(breaks=seq(2, 20, 1))
  p
}

dados <- read.csv(file="tudo_junto.csv",head=TRUE,sep=",")
dados = dados[complete.cases(dados),]

dados$propNov = dados$news_total / (dados$news_total + dados$old_total)
drops = c("count_sim.x","count_pop.x","ecletic",'media_pop','propNov',"count_sim.y","count_pop.y")
mydata = dados[,drops]
mydata = data.frame(scale(mydata))
mydata = mydata[complete.cases(mydata),]

hc = plot_ward(mydata)$hc
plot_height(hc)

k_ = 8
clusters <- cutree(hc, k = k_)
centroides = sapply(unique(clusters), clust.centroid, mydata, clusters)
centroides = t(centroides)
par(mfrow=c(2,6))
for (i in 1:k_) barplot(centroides[i,][1:8],main=length(mydata[(clusters == i),][,1]))
title(titulo,outer=TRUE)
length(mydata[(clusters == i),][,1])
write.csv(centroides,file="C:\\Users\\Andryw\\Dropbox\\Mestrado\\Novos dados\\Dados\\Criar Gr?fico Bonito\\centroides_conhecidas10.csv")

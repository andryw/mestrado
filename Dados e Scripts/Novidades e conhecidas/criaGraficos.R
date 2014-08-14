library(ggplot2)
library(reshape)

theme_set(theme_bw())

setwd("C:/Users/Andryw/Dropbox/Mestrado/Novos dados/mestrado/Dados e Scripts/Novidades e conhecidas")

criaGrafico <- function(dados_filtrados,file, order_criteria){
  dados_filtrados = dados_filtrados[with(dados_filtrados, order(order_criteria) ) ,]
  dados_filtrados$user <- factor(dados_filtrados$user, levels=unique(dados_filtrados$user))
  dados_filtrados$dif = sign(dados_filtrados$pop_novid - dados_filtrados$pop_conhec)
  dados_filtrados = melt(dados_filtrados,id=c("user","dif"))
  
  p <- ggplot(data=dados_filtrados, aes(x=value, y=user, group = user, color = variable )) + 
    geom_vline(xintercept = 0) + 
    geom_line( alpha = 0.2,aes(color = dif)) + geom_point(size = 2, alpha = 0.4) + 
    theme(axis.text.x = element_blank()) + scale_y_discrete(breaks=NULL)+  
    coord_flip()
 # pdf(file,width = 18, height = 8)
  print(p)
#  dev.off()
}

file="pop_nov_con.pdf"
dados = read.csv("dados.csv",head=TRUE,sep=",")
dados$X = NULL
dados_filtrados = dados[,c("user", "pop_novid","pop_conhec")]


dados_filtrados = dados_filtrados[with(dados_filtrados, order(dados_filtrados$pop_novid - dados_filtrados$pop_conhec) ) ,]
xcept = which.min(abs(dados_filtrados$pop_novid-dados_filtrados$pop_conhec))

dados_filtrados$user <- factor(dados_filtrados$user, levels=unique(dados_filtrados$user))
dados_filtrados = melt(dados_filtrados,id=c("user"))


p <- ggplot(data=dados_filtrados, aes(x=value, y=user, group = user, color = variable )) + 
  geom_vline(xintercept = 0) +   geom_hline(yintercept = xcept) + 
  geom_line( alpha = 0.4) + geom_point(size = 1.25, alpha = 0.4) + 
  xlab('correlaçoes') + ylab('ouvintes') +
  theme(text=element_text(size=24), axis.text.x = element_blank()) + scale_y_discrete(breaks=NULL) + coord_flip()
 pdf(file,width = 18, height = 8)
print(p)
  dev.off()







file="fam_nov_con.pdf"
dados = read.csv("dados.csv",head=TRUE,sep=",")
dados$X = NULL
dados_filtrados = dados[,c("user", "fam_novid","fam_conhec")]


dados_filtrados = dados_filtrados[with(dados_filtrados, order(dados_filtrados$fam_novid - dados_filtrados$fam_conhec) ) ,]
xcept = which.min(abs(dados_filtrados$fam_novid-dados_filtrados$fam_conhec))

dados_filtrados$user <- factor(dados_filtrados$user, levels=unique(dados_filtrados$user))
dados_filtrados = melt(dados_filtrados,id=c("user"))


p <- ggplot(data=dados_filtrados, aes(x=value, y=user, group = user, color = variable )) + 
  geom_vline(xintercept = 0) +   geom_hline(yintercept = xcept) + 
  geom_line( alpha = 0.4) + geom_point(size = 1.25, alpha = 0.4) + 
  xlab('correlaçoes') + ylab('ouvintes') +
  theme(text=element_text(size=24), axis.text.x = element_blank()) + scale_y_discrete(breaks=NULL) + coord_flip()
pdf(file,width = 18, height = 8)
print(p)
dev.off()





setwd("~/mestrado/Dados e Scripts/Novidades e conhecidas")
setwd("C:/Users/Andryw/Dropbox/Mestrado/Novos dados/mestrado/Dados e Scripts/Novidades e conhecidas")

library(ggplot2)
library(reshape)

theme_set(theme_bw())


criaGrafico <- function(dados_filtrados,file, order_criteria){
 # dados_filtrados = dados_filtrados[with(dados_filtrados, order(order_criteria) ) ,]
  dados_filtrados$user <- factor(dados_filtrados$user, levels=unique(dados_filtrados$user))
  dados_filtrados = melt(dados_filtrados)
  
  p <- ggplot(data=dados_filtrados, aes(x=value, y=user, group = user, color = variable )) + 
    geom_vline(xintercept = 0) + 
    geom_line( alpha = 0.2) + geom_point(size = 1, alpha = 0.4) + 
    theme(axis.text.x = element_blank()) + scale_y_discrete(breaks=NULL)+  
    coord_flip()
  pdf(file,width = 18, height = 8)
  print(p)
  dev.off()
}


file="pop_divid.pdf"

dados = read.csv("dados.csv",head=TRUE,sep=",")
dados$X = NULL
dados_filtrados = dados[,c("user", "pop_novid","pop_conhec")]
dados_filtrados$sinal_novid = sign(dados_filtrados$pop_novid)
dados_filtrados[dados_filtrados$sinal_novid == 0,]$sinal_novid <- 1
dados_filtrados$sinal_conhec = sign(dados_filtrados$pop_conhec)
dados_filtrados[dados_filtrados$sinal_conhec == 0,]$sinal_conhec <- 1
dados_filtrados$sinal_dif = sign(dados_filtrados$pop_novid - dados_filtrados$pop_conhec)
dados_filtrados[dados_filtrados$sinal_dif == 0,]$sinal_dif <- 1

dados_filtrados = dados_filtrados[with(dados_filtrados, order(dados_filtrados$sinal_dif,dados_filtrados$sinal_novid, dados_filtrados$sinal_conhec, dados_filtrados$pop_novid - dados_filtrados$pop_conhec)),]


dados_filtrados$user <- factor(dados_filtrados$user, levels=unique(dados_filtrados$user))
dados_filtrados = melt(dados_filtrados,id=c("user","sinal_novid","sinal_conhec","sinal_dif"))

p <- ggplot(data=dados_filtrados, aes(x=value, y=user, group = user, color = variable )) + 
  geom_vline(xintercept = 0) + 
  geom_line( alpha = 0.4) + geom_point(size = 1.25, alpha = 0.4) + 
  xlab('correlaçoes') + ylab('ouvintes') +
  
  theme(text=element_text(size=24),axis.text.x = element_blank()) + scale_y_discrete(breaks=NULL)+  
  facet_wrap( ~ sinal_dif + sinal_novid + sinal_conhec  )   + coord_flip()

pdf(file,width = 16, height = 10)
print(p)
dev.off()









file="fam_divid.pdf"
dados = read.csv("dados.csv",head=TRUE,sep=",")
dados$X = NULL
dados_filtrados = dados[,c("user", "fam_novid","fam_conhec")]
dados_filtrados$sinal_novid = sign(dados_filtrados$fam_novid)
dados_filtrados[dados_filtrados$sinal_novid == 0,]$sinal_novid <- 1
dados_filtrados$sinal_conhec = sign(dados_filtrados$fam_conhec)
dados_filtrados[dados_filtrados$sinal_conhec == 0,]$sinal_conhec <- 1
dados_filtrados$sinal_dif = sign(dados_filtrados$fam_novid - dados_filtrados$fam_conhec)
dados_filtrados[dados_filtrados$sinal_dif == 0,]$sinal_dif <- 1

dados_filtrados = dados_filtrados[with(dados_filtrados, order(dados_filtrados$sinal_dif,dados_filtrados$sinal_novid, dados_filtrados$sinal_conhec, dados_filtrados$fam_novid - dados_filtrados$fam_conhec)),]

dados_filtrados$user <- factor(dados_filtrados$user, levels=unique(dados_filtrados$user))
dados_filtrados2 = melt(dados_filtrados,id=c("user","sinal_novid","sinal_conhec","sinal_dif"))

#dados_filtrados2$sinal_dif = factor(dados_filtrados2$sinal_dif, levels = c("a","b"))

p <- ggplot(data=dados_filtrados2, aes(x=value, y=user, group = user, color = variable )) + 
  geom_vline(xintercept = 0) + 
  geom_line( alpha = 0.4) + geom_point(size = 1.25, alpha = 0.4) + 
  xlab('correlaçoes') + ylab('ouvintes') +

  theme(text=element_text(size=24),axis.text.x = element_blank()) + scale_y_discrete(breaks=NULL)+  
  facet_wrap( ~ sinal_dif + sinal_novid + sinal_conhec)   + coord_flip()

pdf(file,width = 16, height = 10)
print(p)
dev.off()













criaGrafico(dados_filtrados,"pop-orderby-pop_conhec.pdf", dados_filtrados$pop_conhec)
criaGrafico(dados_filtrados,"pop-orderby-pop_novid.pdf", dados_filtrados$pop_novid)
criaGrafico(dados_filtrados,"a1.pdf", a)

dados = dados[with(dados, order(fam_novid - fam_conhec) ) ,]
dados_filtrados = dados[,c("user","fam_novid","fam_conhec")]
criaGrafico(dados_filtrados,"fam3.pdf")

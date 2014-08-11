library(ggplot2)
library(reshape)


criaGrafico <- function(dados_filtrados,file,salva){
  dados_filtrados$user <- factor(dados_filtrados$user, levels=unique(dados_filtrados$user))
  dados_filtrados = melt(dados_filtrados)
  
  p = ggplot(data=dados_filtrados, aes(x=value, y=user,group = user, color = variable )) + 
    geom_line() + geom_point() + geom_vline(xintercept = 0)
  p
  
  if(salva){
    pdf(file,width = 15, height = 13)
    
    print(p)
    
    dev.off()
  }

  
}



dados = read.csv("dados.csv",head=TRUE,sep=",")
dados$X = NULL

dados = dados[with(dados, order(pop_novid) ) ,]
dados_filtrados = dados[,c("user","pop_novid","pop_conhec")]
criaGrafico(dados_filtrados,"pop3.pdf",FALSE)


dados = dados[with(dados, order(-fam_conhec) ) ,]
dados_filtrados = dados[,c("user","fam_novid","fam_conhec")]
p = criaGrafico(dados_filtrados,"fam_-c.pdf",TRUE)



dados_filtrados$user <- factor(dados_filtrados$user, levels=unique(dados_filtrados$user))
dados_filtrados = melt(dados_filtrados)

p = ggplot(data=dados_filtrados, aes(x=value, y=user,group = user, color = variable )) + 
  geom_line() + geom_point() + geom_vline(xintercept = 0)
p

setwd("C:/Users/Andryw/Dropbox/Mestrado/Novos dados/Dados/")
setwd("~/mestrado/Dados e Scripts/Novidades e conhecidas")
library(ggplot2)
library(reshape)
tudo_junto = read.csv("dados.csv",head=TRUE,sep=",")
tudo_junto = tudo_junto[complete.cases(tudo_junto),]

#tudo_junto = rename(tudo_junto, c("count_pop.x"="pop_novid", "count_pop.y"="pop_conhec"))
#tudo_junto = rename(tudo_junto, c("count_sim.x"="fam_novid", "count_sim.y"="fam_conhec"))

tudo_junto = tudo_junto[with(tudo_junto, order(pop_novid + pop_conhec) ) ,]
tudo_junto1 = tudo_junto[1:10207,c("user","pop_novid","pop_conhec")]
criaGrafico(tudo_junto1,"pop2.pdf",which.min(abs(tudo_junto1$pop_novid-tudo_junto1$pop_conhec)))

tudo_junto = tudo_junto[with(tudo_junto, order(fam_novid + fam_conhec) ) ,]
tudo_junto1 = tudo_junto[1:10207,c("user","fam_novid","fam_conhec")]
criaGrafico(tudo_junto1,"fam2.pdf",which.min(abs(tudo_junto1$fam_novid-tudo_junto1$fam_conhec)))

criaGrafico <- function(tudo_junto1,file,inter){
  tudo_junto1$user <- factor(tudo_junto1$user, levels=unique(tudo_junto1$user))
  tudo_junto1 = melt(tudo_junto1)
  
  p = ggplot(data=tudo_junto1, aes(x=value, y=user,group = user, color = variable )) + 
    geom_line() + geom_point() + geom_vline(xintercept = 0)  + geom_hline(yintercept = inter)
  p
  
  pdf(file,width = 15, height = 13)
  
  print(p)
  
  dev.off()
  
}



 # total_bill   tip    sex smoker  day   time size
#      16.99  1.01 Female     No  Sun Dinner    2
#      10.34  1.66   Male     No  Sun Dinner    3
#      21.01  3.50   Male     No  Sun Dinner    3
#  ... <244 total rows> ...
#      22.67  2.00   Male    Yes  Sat Dinner    2
#      17.82  1.75   Male     No  Sat Dinner    2
#      18.78  3.00 Female     No Thur Dinner    2

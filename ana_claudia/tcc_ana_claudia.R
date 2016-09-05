# TCC Ana Claudia
# Script: Neylson Crepalde

library(gdata)
library(descr)
library(magrittr)
library(xtable)
library(ggplot2)

dados <- read.xls("~/Documentos/Neylson Crepalde/Izabela Hendrix/Silas/tcc tabulacao-38.xlsx",
                  skip=1, stringsAsFactors=F, fileEncoding="Latin1")
#View(dados)
names(dados) <- c("Entrevistado","idade","sexo","escolaridade","materias","quanto.tempo",
                  "motivo","receptividade.professores","satisfeito.conteudo","tempo.suf",
                  "TPR","ensino","quantidade","result1","result2","result3","result4",
                  "material","efic.metodo","feedback","dominio.conteudo",
                  "conteudo.outras.areas","melhorar","canc.prop.aprend","canc.estagios",
                  "canc.folhas","canc.preco","canc.local","canc.prof","opiniao")
summary(dados)
# Idade
dados$idade %<>% as.factor
levels(dados$idade) <- c("De 18 a 20", "De 21 a 30", "De 31 a 40","De 41 a 50", 
                         "Acima de 50")
freq(dados$idade)
xtable(freq(dados$idade), plot=F)
#sexo não precisa pelo desenho amostral 50 50

# Escolaridade
dados$escolaridade %<>% as.factor
levels(dados$escolaridade) <- c("EF completo", "EM completo","EM incompleto",
                                "ES completo","ES incompleto","Outros",
                                "EF incompleto")
freq(dados$escolaridade, plot = F) %>% xtable

# Matérias
dados$materias %<>% as.factor
levels(dados$materias) <- c("Portuguès","Matemática","Inglês","Português + Matemática",
                            "Português + Inglês","Matemática + Inglês",
                            "Português + Matemática + Inglês")
freq(dados$materias) %>% xtable
ggplot(dados, aes(reorder(materias, materias, length)))+
  geom_bar()+coord_flip()+labs(x="",y="")+theme_bw(base_size = 14)

# Quanto tempo
dados$quanto.tempo %<>% as.factor
levels(dados$quanto.tempo) <- c("De 0 a 1 ano","De 2 a 3 anos","De 4 a 5 anos",
                                "Mais de 5 anos")
freq(dados$quanto.tempo) %>% xtable

# Motivo
dados$motivo %<>% as.factor
levels(dados$motivo) <- c("Dificuldade de acompanhamento na escola regular",
                          "Rever conteúdos que tive maior dificuldade na época escolar",
                          "Estudar para concurso","Outros")
freq(dados$motivo) %>% xtable

# PARTE II
#==============
# Parei aqui!!!
freq(dados $receptividade.professores)


freq(dados[[8]])
freq(dados[[9]])
freq(dados[[10]])

freq(dados[[30]])

library(ggplot2)
ggplot(data=dados, aes(x=idade))+geom_bar()

# Análises
#####################################
tab.contingencia <- table(dados$escolaridade, dados$efic.metodo)

chisq.test(tab.contingencia)
cv.test = function(x,y) {
  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
              (length(x) * (min(length(unique(x)),length(unique(y))) - 1)))
  print.noquote("Cramér V / Phi:")
  return(as.numeric(CV))
}
cor.test(dados$efic.metodo, dados$idade)


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
freq(dados$materias, plot=F) %>% xtable
ggplot(dados, aes(reorder(materias, materias, length)))+
  geom_bar()+coord_flip()+labs(x="",y="")+theme_bw(base_size = 14)+
  annotate(geom="text",x=7,y=9, label="28.95%", col="white")+
  annotate(geom="text",x=6,y=7.5, label="23.68%", col="white")+
  annotate(geom="text",x=5,y=6, label="18.42%", col="white")+
  annotate(geom="text",x=4,y=6, label="13.16%")+
  annotate(geom="text",x=3,y=4.5, label="7.9%")+
  annotate(geom="text",x=2,y=3, label="5.26%")+
  annotate(geom="text",x=1,y=3, label="2.63%")

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

# BLOCO II
#==============
# Parei aqui!!!
dados$receptividade.professores %<>% as.factor
levels(dados$receptividade.professores) <- c("Ótima", "Boa","Ruim")
freq(dados$receptividade.professores) %>% xtable

freq(dados$satisfeito.conteudo)
freq(dados$tempo.suf)
freq(dados$TPR)
freq(dados$ensino)
freq(dados$quantidade)

# Resultados
#=================
freq(dados$result1, plot = F) %>% xtable
freq(dados$result2, plot = F) %>% xtable
freq(dados$result3, plot = F) %>% xtable
freq(dados$result4, plot = F) %>% xtable

freq(dados$material, plot=F) %>% xtable

# BLOCO III
#======================
freq(dados$efic.metodo)
freq(dados$feedback)
freq(dados$dominio.conteudo)
freq(dados$conteudo.outras.areas)

dados$melhorar %<>% as.factor
levels(dados$melhorar) <- c("O conteúdo dos blocos","Nada, acho que está ótimo","Outro")
ggplot(dados, aes(reorder(melhorar, melhorar, length)))+geom_bar()+
  theme_bw(base_size = 14)+labs(x="",y="")+coord_flip()+
  annotate(geom="text",x=3,y=20,label="63.16%",col="white")+
  annotate(geom="text",x=2,y=15,label="28.95%")+
  annotate(geom="text",x=1,y=7,label="7.9%")
freq(dados$melhorar,plot=F) %>% xtable


#BLOCO III
#==================

ggplot(dados, aes(reorder(canc.prop.aprend, canc.prop.aprend, length)))+
  geom_bar()+coord_flip()+theme_bw(base_size = 14)+labs(x="",y="")+
  annotate(geom="text",x=5,y=16,label="50%",col="white")+
  annotate(geom="text",x=4,y=10,label="21.1%")+
  annotate(geom="text",x=3,y=8,label="13.16%")+
  annotate(geom="text",x=2,y=8,label="13.16%")+
  annotate(geom="text",x=1,y=5,label="2.63%")

ggplot(dados, aes(reorder(canc.estagios, canc.estagios, length)))+
  geom_bar()+coord_flip()+theme_bw(base_size = 14)+labs(x="",y="")+
  annotate(geom="text",x=5,y=12,label="42.11%",col="white")+
  annotate(geom="text",x=4,y=12,label="42.11%",col="white")+
  annotate(geom="text",x=3,y=6,label="7.9%")+
  annotate(geom="text",x=2,y=5,label="5.26%")+
  annotate(geom="text",x=1,y=4,label="2.63%")

dados$canc.folhas[dados$canc.folhas=="Discordo "] <- "Discordo"
ggplot(dados, aes(reorder(canc.folhas, canc.folhas, length)))+
  geom_bar()+coord_flip()+theme_bw(base_size = 14)+labs(x="",y="")+
  annotate(geom="text",x=5,y=15,label="47.37%",col="white")+
  annotate(geom="text",x=4,y=10,label="47.37%",col="white")+
  annotate(geom="text",x=3,y=5,label="7.9%")+
  annotate(geom="text",x=2,y=5,label="7.9%")+
  annotate(geom="text",x=1,y=4,label="2.63%")

ggplot(dados, aes(reorder(canc.preco, canc.preco, length)))+
  geom_bar()+coord_flip()+theme_bw(base_size = 14)+labs(x="",y="")+
  annotate(geom="text",x=4,y=14,label="44.74%",col="white")+
  annotate(geom="text",x=3,y=12,label="26.32%")+
  annotate(geom="text",x=2,y=11,label="23.68%")+
  annotate(geom="text",x=1,y=5,label="5.26%")

dados$canc.local[dados$canc.local=="Discordo "] <- "Discordo"
ggplot(dados, aes(reorder(canc.local, canc.local, length)))+
  geom_bar()+coord_flip()+theme_bw(base_size = 14)+labs(x="",y="")+
  annotate(geom="text",x=5,y=11,label="34.21%",col="white")+
  annotate(geom="text",x=4,y=9,label="28.95%",col="white")+
  annotate(geom="text",x=3,y=6,label="21.05%",col="white")+
  annotate(geom="text",x=2,y=6,label="10.53%")+
  annotate(geom="text",x=1,y=4,label="5.26%")

ggplot(dados, aes(reorder(canc.prof, canc.prof, length)))+
  geom_bar()+coord_flip()+theme_bw(base_size = 14)+labs(x="",y="")+
  annotate(geom="text",x=5,y=19,label="57.9%",col="white")+
  annotate(geom="text",x=4,y=9,label="31.58%",col="white")+
  annotate(geom="text",x=3,y=5,label="5.26%")+
  annotate(geom="text",x=2,y=5,label="2.63%")+
  annotate(geom="text",x=1,y=5,label="2.63%")

# Analíticas
#=========================================
table(dados$canc.preco, dados$escolaridade) %>% chisq.test
cv.test(dados$canc.preco, dados$escolaridade)


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
  theme_bw(base_size = 14)+labs(x="",y="")+coord_flip()
freq(dados$melhorar) %>% xtable


#BLOCO III
#==================

ggplot(dados, aes(reorder(canc.prop.aprend, canc.prop.aprend, length)))+
  geom_bar()+coord_flip()+theme_bw(base_size = 14)+labs(x="",y="")
ggplot(dados, aes(reorder(canc.estagios, canc.estagios, length)))+
  geom_bar()+coord_flip()+theme_bw(base_size = 14)+labs(x="",y="")
ggplot(dados, aes(reorder(canc.folhas, canc.folhas, length)))+
  geom_bar()+coord_flip()+theme_bw(base_size = 14)+labs(x="",y="")
ggplot(dados, aes(reorder(canc.preco, canc.preco, length)))+
  geom_bar()+coord_flip()+theme_bw(base_size = 14)+labs(x="",y="")
ggplot(dados, aes(reorder(canc.local, canc.local, length)))+
  geom_bar()+coord_flip()+theme_bw(base_size = 14)+labs(x="",y="")
ggplot(dados, aes(reorder(canc.prof, canc.prof, length)))+
  geom_bar()+coord_flip()+theme_bw(base_size = 14)+labs(x="",y="")



# Analíticas
#=========================================
table(dados$canc.preco, dados$escolaridade) %>% chisq.test
cv.test(dados$canc.preco, dados$escolaridade)



# TCC Ricardo
# Prof. Silas Rocha
# Script: Prof. Neylson Crepalde

dados <- read.csv("~/Documentos/Neylson Crepalde/Izabela Hendrix/Silas/Ricardo/tcc_ricardo.csv", 
                  stringsAsFactors = T)

head(dados)
names(dados) <- c('quest','class','sexo','escolaridade','motivacao','setor',
                  'mercado','tempo_atuacao','idade','contratos_efet',
                  'segmento_contratos','tempo_agenciado','investimento',
                  'retorno','empenho_escola','gerenc_carreira','confianca',
                  'melhoras','imagem','20_preparado','20_conhecimetnos',
                  '20_rede','20_expressao','contrib_escola','preparacao',
                  'oportunidades','comentario_final')

library(descr)
library(magrittr)

dim(dados)

#Classificação em relação à agência
dados$class %<>% as.factor
levels(dados$class) <- c('Aluno(a)','Agenciado(a)','Aluno(a)/Agenciado(a)')
freq(dados$class, plot=F)



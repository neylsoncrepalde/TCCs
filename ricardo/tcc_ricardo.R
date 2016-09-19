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
library(xtable)

dim(dados)

#================================
# BLOCO I
#================================

#Classificação em relação à agência
dados$class %<>% as.factor
levels(dados$class) <- c('Aluno(a)','Agenciado(a)','Aluno(a)/Agenciado(a)')
xtable(freq(dados$class, plot=F))
pie(table(dados$class))
legend('topright',
       legend = c('Aluno(a): 59%','Agenciado(a): 25%','Aluno(a)/Agenciado(a): 16%'))

# sexo
dados$sexo %<>% as.factor
levels(dados$sexo) <- c("Feminino","Masculino")
freq(dados$sexo,plot=F)

# escolaridade
dados$escolaridade %<>% as.factor
levels(dados$escolaridade) <- c('Fund. Inc.','Fund. Comp.','EM Inc.','EM Comp.',
                                'Superior Inc.','Superior Comp.','Pós-grad.')

freq(dados$escolaridade,plot=F)
library(RColorBrewer)
pal <- brewer.pal(8,name="Set2")
pie(table(dados$escolaridade), col = pal)

# motivacao
dados$motivacao %<>% as.factor
levels(dados$motivacao) <- c('Fama','Valor financeiro','Conhecimento','Ser reconhecido',
                             'Admiração por alguém')
freq(dados$motivacao, plot=F)

# setor
dados$setor %<>% as.factor
levels(dados$setor) <- c('Cinema','TV','Seriados','Curta e Festivais')
freq(dados$setor,plot=F)

# mercado
dados$mercado %<>% as.factor
levels(dados$mercado) <- c('Trabalhador','Modelo','Amador','Estudante','Outro')
freq(dados$mercado,plot=F)

# tempo de atuação
dados$tempo_atuacao %<>% as.factor
levels(dados$tempo_atuacao)<- c('Menos de 1 ano','De 1 a 3 anos','De 4 a 6 anos',
                                'Mais de 6 anos')
freq(dados$tempo_atuacao,plot=F)

# idade
summary(dados$idade)
boxplot(dados$idade, horizontal=T, col = "yellow")

#=================================================
# BLOCO II
#=================================================

# Contratos efetivados pela agência (interessante)
summary(dados$contratos_efet)
boxplot(dados$contratos_efet, horizontal=T, col = "red")

# Segmento do contrato
dados$segmento_contratos %<>% as.factor
levels(dados$segmento_contratos) <- c('Nenhum','Cinema','TV','Seriados','Curtas e Festivais','Outros')
segmentos <- dados$segmento_contratos[!dados$segmento_contratos == "Nenhum"] 
freq(segmentos,plot=F) #Apenas entre os que foram contemplados

# quanto tempo agenciado
dados$tempo_agenciado %<>% as.factor
levels(dados$tempo_agenciado) <- c('De 2 meses a 1 ano','De 1 a 2 anos','De 3 a 4 anos','Outros')
freq(dados$tempo_agenciado,plot=F)

# valor investido
dados$investimento %<>% as.factor
levels(dados$investimento) <- c('De R$ 100,00 a R$ 200,00','De R$ 201,00 a R$ 400,00',
                                'De R$ 401,00 a R$ 600,00','Acima de R$ 601,00')
freq(dados$investimento,plot=F)

# retorno
dados$retorno %<>% as.factor
levels(dados$retorno) <- c('Muito abaixo do esperado','Abaixo do esperado',
                           'Dentro do Esperado','Significativo',
                           'Muito significativo', 'Não sei responder')
freq(dados$retorno, plot=F) # 63% estão muito satisfeitos

# 15 empenho da escola
dados$empenho_escola %<>% as.factor
levels(dados$empenho_escola) <- c('Muito abaixo do esperado','Abaixo do esperado',
                                  'Dentro do Esperado','Significativo',
                                  'Muito significativo', 'Não sei responder')

freq(dados$empenho_escola, plot=F)

# 16 



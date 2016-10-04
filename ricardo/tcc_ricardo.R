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
                  'melhoras','imagem','20_preparado','20_conhecimentos',
                  '20_rede','20_expressao','contrib_escola','preparacao',
                  'oportunidades','comentario_final')

library(descr)
library(magrittr)
library(xtable)
library(ggplot2)

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
       legend = c('Aluno(a): 59%','Agenciado(a): 25%','Aluno(a)/Agenciado(a): 16%'), bty = "n")

# sexo
dados$sexo %<>% as.factor
levels(dados$sexo) <- c("Feminino","Masculino")
xtable(freq(dados$sexo,plot=F))


# escolaridade
dados$escolaridade %<>% as.factor
levels(dados$escolaridade) <- c('Fund. Inc.','Fund. Comp.','EM Inc.','EM Comp.',
                                'Superior Inc.','Superior Comp.','Pós-grad.')

xtable(freq(dados$escolaridade,plot=F))
library(RColorBrewer)
pal <- brewer.pal(8,name="Set2")
pie(table(dados$escolaridade), col = pal)

# motivacao
dados$motivacao %<>% as.factor
levels(dados$motivacao) <- c('Fama','Valor financeiro','Conhecimento','Ser reconhecido',
                             'Admiração por alguém')
xtable(freq(dados$motivacao, plot=F))

# setor
dados$setor %<>% as.factor
levels(dados$setor) <- c('Cinema','TV','Seriados','Curta e Festivais')
xtable(freq(dados$setor,plot=F))

# mercado
dados$mercado %<>% as.factor
levels(dados$mercado) <- c('Trabalhador','Modelo','Amador','Estudante','Outro')
freq(dados$mercado,plot=F) %>% xtable

# tempo de atuação
dados$tempo_atuacao %<>% as.factor
levels(dados$tempo_atuacao)<- c('Menos de 1 ano','De 1 a 3 anos','De 4 a 6 anos',
                                'Mais de 6 anos')
freq(dados$tempo_atuacao,plot=F) %>% xtable

# idade
summary(dados$idade)
boxplot(dados$idade, horizontal=T, col = "yellow")
ggplot(dados, aes(x=1, y=idade))+geom_boxplot(fill="yellow")+labs(x="", y="Idade")+coord_flip()

#=================================================
# BLOCO II
#=================================================

# Contratos efetivados pela agência (interessante)
summary(dados$contratos_efet) %>% t %>% xtable
boxplot(dados$contratos_efet, horizontal=T, col = "red")
ggplot(dados, aes(x=1, y=contratos_efet))+geom_boxplot(fill="red")+labs(x="", y="Contratos efetivados pela agência")+coord_flip()

# Segmento do contrato
dados$segmento_contratos %<>% as.factor
levels(dados$segmento_contratos) <- c('Nenhum','Cinema','TV','Seriados','Curtas e Festivais','Outros')
segmentos <- dados$segmento_contratos[!dados$segmento_contratos == "Nenhum"] 
freq(segmentos,plot=F) %>% xtable #Apenas entre os que foram contemplados

# quanto tempo agenciado
dados$tempo_agenciado %<>% as.factor
levels(dados$tempo_agenciado) <- c('De 2 meses a 1 ano','De 1 a 2 anos','De 3 a 4 anos','Outros')
freq(dados$tempo_agenciado,plot=F) %>% xtable

# valor investido
dados$investimento %<>% as.factor
levels(dados$investimento) <- c('De R$ 100,00 a R$ 200,00','De R$ 201,00 a R$ 400,00',
                                'De R$ 401,00 a R$ 600,00','Acima de R$ 601,00')
freq(dados$investimento,plot=F) %>% xtable

# retorno
dados$retorno %<>% as.factor
levels(dados$retorno) <- c('Muito abaixo do esperado','Abaixo do esperado',
                           'Dentro do Esperado','Significativo',
                           'Muito significativo', 'Não sei responder')
freq(dados$retorno, plot=F) %>% xtable # 63% estão muito satisfeitos

# 15 empenho da escola
dados$empenho_escola %<>% as.factor
levels(dados$empenho_escola) <- c('Muito abaixo do esperado','Abaixo do esperado',
                                  'Dentro do Esperado','Significativo',
                                  'Muito significativo', 'Não sei responder')

freq(dados$empenho_escola, plot=F) %>% xtable

# 16 gerenciamento de carreira: o que é mais importante
dados$gerenc_carreira %<>% as.factor
levels(dados$gerenc_carreira) <- c('Formação Técnica','Divulgação em mercado audiovisual',
                                   'Preparação pessoal para o mercado',
                                   'Preparação para testes de elenco', 'Outro')
freq(dados$gerenc_carreira, plot=F) %>% xtable

# 17 Sente segurança e confiança na sua formação?
dados$confianca %<>% as.factor
levels(dados$confianca) <- c('Muita insegurança','Sinto insegurança','Seguro',
                             'Seguro e confiante','Muito seguro e confiante')
freq(dados$confianca, plot=F) %>% xtable

# 18 percebeu melhoras no desempenho por conta da escola?
dados$melhoras %<>% as.factor
levels(dados$melhoras) <- c('Muito abaixo do esperado','Abaixo do esperado',
                            'Dentro do Esperado','Significativo',
                            'Muito significativo', 'Não sei responder')
freq(dados$melhoras, plot=F) %>% xtable

#=========================================
# BLOCO III
#=========================================

# 19 imagem profissional está bem aplicada no mercado?
dados$imagem %<>% as.factor
levels(dados$imagem) <- c('Muito abaixo do esperado','Abaixo do esperado',
                          'Dentro do Esperado','Significativo',
                          'Muito significativo', 'Não sei responder')
freq(dados$imagem, plot=F) %>% xtable

# 20 questões de avaliação de posição no mercado
dados$`20_preparado` %<>% as.factor
dados$`20_conhecimentos` %<>% as.factor
dados$`20_rede` %<>% as.factor
dados$`20_expressao` %<>% as.factor

levels(dados$`20_preparado`) <- c('Discordo totalmente','Discordo',
                                  'Nem concordo nem discordo',
                                  'Concordo', 'Concordo plenamente')
levels(dados$`20_conhecimentos`) <- c('Discordo totalmente','Discordo',
                                  'Nem concordo nem discordo',
                                  'Concordo', 'Concordo plenamente')
levels(dados$`20_rede`) <- c('Discordo totalmente','Discordo',
                                  'Nem concordo nem discordo',
                                  'Concordo', 'Concordo plenamente')
levels(dados$`20_expressao`) <- c('Discordo totalmente','Discordo',
                                  'Nem concordo nem discordo',
                                  'Concordo', 'Concordo plenamente')

freq(dados$`20_preparado`, plot=F)
freq(dados$`20_conhecimentos`, plot=F)
freq(dados$`20_rede`, plot=F)
freq(dados$`20_expressao`, plot=F)

g1 <- ggplot(dados, aes(`20_preparado`))+geom_bar()+coord_flip()+
  labs(y="Porcentagens",x="Preparado")
g2 <- ggplot(dados, aes(`20_conhecimentos`))+geom_bar()+coord_flip()+
  labs(y="Porcentagens",x="Conhecimentos")
g3 <- ggplot(dados, aes(`20_rede`))+geom_bar()+coord_flip()+
  labs(y="Porcentagens",x="Rede")
g4 <- ggplot(dados, aes(`20_expressao`))+geom_bar()+coord_flip()+
  labs(y="Porcentagens",x="Expressão")

# carregar funcao multiplot
multiplot(g1,g2,g3,g4, cols = 2)

# 21 qualidade da contribuicao da escola
dados$contrib_escola %<>% as.factor
levels(dados$contrib_escola) <- c('Abaixo do esperado',
                                  'Dentro do Esperado','Significativo',
                                  'Muito significativo')
freq(dados$contrib_escola, plot=F) %>% xtable

# 22 Como voce se prepara para testes de elenco MUITO INTERESSANTE
dados$preparacao %<>% as.factor
levels(dados$preparacao) <- c('Decoro o texto antes','Decora o texto e busca informações adicionais',
                              'Não decora o texto e foca em exercícios espontâneos',
                              'Estuda e cria comportamentos para o personagem antes do teste')
freq(dados$preparacao, plot=F) %>% xtable

# 23 oportunidades em audiovisual em BH
dados$oportunidades %<>% as.factor
levels(dados$oportunidades) <- c('Muito abaixo do esperado','Abaixo do esperado',
                                 'Dentro do Esperado','Significativo',
                                 'Muito significativo', 'Não sei responder')
freq(dados$oportunidades, plot=F) %>% xtable

#24 comentario final
freq(dados$comentario_final, plot=F)

#=========================
#Modelagem estatística
#=========================
dados <- read.csv("~/Documentos/Neylson Crepalde/Izabela Hendrix/Silas/Ricardo/tcc_ricardo.csv", 
                  stringsAsFactors = T)

head(dados)
names(dados) <- c('quest','class','sexo','escolaridade','motivacao','setor',
                  'mercado','tempo_atuacao','idade','contratos_efet',
                  'segmento_contratos','tempo_agenciado','investimento',
                  'retorno','empenho_escola','gerenc_carreira','confianca',
                  'melhoras','imagem','20_preparado','20_conhecimentos',
                  '20_rede','20_expressao','contrib_escola','preparacao',
                  'oportunidades','comentario_final')

#Classificação em relação à agência
dados$class %<>% as.factor
levels(dados$class) <- c('Aluno(a)','Agenciado(a)','Aluno(a)/Agenciado(a)')
xtable(freq(dados$class, plot=F))
pie(table(dados$class))
legend('topright',
       legend = c('Aluno(a): 59%','Agenciado(a): 25%','Aluno(a)/Agenciado(a): 16%'), bty = "n")

# sexo
dados$sexo %<>% as.factor
levels(dados$sexo) <- c("Feminino","Masculino")
xtable(freq(dados$sexo,plot=F))

# motivacao
dados$motivacao %<>% as.factor
levels(dados$motivacao) <- c('Fama','Valor financeiro','Conhecimento','Ser reconhecido',
                             'Admiração por alguém')
xtable(freq(dados$motivacao, plot=F))

# setor
dados$setor %<>% as.factor
levels(dados$setor) <- c('Cinema','TV','Seriados','Curta e Festivais')
xtable(freq(dados$setor,plot=F))

# mercado
dados$mercado %<>% as.factor
levels(dados$mercado) <- c('Trabalhador','Modelo','Amador','Estudante','Outro')
freq(dados$mercado,plot=F) %>% xtable

# Segmento do contrato
dados$segmento_contratos[dados$segmento_contratos == 0] <- NA
dados$segmento_contratos %<>% as.factor
levels(dados$segmento_contratos) <- c('Cinema','TV','Seriados','Curtas e Festivais','Outros')

freq(dados$segmento_contratos,plot=F) %>% xtable #Apenas entre os que foram contemplados

# 16 gerenciamento de carreira: o que é mais importante
dados$gerenc_carreira %<>% as.factor
levels(dados$gerenc_carreira) <- c('Formação Técnica','Divulgação em mercado audiovisual',
                                   'Preparação pessoal para o mercado',
                                   'Preparação para testes de elenco', 'Outro')
freq(dados$gerenc_carreira, plot=F) %>% xtable

# 22 Como voce se prepara para testes de elenco MUITO INTERESSANTE
dados$preparacao %<>% as.factor
levels(dados$preparacao) <- c('Decoro o texto antes','Decora o texto e busca informações adicionais',
                              'Não decora o texto e foca em exercícios espontâneos',
                              'Estuda e cria comportamentos para o personagem antes do teste')
freq(dados$preparacao, plot=F) %>% xtable



reg <- lm(contratos_efet~factor(segmento_contratos)+factor(motivacao)+
            factor(setor)+factor(preparacao)+factor(sexo)+escolaridade+factor(class)+
            factor(mercado)+tempo_atuacao+idade_cent+tempo_agenciado+investimento+retorno+
            empenho_escola+factor(gerenc_carreira)+confianca+melhoras+imagem+
            `20_preparado`+`20_conhecimentos`+`20_rede`+`20_expressao`+contrib_escola+
            oportunidades, data=dados)
summary(reg)
hist(residuals(reg))

par(mfrow=c(2,2))
plot(reg)
par(mfrow=c(1,1))

# Tentando com a gamma

dados$contratos_efet_gamma <- dados$contratos_efet + 0.1
dados$idade_cent <- dados$idade - mean(dados$idade)

gamma <- glm(contratos_efet_gamma~factor(motivacao)+
            factor(setor)+factor(preparacao)+factor(sexo)+escolaridade+factor(class)+
            factor(mercado)+tempo_atuacao+idade_cent+tempo_agenciado+investimento+retorno+
            empenho_escola+factor(gerenc_carreira)+confianca+melhoras+imagem+
            `20_preparado`+`20_conhecimentos`+`20_rede`+`20_expressao`+contrib_escola+
            oportunidades, data=dados, family = Gamma("log"))
summary(gamma)
exp(coef(gamma))

par(mfrow=c(2,2))
plot(gamma)
par(mfrow=c(1,1))

library(lmtest)
lrtest(reg, gamma)
deviance(reg) - deviance(gamma)

library(texreg)
texreg(list(reg, gamma), single.row = T, center = F, caption.above = T,
       caption = "Modelos estatísticos", custom.model.names = c("MQO", "MLG -- Gamma"))

library(pscl)
pR2(gamma)

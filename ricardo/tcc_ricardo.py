# -*- coding: utf-8 -*-
"""
Testes TCC Ricardo
Script: Prof. Neylson Crepalde
"""
import pandas as pd
import numpy as np
import scipy as sp
import matplotlib.pyplot as plt

dados = pd.read_csv('~/Documentos/tcc_ricardo.csv')

dados[:10]

dados['9'].describe()

plt.boxplot(dados['9'], vert=False, notch=True)
plt.xlabel('Idade dos entrevistados')
plt.show()

dados.columns.values
pd.crosstab()
tabela = pd.crosstab(index = dados['20.1'],
                     columns = 'count')
tabela.columns.values
tabela = tabela['count']
counts = tabela.values


#plt.bar(, height=counts, width=1, color='yellow')
plt.show()

plt.bar(dados['20.1'], bins=5)
'''
plt.hist(dados['20.2'], bins=5)
plt.hist(dados['20.3'], bins=5)
plt.hist(dados['20.4'], bins=5)
'''

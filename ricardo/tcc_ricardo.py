# -*- coding: utf-8 -*-
"""
Testes TCC Ricardo
Script: Prof. Neylson Crepalde
"""
import pandas as pd
import numpy as np
import scipy as sp
import matplotlib.pyplot as plt
from matplotlib.patches import Polygon


dados = pd.read_csv('~/Documentos/tcc_ricardo.csv')

dados[:10]

dados['9'].describe()

plt.boxplot(dados['9'], vert=False, notch=True)
plt.xlabel('Idade dos entrevistados')
plt.show()

dados.columns.values

plt.hist(dados['20.2'], bins=5)

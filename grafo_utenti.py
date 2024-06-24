import pandas as pd
import networkx as nx
import matplotlib.pyplot as plt
import numpy as np

num_news = 91
df = pd.read_csv("Data/BuzzFeed/BuzzFeedNewsUser.csv")

# calcolo affidabilità degli utenti
affidabilita = pd.DataFrame(columns=["User", "Affidability"])

for i in range(1, df['user'].max() + 1):
    num_tot = sum(df['user'] == i)
    num_real = sum((df['user'] == i) & (df['news'] <= num_news))
    val = num_real / num_tot
    newrow = {'User': i, 'Affidability': val}
    affidabilita.loc[len(affidabilita)] = newrow

# grafo affidabilità degli utenti

dfuser = pd.read_csv("Data/PolitiFact/PolitiFactUserUser.csv")

G = nx.from_pandas_edgelist(dfuser, source='user', target='follow', create_using=nx.DiGraph())

scalacolori = plt.get_cmap('RdYlGn', 11)
colorVals = [scalacolori(round(aff*10)) for aff in affidabilita['Affidability']]

# Crea un dizionario di colori per ogni nodo
color_map = dict(zip(G.nodes(), colorVals))

# Crea il layout del grafo
pos = nx.spring_layout(G, k=0.15)

# Disegna il grafo con i colori dei nodi corrispondenti
nx.draw(G, pos, node_color=[color_map[node] for node in G.nodes()], node_size=5, width=0.02, arrowsize=0.2, alpha=0.5)

sm = plt.cm.ScalarMappable(cmap=scalacolori, norm=plt.Normalize(vmin=0, vmax=1))
sm.set_array([])
cbar = plt.colorbar(sm, ticks=np.linspace(0,1,11),
             boundaries=np.arange(-0.05,1.1,.1))
cbar.set_label('Affidabilità', rotation=270, labelpad=20)
plt.show()
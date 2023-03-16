---
jupytext:
  formats: ipynb,md:myst
  text_representation:
    extension: .md
    format_name: myst
    format_version: 0.13
    jupytext_version: 1.14.4
kernelspec:
  display_name: Python 3 (ipykernel)
  language: python
  name: python3
---

```{code-cell} ipython3
:tags: []

import numpy as np
import scipy as sp
import scipy.optimize
import matplotlib.pyplot as plt
import networkx as nx
```

# Solving for trade flows

We want to approximate trade flows between counties, taking into account general equilibrium constraints.

I-O transformation is performed on a full vector of commodities, but trade flows can be solved for each commodity independently.
The optimization problem below is written for a single commodity for notation simplicity.

Let $y_i$ denote total output in county $i$.
Demand (input need) $d_i$ in county $i$ can be calculated as $d_i = A y_i$, where $A$ is the direct requirements matrix.
Supply of intermediate goods $s_i$ from county $i$ is output $y_i$, adjusted for final consumption, foreign trade and capital investment.

We can maybe use national values from the use table to compute and ajustment from $y_i$ to $s_i$.
For example, assuming that for each commodity, a fixed proportion of total output is used as intermediate inputs.
A further adjustment is needed to assure national trade balancing $\sum s_i = \sum d_i$.

The following linear programming problem can be solved numerically to obtain an estimate of trade flows $t_{ij}$ from county $i$ to county $j$.

$$
\min_{t_{ij}} \sum_{ij} r_{ij} t_{ij} \\
\text{s.t. }
\sum_j t_{ij} = s_i
\text{ and }
\sum_i t_{ij} = d_j
$$

The objective of the optimization problem is to minimize loosely defined total transportation cost $\sum_{ij} r_{ij} t_{ij}$, where $r_{ij}$ is some measure of distance between counties $i$ and $j$.
$r_{ij}$ can be simple Euclidean distance between county centroids, but can also account for road networks, put a hard limit on maximum trade distance or increase with geographic distance at an increasing rate.

Trade balance constraints: $\sum_j t_{ij} = s_i$ everything that a county supplies is sold, and $\sum_i t_{ij} = d_j$ everything that a county demands in bought.

+++

# Toy example

```{code-cell} ipython3
:tags: []

n = 7
pos = np.random.rand(n, 2)
dist = np.linalg.norm(np.stack([pos] * n, 0) - np.stack([pos] * n, 1), axis=2)
dem = np.random.rand(n)
dem /= dem.sum() / n
sup = np.random.rand(n)
sup /= sup.sum() / n
```

```{code-cell} ipython3
:tags: []

sup_con = np.kron(np.eye(n), np.ones((1, n)))
dem_con = np.kron(np.ones((1, n)), np.eye(n))

sol = sp.optimize.linprog(
    c=dist.flatten(),
    A_eq=np.concatenate([sup_con, dem_con]),
    b_eq=np.concatenate([sup, dem]))

trade = sol.x.reshape((n, n))
print(sol.message)
```

```{code-cell} ipython3
:tags: []

G = nx.DiGraph()
G.add_nodes_from(range(n))
for s in range(n):
    for d in range(n):
        if s != d and trade[s, d] > 0:
            G.add_edge(s, d, trade=f'{trade[s, d]:.2f} ({dist[s, d]:.2f})')
plt.figure(figsize=(8, 8))
nx.draw(G, pos, node_size=50)
nx.draw_networkx_labels(G, pos, {i: f'{sup[i]:.2f} | {dem[i]:.2f}' for i in range(n)})
nx.draw_networkx_edge_labels(G, pos, nx.get_edge_attributes(G, 'trade'))
plt.title('Nodes: supply | demand. Edges: trade (distance)');
plt.gca().set_aspect('equal')
```

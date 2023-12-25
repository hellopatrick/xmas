import networkx as nx
from math import prod

def part_1(s):
  G  = nx.Graph()

  for line in s.strip().split('\n'):
    a,bs = line.split(': ')

    for b in bs.split():
      G.add_edge(a,b)

  G.remove_edges_from(nx.minimum_edge_cut(G))

  return prod(len(c) for c in nx.connected_components(G))

with open("2023/day25/test.t/input") as f:
  s = f.read()
  ans = part_1(s)
  print(f"part1 = {ans}")

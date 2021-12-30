# AfricanUrbanNetwork


The repository contains three files corresponding to the African Urban Network:
1 - AfricaNetworkNodes
Contains 7361 nodes, with x,y coordinates, and their population. The coordinates and population of cities were obtained from https://africapolis.org/en
For nodes that are not cities, named "road", their population is zero and they are used for describing the whole network.

2 - AfricaNetworkEdges
Contains information about 9159 edges that connnect the network. Each edge has a starting and ending point (from and to) and an estimate of its length, considering its curvature.
Information about the road infrastructure was obtained from https://www.openstreetmap.org/.

3 - Table_summary
Contains a detailed description of all variables in both tables.

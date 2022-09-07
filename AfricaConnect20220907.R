#### Project created to create a connected network based on OSM data
#### the vertices are from Africapolis
####
#### download the sources:
#### https://africapolis.org/
#### https://www.openstreetmap.org/

require(igraph)
require(rworldxtra)
require(sf)
require(geosphere)
require(plyr)

##### Load DB Africa
{
  Af <- read.csv("Africapolis_agglomeration_2020_continental.csv")
  names(Af)[1] <- "aggloID"
  Af <- Af[Af$Pop2015>100000,]
  load("RData/AfricaFull/AfricaFiltRoads.RData")
  DB <- DB[DB$highway != "secondary",]
  NEdges <- dim(DB)[1]
  lenR <- rep(0, NEdges)
   
  for(k in 1:NEdges){
  lenR[k] <- length(DB[k,]$geometry[[1]][,1])
  }
  FF <- table(lenR)
  write.csv(FF, "roadSegmentsFrequency.csv", row.names = F)
  NCities <- dim(Af)[1]
  NEdges <- dim(DB)[1]
}

#### Create DF for store information
{
NodesCities <- data.frame(aggloID = Af$aggloID,
                    agglosName = Af$agglosName,
                    x = Af$Longitude,
                    y = Af$Latitude,
                    Pop2015 = Af$Pop2015
    )

NodesTransport <- data.frame(aggloID = 0,
                          agglosName = "NotNode",
                          x = 0,
                          y = 0,
                          Pop2015 = 0)

Ed <- data.frame(sId = c(),
                 eId = c(),
                 l = c(),
                 h = c())
}

#### for storing breaks
{
 ToStore <- seq(from = 1, to = 500000, by = 5000)
}

#### read edges
{
for(k in 1:NEdges){
  cat(k, "out of", NEdges, "edges \n")
        seg <- length(DB[k,]$geometry[[1]][,1])
        sx = DB[k,]$geometry[[1]][1,1]
        sy = DB[k,]$geometry[[1]][1,2]
        ex = DB[k,]$geometry[[1]][seg,1]
        ey = DB[k,]$geometry[[1]][seg,2]

        ### start node
        ### check if city within 2500m
        dsCities <- distm(data.frame(x = NodesCities$x,
                               y = NodesCities$y),
                    data.frame(x = sx, y = sy))
        if(sum(dsCities < 10*sqrt(NodesCities$Pop2015))>0){#### assign node to city
            u <- which(dsCities == min(dsCities))
            EdgeO <- NodesCities$aggloID[u]
        } else{ ### transportNode, but could be the existent ones
            #q <- which(cbind(NodesTransport$x == sx & NodesTransport$y== sy))
            dq <- distm(data.frame(x = NodesTransport$x, y = NodesTransport$y),
                       data.frame(x = sx, y = sy))
            q <- dq < 1500 # transport node within 500m
            if(sum(q)>0){ #one transport node nearby
             EdgeO <- NodesTransport$aggloID[which.min(dq)[1]]
            } else{ ##new node
                NewO <- data.frame(aggloID = 1000000+2*k-1,
                                   agglosName = "road",
                                   x = DB[k,]$geometry[[1]][1,1],
                                   y = DB[k,]$geometry[[1]][1,2],
                                   Pop2015 = 0)
                NodesTransport <- rbind(NodesTransport, NewO)
                EdgeO <- NewO$aggloID
            }

        }

        ### end node
        deCities <- distm(data.frame(x = NodesCities$x,
                                     y = NodesCities$y),
                          data.frame(x = ex, y = ey))
        if(sum(deCities < 10*sqrt(NodesCities$Pop2015))>0){#### assign node to city
            u <- which(deCities == min(deCities))
            EdgeE <- NodesCities$aggloID[u]
        } else{ ### transportNode, but could be the existent ones
            dq <- distm(data.frame(x = NodesTransport$x, y = NodesTransport$y),
                        data.frame(x = ex, y = ey))
            q <- dq < 1500 # transport node within 500m
            if(sum(q)>0){ #one transport node nearby
                EdgeE <- NodesTransport$aggloID[which.min(dq)[1]]
            } else{ ##new node
                NewE <- data.frame(aggloID = 1000000+2*k,
                                   agglosName = "road",
                                   x = DB[k,]$geometry[[1]][seg,1],
                                   y = DB[k,]$geometry[[1]][seg,2],
                                   Pop2015 = 0)
                NodesTransport <- rbind(NodesTransport, NewE)
                EdgeE <- NewE$aggloID
            }

        }

        ### segment and distances
        l = 0
        for(s in 2:seg){
            l = l + distm(data.frame(x = DB[k,]$geometry[[1]][s-1,1],
                                     y = DB[k,]$geometry[[1]][s-1,2]),
                          data.frame(x = DB[k,]$geometry[[1]][s,1],
                                     y = DB[k,]$geometry[[1]][s,2]))/1000
        }
        h = DB[k,]$highway
        Ed <- rbind(Ed, data.frame(sId = EdgeO,
                                   eId = EdgeE,
                                   l = l,
                                   h = h))

        if(k %in% ToStore){
          NodeNameC <- paste("NodesCities_",k,"_steps.RData", sep = "")
          NodeNameT <- paste("NodesTransport_",k,"_steps.RData", sep = "")
          EdName <- paste("Edges_",k,"_steps.RData", sep = "")
          save(NodesCities, file = NodeNameC)
          save(NodesTransport, file = NodeNameT)
          save(Ed, file = EdName)
        }
}
Nodes = rbind(NodesCities, NodesTransport)
### remove the not-node
Nodes <- Nodes[Nodes$agglosName != "NotNode",]
save(Nodes, file = "NodesAfrica.RData")
save(Ed, file = "EdgesAfrica.RData")
}

#### load
{
NodeNameT <- paste("NodesAfrica.RData", sep = "")
EdName <- paste("EdgesAfrica.RData", sep = "")
load(file = NodeNameT)
load(file = EdName)
}

#### keep secondary nodes Y
{
Af <- read.csv("Africapolis_agglomeration_2020_continental.csv")
names(Af)[1] <- "aggloID"
SecAf <- Af[Af$Pop2015<100000 & Af$Pop2015>30000,]
filt <- Nodes$agglosName == "road"
for(k in 1:dim(SecAf)[1]){
  NearNode <- distm(data.frame(SecAf$Longitude[k],
                               SecAf$Latitude[k]),
                    data.frame(Nodes$x[filt],
                               Nodes$y[filt]))
  if(min(NearNode) < 10000){
    v <- which(NearNode == min(NearNode))[1]
    u <- Nodes$aggloID[which(filt)[v]]
    Nodes$agglosName[which(filt)[v]] <- SecAf$agglosName[k]
    Nodes$Pop2015[which(filt)[v]] <- SecAf$Pop2015[k]
    Nodes$aggloID[which(filt)[v]] <- SecAf$aggloID[k]
    filt <- Nodes$agglosName == "road"
    Ed$sId[Ed$sId == u] <- SecAf$aggloID[k]
    Ed$eId[Ed$eId== u] <- SecAf$aggloID[k]
    cat(min(NearNode), "\n")
  }
}
}

#### create graph y
{
names(Ed)[1] <- "from"
names(Ed)[2] <- "to"
EdIgraph <- Ed[, c(1, 2, 3, 4)] 
G0 <- graph_from_data_frame(EdIgraph,
        directed = F, 
        vertices = Nodes)
com <- components(G0)
save(G0, file = "AfricaPreNetwork.RData")
Sys.time()
cat("GRAPH CREATED \n")
}

##### connect components. Nodes w degree one Y
{
iter <- 5000
while(count_components(G0)>1){
  iter <- iter + 1
  cat(iter, "\n")
  Ve <- as_data_frame(G0, what="vertices")
  com <- components(G0)
  u <- sample.int(count_components(G0), 1)
  x <- com$membership == u
  Or <- Ve$name %in% names(x)[which(x)]
  De <- !Or
  D <- distm(data.frame(Ve$x[Or], Ve$y[Or]),
             data.frame(Ve$x[De], Ve$y[De]))
  near <- min(D)
  cat(near, "\n")
  if(near < 24*iter){
    ToNear <- which(D == near, arr.ind = T)
    Ed <- rbind(Ed, data.frame(from = Ve$name[which(Or)[ToNear[1]]],
                               to = Ve$name[which(De)[ToNear[2]]],
                               l = near/1000,
                               h = "added"))
    EdIgraph <- Ed[, c(1, 2, 3, 4)] 
    G0 <- graph_from_data_frame(EdIgraph,
                                directed = F, 
                                vertices = Nodes)
    G0 <- simplify(G0, remove.multiple = T, 
                   edge.attr.comb="first")
    cat("ONE LESS COMPONENT ", count_components(G0), "\n")
  }
}
  save(G0, file = "AfricaConnectedNetwork.RData")
  Sys.time()
  cat("GRAPH CONNECTED \n") 
}

#### simplify network Y
{
  load( file = "AfricaConnectedNetwork.RData")
  G <- G0
  NodesDin <- as_data_frame(G, what="vertices")
  d <- degree(G)
  w <- which(d == 2 & NodesDin$Pop2015==0)
  
  G <- simplify(G, remove.multiple = T, 
                edge.attr.comb="first")
  while(length(w)>1){
    d <- degree(G)
    w <- which(d == 2 & NodesDin$Pop2015==0)
    toRem <- neighbors(G, w[1], mode = "all")
    G <- add_edges(G, toRem, 
                   attr = list(l = sum(incident(G, w[1])$l),
                               h = incident(G, w[1])$h[1]))
    G <- delete_vertices(G, w[1])
    NodesDin <- NodesDin[1:dim(NodesDin)[1] != w[1],]
    cat(vcount(G), "--", ecount(G), "\n")
  }
  save(G, file = "AfricaSimpleNetwork.RData")
  Sys.time()
  cat("GRAPH SIMPLE \n") 
}

##### connect edges w max net distance Y
{
  load(file = "AfricaSimpleNetwork.RData")
  memory.limit(size = 30000)
  Nodes <- as_data_frame(G, what="vertices")
  Ed <- as_data_frame(G, what="edges")
  
  iter <- 0
  Ve <- as_data_frame(G, what="vertices")
  DN <- distances(G, weights = edge.attributes(G)$l)
  DS <- distm(data.frame(Ve$x, Ve$y))/1000
  diag(DS) <- 1000; diag(DN) <- 0
  DP <- DN/DS
  DP[DS > 5] <- 0
  while(max(DP)>10){
    iter <- iter + 1
    ToAdd <- which(DP == max(DP), arr.ind = T)
    cat(iter, " DS", DS[ToAdd[1], ToAdd[2]], " DN", DN[ToAdd[1], ToAdd[2]]," max", max(DP), "\n")
    Ed <- rbind(Ed, data.frame(from = Ve$name[ToAdd[1]],
                               to = Ve$name[ToAdd[2]],
                               l = DS[ToAdd[1], ToAdd[2]],
                               h = "added"))
    EdIgraph <- Ed[, c(1, 2, 3, 4)] 
    G <- graph_from_data_frame(EdIgraph,
                               directed = F, 
                               vertices = Nodes)
    G <- simplify(G, remove.multiple = T, 
                  edge.attr.comb="first")
    DN <- distances(G, weights = edge.attributes(G)$l)
    diag(DS) <- 1000; diag(DN) <- 0
    DP <- DN/DS
    DP[DS > 5] <- 0
  }
  save(G, file = "AfricaConnectedFarNetwork.RData")
  Sys.time()
  cat("GRAPH CONNECTED FAR \n") 
}

##### connect edges w degree one Y
{
  load(file = "AfricaConnectedFarNetwork.RData")
  Nodes <- as_data_frame(G, what="vertices")
  Ed <- as_data_frame(G, what="edges")
  iter <- 0
  Ve <- as_data_frame(G, what="vertices")
  DN <- distances(G, weights = edge.attributes(G)$l)
  DS <- distm(data.frame(Ve$x, Ve$y))/1000
  diag(DS) <- 1000; diag(DN) <- 0
  DP <- DN/DS
  d <- which(degree(G)>1)
  DP[d,] <- 0; DP[, d] <- 0
  while(max(DP)>10){
    iter <- iter + 1
    ToAdd <- which(DP == max(DP), arr.ind = T)
    cat(iter, " DS", DS[ToAdd[1], ToAdd[2]], " DN", DN[ToAdd[1], ToAdd[2]]," max", max(DP), "\n")
    Ed <- rbind(Ed, data.frame(from = Ve$name[ToAdd[1]],
                               to = Ve$name[ToAdd[2]],
                               l = DS[ToAdd[1], ToAdd[2]],
                               h = "added"))
    EdIgraph <- Ed[, c(1, 2, 3, 4)] 
    G <- graph_from_data_frame(EdIgraph,
                                directed = F, 
                                vertices = Nodes)
    G <- simplify(G, remove.multiple = T, 
                   edge.attr.comb="first")
    DN <- distances(G, weights = edge.attributes(G)$l)
    diag(DS) <- 1000; diag(DN) <- 0
    DP <- DN/DS
    d <- which(degree(G)>1)
    DP[d,] <- 0; DP[, d] <- 0
  }
  save(G, file = "AfricaConnectedDegreeNetwork.RData")
  Sys.time()
  cat("GRAPH CONNECTED DEGREE 1 \n") 
}

#### save reduced network edges and nodes
{
  G <- simplify(G, remove.multiple = T, 
                edge.attr.comb="first")
  Nodes <- as_data_frame(G, what="vertices")
  Ed <- as_data_frame(G, what="edges")
save(Nodes, file = "NodesAfricaConnectedReduced.RData")
save(Ed, file = "EdgesAfricaConnectedReduced.RData")
save(G, file = "GraphConnectedReduced.RData")
Sys.time()
cat("NODES AND EDGES SAVED \n") 
}

#### simplify network Y
{
  NodesDin <- as_data_frame(G, what="vertices")
  d <- degree(G)
  w <- which(d == 2 & NodesDin$Pop2015==0)
  
  G <- simplify(G, remove.multiple = T, 
                edge.attr.comb="first")
  while(length(w)>1){
    d <- degree(G)
    w <- which(d == 2 & NodesDin$Pop2015==0)
    toRem <- neighbors(G, w[1], mode = "all")
    G <- add_edges(G, toRem, 
                   attr = list(l = sum(incident(G, w[1])$l),
                               h = incident(G, w[1])$h[1]))
    G <- delete_vertices(G, w[1])
    NodesDin <- NodesDin[1:dim(NodesDin)[1] != w[1],]
    cat(vcount(G), "--", ecount(G), "\n")
  }
  save(G, file = "GraphConnectedReducedSimple.RData")
  Sys.time()
  cat("GRAPH SIMPLE \n") 
}

##### SOME NETWORK CLEAN REMOVING ISLANDS
{
  load(file = "GraphConnectedReducedSimple.RData")
  ## MADAGASCAR
  filt <- vertex.attributes(G)$x > 41.328074 & vertex.attributes(G)$y < -2.986295
  G <- delete_vertices(G, filt)
  
  # CAPE VERDE
  filt <- vertex.attributes(G)$x < -18.7
  G <- delete_vertices(G, filt)
  
  ##CANARIES
  filt <- vertex.attributes(G)$x < -13.274821 & vertex.attributes(G)$y > 27.650206 
  G <- delete_vertices(G, filt)
  
  filt <- vertex.attributes(G)$x < -15.537923 & vertex.attributes(G)$y > 25.648602
  G <- delete_vertices(G, filt)
  # SAO TOME
  filt <- vertex.attributes(G)$x < 7.561879 & vertex.attributes(G)$y < 1.780068 
  G <- delete_vertices(G, filt)
  save(G, file = "AfricaNetworkClean.RData")
  
}

#### transform distances into travel times (MINUTES)
{
  load(file = "AfricaNetworkClean.RData")
  TravelTimes <- read.csv("inputs/TravelTimes.csv")
  edge.attributes(G)$time <- rep(0, ecount(G))
  for(k in 1:dim(TravelTimes)[1]){
    u <- which(edge.attributes(G)$h == TravelTimes$ï..h[k])
    edge.attributes(G)$time[u] <- 60*edge.attributes(G)$l[u]/TravelTimes$speed[k]
  }
  save(G, file = "AfricaNetworkTime.RData")
  Sys.time()
  cat("GRAPH TRAVEL TIMES \n") 
}

#### extra cost of cities (MINUTES)
{
  load(file = "AfricaNetworkTime.RData")
  edge.attributes(G)$timeU <- edge.attributes(G)$time
  for(k in 1:ecount(G)){
    Po <- vertex.attributes(G, head_of(G, E(G)[k]))$Pop2015
    Pd <- vertex.attributes(G, tail_of(G, E(G)[k]))$Pop2015
    edge.attributes(G)$timeU[k] <- edge.attributes(G)$timeU[k] + sqrt(Po)/30 + sqrt(Pd)/30
  }
  save(G, file = "AfricaNetworkTimeUrban.RData")
  Sys.time()
  cat("GRAPH URBAN CROSSINGS \n") 
}

#### cost of borders
{
  load(file = "AfricaNetworkTimeUrban.RData")
  Ed <- as_data_frame(G, what="edges")
  Ed$border <- (Ed$timeUCB > Ed$timeU)*1
  Ed$border[is.na(Ed$border)] <- 0 ## ocean points
  Ve <- as_data_frame(G, what="vertices")
  urbs <- Ve$name[Ve$Pop2015>0]
  nf <- which(vertex.attributes(G)$Pop2015>0) #filter cities
  CitiesP <- vertex.attributes(G)$Pop2015[nf]
  CountriesP <- vertex.attributes(G)$ISO3[nf]
  PopM <- matrix(rep(CitiesP, length(CitiesP)),
                 ncol = length(CitiesP))
  for(k in 1:65){
    Ed$timeUCB <- Ed$timeU + Ed$border*15*(k-1)
    G2 <- graph_from_data_frame(Ed,
                                directed = F, 
                                vertices = Ve)
    NetD <- distances(G2, 
                      v = nf,
                      to = nf,
                      weights = edge.attributes(G2)$timeUCB)
    
    GravM <- PopM * t(PopM) / (NetD^2.8 + 1)
    diag(GravM) <- 0
    TotG <- apply(GravM, 1, sum)
    TotGravC <- aggregate(TotG, 
                          by = list(CountriesP), 
                          FUN = sum)
    names(TotGravC)[1] <- "ISO3"
    names(TotGravC)[2] <- 15*(k-1)
    if(k == 1){DistanceImpact <- TotGravC} else{ 
      DistanceImpact <- cbind(DistanceImpact, TotGravC[, 2])
    }
  }
  names(DistanceImpact)[2:66] <- 15*(0:64)
  save(DistanceImpact, file = "DistanceImpact.RData")
}

#### extra cost of crossborder
{
  load(file = "DistanceImpact.RData")
  require(rworldmap)
  require(rworldxtra)
  edge.attributes(G)$timeUCB <- edge.attributes(G)$timeU
  countriesSP <- getMap(resolution='high')
  pointsSP = SpatialPoints(data.frame(vertex.attributes(G)$x,
                                      vertex.attributes(G)$y),
                           proj4string=CRS(proj4string(countriesSP))) 
  vertex.attributes(G)$ISO3 = over(pointsSP, countriesSP)$ISO3
  for(k in 1:ecount(G)){
    Po <- vertex.attributes(G, head_of(G, E(G)[k]))$ISO3
    Pd <- vertex.attributes(G, tail_of(G, E(G)[k]))$ISO3
    edge.attributes(G)$timeUCB[k] <- edge.attributes(G)$timeU[k] + (Po != Pd)*120
  }
  save(G, file = "AfricaNetworkTimeBorderC.RData")
  Sys.time()
  cat("GRAPH CROSS BORDER \n") 
}

#### clean vertices
{
  require(geosphere)
  load(file = "AfricaNetworkTimeBorderC.RData")  
  Ed <- as_data_frame(G, what = "edges")
  Ve <- as_data_frame(G, what = "vertices")
  levels(Ve$ISO3) <- c(levels(Ve$ISO3),"SSD")
  Ve$ISO3[Ve$ISO3 == "SOL"] <- "SOM"
  Ve$ISO3[Ve$ISO3 == "ISR"] <- "EGY"
  Ve$ISO3[Ve$ISO3 == "ESP"] <- "MAR"
  Ve$ISO3[Ve$ISO3 == "SDS"] <- "SSD"
  f <- which(is.na(Ve$ISO3))
  TVe <- Ve[-f, ]
  for(k in f){
    d <- which.min(distm(data.frame(x = Ve$x[k], y = Ve$y[k]),
                         data.frame(x = TVe$x, y = TVe$y)))
    Ve$ISO3[k] <- TVe$ISO3[d]
  }
  G <- graph_from_data_frame(Ed,
                             directed = F, 
                             vertices = Ve)
  
  Regions <- read.csv("inputs/CountryRegions.csv")
  Ve <- as_data_frame(G, what = "vertices")
  Ve <- join(Ve, Regions, by = "ISO3")
  
  G <- graph_from_data_frame(Ed,
                             directed = F, 
                             vertices = Ve)
  save(G, file = "AfricaNetworkCleanNodes.RData")
}

#### compute node betweeness (not sorted so for all)
{
load(file = "AfricaNetworkCleanNodes.RData")
NetworkB <- rep(0, vcount(G))
nf <- which(vertex.attributes(G)$Pop2015>0) #filter cities
nCities <- length(nf)
CitiesP <- vertex.attributes(G)$Pop2015
PopM <- matrix(rep(CitiesP, length(CitiesP)),
               ncol = length(CitiesP))
NetD <- distances(G, 
                  weights = edge.attributes(G)$timeUCB)

GravM <- PopM * t(PopM) / (NetD^2.8 + 1)
diag(GravM) <- 0
vertex.attributes(G)$Between  <- rep(0, vcount(G))
for(k in nf){
  cat(k/nCities, " ", k, " steps \n")
  for(j in nf){if(k < j){ #with this, 97.7 of gravity is there
  ASP <- shortest_paths(G, 
                        from = k,
                        to = j,
                        weights = edge.attributes(G)$timeUCB)
  inodes <- ASP$vpath[[1]][1:(length(ASP$vpath[[1]]))]
  vertex_attr(G, "Between", index = inodes) <- vertex_attr(G, "Between", index = inodes) + GravM[j, k]
    }}}
save(G, file = "AfricaNetworkBetween.RData")
}

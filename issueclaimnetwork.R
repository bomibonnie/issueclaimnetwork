#########################################################
# Name: Issue Claim Network in the Western Hemisphere   #
# Date: September 6, 2019                               #
# Author: Bomi Lee                                      #
# Input Files:  ICOWdyadyr.csv                          #
#########################################################

# Load packages
library(rio)
library(tidyverse)
library(igraph)

# Load data
claimyr<-import("icowdyadwithissuerivalry.dta")
ccode<-read.csv("COW country codes.csv")

ccode <- ccode %>%
  select("CCode", "StateAbb") %>%
  transmute(ccode=as.character(CCode),
         stateabb=StateAbb) %>%
  distinct()


# Keep variables necessary and region==1
## Note: change ccode to characters(not numeric)

## Edge list
claimyr_wh <- claimyr %>%
  filter(region==1) %>%
  select("issue", "chal", "tgt", "year") %>%
  arrange(year, chal, tgt) %>%
  mutate(chal=as.character(chal),
         tgt=as.character(tgt))

## Node list
vtx1 <- claimyr %>%
  filter(region==1) %>%
  select(year, chal) %>%
  rename(ccode= chal) %>%
  distinct()

vtx2 <- claimyr %>%
  filter(region==1) %>%
  select(year, tgt) %>%
  rename(ccode= tgt) %>%
  distinct()

vtx3 <- vtx1 %>%
  rbind(vtx2) %>%
  distinct() %>%
  arrange(year, ccode) %>%
  mutate(ccode=as.character(ccode))

# Add label (ccode: Stateabb)
vtx <- vtx3 %>%
  left_join(ccode, by = "ccode")

## Network sample, year==1981

# 1981
claim_1981 <-subset(claimyr_wh, year==1981, c(chal, tgt))
vtx_1981 <-subset(vtx, year==1981, -1)
net_1981<-graph_from_data_frame(d=claim_1981, v=vtx_1981, directed=T)

V(net_1981)$stateabb

centr_degree(net_1981)$res
round(eigen_centrality(net_1981)$vector, digits=3)
#subgraph_centrality(net_1981)

round(alpha.centrality(net_1981), digits=3)
round(power_centrality(net_1981), digits=3)




claim_1981_type <-subset(claimyr_wh, year==1981, c(chal, tgt, issue))
net_1981_type<-graph_from_data_frame(d=claim_1981_type, v=vtx_1981, directed=T)

E(net_1981_type)$color <- ifelse(E(net_1981_type)$issue==1, "red", 
                                 ifelse(E(net_1981_type)$issue==2, "blue", "black"))


windows()
plot(net_1981, edge.arrow.size=.4, vertex.size=4, 
     vertex.frame.color="gray", vertex.label=V(net_1981)$stateabb,
     edge.arrow.size=.5, vertex.size=4, 
     vertex.frame.color="white", 
     vertex.color="gray",
     vertex.label.cex=1,
     vertex.label.dist=1)

windows()
plot(net_1981_type, edge.arrow.size=.3, vertex.size=3.5, 
     vertex.frame.color="white", 
     vertex.color="gray",
     edge.color=E(net_1981_type)$color,
     vertex.label=V(net_1981)$stateabb,
     vertex.label.cex=0.8,
     vertex.label.dist=1)


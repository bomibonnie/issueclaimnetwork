#########################################################
# Name: Issue Claim Network in the Western Hemisphere   #
# Date: September 6, 2019                               #
# Author: Bomi Lee                                      #
# Input Files:  ICOWdyadyr.csv                          #
#########################################################

# Load packages
library(foreign)
library(tidyverse)
library(igraph)

# Load data
claimyr<-read.csv("ICOWdyadyr.csv")

# Keep variables necessary and region==1
## Note: change ccode to characters(not numeric)

## Edge list
claimyr_wh <- claimyr %>%
  filter(region==1) %>%
  select("issue", "chal", "tgt", "year", "icowsal") %>%
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


## Network sample, year==2001
# 2001
claim_2001 <-subset(claimyr_wh, year==2001, c(chal, tgt))
vtx_2001 <-subset(vtx3, year==2001, -1)
net_2001<-graph_from_data_frame(d=claim_2001, v=vtx_2001, directed=T)

windows()
plot(net_2001, edge.arrow.size=.4, vertex.size=4, 
     vertex.frame.color="gray")

claim_2001_type <-subset(claimyr_wh, year==2001, c(chal, tgt, issue))
net_2001_type<-graph_from_data_frame(d=claim_2001_type, v=vtx_2001, directed=T)

                 
E(net_2001_type)$color <- ifelse(E(net_2001_type)$issue==1, "red", 
                            ifelse(E(net_2001_type)$issue==2, "blue", "black"))


windows()
plot(net_2001_type, edge.arrow.size=.4, vertex.size=3.5, 
     vertex.frame.color="white", 
     vertex.color="white",
     edge.color=E(net_2001_type)$color)


# Libraries
library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)

library(networkD3)

data_l <- read.csv('data/resources_q1_q9.csv')


data_long <- data_l %>%
            pivot_longer(RESOURCES:PRIORITIES,
                         names_to = 'target',
                         values_to = 'value')

data_long <- data_long %>% group_by(target) %>% summarise(value = sum(value))

data_long$source <- 'Resources'

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(data_long$source), as.character(data_long$target)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
data_long$IDsource=match(data_long$source, nodes$name)-1 
data_long$IDtarget=match(data_long$target, nodes$name)-1

# prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Make the Network
sankeyNetwork(Links = data_long, Nodes = nodes,
                                  Source = "IDsource", Target = "IDtarget",
                                  Value = "value", NodeID = "name", 
                                  sinksRight=FALSE, colourScale=ColourScal,
                                  nodeWidth=40, fontSize=16, nodePadding=20,
                                  fontFamily = 'Arial')




# ggplot sankey -----------------------------------------------------------

install.packages("remotes")
remotes::install_github("davidsjoberg/ggsankey")
library(ggsankey)

# https://r-charts.com/flow/sankey-diagram-ggplot2/



library(tidyverse)
library(scales) # label wrap

q9 <- read.csv("data/q9_data_cleaned.csv")
q9

q9_themes <- q9 %>% group_by(Theme) %>% 
           summarise(sum = sum(Count))

# Theme                sum
# 1 Data_evidence         43
# 2 Resources             81
# 3 collab_engage         38
# 4 external_factors       2
# 5 priorities            45
# 6 quality_governance    40
# 7 use_data_evidence     35

theme_plot <- q9_themes %>% mutate(Theme = fct_reorder(Theme, sum, .desc = FALSE)) %>%
            ggplot(aes(x = Theme, y = sum, fill = Theme)) +
            geom_bar(position = "stack", stat = "identity") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 14),
                  legend.position = 'none') +
            ylab("Count") +
            xlab(" ") +
            ylim(0, 100) +
            scale_x_discrete(labels = c('resources' = "Resources",
                                        'priorities' = "Priorities",
                                        'data_evidence' = "Data & Evidence",
                                        'quality_governance' = "Quality of Governance",
                                        'collab_engage' = 'Collaboration & Engagement',
                                        'use_data_evidence' = 'Use of Data & Evidence',
                                        'external_factors' = 'External Factors')) +
            scale_fill_viridis(discrete = T)



# Sankey Figures ----------------------------------------------------------


# Libraries
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)

library(networkD3)

data_long <- read.csv('data/q9_data_sankey.csv')

### resources ###
resources_long <- data_long %>% filter(source == 'Resources') 

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(resources_long$source), as.character(resources_long$target)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
resources_long$IDsource=match(resources_long$source, nodes$name)-1 
resources_long$IDtarget=match(resources_long$target, nodes$name)-1

# prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Make the Network
resources_sankey <- sankeyNetwork(Links = resources_long, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", 
              sinksRight=FALSE, colourScale=ColourScal,
              nodeWidth=40, fontSize=16, nodePadding=20,
              fontFamily = 'Arial')


#### resources bar chart

resourcse_bar <- resources_long %>% ggplot(aes(x = reorder(target, +value), y = value)) + 
            geom_bar(position = "stack", stat = "identity") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 15)) +
            ylab("Count") +
            xlab(" ") +
            ylim(0,22) +
            ggtitle('Resources')


##### data_evidence ####

dat_evidence <- data_long %>% filter(source == 'Data and Evidence') 

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(dat_evidence$source),
                           as.character(dat_evidence$target)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
dat_evidence$IDsource=match(dat_evidence$source, nodes$name)-1 
dat_evidence$IDtarget=match(dat_evidence$target, nodes$name)-1

# prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#35b779","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Make the Network
datev_sankey <- sankeyNetwork(Links = dat_evidence, Nodes = nodes,
                                  Source = "IDsource", Target = "IDtarget",
                                  Value = "value", NodeID = "name", 
                                  sinksRight=FALSE, colourScale=ColourScal,
                                  nodeWidth=40, fontSize=16, nodePadding=20,
                                  fontFamily = 'Arial')


#### data ev barchart #####

data_bar <- dat_evidence %>% ggplot(aes(x = reorder(target, +value), y = value)) + 
            geom_bar(position = "stack", stat = "identity") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 15)) +
            ylab("Count") +
            xlab(" ") +
            ylim(0,22) +
            ggtitle('Data and Evidence')


#### quality of governance ####

governance <- data_long %>% filter(source == 'Quality of Governance Process') 

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(governance$source),
                           as.character(governance$target)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
governance$IDsource=match(governance$source, nodes$name)-1 
governance$IDtarget=match(governance$target, nodes$name)-1

# prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#21918c","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Make the Network
gov_sankey <- sankeyNetwork(Links = governance, Nodes = nodes,
                              Source = "IDsource", Target = "IDtarget",
                              Value = "value", NodeID = "name", 
                              sinksRight=FALSE, colourScale=ColourScal,
                              nodeWidth=40, fontSize=16, nodePadding=20,
                              fontFamily = 'Arial')



gov_bar <- governance %>% ggplot(aes(x = reorder(target, +value), y = value)) + 
            geom_bar(position = "stack", stat = "identity") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 15)) +
            ylab("Count") +
            xlab(" ") +
            ylim(0,22) +
            ggtitle('Quality of Governance Process')



#### use_data ####

use_data <- data_long %>% filter(source == 'Use of Data and Evidence') 

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(use_data$source),
                           as.character(use_data$target)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
use_data$IDsource=match(use_data$source, nodes$name)-1 
use_data$IDtarget=match(use_data$target, nodes$name)-1

# prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#443983","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Make the Network

use_sankey <- sankeyNetwork(Links = use_data, Nodes = nodes,
                            Source = "IDsource", Target = "IDtarget",
                            Value = "value", NodeID = "name", 
                            sinksRight=FALSE, colourScale=ColourScal,
                            nodeWidth=40, fontSize=16, nodePadding=20,
                            fontFamily = 'Arial')

##### data use barchart ###

use_bar <- use_data %>% ggplot(aes(x = reorder(target, +value), y = value)) + 
            geom_bar(position = "stack", stat = "identity") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 15)) +
            ylab("Count") +
            xlab(" ") +
            ylim(0,22) +
            ggtitle('Use of Data and Evidence')


#### priorities ####

prioties <- data_long %>% filter(source == 'Priorities') 

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(prioties$source),
                           as.character(prioties$target)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
prioties$IDsource=match(prioties$source, nodes$name)-1 
prioties$IDtarget=match(prioties$target, nodes$name)-1

# prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#90d743","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Make the Network

priorities_sank <- sankeyNetwork(Links = prioties, Nodes = nodes,
                            Source = "IDsource", Target = "IDtarget",
                            Value = "value", NodeID = "name", 
                            sinksRight=FALSE, colourScale=ColourScal,
                            nodeWidth=40, fontSize=16, nodePadding=20,
                            fontFamily = 'Arial')



# priorities barchart

priority_bar <- prioties %>% ggplot(aes(x = reorder(target, +value), y = value)) + 
            geom_bar(position = "stack", stat = "identity") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 15)) +
            ylab("Count") +
            xlab(" ") +
            ylim(0,22) +
            ggtitle('Priorities')



#### collab engagement ####

collab <- data_long %>% filter(source == 'Collaboration and Engagement') 

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(collab$source),
                           as.character(collab$target)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
collab$IDsource=match(collab$source, nodes$name)-1 
collab$IDtarget=match(collab$target, nodes$name)-1

# prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#31688e","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Make the Network

collab_sankey <- sankeyNetwork(Links = collab, Nodes = nodes,
                            Source = "IDsource", Target = "IDtarget",
                            Value = "value", NodeID = "name", 
                            sinksRight=FALSE, colourScale=ColourScal,
                            nodeWidth=40, fontSize=16, nodePadding=20,
                            fontFamily = 'Arial')

# collab barchart

collab_bar <- collab %>% ggplot(aes(x = reorder(target, +value), y = value)) + 
            geom_bar(position = "stack", stat = "identity") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 15)) +
            ylab("Count") +
            xlab(" ")  +
            ylim(0,22) +
            ggtitle('Collaboration and Engagement')




#### External Factors ####

external <- data_long %>% filter(source == 'External Factors') 

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(external$source),
                           as.character(external$target)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
external$IDsource=match(external$source, nodes$name)-1 
external$IDtarget=match(external$target, nodes$name)-1

# prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#440154","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Make the Network
external_sankey <- sankeyNetwork(Links = external, Nodes = nodes,
                               Source = "IDsource", Target = "IDtarget",
                               Value = "value", NodeID = "name", 
                               sinksRight=FALSE, colourScale=ColourScal,
                               nodeWidth=40, fontSize=16, nodePadding=20,
                               fontFamily = 'Arial')

# external chart

ext_bar <- external %>% ggplot(aes(x = reorder(target, +value), y = value)) + 
            geom_bar(position = "stack", stat = "identity") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 15)) +
            ylab("Count") +
            ylim(0,22) +
            xlab(" ")  

######## panel ############
library(patchwork)


theme_plot 

panel <- data_bar + resourcse_bar + 
            collab_bar + priority_bar + 
             gov_bar + 
            use_bar 

ggsave('output/q9_themes_panel.png',
       width = 15,
       height = 10)  

# Session Info ------------------------------------------------------------

sessionInfo()

# R version 4.3.0 (2023-04-21 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 11 x64 (build 22631)
# 
# Matrix products: default
# 
# 
# locale:
#   [1] LC_COLLATE=English_Canada.utf8  LC_CTYPE=English_Canada.utf8   
# [3] LC_MONETARY=English_Canada.utf8 LC_NUMERIC=C                   
# [5] LC_TIME=English_Canada.utf8    
# 
# time zone: America/New_York
# tzcode source: internal
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] patchwork_1.2.0   viridis_0.6.5     viridisLite_0.4.2 scales_1.3.0     
# [5] likert_1.3.5      xtable_1.8-4      lubridate_1.9.3   forcats_1.0.0    
# [9] stringr_1.5.1     dplyr_1.1.2       purrr_1.0.2       readr_2.1.5      
# [13] tidyr_1.3.1       tibble_3.2.1      ggplot2_3.5.0     tidyverse_2.0.0  
# 
# loaded via a namespace (and not attached):
#   [1] utf8_1.2.3        generics_0.1.3    stringi_1.8.3     lattice_0.22-5   
# [5] hms_1.1.3         magrittr_2.0.3    grid_4.3.0        timechange_0.3.0 
# [9] plyr_1.8.9        gridExtra_2.3     fansi_1.0.4       textshaping_0.3.7
# [13] mnormt_2.1.1      cli_3.6.1         rlang_1.1.3       munsell_0.5.0    
# [17] withr_3.0.0       tools_4.3.0       parallel_4.3.0    reshape2_1.4.4   
# [21] tzdb_0.4.0        colorspace_2.1-0  vctrs_0.6.5       R6_2.5.1         
# [25] lifecycle_1.0.4   psych_2.4.3       ragg_1.3.0        pkgconfig_2.0.3  
# [29] pillar_1.9.0      gtable_0.3.4      glue_1.6.2        Rcpp_1.0.12      
# [33] systemfonts_1.0.6 tidyselect_1.2.1  rstudioapi_0.15.0 farver_2.1.1     
# [37] nlme_3.1-164      labeling_0.4.3    compiler_4.3.0   


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(scales) # label wrap
library(viridis)
library(patchwork)
library(hrbrthemes)


# Solutions (Q9) data -----------------------------------------------------

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

write.csv(q9_themes, "data/q9_themes_summaries.csv")


# quick look at data

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



# Figure ----------------------------------------------------------
# break down results by overall theme
# organized long version of data
data_long <- read.csv('data/q9_data_sankey.csv')

### resources ####
resources_long <- data_long %>% filter(source == 'Resources') 

resources_bar <- resources_long %>% 
  ggplot(aes(x = reorder(target, +value), y = value)) + 
            geom_bar(position = "stack", stat = "identity",
                     fill = '#569DA9') +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 15)) +
            ylab("Count") +
            xlab(" ") +
            ylim(0,22) +
            ggtitle('Resources')


##### data and evidence ####

dat_evidence <- data_long %>% filter(source == 'Data and Evidence') 

data_bar <- dat_evidence %>% 
  ggplot(aes(x = reorder(target, +value), y = value)) + 
            geom_bar(position = "stack", stat = "identity",
                     fill = '#145277') +
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

gov_bar <- governance %>% ggplot(aes(x = reorder(target, +value), y = value)) + 
            geom_bar(position = "stack", stat = "identity", fill = '#408498') +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 15)) +
            ylab("Count") +
            xlab(" ") +
            ylim(0,22) +
            ggtitle('Quality of Governance Process')



#### use of data and evidence ####

use_data <- data_long %>% filter(source == 'Use of Data and Evidence') 

use_bar <- use_data %>% ggplot(aes(x = reorder(target, +value), y = value)) + 
            geom_bar(position = "stack", stat = "identity", fill = '#83D0CB') +
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

priority_bar <- prioties %>% ggplot(aes(x = reorder(target, +value), y = value)) + 
            geom_bar(position = "stack", stat = "identity", fill = '#6CB6BA') +
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

collab_bar <- collab %>% ggplot(aes(x = reorder(target, +value), y = value)) + 
            geom_bar(position = "stack", stat = "identity", fill = "#2A6B87") +
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

ext_bar <- external %>% ggplot(aes(x = reorder(target, +value), y = value)) + 
            geom_bar(position = "stack", stat = "identity", fill = '#E76F51') +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 15)) +
            ylab("Count") +
            ylim(0,22) +
            xlab(" ")  

######## panel ############
theme_plot 

panel <- data_bar + resources_bar + 
  collab_bar + priority_bar + 
  gov_bar + use_bar + 
  plot_layout(ncol = 2)

ggsave('output/q9_themes_panel.jpg',
       width = 16,
       height = 15)  

ggsave('output/q9_themes_panel.tiff',
       width = 16,
       height = 15,
       dpi = 300) 

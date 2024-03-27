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
#   [1] LC_COLLATE=English_Canada.utf8  LC_CTYPE=English_Canada.utf8    LC_MONETARY=English_Canada.utf8
# [4] LC_NUMERIC=C                    LC_TIME=English_Canada.utf8    
# 
# time zone: America/New_York
# tzcode source: internal
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] ggpubr_0.6.0      fmsb_0.7.6        patchwork_1.2.0   viridis_0.6.5     viridisLite_0.4.2
# [6] scales_1.3.0      lubridate_1.9.3   forcats_1.0.0     stringr_1.5.1     dplyr_1.1.2      
# [11] purrr_1.0.2       readr_2.1.5       tidyr_1.3.1       tibble_3.2.1      ggplot2_3.5.0    
# [16] tidyverse_2.0.0  
# 
# loaded via a namespace (and not attached):
#   [1] utf8_1.2.3        generics_0.1.3    rstatix_0.7.2     stringi_1.8.3     hms_1.1.3        
# [6] magrittr_2.0.3    grid_4.3.0        timechange_0.3.0  backports_1.4.1   gridExtra_2.3    
# [11] fansi_1.0.4       textshaping_0.3.7 abind_1.4-5       cli_3.6.1         rlang_1.1.3      
# [16] munsell_0.5.0     withr_3.0.0       tools_4.3.0       tzdb_0.4.0        ggsignif_0.6.4   
# [21] colorspace_2.1-0  broom_1.0.5       vctrs_0.6.5       R6_2.5.1          lifecycle_1.0.4  
# [26] car_3.1-2         ragg_1.3.0        pkgconfig_2.0.3   pillar_1.9.0      gtable_0.3.4     
# [31] glue_1.6.2        systemfonts_1.0.6 tidyselect_1.2.1  rstudioapi_0.15.0 farver_2.1.1     
# [36] carData_3.0-5     labeling_0.4.3    compiler_4.3.0


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(fmsb)
library(forcats)
library(patchwork)
library(ggpubr) #get_legend

q1 <- read.csv("data/question1_28jun23.csv")

q1_sum <- q1
q1_sum$sum <- rowSums(q1[,3:7])

#### Summed Total #####
q1_sum %>% mutate(CODE = fct_reorder(CODE, sum, .desc = FALSE)) %>%
            ggplot(aes(x = CODE, y = sum)) +
            geom_segment(aes(x = CODE, xend = CODE, y = 0, yend = sum),
                         lwd = 2) +
            geom_point( size = 6, alpha = 1) +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 13)) +
            ylab("Count") +
            xlab(" ") 

q1_sum %>%  mutate(THEME = fct_reorder(THEME, sum, .desc = TRUE)) %>% 
  ggplot(aes(x = THEME , y = sum)) +
            geom_segment(aes(x = THEME , xend = THEME , y = 0, yend = sum),
                         lwd = 2) +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 13)) +
            ylab("Count") +
            xlab(" ") 

######## Stacked Bar Chart ########

# create a dataset

q1_long <- q1 %>% select(CODE,RANK1:RANK5) %>%
            pivot_longer(RANK1:RANK5,
                    names_to = "rank",
                    values_to = "count")

q1_long$CODE <- as.factor(q1_long$CODE)


q1_long$rank <- factor(q1_long$rank, levels = c('RANK5',
                                                'RANK4',
                                                'RANK3',
                                                'RANK2',
                                                'RANK1'))



unique(q1_long$CODE)


# Stacked

colours = c("RANK1" = "#264653",
            "RANK2" = "#2A9D8F",
            "RANK3" = "#E9C46A",
            "RANK4" = "#F4A261",
            "RANK5" = "#e76F51")


unique(q1_long$CODE)

q1_long$CODE <- recode(q1_long$CODE,
                SITE = "SITE_INFO",
                SYSTEMS = "SOCPOLSCI_SYSTEMS",
                TIMELINE = "TIMELINES",
                INDIGENOUS = "INDIGENOUS_RIGHTS",
                ACCESS = "ACCESS_TO_EVIDENCE",
                LONGTERM = "LONGTERM_DATA",
                LANDACCESS = "LAND_ACCESS",
                CUMULATIVE = "CUMULATIVE_IMPACTS",
                IMPACT = "IMPACT_INFO",
                INTERNALSUPPORT = "INTERNAL_SUPPORT",
                RESOURCESCAPACITY = "RESOURCES_CAPACITY",
                COMMCOLLAB = "COMMUNICATION_COLLABORATION",
                SUCCESS = "SUCCESS_CRITERIA",
                CLIMATECHANGE = "CLIMATE_CHANGE",
                SPECIES = "SPECIES_DATA",
                EXTERNALSUPPORT = "EXTERNAL_SUPPORT")



q1_bar <- q1_long %>%
            ggplot(aes(fill = rank, y = count, #label = count,
                       x = factor(CODE,
                                  level = c('COSTS',
                                            'SITE_INFO',
                                            'JUSTICE',
                                            'CONSENSUS',
                                            'ECOLOGY',
                                            'RESPONSIBILITIES',
                                            'EXPERTISE',
                                            'SOCPOLSCI_SYSTEMS',
                                            'TIMELINES',
                                            'INDIGENOUS_RIGHTS',
                                            'ACCESS_TO_EVIDENCE',
                                            'LONGTERM_DATA',
                                            'TRADEOFFS',
                                            'LAND_ACCESS',
                                            'PERSONNEL',
                                            'CUMULATIVE_IMPACTS',
                                            'IMPACT_INFO',
                                            'NATURE',
                                            'SCIENCE',
                                            'PREDICTION',
                                            'IMPLEMENTATION',
                                            'METHODS',
                                            'INTERNAL_SUPPORT',
                                            'RESOURCES_CAPACITY',
                                            'LEGISLATION',
                                            'OTHER',
                                            'COMMUNICATION_COLLABORATION',
                                            'SUCCESS_CRITERIA',
                                            'CLIMATE_CHANGE',
                                            'SPECIES_DATA',
                                            'EXTERNAL_SUPPORT',
                                            'POLITICS',
                                            'DATA',
                                            'FUNDING'
                                            )))) + 
            geom_bar(position = "stack", stat = "identity") +
            coord_flip() +
            xlab(" ") +
            ylab("Count") +
            theme_minimal() +
            theme(axis.text = element_text(size = 16),
                  axis.title = element_text(size = 16),
                  legend.position = "none",
                  legend.key.size = unit(0.7, 'cm'),
                  legend.text = element_text(size = 10),
                  legend.direction = "horizontal",
                  legend.title = element_blank()) +
            # geom_text(size = 4, alpha = 1, colour = "white",
            #           position = position_stack(vjust = 0.55)) +
            guides(fill = guide_legend(reverse = TRUE,
                                       label.position = "bottom")) +
            scale_fill_manual(values = colours) +
            scale_x_discrete(labels = c('COSTS' = "Costs",
                                        'SITE_INFO' = "Site Info",
                                        'JUSTICE' = 'Justice',
                                        'CONSENSUS' = 'Consensus',
                                        'ECOLOGY' = 'Ecology',
                                        'RESPONSIBILITIES' = 'Responsibilities',
                                        'EXPERTISE' = 'Expertise',
                                        'SOCPOLSCI_SYSTEMS' = 'Soc-Poli-Sci Systems',
                                        'TIMELINES' = 'Timelines',
                                        'INDIGENOUS_RIGHTS' = 'Indigenous Rights',
                                        'ACCESS_TO_EVIDENCE' = 'Access to evidence',
                                        'LONGTERM_DATA' = 'Long-term data',
                                        'TRADEOFFS' = 'Tradeoffs',
                                        'LAND_ACCESS' = 'Land access',
                                        'PERSONNEL' = 'Personnel',
                                        'CUMULATIVE_IMPACTS' = 'Cumulative impacts',
                                        'IMPACT_INFO' = 'Impact info',
                                        'NATURE' = 'Nature',
                                        'SCIENCE' = 'Science',
                                        'PREDICTION' = 'Prediction',
                                        'IMPLEMENTATION' = 'Implementation',
                                        'METHODS' = 'Methods',
                                        'INTERNAL_SUPPORT' = 'Internal support',
                                        'RESOURCES_CAPACITY' = 'Resources/Capacity',
                                        'LEGISLATION' = 'Legislation',
                                        'OTHER' = 'Other',
                                        'COMMUNICATION_COLLABORATION' = 'Communcation/Collaboration',
                                        'SUCCESS_CRITERIA' = 'Success criteria',
                                        'CLIMATE_CHANGE' = 'Climate change',
                                        'SPECIES_DATA' = 'Species data',
                                        'EXTERNAL_SUPPORT' = 'External support',
                                        'POLITICS' = 'Politics',
                                        'DATA' = 'Data',
                                        'FUNDING' = 'Funding'))
q1_bar 

ggsave("output/q1_bar.jpeg",
       width = 10,
       height = 14)

# c(0.8, 0.15)

# Each Rank ---------------------------------------------------------------
colours = c("RANK1" = "#264653",
            "RANK2" = "#2A9D8F",
            "RANK3" = "#E9C46A",
            "RANK4" = "#F4A261",
            "RANK5" = "#e76F51")

q1$CODE <- recode(q1$CODE,
                       SITE = "SITE_INFO",
                       SYSTEMS = "SOCPOLSCI_SYSTEMS",
                       TIMELINE = "TIMELINES",
                       INDIGENOUS = "INDIGENOUS_RIGHTS",
                       ACCESS = "ACCESS_TO_EVIDENCE",
                       LONGTERM = "LONGTERM_DATA",
                       LANDACCESS = "LAND_ACCESS",
                       CUMULATIVE = "CUMULATIVE_IMPACTS",
                       IMPACT = "IMPACT_INFO",
                       INTERNALSUPPORT = "INTERNAL_SUPPORT",
                       RESOURCESCAPACITY = "RESOURCES_CAPACITY",
                       COMMCOLLAB = "COMMUNICATION_COLLABORATION",
                       SUCCESS = "SUCCESS_CRITERIA",
                       CLIMATECHANGE = "CLIMATE_CHANGE",
                       SPECIES = "SPECIES_DATA",
                       EXTERNALSUPPORT = "EXTERNAL_SUPPORT")

######### Rank 1 ############
rank1 <- q1 %>% select(CODE:RANK1) %>%
            ggplot(aes(y = RANK1,
                       x = factor(CODE,
                                  level = c('COSTS',
                                            'SITE_INFO',
                                            'JUSTICE',
                                            'CONSENSUS',
                                            'ECOLOGY',
                                            'RESPONSIBILITIES',
                                            'EXPERTISE',
                                            'SOCPOLSCI_SYSTEMS',
                                            'TIMELINES',
                                            'INDIGENOUS_RIGHTS',
                                            'ACCESS_TO_EVIDENCE',
                                            'LONGTERM_DATA',
                                            'TRADEOFFS',
                                            'LAND_ACCESS',
                                            'PERSONNEL',
                                            'CUMULATIVE_IMPACTS',
                                            'IMPACT_INFO',
                                            'NATURE',
                                            'SCIENCE',
                                            'PREDICTION',
                                            'IMPLEMENTATION',
                                            'METHODS',
                                            'INTERNAL_SUPPORT',
                                            'RESOURCES_CAPACITY',
                                            'LEGISLATION',
                                            'OTHER',
                                            'COMMUNICATION_COLLABORATION',
                                            'SUCCESS_CRITERIA',
                                            'CLIMATE_CHANGE',
                                            'SPECIES_DATA',
                                            'EXTERNAL_SUPPORT',
                                            'POLITICS',
                                            'DATA',
                                            'FUNDING'
                                  )))) + 
            geom_bar(position = "stack", stat = "identity",
                     fill = "#264653") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 10)) +
            ylab("Count") +
            xlab(" ") +
            ggtitle("Rank 1") +
            ylim(0,35) +
            scale_x_discrete(labels = c('COSTS' = "Costs",
                                        'SITE_INFO' = "Site Info",
                                        'JUSTICE' = 'Justice',
                                        'CONSENSUS' = 'Consensus',
                                        'ECOLOGY' = 'Ecology',
                                        'RESPONSIBILITIES' = 'Responsibilities',
                                        'EXPERTISE' = 'Expertise',
                                        'SOCPOLSCI_SYSTEMS' = 'Soc-Poli-Sci Systems',
                                        'TIMELINES' = 'Timelines',
                                        'INDIGENOUS_RIGHTS' = 'Indigenous Rights',
                                        'ACCESS_TO_EVIDENCE' = 'Access to evidence',
                                        'LONGTERM_DATA' = 'Long-term data',
                                        'TRADEOFFS' = 'Tradeoffs',
                                        'LAND_ACCESS' = 'Land access',
                                        'PERSONNEL' = 'Personnel',
                                        'CUMULATIVE_IMPACTS' = 'Cumulative impacts',
                                        'IMPACT_INFO' = 'Impact info',
                                        'NATURE' = 'Nature',
                                        'SCIENCE' = 'Science',
                                        'PREDICTION' = 'Prediction',
                                        'IMPLEMENTATION' = 'Implementation',
                                        'METHODS' = 'Methods',
                                        'INTERNAL_SUPPORT' = 'Internal support',
                                        'RESOURCES_CAPACITY' = 'Resources/Capacity',
                                        'LEGISLATION' = 'Legislation',
                                        'OTHER' = 'Other',
                                        'COMMUNICATION_COLLABORATION' = 'Communcation/Collaboration',
                                        'SUCCESS_CRITERIA' = 'Success criteria',
                                        'CLIMATE_CHANGE' = 'Climate change',
                                        'SPECIES_DATA' = 'Species data',
                                        'EXTERNAL_SUPPORT' = 'External support',
                                        'POLITICS' = 'Politics',
                                        'DATA' = 'Data',
                                        'FUNDING' = 'Funding'))

rank1

######### Rank 2 ###############
rank2 <- q1 %>% select(CODE:RANK2) %>%
            ggplot(aes(y = RANK2,
                       x = factor(CODE,
                                  level = c('COSTS',
                                            'SITE_INFO',
                                            'JUSTICE',
                                            'CONSENSUS',
                                            'ECOLOGY',
                                            'RESPONSIBILITIES',
                                            'EXPERTISE',
                                            'SOCPOLSCI_SYSTEMS',
                                            'TIMELINES',
                                            'INDIGENOUS_RIGHTS',
                                            'ACCESS_TO_EVIDENCE',
                                            'LONGTERM_DATA',
                                            'TRADEOFFS',
                                            'LAND_ACCESS',
                                            'PERSONNEL',
                                            'CUMULATIVE_IMPACTS',
                                            'IMPACT_INFO',
                                            'NATURE',
                                            'SCIENCE',
                                            'PREDICTION',
                                            'IMPLEMENTATION',
                                            'METHODS',
                                            'INTERNAL_SUPPORT',
                                            'RESOURCES_CAPACITY',
                                            'LEGISLATION',
                                            'OTHER',
                                            'COMMUNICATION_COLLABORATION',
                                            'SUCCESS_CRITERIA',
                                            'CLIMATE_CHANGE',
                                            'SPECIES_DATA',
                                            'EXTERNAL_SUPPORT',
                                            'POLITICS',
                                            'DATA',
                                            'FUNDING'
                                  )))) + 
            geom_bar(position = "stack", stat = "identity",
                     fill = "#2A9D8F") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 10)) +
            ylab("Count") +
            xlab(" ") +
            ggtitle("Rank 2") +
            ylim(0,35) +
            scale_x_discrete(labels = c('COSTS' = "Costs",
                                        'SITE_INFO' = "Site Info",
                                        'JUSTICE' = 'Justice',
                                        'CONSENSUS' = 'Consensus',
                                        'ECOLOGY' = 'Ecology',
                                        'RESPONSIBILITIES' = 'Responsibilities',
                                        'EXPERTISE' = 'Expertise',
                                        'SOCPOLSCI_SYSTEMS' = 'Soc-Poli-Sci Systems',
                                        'TIMELINES' = 'Timelines',
                                        'INDIGENOUS_RIGHTS' = 'Indigenous Rights',
                                        'ACCESS_TO_EVIDENCE' = 'Access to evidence',
                                        'LONGTERM_DATA' = 'Long-term data',
                                        'TRADEOFFS' = 'Tradeoffs',
                                        'LAND_ACCESS' = 'Land access',
                                        'PERSONNEL' = 'Personnel',
                                        'CUMULATIVE_IMPACTS' = 'Cumulative impacts',
                                        'IMPACT_INFO' = 'Impact info',
                                        'NATURE' = 'Nature',
                                        'SCIENCE' = 'Science',
                                        'PREDICTION' = 'Prediction',
                                        'IMPLEMENTATION' = 'Implementation',
                                        'METHODS' = 'Methods',
                                        'INTERNAL_SUPPORT' = 'Internal support',
                                        'RESOURCES_CAPACITY' = 'Resources/Capacity',
                                        'LEGISLATION' = 'Legislation',
                                        'OTHER' = 'Other',
                                        'COMMUNICATION_COLLABORATION' = 'Communcation/Collaboration',
                                        'SUCCESS_CRITERIA' = 'Success criteria',
                                        'CLIMATE_CHANGE' = 'Climate change',
                                        'SPECIES_DATA' = 'Species data',
                                        'EXTERNAL_SUPPORT' = 'External support',
                                        'POLITICS' = 'Politics',
                                        'DATA' = 'Data',
                                        'FUNDING' = 'Funding'))

rank2

####### Rank 3 ##############
rank3<- q1 %>% select(CODE:RANK3) %>%
            ggplot(aes(y = RANK3,
                       x = factor(CODE,
                                  level = c('COSTS',
                                            'SITE_INFO',
                                            'JUSTICE',
                                            'CONSENSUS',
                                            'ECOLOGY',
                                            'RESPONSIBILITIES',
                                            'EXPERTISE',
                                            'SOCPOLSCI_SYSTEMS',
                                            'TIMELINES',
                                            'INDIGENOUS_RIGHTS',
                                            'ACCESS_TO_EVIDENCE',
                                            'LONGTERM_DATA',
                                            'TRADEOFFS',
                                            'LAND_ACCESS',
                                            'PERSONNEL',
                                            'CUMULATIVE_IMPACTS',
                                            'IMPACT_INFO',
                                            'NATURE',
                                            'SCIENCE',
                                            'PREDICTION',
                                            'IMPLEMENTATION',
                                            'METHODS',
                                            'INTERNAL_SUPPORT',
                                            'RESOURCES_CAPACITY',
                                            'LEGISLATION',
                                            'OTHER',
                                            'COMMUNICATION_COLLABORATION',
                                            'SUCCESS_CRITERIA',
                                            'CLIMATE_CHANGE',
                                            'SPECIES_DATA',
                                            'EXTERNAL_SUPPORT',
                                            'POLITICS',
                                            'DATA',
                                            'FUNDING'
                                  )))) + 
            geom_bar(position = "stack", stat = "identity",
                     fill = "#E9C46A") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 10)) +
            ylab("Count") +
            xlab(" ") +
            ggtitle("Rank 3") +
            ylim(0,35) +
            scale_x_discrete(labels = c('COSTS' = "Costs",
                                        'SITE_INFO' = "Site Info",
                                        'JUSTICE' = 'Justice',
                                        'CONSENSUS' = 'Consensus',
                                        'ECOLOGY' = 'Ecology',
                                        'RESPONSIBILITIES' = 'Responsibilities',
                                        'EXPERTISE' = 'Expertise',
                                        'SOCPOLSCI_SYSTEMS' = 'Soc-Poli-Sci Systems',
                                        'TIMELINES' = 'Timelines',
                                        'INDIGENOUS_RIGHTS' = 'Indigenous Rights',
                                        'ACCESS_TO_EVIDENCE' = 'Access to evidence',
                                        'LONGTERM_DATA' = 'Long-term data',
                                        'TRADEOFFS' = 'Tradeoffs',
                                        'LAND_ACCESS' = 'Land access',
                                        'PERSONNEL' = 'Personnel',
                                        'CUMULATIVE_IMPACTS' = 'Cumulative impacts',
                                        'IMPACT_INFO' = 'Impact info',
                                        'NATURE' = 'Nature',
                                        'SCIENCE' = 'Science',
                                        'PREDICTION' = 'Prediction',
                                        'IMPLEMENTATION' = 'Implementation',
                                        'METHODS' = 'Methods',
                                        'INTERNAL_SUPPORT' = 'Internal support',
                                        'RESOURCES_CAPACITY' = 'Resources/Capacity',
                                        'LEGISLATION' = 'Legislation',
                                        'OTHER' = 'Other',
                                        'COMMUNICATION_COLLABORATION' = 'Communcation/Collaboration',
                                        'SUCCESS_CRITERIA' = 'Success criteria',
                                        'CLIMATE_CHANGE' = 'Climate change',
                                        'SPECIES_DATA' = 'Species data',
                                        'EXTERNAL_SUPPORT' = 'External support',
                                        'POLITICS' = 'Politics',
                                        'DATA' = 'Data',
                                        'FUNDING' = 'Funding'))

rank3


####### Rank 4 #############

rank4<- q1 %>% select(CODE:RANK4) %>%
            ggplot(aes(y = RANK4,
                       x = factor(CODE,
                                  level = c('COSTS',
                                            'SITE_INFO',
                                            'JUSTICE',
                                            'CONSENSUS',
                                            'ECOLOGY',
                                            'RESPONSIBILITIES',
                                            'EXPERTISE',
                                            'SOCPOLSCI_SYSTEMS',
                                            'TIMELINES',
                                            'INDIGENOUS_RIGHTS',
                                            'ACCESS_TO_EVIDENCE',
                                            'LONGTERM_DATA',
                                            'TRADEOFFS',
                                            'LAND_ACCESS',
                                            'PERSONNEL',
                                            'CUMULATIVE_IMPACTS',
                                            'IMPACT_INFO',
                                            'NATURE',
                                            'SCIENCE',
                                            'PREDICTION',
                                            'IMPLEMENTATION',
                                            'METHODS',
                                            'INTERNAL_SUPPORT',
                                            'RESOURCES_CAPACITY',
                                            'LEGISLATION',
                                            'OTHER',
                                            'COMMUNICATION_COLLABORATION',
                                            'SUCCESS_CRITERIA',
                                            'CLIMATE_CHANGE',
                                            'SPECIES_DATA',
                                            'EXTERNAL_SUPPORT',
                                            'POLITICS',
                                            'DATA',
                                            'FUNDING'
                                  )))) + 
            geom_bar(position = "stack", stat = "identity",
                     fill = "#F4A261") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 10)) +
            ylab("Count") +
            xlab(" ") +
            ggtitle("Rank 4") +
            ylim(0,35) +
            scale_x_discrete(labels = c('COSTS' = "Costs",
                                        'SITE_INFO' = "Site Info",
                                        'JUSTICE' = 'Justice',
                                        'CONSENSUS' = 'Consensus',
                                        'ECOLOGY' = 'Ecology',
                                        'RESPONSIBILITIES' = 'Responsibilities',
                                        'EXPERTISE' = 'Expertise',
                                        'SOCPOLSCI_SYSTEMS' = 'Soc-Poli-Sci Systems',
                                        'TIMELINES' = 'Timelines',
                                        'INDIGENOUS_RIGHTS' = 'Indigenous Rights',
                                        'ACCESS_TO_EVIDENCE' = 'Access to evidence',
                                        'LONGTERM_DATA' = 'Long-term data',
                                        'TRADEOFFS' = 'Tradeoffs',
                                        'LAND_ACCESS' = 'Land access',
                                        'PERSONNEL' = 'Personnel',
                                        'CUMULATIVE_IMPACTS' = 'Cumulative impacts',
                                        'IMPACT_INFO' = 'Impact info',
                                        'NATURE' = 'Nature',
                                        'SCIENCE' = 'Science',
                                        'PREDICTION' = 'Prediction',
                                        'IMPLEMENTATION' = 'Implementation',
                                        'METHODS' = 'Methods',
                                        'INTERNAL_SUPPORT' = 'Internal support',
                                        'RESOURCES_CAPACITY' = 'Resources/Capacity',
                                        'LEGISLATION' = 'Legislation',
                                        'OTHER' = 'Other',
                                        'COMMUNICATION_COLLABORATION' = 'Communcation/Collaboration',
                                        'SUCCESS_CRITERIA' = 'Success criteria',
                                        'CLIMATE_CHANGE' = 'Climate change',
                                        'SPECIES_DATA' = 'Species data',
                                        'EXTERNAL_SUPPORT' = 'External support',
                                        'POLITICS' = 'Politics',
                                        'DATA' = 'Data',
                                        'FUNDING' = 'Funding'))


rank4

######### Rank 5 ###############
rank5<- q1 %>% select(CODE:RANK5) %>%
            ggplot(aes(y = RANK5,
                       x = factor(CODE,
                                  level = c('COSTS',
                                            'SITE_INFO',
                                            'JUSTICE',
                                            'CONSENSUS',
                                            'ECOLOGY',
                                            'RESPONSIBILITIES',
                                            'EXPERTISE',
                                            'SOCPOLSCI_SYSTEMS',
                                            'TIMELINES',
                                            'INDIGENOUS_RIGHTS',
                                            'ACCESS_TO_EVIDENCE',
                                            'LONGTERM_DATA',
                                            'TRADEOFFS',
                                            'LAND_ACCESS',
                                            'PERSONNEL',
                                            'CUMULATIVE_IMPACTS',
                                            'IMPACT_INFO',
                                            'NATURE',
                                            'SCIENCE',
                                            'PREDICTION',
                                            'IMPLEMENTATION',
                                            'METHODS',
                                            'INTERNAL_SUPPORT',
                                            'RESOURCES_CAPACITY',
                                            'LEGISLATION',
                                            'OTHER',
                                            'COMMUNICATION_COLLABORATION',
                                            'SUCCESS_CRITERIA',
                                            'CLIMATE_CHANGE',
                                            'SPECIES_DATA',
                                            'EXTERNAL_SUPPORT',
                                            'POLITICS',
                                            'DATA',
                                            'FUNDING'
                                  )))) + 
            geom_bar(position = "stack", stat = "identity",
                     fill = "#e76F51") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 10)) +
            ylab("Count") +
            xlab(" ") +
            ggtitle("Rank 5") +
            ylim(0,35) +
            scale_x_discrete(labels = c('COSTS' = "Costs",
                                        'SITE_INFO' = "Site Info",
                                        'JUSTICE' = 'Justice',
                                        'CONSENSUS' = 'Consensus',
                                        'ECOLOGY' = 'Ecology',
                                        'RESPONSIBILITIES' = 'Responsibilities',
                                        'EXPERTISE' = 'Expertise',
                                        'SOCPOLSCI_SYSTEMS' = 'Soc-Poli-Sci Systems',
                                        'TIMELINES' = 'Timelines',
                                        'INDIGENOUS_RIGHTS' = 'Indigenous Rights',
                                        'ACCESS_TO_EVIDENCE' = 'Access to evidence',
                                        'LONGTERM_DATA' = 'Long-term data',
                                        'TRADEOFFS' = 'Tradeoffs',
                                        'LAND_ACCESS' = 'Land access',
                                        'PERSONNEL' = 'Personnel',
                                        'CUMULATIVE_IMPACTS' = 'Cumulative impacts',
                                        'IMPACT_INFO' = 'Impact info',
                                        'NATURE' = 'Nature',
                                        'SCIENCE' = 'Science',
                                        'PREDICTION' = 'Prediction',
                                        'IMPLEMENTATION' = 'Implementation',
                                        'METHODS' = 'Methods',
                                        'INTERNAL_SUPPORT' = 'Internal support',
                                        'RESOURCES_CAPACITY' = 'Resources/Capacity',
                                        'LEGISLATION' = 'Legislation',
                                        'OTHER' = 'Other',
                                        'COMMUNICATION_COLLABORATION' = 'Communcation/Collaboration',
                                        'SUCCESS_CRITERIA' = 'Success criteria',
                                        'CLIMATE_CHANGE' = 'Climate change',
                                        'SPECIES_DATA' = 'Species data',
                                        'EXTERNAL_SUPPORT' = 'External support',
                                        'POLITICS' = 'Politics',
                                        'DATA' = 'Data',
                                        'FUNDING' = 'Funding'))
rank5

# Panel -------------------------------------------------------------------
library(patchwork)

ranks <- rank1 + rank2 + rank3 + rank4 + rank5 +
            plot_layout(ncol = 2)


panel_test <- q1_bar + ranks

ggsave("output/panel_test_all.jpg",
       width = 20,
       height = 14)


ggsave("output/panel_final.jpg",
       width = 20,
       height = 14)


ggsave("output/panel_final.tiff",
       width = 20,
       height = 14)


ggsave("output/panel_final.pdf",
       width = 20,
       height = 14)



# Themes ------------------------------------------------------------------
theme_long <- q1 %>% select(THEME,RANK1:RANK5) %>%
            pivot_longer(RANK1:RANK5,
                         names_to = "rank",
                         values_to = "count")

theme_long$THEME <- as.factor(theme_long$THEME)


theme_long$rank <- factor(theme_long$rank, levels = c('RANK5',
                                                'RANK4',
                                                'RANK3',
                                                'RANK2',
                                                'RANK1'))



unique(theme_long$THEME)


# Stacked

colours = c("RANK1" = "#264653",
            "RANK2" = "#2A9D8F",
            "RANK3" = "#E9C46A",
            "RANK4" = "#F4A261",
            "RANK5" = "#e76F51")

q1_sum %>% group_by(THEME) %>% 
            summarise(summed = sum(sum)) %>% 
            arrange(desc(summed))

#   THEME                 summed
# 1 Data                     160
# 2 Resources                129
# 3 Governance               127
# 4 Evidence                 106
# 5 Public_support            60
# 6 Social_complexity         59
# 7 Ecological_complexity     51
# 8 Other                     24


# THEME	            RANK1	RANK2	RANK3	RANK4	RANK5	Total
# Data	                52	45	  30	   23	   10   160
# Resources	            40	31	  25	   19	   14   129
# Governance	          46	36	  22	   11	   12   127
# Evidence	            14	24	  36	   21	   11   106
# Public_support	       9	16	  20	   10	   5	   60
# Social_complexity	    13	11	  17	   10	   8	   59
# Ecological_complexity	13	10	  11	   12	   5	   51
# Other	                 2	4		  8	     6	   4     24


theme_bar <- theme_long %>%
            ggplot(aes(fill = rank, y = count, #label = count,
                       x = factor(THEME,
                                  level = c('Other',
                                 'Ecological_complexity',
                                 'Social_complexity',
                                 'Public_support',
                                 'Evidence',
                                 'Governance',
                                 'Resources',
                                 'Data')))) + 
            geom_bar(position = "stack", stat = "identity") +
            coord_flip() +
            xlab(" ") +
            ylab("Count") +
            theme_minimal() +
            theme(axis.text = element_text(size = 16),
                  axis.title = element_text(size = 16),
                  legend.key.size = unit(1, 'cm'),
                  legend.text = element_text(size = 10),
                  legend.direction = "horizontal",
                  legend.title = element_blank(),
                  legend.position = "none",
                  plot.title = element_text(size = 16, face = "bold")) +
            scale_fill_manual(values = colours) +
            scale_x_discrete(labels = c('Other',
                                        'Ecological_complexity' = 'Ecological Complexity',
                                        'Social_complexity' = 'Social Complexity',
                                        'Public_support' = 'Public Support',
                                        'Evidence',
                                        'Governance',
                                        'Resources',
                                        'Data')) +
            ggtitle('Themes') 


            # geom_text(size = 4, alpha = 1, colour = "white",
            #           position = position_stack(vjust = 0.55)) 
         
          

legend <- get_legend(theme_bar)
as_ggplot(legend) 

ggsave('output/q1_themes_only.jpg')

### Resources ranks #####

resources_long <- q1 %>% filter(THEME == "Resources") %>% 
            pivot_longer(RANK1:RANK5,
                         names_to = "rank",
                         values_to = "count")


resources_long$rank <- factor(resources_long$rank, levels = c('RANK5',
                                                'RANK4',
                                                'RANK3',
                                                'RANK2',
                                                'RANK1'))
unique(resources_long$CODE)

resources <- resources_long %>% 
            ggplot(aes(y = count, fill = rank,
                       x = factor(CODE,
                                  level = c('EXPERTISE',
                                            'TIMELINE',
                                            'PERSONNEL',
                                            'RESOURCESCAPACITY',
                                            'FUNDING')))) + 
            geom_bar(position = "stack", stat = "identity") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 14),
                  plot.title = element_text(size = 16, face = "bold"),
                  legend.position = "none") +
            ylab("Count") +
            xlab(" ") +
            ylim(0, 80) + 
            scale_fill_manual(values = colours) +
            scale_x_discrete(labels = c('EXPERTISE' = "Expertise",
                                        'TIMELINE' = "Timelines",
                                        'PERSONNEL' = 'Personnel',
                                        'RESOURCESCAPACITY' = 'Resources/Capacity',
                                        'FUNDING' = 'Funding')) +
            ggtitle('Resources')




####### Data ######


data_long <- q1 %>% filter(THEME == "Data") %>% 
            pivot_longer(RANK1:RANK5,
                         names_to = "rank",
                         values_to = "count")


data_long$rank <- factor(data_long$rank, levels = c('RANK5',
                                                              'RANK4',
                                                              'RANK3',
                                                              'RANK2',
                                                              'RANK1'))


unique(data_long$CODE)


data <- data_long %>% 
            ggplot(aes(y = count, fill = rank,
                       x = factor(CODE,
                                  level = c('SITE',
                                            'LONGTERM',
                                            'IMPACT',
                                            'METHODS',
                                            'SPECIES',
                                            'DATA')))) + 
            geom_bar(position = "stack", stat = "identity") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            ylim(0, 80) + 
            theme(axis.text = element_text(size = 14),
                  plot.title = element_text(size = 16, face = "bold"),
                  legend.position = "none") +
            ylab("Count") +
            xlab(" ") +
            scale_fill_manual(values = colours) +
            scale_x_discrete(labels = c('SITE' = 'Site',
                                        'LONGTERM' = 'Longterm',
                                        'IMPACT' = 'Impact',
                                        'METHODS' = 'Methods',
                                        'SPECIES' = 'Species',
                                        'DATA' = 'Data')) +
            ggtitle('Data')



unique(q1$THEME)

####### Evidence ######


evidence_long <- q1 %>% filter(THEME == "Evidence") %>% 
            pivot_longer(RANK1:RANK5,
                         names_to = "rank",
                         values_to = "count")


evidence_long$rank <- factor(evidence_long$rank, levels = c('RANK5',
                                                    'RANK4',
                                                    'RANK3',
                                                    'RANK2',
                                                    'RANK1'))

unique(evidence_long$CODE)

evidence <- evidence_long %>% 
            ggplot(aes(y = count, fill = rank,
                       x = factor(CODE,
                                  level = c('CONSENSUS',
                                            'ACCESS',
                                            'CUMULATIVE',
                                            'SCIENCE',
                                            'PREDICTION',
                                            'IMPLEMENTATION',
                                            'SUCCESS')))) + 
            geom_bar(position = "stack", stat = "identity") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            ylim(0, 80) + 
            theme(axis.text = element_text(size = 14),
                  plot.title = element_text(size = 16, face = "bold"),
                  legend.position = "none") +
            ylab("Count") +
            xlab(" ") +
            scale_fill_manual(values = colours) +
            scale_x_discrete(labels = c('CONSENSUS' = 'Consensus',
                                        'ACCESS' = ' Access',
                                        'CUMULATIVE' = 'Cumulative',
                                        'SCIENCE' = 'Science',
                                        'PREDICTION' = 'Prediction',
                                        'IMPLEMENTATION' = 'Implementation',
                                        'SUCCESS' = 'Success criteria')) +
            ggtitle("Evidence")


# Governance  ---------------------------------------------------

governance_long <- q1 %>% filter(THEME == "Governance") %>% 
            pivot_longer(RANK1:RANK5,
                         names_to = "rank",
                         values_to = "count")


governance_long$rank <- factor(governance_long$rank, levels = c('RANK5',
                                                            'RANK4',
                                                            'RANK3',
                                                            'RANK2', 'RANK1'))

unique(governance_long$CODE)

governance <- governance_long %>% 
            ggplot(aes(y = count, fill = rank,
                       x = factor(CODE))) + 
            geom_bar(position = "stack", stat = "identity") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 14),
                  plot.title = element_text(size = 16, face = "bold"),
                  legend.position = "none") +
            ylab("Count") +
            ylim(0, 80) + 
            xlab(" ") +
            scale_fill_manual(values = colours) +
            scale_x_discrete(labels = c('POLITICS' = 'Politics',
                                        'LEGISLATION' = 'Legislation',
                                        'INTERNALSUPPORT' = 'Internal Support',
                                        'INDIGENOUS' = 'Indigenous Rights')) +
            ggtitle("Governance")

# Ecological complexity ---------------------------------------------------

ecological_long <- q1 %>% filter(THEME == "Ecological_complexity") %>% 
            pivot_longer(RANK1:RANK5,
                         names_to = "rank",
                         values_to = "count")


ecological_long$rank <- factor(ecological_long$rank, levels = c('RANK5',
                                                                'RANK4',
                                                                'RANK3',
                                                                'RANK2',
                                                                'RANK1'))


unique(ecological_long$CODE)

ecological <- ecological_long %>% 
            ggplot(aes(y = count, fill = rank,
                       x = factor(CODE,
                                  level = c('ECOLOGY',
                                            'NATURE',
                                            'CLIMATECHANGE')))) + 
            geom_bar(position = "stack", stat = "identity") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 14),
                  plot.title = element_text(size = 16, face = "bold"),
                  legend.position = "none") +
            ylab("Count") +
            xlab(" ") +
            ylim(0, 80) + 
            scale_fill_manual(values = colours) +
            scale_x_discrete(labels = c('ECOLOGY' = 'Ecology',
                                        'NATURE' = 'Nature',
                                        'CLIMATECHANGE' = 'Climate change')) +
            ggtitle("Ecological Complexity")


# Social complexity  ---------------------------------------------------

social_long <- q1 %>% filter(THEME == "Social_complexity") %>% 
            pivot_longer(RANK1:RANK5,
                         names_to = "rank",
                         values_to = "count")


social_long$rank <- factor(social_long$rank, levels = c('RANK5',
                                                                'RANK4',
                                                                'RANK3',
                                                                'RANK2',
                                                                'RANK1'))



unique(social_long$CODE)

social <- social_long %>% 
            ggplot(aes(y = count, fill = rank,
                       x = factor(CODE,
                                  level = c('COSTS',
                                            'JUSTICE',
                                            'RESPONSIBILITIES',
                                            'SYSTEMS',
                                            'TRADEOFFS',
                                            'COMMCOLLAB')))) + 
            geom_bar(position = "stack", stat = "identity") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 14),
                  plot.title = element_text(size = 16, face = "bold"),
                  legend.position = "none") +
            ylab("Count") +
            xlab(" ") +
            ylim(0, 80) + 
            scale_fill_manual(values = colours) +
            scale_x_discrete(labels = c('COSTS' = 'Costs',
                                        'JUSTICE' = 'Justice',
                                        'RESPONSIBILITIES' = 'Responsibilities',
                                        'SYSTEMS' = 'System',
                                        'TRADEOFFS' = 'Tradeoffs',
                                        'COMMCOLLAB' = 'Communication/Collaboration')) +
            ggtitle("Social Complexity")

# Public support  ---------------------------------------------------

public_long <- q1 %>% filter(THEME == "Public_support") %>% 
            pivot_longer(RANK1:RANK5,
                         names_to = "rank",
                         values_to = "count")


public_long$rank <- factor(public_long$rank, levels = c('RANK5',
                                                        'RANK4',
                                                        'RANK3',
                                                        'RANK2',
                                                        'RANK1'))
unique(public_long$CODE)

public <- public_long %>% 
            ggplot(aes(y = count, fill = rank,
                       x = factor(CODE,
                                  level = c( 'LANDACCESS',
                                             'EXTERNALSUPPORT')))) + 
            geom_bar(position = "stack", stat = "identity") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 14),
                  plot.title = element_text(size = 16, face = "bold"),
                  legend.position = "none") +
            ylab("Count") +
            xlab(" ") +
            ylim(0, 80) + 
            scale_fill_manual(values = colours) +
            scale_x_discrete(labels = c('LANDACCESS' = 'Land access',
                                         'EXTERNALSUPPORT' = 'External support')) +
            ggtitle('Public Support')

# Panel  ------------------------------------------------------------------
library(patchwork)

theme_indv <- data + resources + governance + 
            evidence + public + social +
            ecological + legend +
            plot_layout(ncol = 2) 

ggsave("output/theme_codes.tiff")
ggsave("output/theme_codes.jpg")

layout <- "
AABBBB
AABBBB
"

panel_theme <- theme_bar + theme_indv + 
            plot_layout(design = layout)

ggsave("output/panel_theme_final.pdf",
       width = 20,
       height = 14)


ggsave("output/panel_theme_final.tiff",
       width = 20,
       height = 14)

ggsave("output/panel_theme_final.jpg",
       width = 20,
       height = 14)

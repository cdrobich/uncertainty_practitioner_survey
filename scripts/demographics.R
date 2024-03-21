
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
#   [1] patchwork_1.1.2   viridis_0.6.3     viridisLite_0.4.2 lubridate_1.9.2   forcats_1.0.0    
# [6] stringr_1.5.0     dplyr_1.1.2       purrr_1.0.1       readr_2.1.4       tidyr_1.3.0      
# [11] tibble_3.2.1      ggplot2_3.5.0     tidyverse_2.0.0  
# 
# loaded via a namespace (and not attached):
#   [1] gtable_0.3.4     compiler_4.3.0   tidyselect_1.2.0 gridExtra_2.3    scales_1.3.0    
# [6] R6_2.5.1         labeling_0.4.3   generics_0.1.3   munsell_0.5.0    pillar_1.9.0    
# [11] tzdb_0.4.0       rlang_1.1.1      utf8_1.2.3       stringi_1.7.12   timechange_0.2.0
# [16] cli_3.6.1        withr_2.5.1      magrittr_2.0.3   grid_4.3.0       rstudioapi_0.14 
# [21] hms_1.1.3        lifecycle_1.0.3  vctrs_0.6.2      glue_1.6.2       farver_2.1.1    
# [26] fansi_1.0.4      colorspace_2.1-0 tools_4.3.0      pkgconfig_2.0.3

# libraries -----------------------------------------------

library(dplyr)
library(ggplot2)
library(forcats)
library(viridis)
library(stringr)
library(patchwork)
library(cowplot)


# Location of work --------------------------------------------------------
# using output directly from Qualtrics (summed English and French surveys)

organizations <- read.csv("data/qualtrics_output/organization_qualitrics_output.csv")
str(organizations)

organizations$Organization <- recode_factor(organizations$Organization,
                                   "Non-government agency (e.g., Nature Conservancy Canada, Ducks Unlimited Canada)" = "Non-government Agency",
                                   "Canadian provincial or territorial government agency" = "Provincial/Territorial Government",
                                   "Canadian federal government agency" = "Federal Government")




org_colour = c("Non-government Agency" = '#277F8E',
               'Provincial/Territorial Government' = '#A0DA39',
               'Federal Government' = '#4AC16D',
               'Municipal agency' = '#1FA187',
               'Other' = '#FDE725',
               'Conservation authority' = '#365C8D',
               'Industry/private company' = '#46327E',
               'Indigenous government' = '#440154')

colnames(organizations)

org_n <- organizations %>% mutate(Organization = fct_reorder(Organization,
                                           Sum, .desc = FALSE)) %>% 
            ggplot() + 
            geom_segment(aes(x = Organization, xend = Organization, 
                              y = 0, yend = Sum, 
                             colour = Organization),lwd = 5) +
  geom_point(aes(x = Organization, y = Sum, colour = Organization),
             size=7, shape = 19, stroke = 1.5) +
            coord_flip() +
            xlab(" ") +
            ylab("Number of respondents") +
            theme_minimal() +
            theme(axis.text = element_text(size = 13),
                  axis.title = element_text(size = 13),
                  legend.position = "none",
                  legend.key.size = unit(1, 'cm'),
                  legend.text = element_text(size = 12),
                  #legend.direction = "horizontal",
                  legend.title = element_blank()) +
            ylim(0, 50) +
            scale_colour_manual(values = org_colour)


# Duration at workplace ---------------------------------------------------
duration <- read.csv("data/qualtrics_output/duration_qualtrics_output.csv")
data <- read.csv("data/survey_likerts.csv")

duration$Duration <- recode_factor(duration$Duration,
                                   "over 10 years" = "> 10 years",
                                   "Less than 1 year" = "< 1 year")

duration <- duration %>% select(Duration,Sum)

# Duration Sum
# 1     < 1 year  12
# 2  1 - 2 years  18
# 3  3 - 5 years  27
# 4 5 - 10 years  31
# 5   > 10 years  62

###### years worked x place worked ####
data$duration <- recode_factor(data$duration,
                               "Over 10 years" = "over 10 years")


data$duration <- recode_factor(data$duration,
                               "over 10 years" = "> 10 years",
                               "Less than 1 year" = "< 1 year")

colnames(data)

duration_org <- data %>% group_by(organization) %>% 
            count(duration)

duration_org <- duration_org[-1,]
duration_org <- duration_org[-31,]

duration_org$organization <- recode_factor(duration_org$organization,
                                            "Non-government agency (e.g., Nature Conservancy Canada, Ducks Unlimited Canada)" = "Non-government Agency",
                                            "Canadian provincial or territorial government agency" = "Provincial/Territorial Government",
                                            "Canadian federal government agency" = "Federal Government")


write.csv(duration_org, "data/duration_workplace_organized.csv")

###### generate duration x workplace figure #####
duration_org <- read.csv("data/duration_workplace_organized.csv")

duration_n <- duration_org %>% ggplot(aes(fill = organization, y = n, 
                        x = factor(duration,
                                   level = c('< 1 year',
                                             '1 - 2 years',
                                             '3 - 5 years',
                                             '5 - 10 years',
                                             '> 10 years')))) +
            geom_bar(position = "dodge", stat = "identity") +
            theme_minimal() +
            coord_flip() +
            xlab(" ") +
            ylab("Number of respondents") +
            theme_minimal() +
            theme(axis.text = element_text(size = 13),
                  axis.title = element_text(size = 13),
                  legend.position = 'right',
                  legend.key.size = unit(0.5, 'cm'),
                  legend.text = element_text(size = 11),
                  #legend.direction = "horizontal",
                  legend.title = element_blank()) +
            scale_fill_manual(values = org_colour)


# Location ----------------------------------------------------------------
location <- read.csv("data/qualtrics_output/location_qualtrics_output.csv")
location <- location %>% select(Location, Sum)

location_n <- location %>% mutate(Location = fct_reorder(Location,
                                           Sum, .desc = FALSE)) %>% 
            ggplot() + 
            geom_segment(aes(x = Location, xend = Location, 
                              y = 0, yend = Sum),
                         colour = '#36a3ab', lwd = 4) +
            geom_point(aes(x = Location, y = Sum), 
                       size=6, shape = 19,
                       stroke = 1.5, colour = '#36a3ab') +
            coord_flip() +
            xlab(" ") +
            ylab("Number of respondents") +
            theme_minimal() +
            theme(axis.text = element_text(size = 13),
                  axis.title = element_text(size = 13),
                  legend.position = "none",
                  legend.key.size = unit(1, 'cm'),
                  legend.text = element_text(size = 12),
                  #legend.direction = "horizontal",
                  legend.title = element_blank()) +
            ylim(0, 70) 


# put panel together ------------------------------------------------------


panel <- org_n + duration_n + location_n + location_n +
            plot_annotation(tag_levels = "A")
            

ggsave('output/panel_demographics.jpg',
       width = 14, height = 10)


ggsave('output/panel_demographics.tiff',
       width = 14, height = 10)

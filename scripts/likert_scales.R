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
# loaded via a namespace (and not attached):
#   [1] utf8_1.2.3        R6_2.5.1          tidyselect_1.2.1  magrittr_2.0.3    gtable_0.3.4     
# [6] glue_1.6.2        tibble_3.2.1      pkgconfig_2.0.3   generics_0.1.3    dplyr_1.1.2      
# [11] lifecycle_1.0.4   ggplot2_3.5.0     cli_3.6.1         fansi_1.0.4       scales_1.3.0     
# [16] grid_4.3.0        vctrs_0.6.2       compiler_4.3.0    rstudioapi_0.15.0 tools_4.3.0      
# [21] munsell_0.5.0     pillar_1.9.0      colorspace_2.1-0  rlang_1.1.1  


# libraries ---------------------------------------------------------------
library(likert) # likert figures

library(dplyr)
library(forcats)
library(ggplot2)
library(tidyr)

library(tidyverse)

library(stringr) # wrap labels
library(scales)
library(viridis)
library(patchwork)


# Prevalence and Impact of Uncertainty (Q6, Q30) ---------------------------------------
likert_often <- read.csv("data/qualtrics_output/q6_often_qualtrics_output.csv")
likert_impact <- read.csv("data/qualtrics_output/q30_impacts_qualtrics_output.csv")

colnames(likert_often)

# [1] "Often"               "Always"              "Most.of.the.time"   
# [4] "About.half.the.time" "Sometimes"           "Never"


colnames(likert_impact)

# [1] "Impact"                     "Strongly.agree"            
# [3] "Agree"                      "Neither.agree.nor.disagree"
# [5] "Disagree"                   "Strongly.disagree" 


######### Likert Uncertainty, How Often ########

unique(likert_often$Question)
likert_often$Question <- recode_factor(likert_often$Question,
                           "The environment (e.g., any physical or ecological aspect of a system)"  = "The environment",
                           "The evidence (e.g., existing knowledge on an intervention or management strategy)" = "Evidence",
                           "The actions or decisions of teammates, and/or colleagues"  = "Teammates and/or colleagues",
                           "The actions or decisions of leaders of my agency or organization" = "Agency or organization leaders",
                           "The actions or decisions of elected leaders" = "Elected leaders",
                           "The actions or decisions of stakeholders or rightsholders." = "Stakeholders or rightsholders",
                           "Funding (e.g., availability and timelines)" = "Funding")

likert_impact$Question <- recode_factor(likert_impact$Question,
                                   "The environment (e.g., any physical or ecological aspect of a system)"  = "The environment",
                                   "The evidence (e.g., existing knowledge on an intervention or management strategy)" = "Evidence",
                                   "The actions or decisions of teammates, and/or colleagues"  = "Teammates and/or colleagues",
                                   "The actions or decisions of leaders of my agency or organization" = "Agency or organization leaders",
                                   "The actions or decisions of elected leaders" = "Elected leaders",
                                   "The actions or decisions of stakeholders or rightsholders." = "Stakeholders or rightsholders",
                                   "Funding (e.g., availability and timelines)" = "Funding")

likert_oft <- likert_often %>% pivot_longer(Always:Never,
                             names_to = "ranking",
                             values_to = "count")

often_sum <- likert_oft %>% 
            group_by(Question) %>% 
            mutate(Sum = sum(count)) %>% 
            group_by(ranking) %>% 
            mutate(percent = round(100*count/Sum, 2))


unique(likert_oft$ranking)

likert_oft$ranking <- factor(likert_oft$ranking,
                               levels = c("Never",
                                          "Sometimes",
                                          "About.half.the.time",
                                          "Most.of.the.time",
                                          "Always"))


write.csv(likert_oft, 'data/likert_often.csv', row.names = FALSE)
write.csv(often_sum, 'data/likert_often_sum.csv', row.names = FALSE)

######## 'Often' (Q6) ggplot figure ###
likert_often <- read.csv('data/likert_often.csv')
often_sum <- read.csv('data/likert_often_sum.csv')

colours = c("Always" = "#005f73",
            "Most of the time" = "#0a9396",
            "About half the time" = "#ee9b00",
            "Sometimes" = "#ca6702",
            "Never" = "#9b2226")

often_sum$ranking <- recode_factor(often_sum$ranking,
                                   'Most.of.the.time' = 'Most of the time',
                                   'About.half.the.time' = 'About half the time')

often_sum$ranking <- factor(often_sum$ranking, levels = c('Always',
                                                          'Most of the time',
                                                          'About half the time',
                                                          'Sometimes',
                                                          'Never'))

often_plot <- ggplot(data = often_sum, aes(x = Question, y = percent, fill = ranking)) +
            geom_bar(stat="identity", width = 0.7) +
            theme_classic() +
            coord_flip() +
            ylab("Percentage (%)") +
            xlab(" ") +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
            theme(axis.text=element_text(size=14),
                  axis.title=element_text(size=14),
                  legend.text = element_text(size=12),
                  legend.title = element_blank(),
                  plot.title = element_text(size = 16, face = "bold")) +
            ggtitle("I experience uncertainty when considering...") +
            guides(fill = guide_legend(reverse = TRUE)) +
            scale_fill_manual(values = colours)


######### Likert Uncertainty, Percieved Impact ########

impacts <- likert_impact %>% pivot_longer(Strongly.agree:Strongly.disagree,
                                             names_to = "ranking",
                                             values_to = "count")

impact_sum <- impacts %>% replace_na(list(count = 0)) %>% 
            group_by(Question) %>% 
            mutate(Sum = sum(count)) %>% 
            group_by(ranking) %>% 
            mutate(percent = round(100*count/Sum, 2))

unique(impact_sum$ranking)

impact_sum$ranking <- recode_factor(impact_sum$ranking,
                                   'Strongly.disagree' = 'Strongly disagree',
                                   'Neither.agree.nor.disagree' = 'Neither agree nor disagree',
                                 'Strongly.agree' = 'Strongly agree')



write.csv(impacts,'data/likert_impacts.csv', row.names = FALSE)
write.csv(impact_sum, 'data/likert_impact_sum.csv', row.names = FALSE)

#### Impact Plot #####
impact_sum <- read.csv('data/likert_impact_sum.csv')

impact_sum$ranking <- factor(impact_sum$ranking,
                             levels = c("Strongly agree",
                                        "Agree",
                                        "Neither agree nor disagree",
                                        "Disagree",
                                        "Strongly disagree"))


colours_imp = c("Strongly agree" = "#005f73",
            "Agree" = "#0a9396",
            "Neither agree nor disagree" = "#ee9b00",
            "Disagree" = "#ca6702",
            "Strongly disagree" = "#9b2226")


impact_plot <- ggplot(data = impact_sum, aes(x = Question,
                                             y = percent,
                                             fill = ranking)) +
            geom_bar(stat="identity", width = 0.7) +
            scale_fill_manual(values = colours_imp)+
            theme_classic() +
            coord_flip() +
            ylab("Percentage (%)") +
            xlab(" ") +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
            theme(axis.text=element_text(size=14),
                  axis.title=element_text(size=14),
                  legend.text = element_text(size=12),
                  legend.title = element_blank(),
                  plot.title = element_text(size = 16, face = "bold")) +
            ggtitle("The following sources of uncertainty impact my decisions...") +
            guides(fill = guide_legend(reverse = T))


likert_plot <- often_plot / impact_plot + plot_annotation(tag_levels = 'A')
likert_plot

ggsave('output/likert_often_impact.jpg', likert_plot)
ggsave('output/likert_often_impact.tiff', likert_plot,
       dpi = 300)


# Collaborative questions (Q7) -------------------------------------------------
# survey question "in my prof capacity, I experience uncertainty when" 

q7 <- read.csv("data/qualtrics_output/q7_decisions_qualtrics_output.csv")

q7_long <- q7 %>% pivot_longer(Always:Never,
                                            names_to = "ranking",
                                            values_to = "count")


q7_sum <- q7_long %>% 
  group_by(Question) %>% 
  mutate(Sum = sum(count)) %>% 
  group_by(ranking) %>% 
  mutate(percent = round(100*count/Sum, 2))

q7_sum$ranking <- factor(q7_sum$ranking,
                             levels = c("Never",
                                        "Sometimes",
                                        "About.half.the.time",
                                        "Most.of.the.time",
                                        "Always"))


# q7 figures
q7_sum$ranking <- recode_factor(q7_sum$ranking,
                                   'Most.of.the.time' = 'Most of the time',
                                   'About.half.the.time' = 'About half the time')

write.csv(q7_sum, 'data/q7_sum_long.csv', row.names = FALSE) # export data for re-creating figure later

q7_sum$ranking <- factor(q7_sum$ranking, levels = c('Always',
                                                    'Most of the time',
                                                    'About half the time',
                                                    'Sometimes',
                                                    'Never'))


q7_plot <- ggplot(data = q7_sum, aes(x = factor(Question,
                                                level = c('Decisions with people outside my organization',
                                                          'Decisions with people within my organization',
                                                          'Long-term decisions (> 3 yrs)',
                                                          'Short-term decisions (<= 3 yrs)')),
                                     y = percent, fill = ranking)) +
  geom_bar(stat="identity", width = 0.7) +
  theme_classic() +
  coord_flip() +
  ylab("Percentage (%)") +
  xlab(" ") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_blank(),
        plot.title = element_text(size = 16, face = "bold")) +
  ggtitle("I experience uncertainty when making") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = colours) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))

ggsave('output/q7_plot.jpg')
ggsave('output/q7_plot.tiff', dpi = 300)

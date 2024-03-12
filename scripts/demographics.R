library(tidyverse)
library(viridis)
library(stringr)
library(patchwork)


data <- read.csv("data/survey_likerts.csv")
colnames(data)
str(data)

# converted some 'others' into the proper org 
data <- data %>% filter(Finished == TRUE)

# organization  n
# 1             Non-government Agency 46
# 2 Provincial/Territorial Government 29
# 3                Federal Government 30
# 4                  Municipal agency  2
# 5            Conservation authority 21
# 6                             Other  7
# 7          Industry/private company  9
# 8             Indigenous government  1

46+29+30+2+21+7+9+1 #145




data$organization <- recode_factor(data$organization,
                                   "Non-government agency (e.g., Nature Conservancy Canada, Ducks Unlimited Canada)" = "Non-government Agency",
                                   "Canadian provincial or territorial government agency" = "Provincial/Territorial Government",
                                   "Canadian federal government agency" = "Federal Government")
unique(data$organization)


data %>% count(organization) 

#                        organization  n
# 1             Non-government Agency 46
# 2 Provincial/Territorial Government 29
# 3                Federal Government 30
# 4                  Municipal agency  2
# 5            Conservation authority 21
# 6                             Other  7
# 7          Industry/private company  9
# 8             Indigenous government  1






data$duration <- recode_factor(data$duration,
                               "Over 10 years" = "over 10 years")


data$duration <- recode_factor(data$duration,
                               "over 10 years" = "> 10 years",
                               "Less than 1 year" = "< 1 year")
unique(data$duration)

count <- data %>% count(duration)

count %>%  
          mutate(Sum = sum(n)) %>% 
          group_by(duration) %>% 
          mutate(percent = round(100*n/Sum, 2))

# # Groups:   duration [5]
# duration         n   Sum percent

# 1 > 10 years      61   145   42.1 
# 2 < 1 year        12   145    8.28
# 3 1 - 2 years     18   145   12.4 
# 4 3 - 5 years     25   145   17.2 
# 5 5 - 10 years    29   145   20

org_colour = c("Non-government Agency" = '#440154',
               'Provincial/Territorial Government' = '#46327e',
               'Federal Government' = '#365c8d',
               'Municipal agency' = '#277f8e',
               'Other' = '#1fa187',
               'Conservation authority' = '#4ac16d',
               'Industry/private company' = '#a0da39',
               'Indigenous government' = '#fde725')



orgs <- data %>% count(organization)
colnames(orgs)



org_n <- orgs %>% mutate(organization = fct_reorder(organization,
                                           n, .desc = FALSE)) %>% 
            ggplot() + 
            geom_segment(aes(x = organization, xend = organization, 
                              y = 0, yend = n, 
                             colour = organization),lwd = 5) +
  geom_point(aes(x = organization, y = n, colour = organization),
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



###### years worked

colnames(data)

duration <- data %>% group_by(organization) %>% 
            count(duration)

unique(duration$duration)


duration_n <- duration %>% ggplot(aes(fill = organization, y = n, 
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

### location

location <- data %>% select(location)

location <- location %>% separate_longer_delim(location, delim = ",")

loc <- location %>% count(location)

write.csv(loc, "location.csv")


loc_colour = c( "Alberta" = '#88C4AA',
                "British Columbia" = '#B3E0A6',
                "Manitoba" = '#FED38C',
                "New Brunswick" = '#D7191C', 
                "Newfoundland and Labrador" = '#E54F35', 
                "Northwest Territories" = '#F3854E',
                "Nova Scotia" = '#FDB56A',
                "Nunavut" = '#E54F35',
                "Ontario" = '#5AA4B2',
                "Prince Edward Island" = '#F3854E',
                "Quebec" = '#F0F9BA',
                "Saskatchewan" = '#D1ECB0',
                "Yukon" = '#FFF0AE')


unique(loc$location)
loc <- loc[-10,]

location_n <- loc %>% mutate(location = fct_reorder(location,
                                           n, .desc = FALSE)) %>% 
            ggplot() + 
            geom_segment(aes(x = location, xend = location, 
                              y = 0, yend = n, colour = location), lwd = 4) +
            geom_point(aes(x = location, y = n, colour = location), 
                       fill = "white", size=6, shape = 19, stroke = 1.5) +
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
            ylim(0, 70) +
            scale_colour_manual(values = loc_colour)



panel <- org_n + duration_n + location_n + location_n +
            plot_layout(ncol = 2) +
            plot_annotation(tag_levels = "A")
            

ggsave('output/panel_figures_test.jpg',
       width = 14, height = 10)

############ LIKERT ###########

colnames(data)

unique(data$organization)

likert <- data %>% filter(organization %in% org_list)

colours = c("Always" = "#264653",
            "Most of the time" = "#2A9D8F",
            "Never" = "#E9C46A",
            "About half the time" = "#F4A261",
            "Sometimes" = "#e76F51")


likert$collaborative_within <- factor(likert$collaborative_within, levels = c('Never',
                                                'Sometimes',
                                                'About half the time',
                                                'Most of the time',
                                                'Always'))



org_list <- c("Conservation authority",
              "Non-government Agency",
              "Federal Government",
              "Industry/private company",
              "Provincial/Territorial Government")

##### Decision making within my org. is collaborative #####
collab_within_org <- likert %>% group_by(organization) %>% 
            count(collaborative_within)


collab_within_org <- collab_within_org[-5,]

collab_within_org2 <- read.csv("collaborative_within.csv")

### add a 'never' for each org

collab_within_org %>% ggplot(aes(fill = collaborative_within, y = n, 
                                 label = n,
                         x = organization)) + 
            geom_bar(position = "dodge", stat = "identity") +
            coord_flip() +
            xlab(" ") +
            ylab("Count") +
            theme_minimal() +
            theme(axis.text = element_text(size = 15),
                  axis.title = element_text(size = 15),
                  legend.position = "none",
                  legend.key.size = unit(0.5, 'cm'),
                  legend.text = element_text(size = 12),
                  #legend.direction = "horizontal",
                   legend.title = element_blank(),
                  plot.title = element_text(size = 15, face = "bold")) +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
            scale_fill_manual(values = colours) +
            guides(fill = guide_legend(reverse = TRUE)) +
            ggtitle("...collaborative within my organization") +
            ylim(0,20)


collab_outside_org <- likert %>% group_by(organization) %>% 
            count(collaborative_outside)

collab_outside_org <- collab_outside_org[-5,]



collab_outside_org$collaborative_outside <- factor(collab_outside_org$collaborative_outside, levels = c('Never',
                                                                              'Sometimes',
                                                                              'About half the time',
                                                                              'Most of the time',
                                                                              'Always'))



collab_outside_org %>% ggplot(aes(fill = collaborative_outside, y = n, 
                                 label = n,
                                 x = organization)) + 
            geom_bar(position = "dodge", stat = "identity") +
            coord_flip() +
            xlab(" ") +
            ylab("Count") +
            theme_minimal() +
            theme(axis.text = element_text(size = 15),
                  axis.title = element_text(size = 15),
                  legend.position = "none",
                  #legend.key.size = unit(0.5, 'cm'),
                  legend.text = element_text(size = 12),
                  legend.title = element_blank(),
                  plot.title = element_text(size = 15, face = "bold")) +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
            scale_fill_manual(values = colours) +
            guides(fill = guide_legend(reverse = TRUE)) +
            ggtitle("...collaborative outside my organization") +
            ylim(0, 20)



# chloropleth map ---------------------------------------------------------

# https://cengel.github.io/R-spatial/mapping.html

library(sf)

canada <- st_read('data/shapefiles/lpr_000b21a_e.shp')


class(canada)
# [1] "sf"         "data.frame"

attr(canada, "sf_column")
str(canada)

class(canada_sf)
#[1] "sf"         "tbl_df"     "tbl"        "data.frame"

plot(canada['Respondent'],
     main = 'Number of Respondents')

heatmap <- ggplot(canada) +
            geom_sf(aes(fill = Respondent)) +
            theme_void() +
            theme(legend.position = "bottom")


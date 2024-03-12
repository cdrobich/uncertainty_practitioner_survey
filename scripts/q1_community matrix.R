library(tidyverse)

rank1_data <- read.csv('data/nested_q1/q1_rank.csv')




rank1_data$organization <- recode_factor(rank1_data$organization,
                                   "Non-government agency (e.g., Nature Conservancy Canada, Ducks Unlimited Canada)" = "Non-government Agency",
                                   "Canadian provincial or territorial government agency" = "Provincial/Territorial Government",
                                   "Canadian federal government agency" = "Federal Government")
unique(rank1_data$organization)


rank1_data %>% count(organization) 

#                        organization   n
# 1             Non-government Agency  46
# 2 Provincial/Territorial Government  31
# 3                Federal Government  30
# 4                  Municipal agency   2
# 5            Conservation authority  22
# 6                             Other   8
# 7          Industry/private company   9
# 8                                   303
# 9             Indigenous government   1
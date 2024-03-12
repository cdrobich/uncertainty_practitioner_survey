library(tidyverse)


complete_data <- read.csv('data/uncertainty_data_cleaned_total.csv')
str(complete_data)


complete <- complete_data %>% filter(Finished == "True") #146


complete %>% count(Q10) 

# Q10  n
# 1                                                         Agence du gouvernement f\xe9d\xe9ral canadien  3
# 2                                          Agence gouvernementale provinciale ou territoriale au Canada  2
# 3                                                                                                 Autre  2
# 4                                                                    Canadian federal government agency 27
# 5                                                  Canadian provincial or territorial government agency 27
# 6                                                                                Conservation authority 21
# 7                                                                                 Indigenous government  1
# 8                                                                        Industrie/entreprise priv\xe9e  1
# 9                                                                              Industry/private company  8
# 10                                                                                     Municipal agency  2
# 11                      Non-government agency (e.g., Nature Conservancy Canada, Ducks Unlimited Canada) 43
# 12 Organisme non gouvernemental (p. ex., Conservation de la Nature Canada, Canards Illimit\xe9s Canada)  2
# 13                                                                                                Other  7



# totals
# federal government - 30
# provincial - 29
# industry - 9
# municipal - 2
# NGO - 45

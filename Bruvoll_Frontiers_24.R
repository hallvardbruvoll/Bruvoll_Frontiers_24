# Frontiers paper, script for analysis and plots
library(tidyverse)
library(poweRlaw)

# Data preparation --------------------------------------------------------
  # Load data sets
lbk_house_sizes <- read.csv2("Data/lbk_house_sizes.csv",
                             encoding = "UTF-8") %>%
  mutate_if(is.character, as.factor)

tryp_house_sizes <- read.csv2("Data/trypillia_house_sizes.csv",
                              encoding = "UTF-8") %>%
  mutate_if(is.character, as.factor)

vrable_neigh <- filter(lbk_house_sizes, neighbourhood_logic == TRUE) %>%
  rename(Quarter = site_name_ill_short) %>%
  select(house_size, mean_orien, Quarter)

lbk_house_sizes <- filter(lbk_house_sizes, neighbourhood_logic == FALSE &
                            region == "zitava_valley")

lbk_house_sizes <- left_join(lbk_house_sizes, vrable_neigh,
                             by = c("mean_orien", "house_size"))

my_settlements <- bind_rows(lbk_house_sizes %>%
  dplyr::select(house_size, Settlement, site_name_ill_short,
                mean_orien, Quarter) %>%
  mutate(Culture = "Linear Pottery") %>%
  rename(site_name_ill = site_name_ill_short),
  tryp_house_sizes %>%
    dplyr::select(house_size, Settlement, site_name_ill, culture, quarter) %>%
    rename(Culture = culture,
           Quarter = quarter)) %>%
  group_by(Settlement) %>%
  filter(n() > 10) # leaves out Vráble 'Drakovo' and Slazany.
# 4354 houses and 6 variables.

my_settlements$site_name_ill <- as.factor(gsub(
  pattern = "Horn\u00fd", replacement = "Horn\u00fd Ohaj",
  x = my_settlements$site_name_ill))

write.csv2(my_settlements, file = "Data/my_settlements.csv",
           row.names = FALSE)

my_settlements <- read.csv2("Data/my_settlements.csv", encoding = "UTF-8") %>%
  mutate_if(is.character, as.factor)

slovak_characters <- read.csv2("Data/site_names_unicode.csv",
                               encoding = "UTF-8", rownam)


# Analysis ----------------------------------------------------------------
#  mutate(Gini = round(Gini(house_size), 3))




# Figures -----------------------------------------------------------------



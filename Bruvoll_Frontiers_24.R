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

# Tidy the data input so that the following code becomes redundant:

## Make object with settlements and house sizes
my_settlements <- bind_rows(filter(zitava_sites_only, region == "zitava_valley") %>%
                              dplyr::select(house_size, Settlement, site_name_ill_short) %>%
                              mutate(Culture = "Linear Pottery") %>%
                              rename(site_name_ill = site_name_ill_short),
                            tryp_house_sizes %>%
                              dplyr::select(house_size, Settlement, site_name_ill, culture) %>%
                              rename(Culture = culture))

## Filter out settlements with n < 10 (too small for dist.fit); add Gini index
my_settlements <- my_settlements %>%
  group_by(Settlement) %>%
  filter(n() > 10) %>%
  mutate(Gini = round(Gini(house_size), 3))

## Remove unused factor levels (should be 13 settlements left)
my_settlements <- droplevels.data.frame(my_settlements)
levels(my_settlements$Settlement)

## Also, try to resolve the Vrable neighbourhood issue (remove the double
## entry, rename column to just Neighbourhood)

# Testing connection to github repo

# Analysis ----------------------------------------------------------------




# Figures -----------------------------------------------------------------



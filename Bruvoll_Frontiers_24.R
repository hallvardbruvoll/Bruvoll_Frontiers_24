# Frontiers paper, script for analysis and plots

# Load custom functions with dependencies
source("Distfit-functions.R")

# Data preparation --------------------------------------------------------
  # Load data sets

# slovak_characters <- read.csv2("Data/site_names_unicode.csv",
#                                encoding = "UTF-8")

my_settlements <- read.csv2("Data/my_settlements.csv",
                            encoding = "UTF-8") %>%
  mutate_if(is.character, as.factor)

# Analysis ----------------------------------------------------------------

# Settlements

# Quarters

# Time samples


# Figures -----------------------------------------------------------------



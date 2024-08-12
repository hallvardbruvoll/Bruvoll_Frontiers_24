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
settlements_results <- dist.fit.all(x = my_settlements$house_size,
                                    set = my_settlements$Settlement,
                                    culture = my_settlements$Culture)
settlements_pl_tails <- filter(settlements_results,
                               model == FALSE & tail == "pl") %>%
  group_by(set) %>%
  filter(value >= xmin)

ggplot(filter(settlements_results, model == FALSE))+
  aes(x = value, y = ccdf)+
  geom_point(aes(shape = Culture), colour = "grey")+
  scale_shape_manual(values = c(1,2))+
  geom_point(data = settlements_pl_tails,
              aes(shape = Culture, colour = Culture))+
  geom_line(data = settlements_pl_tails, aes(group = set, colour = Culture))+
  scale_y_log10()+
  scale_x_log10()

# ggplot(filter(settlements_results, model == FALSE & tail != "pl"))+
#   aes(x = value, y = ccdf)+
#   geom_point(aes(shape = Culture, colour = Culture))+
#   scale_shape_manual(values = c(1,2))+
#   geom_line(aes(group = set, colour = Culture))+
#   scale_y_log10()+
#   scale_x_log10()

# Quarters

by_quarters <- filter(my_settlements, Quarter != "") %>%
  droplevels()
# Important to include only levels that are used in the selection
# orelse it doesn't run

quarters_results <- dist.fit.all(x = by_quarters$house_size,
                                 set = by_quarters$Quarter,
                                 culture = by_quarters$Culture)

quarters_pl_tails <- filter(quarters_results,
                            model == FALSE & tail == "pl") %>%
  group_by(set) %>%
  filter(value >= xmin)

ggplot(filter(quarters_results, model == FALSE))+
  aes(x = value, y = ccdf)+
  geom_point(aes(shape = Culture), colour = "grey")+
  scale_shape_manual(values = c(1,2))+
  geom_point(data = settlements_pl_tails,
             aes(shape = Culture, colour = Culture))+
  geom_line(data = settlements_pl_tails, aes(group = set, colour = Culture))+
  scale_y_log10()+
  scale_x_log10()


# Time samples


# Figures -----------------------------------------------------------------



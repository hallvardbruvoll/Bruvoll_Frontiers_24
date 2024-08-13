# Frontiers paper, script for analysis and plots

# Load custom functions with dependencies
source("Distfit-functions.R")

# Data preparation --------------------------------------------------------
  # Load data sets

slovak_characters <- read.csv2("Data/site_names_unicode.csv",
                                encoding = "UTF-8")

my_settlements <- read.csv2("Data/my_settlements.csv",
                            encoding = "UTF-8") %>%
  mutate_if(is.character, as.factor)

# Settlements -------------------------------------------------------------

# Table of settlements and house count
settlements_table <- my_settlements %>%
  group_by(site_name_ill, Culture) %>%
  summarise(N = n()) %>%
  arrange(desc(N)) %>%
  rename(Settlement = site_name_ill)

# Fit models to settlements
settlements_results <- dist.fit.all(x = my_settlements$house_size,
                                    set = my_settlements$site_name_ill,
                                    culture = my_settlements$Culture)

# Filter out power-law tails
settlements_pl_tails <- filter(settlements_results,
                               model == FALSE & tail == "pl") %>%
  group_by(set) %>%
  filter(value >= xmin)

# Make label vector
settlement_labs <- settlements_pl_tails %>%
  group_by(set, Culture) %>%
  summarise(x = max(value), y = min(ccdf))
settlement_labs <- settlement_labs %>%
  ungroup() %>% # Setting label coordinates manually
  mutate(x = c(250, 300, 260, 850, 160),
         y = c(0.15, 0.0004, 0.04, 0.0015, 0.0025))

# Make cCDF plot for settlements, all in grey, pl tails in colour
ggplot(filter(settlements_results, model == FALSE))+
  aes(x = value, y = ccdf, colour = Culture, shape = Culture, group = set)+
  geom_point(colour = "grey")+
  geom_point(data = settlements_pl_tails)+
  scale_shape_manual(values = c(1,2))+
  geom_line(data = settlements_pl_tails)+
  # geom_line(data = filter(settlements_results, model == TRUE & tail == "pl"),
  #            size = 1, colour = "black", linetype = 1)+
  geom_text(data = settlement_labs,
            aes(x = x, y = y, label = set),
            show.legend = FALSE)+
  scale_y_log10(labels = scales::label_number())+
  scale_x_log10()+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x = "House size", y = "cCDF")

# Quarters ----------------------------------------------------------------

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



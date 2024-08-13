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
Fig_Settlements_cCDF <- ggplot(filter(settlements_results, model == FALSE))+
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

save(Fig_Settlements_cCDF, file = "Figures/Fig_Settlements_cCDF.RData")

# Quarters ----------------------------------------------------------------

# Filter out settlements with quarter subdivs (Vráble and Nebelivka)
by_quarters <- filter(my_settlements, Quarter != "") %>%
  droplevels()
# Important to include only levels that are used in the selection
# orelse it doesn't run

# Fit models
quarters_results <- dist.fit.all(x = by_quarters$house_size,
                                 set = by_quarters$Quarter,
                                 culture = by_quarters$Culture)

# Filter out power-law tails
quarters_pl_tails <- filter(quarters_results,
                            model == FALSE & tail == "pl") %>%
  group_by(set) %>%
  filter(value >= xmin)

# Make label vector (this time for non-pl for clarity)
quarter_labs <- filter(quarters_results, model == FALSE & tail != "pl") %>%
  group_by(set, Culture) %>%
  summarise(x = max(value), y = min(ccdf)) %>%
  mutate(label = if_else(Culture == "Trypillia", paste0("Neb. ", set),
                         set))
# Adjust positions manually
quarter_labs <- quarter_labs %>%
  ungroup() %>% # Setting label coordinates manually
  mutate(x = c(123, 65, 183),
         y = c(0.013, 0.075, 0.1))

# Make plot, everything in grey, add pl tails in colour (all series but three)
Fig_Quarters_cCDF <- ggplot(filter(quarters_results, model == FALSE))+
  aes(x = value, y = ccdf, colour = Culture, shape = Culture, group = set)+
  geom_point(colour = "grey")+
  geom_line(colour = "grey")+
  scale_shape_manual(values = c(1,2))+
  geom_point(data = quarters_pl_tails)+
  geom_line(data = quarters_pl_tails)+
  geom_text(data = quarter_labs,
            aes(x = x, y = y, label = label,),
            colour = "darkgrey", show.legend = FALSE)+
  scale_y_log10()+
  scale_x_log10()+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x = "House size", y = "cCDF")

save(Fig_Quarters_cCDF, file = "Figures/Fig_Quarters_cCDF.RData")

# Time samples ------------------------------------------------------------

# Filter for Vráble houses, and transform high orientation values to negative
# (necessary for construction year estimation).
# Add columns for construction year and abandonment after median duration.
# All dates are calculated as positive numbers, though they are BC
# (i.e. negative). Time axis in plots will need to be reversed!
# Regression model from Müller-Scheessel et al. 2020.
# Expected house duration from Meadows et al. 2019.
vrable_time <- filter(my_settlements, Settlement == "Vrable") %>%
  mutate(mean_orien = if_else(mean_orien > 180,
                              mean_orien - 360,
                              mean_orien),
         constr_BC = (mean_orien+651.016)/0.129486,
         abandon_BC = constr_BC-27.5)

# The following time samples are used since they leave no voids,
# all houses (except early outliers) are included, and they correspond
# roughly to one human generation.
# The degrees and degrees_max columns provide the orientation
# interval of houses that were in use (i.e. constructed but not yet
# abandoned) at the given sample year.
# "abs" columns for absolute degrees (no negatives)
time_samples <- tibble(dates = sort(seq(4990, 5290, by = 20),
                                    decreasing = TRUE),
                       sample = seq(1, length(dates), by = 1),
                       degrees = (0.129486*dates)-651.016,
                       degrees_max = degrees+(0.129486*27.5),
                       abs_degrees = if_else(degrees < 0,
                                             degrees+360, degrees),
                       abs_degrees_max = if_else(degrees_max < 0,
                                                 degrees_max+360,
                                                 degrees_max))
time_samples # Check

save(time_samples, file = "Data/time_samples.RData") # For shortcut

# Assign houses to each time sample and combine (some houses are assigned to
# two or three samples, so table gets longer than the total number of houses).
vrable_samples <- tibble()

for (i in 1:nrow(time_samples)) {
  single_sample <- filter(vrable_time,
                          constr_BC > time_samples$dates[i] &
                            abandon_BC < time_samples$dates[i]) %>%
    mutate(sample = time_samples$sample[i])
  vrable_samples <- bind_rows(vrable_samples, single_sample)
}

# Prepare data objects: Vráble total and SW only,
# and only samples with > 10 houses.
# For Vráble SW, also remove largest house per sample (details in text)
vrable_samples <- vrable_samples %>%
  group_by(sample) %>%
  filter(n() > 10)
vrable_SW_samples <- filter(vrable_samples, Quarter == "Vr\u00e1ble SW") %>%
  group_by(sample) %>%
  filter(n() > 10) %>%
  filter(house_size < max(house_size))

# Analyse (fit distribution tails)
vrable_results <- dist.fit.all(x = vrable_samples$house_size,
                             set = vrable_samples$sample)
vrable_SW_results <- dist.fit.all(x = vrable_SW_samples$house_size,
                                  set = vrable_SW_samples$sample)

# Re-add date estimates (this is a bit clumsy, sorry)
sample_dates <- time_samples %>% dplyr::select(dates, sample) %>%
  mutate(sample = as.character(sample),
         dates = as.factor(dates))
# Set levels in descending order (for the table)
#levels(sample_dates$dates)
sample_dates$dates <- factor(sample_dates$dates,
                             levels = rev(levels(sample_dates$dates)))
#levels(sample_dates$dates)
vrable_results <- left_join(vrable_results, sample_dates,
                          by = c("set" = "sample"))
vrable_SW_results <- left_join(vrable_SW_results, sample_dates,
                               by = c("set" = "sample"))

# Puh, now single out power-law houses
vrable_pl <- vrable_results %>%
  group_by(set) %>%
  filter(tail == "pl" & value >= xmin)
vrable_SW_pl <- vrable_SW_results %>%
  group_by(set) %>%
  filter(tail == "pl" & value >= xmin)

# Plot cCDF
fig_vrable_ccdf_a <- ggplot(filter(vrable_results, model == FALSE))+
  aes(x = value, y = ccdf, group = set)+
  geom_point(colour = "grey", shape = 1)+
  geom_line(colour = "grey")+
  geom_point(data = filter(vrable_pl, model == FALSE),
             colour = "red", shape = 1)+
  geom_line(data = filter(vrable_pl, model == FALSE), colour = "red")+
  scale_x_log10()+
  scale_y_log10()+
  theme_bw()+
  labs(x = "House size", y = "cCDF")

fig_vrable_ccdf_b <- ggplot(filter(vrable_SW_results, model == FALSE))+
  aes(x = value, y = ccdf, group = set)+
  geom_point(colour = "grey", shape = 1)+
  geom_line(colour = "grey")+
  geom_point(data = filter(vrable_SW_pl, model == FALSE),
             colour = "red", shape = 1)+
  geom_line(data = filter(vrable_SW_pl, model == FALSE), colour = "red")+
  scale_x_log10()+
  scale_y_log10()+
  theme_bw()+
  labs(x = "House size", y = "")

Fig_Vrable_cCDF <- plot_grid(fig_vrable_ccdf_a, fig_vrable_ccdf_b,
          labels = "auto", nrow = 1)

save(Fig_Vrable_cCDF, file = "Figures/Fig_Vrable_cCDF.RData")

DELETE START

# N count per time sample, list
vrable_samples %>%
  group_by(sample) %>%
  summarise(n = n(),
            gini = Gini(house_size))
# Drop Gini, filter samples with n>10
# Do I need a graph of the temporal dynamics of house count in Vráble?
DELETE END



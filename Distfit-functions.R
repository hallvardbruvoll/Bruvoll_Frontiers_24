# Functions and libraries needed for distribution fitting
library(poweRlaw) # for heavy-tailed distribution fitting functions
library(MASS) # for whole distribution fitting
library(AICcmodavg) # for multiple model selection by AIC instead of
                    # Vuong's pairwise log-likelihood test (poweRlaw)
library(ineq) # for Gini index
library(scales) # for handling log-scales in plots
library(cowplot) # for combining multiple plots
library(flextable) # for printable results tables
library(officer) # export tables to MS Words
library(tidyverse)  # general syntax (mainly dplyr and ggplot2)

# Wrapper and loop functions ----------------------------------------------

#Fit a power-law, log-normal, exponential and stretched exponential
#(Weibull cCDF) models to the data by MLE, and estimating the best
#power-law xmin through KS-testing (cf. @clauset2009; @gillespie2015).
tail.models <- function(data) { #"data" must be a continuous numeric vector
  pl.model <- conpl$new(data) # power-law model
  xmin <- estimate_xmin(pl.model)
  pl.model$setXmin(xmin)
  ln.model <- conlnorm$new(data) # lognormal model
  ln.model$setXmin(xmin) # xmin for all models are set at the best pl fit
  ln.pars <- estimate_pars(ln.model)
  ln.model$setPars(ln.pars)
  exp.model <- conexp$new(data) # exponential model
  exp.model$setXmin(xmin)
  exp.pars <- estimate_pars(exp.model)
  exp.model$setPars(exp.pars)
  strexp.model <- conweibull$new(data) # stretched exponential/Weibull model
  strexp.model$setXmin(xmin)
  strexp.pars <- estimate_pars(strexp.model)
  strexp.model$setPars(strexp.pars)
  return(list("ln" = ln.model, # mind the model order here (for AIC below)
              "str_exp" = strexp.model,
              "exp" = exp.model,
              "pl" = pl.model))
}
#Early experiments included a model function for power law with exponential
#cutoff, discussed in the Clauset et al. (2009) paper, not implemented in
#the poweRlaw package (May 2023). The function was borrowed from
#"https://github.com/jeffalstott/powerlaw/tree/master/testing/
#pli-R-v0.0.3-2007-07-25", written by C. Shalizi, co-author of the 2009 paper.
#Because of copyright issues, this model is not included here. However, when
#included it never passed as the best fit for the data used in this thesis
#(note that the code does not include any function for setting xmin, unlike
#all the models included in the poweRlaw package).

#Extract coordinates for cCDF (i.e. survival function) plot of models.
#Input "models" must be a list of the type produced by the tail.models
#function above.
extract.xy <- function(models) { #This function goes into the next one below
  plot.new()
  model_xy <- lines(models)
  model_xy <- tibble(model_xy) %>%
    # Filter out rows where y = 0 (a bug in the poweRlaw package)
    # This doesn't affect analyses, only graphical representation
    filter(y != 0) %>%
    rename(value = x,
           ccdf = y) %>%
    mutate(model = TRUE)
  return(model_xy)
}

#Loop the above function for all input models
models.xy <- function(models) {
  output <- tibble()
  for (i in 1:length(models)) {
    modxy <- extract.xy(models = models[[i]]) %>%
      mutate(model = names(models[i]))
    output <- bind_rows(output, modxy)
  }
  return(output)
}

#Table with input and columns for storing results
dist.fit.object <- function(data.vector, set, culture) {
  object <- tibble(value = data.vector,
                   rank = min_rank(value),
                   ccdf = round((length(rank)-rank+1)/length(rank), 4),
                   model = FALSE,
                   tail = NA, xmin = NA, ntail = NA, par1 = NA, par2 = NA,
                   set = set, Gini = NA, Culture = culture)
  return(object)
}

# Model selection with AICc (second order Akaike's Information Criterion)
aic.selection <- function(tail_models){ # For a single data series
  # Find log-likelihood for each model
  log_lik <- map_dbl(tail_models, dist_ll)
  # Number of parameters for each model (I gave up vectorising this properly)
  no_pars <- c(tail_models[[1]]$no_pars, tail_models[[2]]$no_pars,
               tail_models[[3]]$no_pars, tail_models[[4]]$no_pars)
  modnames <- names(tail_models)
  # Sample size (number of observations) above xmin
  nobs <- tail_models[[1]]$internal$n
  sample_AIC <- aictabCustom(logL = log_lik, K = no_pars,
                             modnames = modnames,
                             second.ord = TRUE, # better for small samples
                             nobs = nobs)
  return(sample_AIC)
}

# Add results to dist.fit.object
add.results <- function(data, tail_models, AIC.results) {
  # data must be a dist.fit.object (see above)
  data$tail <- AIC.results[1,1]
  data$xmin <- tail_models[[1]]$xmin
  data$ntail <- tail_models[[1]]$internal$n
  data$Gini <- round(Gini(data$value), 3)
  if (data$tail[1] == "ln") { # the poweRlaw objects are hard to vectorise
    # so this gets a bit repetitive
    data$par1 <- tail_models$ln$pars[1]
    data$par2 <- tail_models$ln$pars[2]
  }
  if (data$tail[1] == "str_exp") {
    data$par1 <- tail_models$str_exp$pars[1]
    data$par2 <- tail_models$str_exp$pars[2]
  }
  if (data$tail[1] == "exp") {
    data$par1 <- tail_models$exp$pars
  }
  if (data$tail[1] == "pl") {
    data$par1 <- tail_models$pl$pars
  }
  return(data)
}

# The function that we actually end up using for analysis.
# It depends on all the above.
# "x" and "set" must be in the same order, preferably as columns in a table
# For this paper, "set" corresponds to settlement,
# quarter or time sample vector, and "x" are house sizes.
dist.fit.all <- function(x, set, culture=NA){
  data <- tibble(x, set = as.factor(set), culture = as.factor(culture)) %>%
    group_by(set) # Group to analyse each set separately
  sets <- levels(data$set) # Causes output to be in alphabetic order by sets
  output <- tibble()
  for (i in 1:length(sets)) {
    # Filter out one set and create object with necessary columns
    one_sample <- filter(data, set == sets[i])
    one_set <- dist.fit.object(data.vector = one_sample$x,
                               set = sets[i],
                               culture = one_sample$culture[1])
    one_set_models <- tail.models(one_set$value) # Fit models to data
    one_set_AIC <- aic.selection(tail_models = one_set_models) # Compare models
    one_set <- add.results(data = one_set, # Add param values from best fit
                           tail_models = one_set_models,
                           AIC.results = one_set_AIC)
    one_model <- extract.xy(eval(parse(text = paste0("one_set_models$",
                                                     one_set_AIC[1,1]))))
    one_model <- one_model %>%
      mutate(tail = one_set$tail[1], xmin = one_set$xmin[1],
             par1 = one_set$par1[1], par2 = one_set$par2[1],
             set = one_set$set[1], Culture = one_set$Culture[1])
    # Add results for each set to output
    output <- bind_rows(output, one_set, one_model)
  }
  return(output)
}

# Now test the entire distributions (not just the tails)

# Fit models to house-size data
# Weibull is excluded here since it is more general
# Underlying function:
dist.models <- function(data) { # again, data is a cont. num. vector
  norm.model <- fitdistr(data, "normal")
  lnorm.model <- fitdistr(data, "lognormal")
  exp.model <- fitdistr(data, "exponential")
  #weib.model <- fitdistr(data, "weibull")
  return(list("norm" = norm.model, # again, mind the model order here
              "ln" = lnorm.model,  # for AIC below
              "exp" = exp.model
              #"weib" = weib.model
              ))
}

# Loop for grouped data and select models with AICc
# Analysis function (dependent on the previous):
whole.dist <- function(x, set, culture=NA) {
  data <- tibble(x, set = as.factor(set), culture) %>%
    group_by(set)
  sets <- levels(data$set)
  output <- tibble()
  for (i in 1:length(sets)) {
    one_set <- filter(data, set == sets[i])
    one_set_models <- dist.models(one_set$x)
    one_set_aic <- aictab(one_set_models, second.ord = TRUE)
    one_out <- tibble(Settlement = sets[i],
                      Model = one_set_aic[1,1],
                      Par1 = round(eval(parse(text = paste0("one_set_models$",
                                                      one_set_aic[1,1],
                                                      "$estimate[1]"))), 3),
                      Par2 = round(eval(parse(text = paste0("one_set_models$",
                                                      one_set_aic[1,1],
                                                      "$estimate[2]"))), 3),
                      Gini = round(Gini(one_set$x), 3), N = nrow(one_set),
                      Culture = one_set$culture[1])
    output <- bind_rows(output, one_out)
  }
  return(output)
}


# Examples ----------------------------------------------------------------
# Fig. 5.1 in Bruvoll 2023, adapted from fig. 5a in Clauset et al. 2009:
# The power law is correctly identified, but the log-normal gives a false
# positive result. See text for further discussion.

set.seed(100)
data <- bind_rows(tibble(x = rlnorm(1000, 0.3, 2), set = "ln", culture = "A"),
                  tibble(x = rexp(1000, 0.125), set = "exp", culture = "A"),
                  tibble(x = rweibull(1000, 0.5, 3), set = "str_exp",
                         culture = "B"),
                  tibble(x = rplcon(100, 15, 2.5), set = "pl",
                         culture = "B")) %>%
  filter(x > 15) %>%
  group_by(set) %>%
  slice_sample(n = 100)

test <- dist.fit.all(x = data$x, set = data$set, culture = data$culture)

ggplot(filter(test, model == FALSE))+
  aes(x = value, y = ccdf)+
  geom_point(aes(colour = set, shape = set))+
  #  scale_shape_manual(values = c(1,2,3,4))+
  geom_line(data = filter(test, model == TRUE & tail == "pl"),
            aes(group = set), size = 1)+
  scale_y_log10()+
  scale_x_log10()

# Same without truncation (xmin): the stretched exponential tail is also
# interpreted as a power law.
data2 <- bind_rows(tibble(x = rlnorm(100, 0.3, 2), set = "ln"),
                   tibble(x = rexp(100, 0.125), set = "exp"),
                   tibble(x = rweibull(100, 0.5, 3), set = "str_exp"),
                   tibble(x = rplcon(100, 15, 2.5), set = "pl"))

test2 <- dist.fit.all(x = data2$x, set = data2$set)

ggplot(filter(test2, model == FALSE))+
  aes(x = value, y = ccdf)+
  geom_point(aes(colour = set, shape = set))+
  #  scale_shape_manual(values = c(1,2,3,4))+
  geom_line(data = filter(test2, model == TRUE & tail == "pl"),
            aes(group = set), size = 1)+
  scale_y_log10()+
  scale_x_log10()

# Whole distribution:
data3 <- bind_rows(tibble(x = rexp(100, 0.125), set = "exp"),
                   tibble(x = rlnorm(100, 0.3, 2), set = "ln"),
                  tibble(x = rnorm(100, 50, 5), set = "norm"))

test_whole <- whole.dist(x = data3$x, set = data3$set)
# Results table: First column is input series,
# second column is selected best model, and then estimated parameter values.
test_whole

# Just to illustrate what the data looks like
ggplot(data3)+
  aes(x, group = set)+
  geom_density(aes(colour = set))

# Clean up
rm(data, data2, data3, test, test_whole, test2)

# END of dist-fit script

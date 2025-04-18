## install packages user might not have by replacing FALSE with TRUE
if(FALSE) {
    install.packages("RSQLite", "nls.multstart")
    ##  Install the thermal curve package from git_hub, not cran
    remotes::install_github("padpadpadpad/rTPC")
}

## Install libraries
library(RSQLite)
library(rTPC)
library(nls.multstart)
library(broom)
library(tidyverse)

## Read in ZEFI Data sets
repeatability <- read.csv(file.path("..", "data", "raw_data", "HSPi-Repeatability-Song-Count.csv"))
heat_round1 <- read.csv(file.path("..", "data", "raw_data", "HSPi-Round-1-Heat-Trials.csv"))
heat_round2 <-read.csv(file.path("..", "data", "raw_data", "HSPi-Round-2-Heat-Trials.csv"))


## Create a database
ZEFI_DB <-"portalR.db"
myConn <-dbConnect(drv = SQLite(), dbname=ZEFI_DB)
dbListTables(myConn)

## add datframes into the database
dbWriteTable(myConn, "repeatability", repeatability)
dbWriteTable(myConn, "heat_round1", heat_round1)
dbWriteTable(myConn, "heat_round2", heat_round2)
dbListTables(myConn)


## #PRACTICE##
## keep a single curve
d <-filter(heat_round2, Male == "T247")

## show the data
windows()
ggplot(d, aes(Desired.Temp,song_count)) + 
  geom_point()+ 
  theme_bw(base_size = 12) +
  labs(x='Temperature (C)', 
       y='Song Count', 
       title = 'Song Count Across Temperature')

## choose the model
mod = 'lactin2_1995'

## get starting values
start_vals <- get_start_vals(d$Desired.Temp, d$song_count, model_name = 'lactin2_1995')
  
## Get limits
low_lims <- get_lower_lims(d$Desired.Temp,d$song_count,model_name = 'lactin2_1995')
upper_lims <- get_upper_lims(d$Desired.Temp, d$song_count, model_name = 'lactin2_1995')

start_vals
low_lims
upper_lims
  
## fit model
fit <- nls_multstart(song_count~lactin2_1995(temp = Desired.Temp, a,b,tmax,delta_t),
                                  data = d,
                                  iter = 600,
                                  start_lower = start_vals-10,
                                  start_upper = start_vals+10,
                                  lower = low_lims,
                                  upper = upper_lims,
                                  supp_errors = 'Y',
                                  convergence_count = FALSE)
  
## look at model fit
summary(fit)

## Predict new data 
preds <- data.frame(temp = seq(min(d$Desired.Temp), max(d$Desired.Temp), length.out = 6))
preds <- broom::augment(fit, newdata = preds)

## plot data and model fit
windows()
ggplot(preds)+
  geom_point(aes(Desired.Temp,song_count),d)+
  geom_line(aes(temp, .fitted), preds, col = 'blue')+
  theme_bw()+
  labs(x='Temperature (C)',
       y='Song Count',
       title = 'Song Count Across Temperatures')




#####07/19/22##########
###Unweighted model###

## Fit 4 Chosen model formulations in rTPC
d <-filter(heat_round2, Male =="T236")
d_fits <- nest(d, data = c(Desired.Temp,song_count)) %>%
  mutate(lactin = map(data,~nls_multstart(song_count~lactin2_1995(temp = Desired.Temp, a, b, tmax, delta_t),
                      data = .x,
                      iter = c(3,3,3,3),
                      start_lower = get_start_vals(.x$Desired.Temp, .x$song_count, model_name = 'lactin2_1995')-10,
                      start_upper = get_start_vals(.x$Desired.Temp, .x$song_count, model_name = 'lactin2_1995')+10,
                      lower = get_lower_lims(.x$Desired.Temp, .x$song_count, model_name = 'lactin2_1995'),
                      upper= get_upper_lims(.x$Desired.Temp, .x$song_count, model_name = 'lactin2_1995'),
                      supp_errors = 'Y',
                      convergence_count = FALSE)),
weibull = map(data,~nls_multstart(song_count~weibull_1995(temp = Desired.Temp, a, topt, b,c),
                                  data = .x,
                                  iter = c(4,4,4,4),
                                  start_lower = get_start_vals(.x$Desired.Temp, .x$song_count, model_name = 'weibull_1995')-10,
                                  start_upper = get_start_vals(.x$Desired.Temp, .x$song_count, model_name = 'weibull_1995')+10,
                                  lower = get_lower_lims(.x$Desired.Temp, .x$song_count, model_name = 'weibull_1995'),
                                  upper= get_upper_lims(.x$Desired.Temp, .x$song_count, model_name = 'weibull_1995'),
                                  supp_errors = 'Y',
                                  convergence_count = FALSE)),
modifiedgaussian = map(data, ~nls_multstart(song_count~modifiedgaussian_2006(temp = Desired.Temp, rmax,topt, a, b),
                                            data = .x,
                                            iter = c(3,3,3,3),
                                            start_lower = get_start_vals(.x$Desired.Temp, .x$song_count, model_name = 'modifiedgaussian_2006')-10,
                                            start_upper = get_start_vals(.x$Desired.Temp, .x$song_count, model_name = 'modifiedgaussian_2006')+10,
                                            lower = get_lower_lims(.x$Desired.Temp, .x$song_count, model_name = 'modifiedgaussian_2006'),
                                            upper= get_upper_lims(.x$Desired.Temp, .x$song_count, model_name = 'modifiedgaussian_2006'),
                                            supp_errors = 'Y',
                                            convergence_count = FALSE)),
briere = map(data,nls_multstart(song_count~briere2_1999(temp = Desired.Temp, tmin, tmax, a,b),
                                data = .x,
                                iter = c(4,4,4,4),
                                start_lower = get_start_vals(.x$Desired.Temp, .x$song_count, model_name = 'briere2_1999')-10,
                                start_upper = get_start_vals(.x$Desired.Temp, .x$song_count, model_name = 'briere2_1999')+10,
                                lower = get_lower_lims(.x$Desired.Temp, .x$song_count, model_name = 'briere2_1999'),
                                upper= get_upper_lims(.x$Desired.Temp, .x$song_count, model_name = 'briere2_1999'),
                                supp_errors = 'Y',
                                convergence_count = FALSE)))

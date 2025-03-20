#####################
#Analyses of behavioral trials

#R script to accompany Danner, Coomes, and Derryberry. Simulated heat waves reduce cognitive and motor performance of an endotherm. Ecology and Evolution.
#####################

#####################
#Table of contents

#I. Color Association
  #1. Time to finish trial 
  #2. Time between flips
  #3. Time to husk seeds
  #4. Latency to begin task
  #5. Accuracy of color association
  #6. Missed food rewards
  
#II. Detour Reaching
  #1. Number of trials
#####################

#####################
#I. Color Association
#####################
library(nlme)
library(lme4)
library(dplyr)
library("r2glmm")

drop.levels <- function (dat) {
  if (is.factor(dat)) dat <- dat[, drop = TRUE] else dat[] <-
      lapply(dat, function(x) x[, drop = TRUE]);
  return(dat) ;}; #define the function to drop removed levels (i.e. individuals of unknown sex)

mods<-list()
color_trials <- 
  read.csv("color_association_thermal_trials.csv", 
           header=TRUE) %>% 
  mutate(Date=as.Date(Date, format = "%m/%d/%y", origin = "1899-12-30")) %>%
  mutate(fExam.temp = factor(Exam.temp))

# Convert time during video to time format.
time_cols <- 
  c("Cage.door.closed","Touch.tray",
    "cor.col.1", "cor.col.2", "cor.col.3",
    "cor.col.4", "cor.col.5", "cor.col.6",
    "cor.col.7", "cor.col.8", "cor.col.9",
    
    "inc.col.1", "inc.col.2", "inc.col.3",
    "inc.col.4", "inc.col.5", "inc.col.6",
    "inc.col.7", "inc.col.8", "inc.col.9")
color_trials[, time_cols] <- 
  as.POSIXct(as.character(unlist(color_trials[ , time_cols])), 
             tz="EST", format="%H:%M:%S")

#Then subtract times to get intervals. 
dif_cols <- paste0("c.dif",1:8)
color_trials <-
  color_trials %>% 
  mutate(
latency= Touch.tray - Cage.door.closed,
finish= cor.col.9 - Touch.tray,
c.dif1= cor.col.2 - cor.col.1,
c.dif2= cor.col.3 - cor.col.2,
c.dif3= cor.col.4 - cor.col.3,
c.dif4= cor.col.5 - cor.col.4,
c.dif5= cor.col.6 - cor.col.5,
c.dif6= cor.col.7 - cor.col.6,
c.dif7= cor.col.8 - cor.col.7,
c.dif8= cor.col.9 - cor.col.8 )

color_trials$c.dif.mean <- 
  apply(color_trials %>% select(one_of(dif_cols)), MARGIN=1, FUN=mean, na.rm=TRUE)
color_trials$c.dif.sd <- 
  apply(color_trials %>% select(one_of(dif_cols)), MARGIN=1, FUN=sd, na.rm=TRUE)
color_trials$c.dif.max <- 
  apply(color_trials %>% select(one_of(dif_cols)), MARGIN=1, FUN=max, na.rm=TRUE)

color_trials <-
  color_trials %>% filter(Finish. == "Yes") %>%
  drop.levels() %>% 
  mutate(fExam.temp = factor(Exam.temp)) # drop removed levels

# Foraging efficiency data 
forage <- 
  read.csv("color_association_foraging_efficiency.csv", 
           header=TRUE)  %>%
  mutate(birddate=paste(Bird, Date, sep=""),
         fTemp.set=factor(Temp.set))
# convert time measurements to time difference
time_cols_f <- c("Seed.1.duration","Seed.2.duration", "Seed.3.duration")
forage[, time_cols_f] <- 
  as.POSIXct(as.character(unlist(forage[ , time_cols_f])), 
             tz="EST", format="%H:%M:%S") -
  as.POSIXct("0:00:00",tz="EST", format="%H:%M:%S") 

forage$Seed.duration.mean <- 
  apply(forage %>% select(one_of(time_cols_f)), MARGIN=1, FUN=mean, na.rm=F)


# 1. Time to finish trial
mods$ttfin_nlme <-list()
summary(mods$ttfin_nlme$temp <-lme(finish ~ fExam.temp,random = ~1|Bird, data=color_trials, method = "ML"))
summary(mods$ttfin_nlme$const <-lme(finish ~ 1,random = ~1|Bird, data=color_trials, method = "ML"))
print(with(mods$ttfin_nlme,anova(const,temp)))

# 2. Time between flips
mods$ttflip_nlme <-list()
summary(mods$ttflip_nlme$temp <-
          lme(c.dif.mean ~ fExam.temp,random = ~1|Bird, 
              data=color_trials %>% filter(is.finite(c.dif.mean)), method = "ML"))
summary(mods$ttflip_nlme$const <-
          lme(c.dif.mean ~ 1,random = ~1|Bird, 
              data=color_trials %>% filter(is.finite(c.dif.mean)), method = "ML"))
print(with(mods$ttflip_nlme,anova(const,temp)))

# 3. Time to husk seeds
mods$tthusk_nlme <-list()
f_husk <- forage %>% filter(is.finite(Seed.duration.mean))
summary(mods$tthusk_nlme$temp <-
          lme(Seed.duration.mean ~ fTemp.set,random = ~1|Bird, data=f_husk, method = "ML"))
summary(mods$tthusk_nlme$const <-
          lme(Seed.duration.mean ~ 1,random = ~1|Bird, data=f_husk, method = "ML"))
print(with(mods$tthusk_nlme,anova(const,temp)))
print(summary(mods$tthusk_nlme$temp)$tTable)

# 4. Latency to begin
mods$latency_nlme <-list()
d_latency<- color_trials %>% filter(is.finite(latency))
summary(mods$latency_nlme$temp <-
          lme(latency ~ fExam.temp,random = ~1|Bird, data=d_latency, method = "ML"))
summary(mods$latency_nlme$const <-
          lme(latency ~ 1,random = ~1|Bird, data=d_latency, method = "ML"))
print(with(mods$latency_nlme,anova(const,temp)))

# 5. Accuracy of color association
mods$acc_lme4 <-list()
summary(mods$acc_lme4$temp <-
          lme4::glmer(Error.ratio ~ fExam.temp+(1|Bird), data=color_trials,
                      family = binomial, weights=rep(9,nrow(color_trials))))
summary(mods$acc_lme4$const <-
          lme4::glmer(Error.ratio ~ 1+(1|Bird), data=color_trials,
                      family = binomial, weights=rep(9,nrow(color_trials))))
print(with(mods$acc_lme4,anova(const,temp)))
summary(mods$acc_lme4$temp)

# 6. Missed food rewards
mods$seed_lme4 <-list()
f_seeds <- forage %>% filter(is.finite(Num.seeds.eaten))
summary(mods$seed_lme4$temp <-
          lme4::glmer(cbind(Num.seeds.eaten,3-Num.seeds.eaten) ~ fTemp.set+( 1|Bird), 
                data=f_seeds, family=binomial))
summary(mods$seed_lme4$const <-
          glmer(cbind(Num.seeds.eaten,3-Num.seeds.eaten) ~ 1+( 1|Bird), 
                data=f_seeds, family=binomial))
print(with(mods$seed_lme4,anova(const,temp)))

#########################
#End I. Color Association
#########################

####################
#II. Detour Reaching
####################  
 

# 1. Number of trials
detour <- 
  read.csv("detour_reaching_thermal_trials.csv", 
           header=TRUE) %>% 
  mutate(Date=as.Date(Date, format = "%m/%d/%y", origin = "1899-12-30"),
         Trial.temp=c(Ambient=22, High=44)[as.character(Trial.type)])
mods$detour <-list()
mods$detour$temp <- glmer(Num.trials ~ Trial.type+( 1|Ind),
                          data=detour, family=poisson)
mods$detour$const <- glmer(Num.trials ~ 1+( 1|Ind),
                          data=detour, family=poisson)
summary(mods$detour$temp)
with(mods$detour,anova(const,temp))

r2beta(mods$detour$temp, method="sgv")


mods$detour$sess <- glmer(Num.trials ~ Sess.num+( 1|Ind),
                          data=detour, family=poisson)
summary(mods$detour$sess)
with(mods$detour,anova(const,sess))

########################
#End II. Detour Reaching
########################  

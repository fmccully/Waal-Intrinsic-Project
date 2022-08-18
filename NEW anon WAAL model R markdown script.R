
## @knitr setup2
#setup
library(dplyr)
library(MuMIn)
library(ggplot2)
library(lme4)
library(tidyverse)
library(optimx)
library(knitr)
library(car)
library(effects)
library(sjPlot)
library(here)

# upload data and remove extra variable
total_df2 <- read_csv("anon_waal_dataset3.csv")

#binary variables as factors
total_df2$sex<- as.factor(total_df2$sex)
total_df2$partner_type<- as.factor(total_df2$partner_type)
total_df2$breed_stage<-as.factor(total_df2$breed_stage)
total_df2$new_part<-as.factor(total_df2$new_part)
total_df2$cycle<-as.factor(total_df2$cycle)
total_df2$cycle_pair<-as.factor(total_df2$cycle_pair)

## @knitr div

# dividing brooding and incubation observations
incu_new<-subset(total_df2, breed_stage == "incu") # 260 observations
brood_new<-subset(total_df2, breed_stage== "brood") # 611 observations

## @knitr des-incu
#sample sizes- incubation
unique(incu_new$cycle_pair)# 35 breeding attempts
unique(incu_new$pair_id)
unique(incu_new$tag)#62 birds, 31 pairs 
unique(incu_new$cycle) #7 breeding seasons
unique(incu_new$age) #no ages

## @knitr sam-sp-in
with(incu_new, tapply(pair_id, cycle, FUN = function(x) length(unique(x)))) # pair sample size per year
# number birds per age group (individual birds may appear more than once)
with(incu_new, tapply(tag, age, FUN = function(x) length(unique(x)))) 
table(incu_new$cycle) # no obs per year

## @knitr cent-incu
# measures of centrality- incubation
incu_new%>%summarise(average=mean(no_hours),med=median(no_hours), 
                     sd=sd(no_hours))
# males and female comparison
incu_new%>%group_by(sex)%>%summarise(average=mean(no_hours),
                                     med=median(no_hours), sd=sd(no_hours))
# loggered and partner bird (unloggered) comparison
incu_new%>%group_by(partner_type)%>%summarise(average=mean(no_hours),
                                              med= median(no_hours), sd=sd(no_hours))
# interannual comparison
incu_new%>%group_by(cycle)%>%summarise(average=mean(no_hours),
                                       med=median(no_hours), sd=sd(no_hours))

## @knitr des-brooding
#sample sizes- brooding
unique(brood_new$cycle_pair)#75 breeding attmepts
unique(brood_new$pair_id)
unique(brood_new$tag)#132 birds, 66 pairs 
unique(brood_new$cycle) #6 breeding seasons
unique(brood_new$age) #no ages

## @knitr sam-sp-br
# pair sample size per year
with(brood_new, tapply(pair_id, cycle, FUN = function(x) length(unique(x)))) 
# number birds per age group 
with(brood_new, tapply(tag, age, FUN = function(x) length(unique(x)))) 
table(brood_new$cycle) # no obs per year

## @knitr cent-brood
# measures of centrality- brooding
brood_new%>%summarise(average=mean(no_hours),med=median(no_hours), 
                      sd=sd(no_hours))
#males and females comparison
brood_new%>%group_by(sex)%>%summarise(average=mean(no_hours),
                                      med=median(no_hours), sd=sd(no_hours))
# loggered and partner birds comparison
brood_new%>%group_by(partner_type)%>%summarise(average=mean(no_hours),
                                               med= median(no_hours), sd=sd(no_hours))
#interannual comparison 
brood_new%>%group_by(cycle)%>%summarise(average=mean(no_hours),
                                        med=median(no_hours), sd=sd(no_hours))

## @knitr incu-data
incu_new2<-na.omit(incu_new)# double check there are no NAs

## @knitr incu-age
# collapse age group and create quadratic variable incubation

# focal bird age
with(incu_new2, tapply(tag, age, FUN = function(x) length(unique(x))))
incu_new2$age[incu_new2$age>36]<-36 # higher end of age spectrum
incu_new2$age[incu_new2$age<12]<-12 # lower end of age spectrum

#partner bird age
with(incu_new2, tapply(tag, p_age, FUN = function(x) length(unique(x))))
incu_new2$p_age[incu_new2$p_age>36]<-36 # higher end of age spectrum
incu_new2$p_age[incu_new2$p_age<12]<-12 # lower end of age spectrum

# age quadratic variable conversion 
incu_new2$age_sq <-poly(incu_new2$age,2, raw=TRUE)[,2]
incu_new2$p_age_sq <-poly(incu_new2$p_age,2, raw=TRUE)[,2]


## @knitr check-age
with(incu_new2, tapply(tag, age, FUN = function(x) length(unique(x))))
with(incu_new2, tapply(tag, p_age, FUN = function(x) length(unique(x))))


## @knitr incu-ppb
# creating final partner's previous bout variable
# calculate mean previous trip duration for each bird's partner in each season
incu_aves<-incu_new2%>%group_by(cycle_p_tag)%>%
  summarise(ppb_ave_no_hours=mean(part_prev_bout)) 

# convert this into a data frame
incu_avesdf<- data.frame(incu_aves) 

# add partner mean to original data set
incu_new2<-merge(incu_new2, incu_avesdf, by.x = "cycle_p_tag", by.y="cycle_p_tag") 

# subtract raw partners previous bout from partner's mean
incu_new2<-incu_new2%>%mutate(ppb_ave_diff=part_prev_bout-ppb_ave_no_hours) 

# new variable summary
incu_new2%>%summarise(average=mean(ppb_ave_diff),med=median(ppb_ave_diff), sd=sd(ppb_ave_diff))

## @knitr brood-data
brood_new2<-na.omit(brood_new)# check to ensure no NAs

## @knitr brood-age
# collapse age groups and creating quadratic variable brooding
with(brood_new2, tapply(tag, age, FUN = function(x) length(unique(x))))
brood_new2$age[brood_new2$age>34]<-34 # higher end of age spectrum
brood_new2$age[brood_new2$age<8]<-8 # lower end of age spectrum

with(brood_new2, tapply(tag, p_age, FUN = function(x) length(unique(x))))
brood_new2$p_age[brood_new2$p_age>34]<-34 # higher end of age spectrum
brood_new2$p_age[brood_new2$p_age<8]<-8 # lower end of age spectrum

# age quadratic variable conversion 
brood_new2$age_sq <-poly(brood_new2$age,2, raw=TRUE)[,2]
brood_new2$p_age_sq <-poly(brood_new2$p_age,2, raw=TRUE)[,2]

## @knitr brood-check
with(brood_new2, tapply(tag, age, FUN = function(x) length(unique(x))))
with(brood_new2, tapply(tag, p_age, FUN = function(x) length(unique(x))))

## @knitr brood-ppb
# creating final partner's previous bout variable
# calculate mean previous trip duration for each bird's partner in each season
brood_aves<-brood_new2%>%group_by(cycle_p_tag)%>%
  summarise(ppb_ave_no_hours=mean(part_prev_bout))

# converted into a dataframe
brood_avesdf<- data.frame(brood_aves)

# add new mean to original data set
brood_new2<-merge(brood_new2, brood_avesdf, by.x = "cycle_p_tag", by.y="cycle_p_tag") 

# subtract previous partner bout from mean
brood_new2<-brood_new2%>%mutate(ppb_ave_diff=part_prev_bout-ppb_ave_no_hours)

# new variable summary
brood_new2%>%summarise(average=mean(ppb_ave_diff),med=median(ppb_ave_diff), sd=sd(ppb_ave_diff))


## @knitr data-scale
#recombine into one dataframe for scaling (so coordination is comparable)
# a combined data set of both incubation and brooding data is created
total_df4<-full_join(incu_new2, brood_new2)

#rescaling

total_df4$age_sc<-scale(total_df4$age)
total_df4$p_age_sc<-scale(total_df4$p_age)
total_df4$age_sq_sc<-scale(total_df4$age_sq)
total_df4$p_age_sq_sc<-scale(total_df4$p_age_sq)
total_df4$pers_sc<-scale(total_df4$pers)
total_df4$num_days_sc<-scale(total_df4$NumDays)
total_df4$p_pers_sc<-scale(total_df4$p_pers)
total_df4$ppb_ave_diff_sc<-scale(total_df4$ppb_ave_diff)

#redivide into incubation and brooding
incu_new2<-subset(total_df4, breed_stage == "incu") # 260 observations
brood_new2<-subset(total_df4, breed_stage== "brood") # 611 observations


## @knitr norm-incu
#normality
# no_hours is non-normal requires transformation
histogram_hours_sqrt<-incu_new2 %>%ggplot(aes(x=no_hours))+
  geom_histogram (binwidth = 10)
histogram_hours_sqrt

# creating new sqrt response variable
incu_new2$no_hours_sqrt<-sqrt(incu_new2$no_hours)

#check success
histogram_hours_sqrt<-incu_new2 %>%ggplot(aes(x=no_hours_sqrt))+
 geom_histogram (binwidth = 0.5)
histogram_hours_sqrt

## @knitr incu-hov
plot(lm(no_hours_sqrt~pers_sc,data=incu_new2))
plot(lm(no_hours_sqrt~p_pers_sc,data=incu_new2))
plot(lm(no_hours_sqrt~age_sc,data=incu_new2))
plot(lm(no_hours_sqrt~age_sq_sc,data=incu_new2))
plot(lm(no_hours_sqrt~p_age_sc,data=incu_new2))
plot(lm(no_hours_sqrt~p_age_sq_sc,data=incu_new2))
plot(lm(no_hours_sqrt~ppb_ave_diff_sc,data=incu_new2))

## @knitr co-in
cor.test(incu_new2$age_sc,incu_new2$pers_sc) # view correlation
col_g<-incu_new2%>%ggplot(aes(x =age_sc , y =pers_sc))+
  geom_point(size=2)
col_g

colin_model <- lm(no_hours_sqrt ~ age_sc +pers_sc, data = incu_new2)
summary(colin_model)
vif(colin_model) #>3 = problematic 

#age and ppb
cor.test(incu_new2$age_sc,incu_new2$ppb_ave_diff_sc) #view correlation
col_g<-incu_new2%>%ggplot(aes(x =age_sc , y =ppb_ave_diff_sc))+
  geom_point(size=2)
col_g

colin_model <- lm(no_hours_sqrt ~ age_sc +ppb_ave_diff_sc, data = incu_new2)
summary(colin_model)
vif(colin_model) #>3 = problematic 

#age and p_age
cor.test(incu_new2$age_sc,incu_new2$p_age_sc) # view correlation 
col_g<-incu_new2%>%ggplot(aes(x =age_sc , y =p_age_sc))+
  geom_point(size=2)
col_g

colin_model <- lm(no_hours_sqrt ~ age_sc +p_age_sc, data = incu_new2)
summary(colin_model)
vif(colin_model) #>3 = problematic

#pers and p_pers
cor.test(incu_new2$pers_sc,incu_new2$p_pers_sc) # view correlation
col_g<-incu_new2%>%ggplot(aes(x = pers_sc , y = p_pers_sc))+
  geom_point(size=2)
col_g

colin_model <- lm(no_hours_sqrt ~ pers_sc +p_pers_sc, data = incu_new2)
summary(colin_model)
vif(colin_model) #>3 = problematic


## @knitr brood-norm
#no_hour is non normal and requires transformation 
histogram_hours_sqrt<-brood_new2 %>%ggplot(aes(x=no_hours))+
geom_histogram (binwidth = 10)
histogram_hours_sqrt

# creating new sqrt response variable
brood_new2$no_hours_sqrt<-sqrt(brood_new2$no_hours)

# check success
histogram_hours_sqrt<-brood_new2 %>%ggplot(aes(x=no_hours_sqrt))+
geom_histogram (binwidth = 0.5)
histogram_hours_sqrt


## @knitr brood-hov
plot(lm(no_hours_sqrt~pers_sc,data=brood_new2))
plot(lm(no_hours_sqrt~age_sc,data=brood_new2))
plot(lm(no_hours_sqrt~age_sq_sc,data=brood_new2))
plot(lm(no_hours_sqrt~p_pers_sc,data=brood_new2))
plot(lm(no_hours_sqrt~p_age_sc,data=brood_new2))
plot(lm(no_hours_sqrt~p_age_sq_sc,data=brood_new2))
plot(lm(no_hours_sqrt~ppb_ave_diff_sc,data=brood_new2))


## @knitr brood-co
#pers and age
cor.test(brood_new2$age_sc,brood_new2$pers_sc)
col_g<-brood_new2%>%ggplot(aes(x =age_sc , y =pers_sc))+
  geom_point(size=2)
col_g

colin_model <- lm(no_hours_sqrt ~ age_sc +pers_sc, data = brood_new2)
summary(colin_model)
vif(colin_model) #>3 = problematic 

#age and ppb
cor.test(brood_new2$age_sc,brood_new2$ppb_ave_diff_sc)
col_g<-brood_new2%>%ggplot(aes(x =age_sc , y =ppb_ave_diff_sc))+
  geom_point(size=2)
col_g

colin_model <- lm(no_hours_sqrt ~ age_sc +ppb_ave_diff_sc, data = brood_new2)
summary(colin_model)
vif(colin_model) #>3 = problematic 

#age and p_age
cor.test(brood_new2$age_sc,brood_new2$p_age_sc)
col_g<-brood_new2%>%ggplot(aes(x =age_sc , y =p_age_sc))+
  geom_point(size=2)
col_g

colin_model <- lm(no_hours_sqrt ~ age_sc +p_age_sc, data = brood_new2)
summary(colin_model)
vif(colin_model) #>3 = problematic

#pers and p_pers
cor.test(brood_new2$pers_sc,brood_new2$p_pers_sc)
col_g<-brood_new2%>%ggplot(aes(x =pers_sc , y =p_pers_sc))+
  geom_point(size=2)
col_g

colin_model <- lm(no_hours_sqrt ~ pers_sc +p_pers_sc, data = brood_new2)
summary(colin_model)
vif(colin_model) #>3 = problematic

## @knitr combine
# an updated combined data set of both incubation and brooding data is created
total_df4<-full_join(incu_new2, brood_new2)

## @knitr total_sample

# sample sizes of total data set for the record
unique(total_df4$cycle_pair) # 95 breeding attempts
unique(total_df4$pair_id)
unique(total_df4$tag)# 142 birds, 71 pairs
unique(total_df4$cycle) #7 breeding seasons
unique(total_df4$age) #no ages
# pair sample size per year
with(total_df4, tapply(pair_id, cycle, FUN = function(x) length(unique(x)))) 
with(total_df4, tapply(tag, age, FUN = function(x) length(unique(x))))# age frequency


# means, medians and SD full dataset 
mean(total_df4$no_hours)
sd(total_df4$no_hours)
mean(total_df4$age)
sd(total_df4$age)
total_df4%>%group_by(sex)%>%summarise(average=mean(no_hours),
                                      med=median(no_hours), sd=sd(no_hours))
total_df4%>%group_by(partner_type)%>%summarise(average=mean(no_hours),
                                               med= median(no_hours), sd=sd(no_hours))
total_df4%>%group_by(breed_stage)%>%summarise(average=mean(no_hours),
                                              med=median(no_hours), sd=sd(no_hours))
total_df4%>%group_by(cycle)%>%summarise(average=mean(no_hours),
                                        med=median(no_hours), sd=sd(no_hours))


## @knitr save
write.csv(incu_new2, "incu_new2.csv")# incubation data
write.csv(brood_new2, "brood_new2.csv")# brooding data
write.csv(total_df4, "total_df4.csv") # all (combined) data

## @knitr inlinmod
lin_model <- lm(no_hours_sqrt ~ 
                  sex + 
                  pers_sc +
                  p_pers_sc+
                  age_sc +
                  age_sq_sc +
                  p_age_sc+
                  p_age_sq_sc+
                  num_days_sc+
                  new_part+
                  ppb_ave_diff_sc+
                  age_sc:pers_sc+
                  age_sq_sc:pers_sc+
                  age_sc:ppb_ave_diff_sc+
                  age_sq_sc:ppb_ave_diff_sc+
                  pers_sc:ppb_ave_diff_sc+
                  new_part:ppb_ave_diff_sc+
                  p_age_sc:p_pers_sc+
                  p_age_sq_sc:p_pers_sc+
                  p_age_sc:ppb_ave_diff_sc+
                  p_age_sq_sc:ppb_ave_diff_sc+
                  p_pers_sc:ppb_ave_diff_sc,
                  data = incu_new2)
summary(lin_model)


## @knitr incomod 
co_ml_incu<- lmer(no_hours_sqrt ~
                    sex + 
                    pers_sc +
                    p_pers_sc+
                    age_sc +
                    age_sq_sc +
                    p_age_sc+
                    p_age_sq_sc+
                    num_days_sc+
                    new_part+
                    ppb_ave_diff_sc+
                    age_sc:pers_sc+
                    age_sq_sc:pers_sc+
                    age_sc:ppb_ave_diff_sc+
                    age_sq_sc:ppb_ave_diff_sc+
                    pers_sc:ppb_ave_diff_sc+
                    new_part:ppb_ave_diff_sc+
                    p_age_sc:p_pers_sc+
                    p_age_sq_sc:p_pers_sc+
                    p_age_sc:ppb_ave_diff_sc+
                    p_age_sq_sc:ppb_ave_diff_sc+
                    p_pers_sc:ppb_ave_diff_sc+
                    p_pers_sc:sex+
                    pers_sc:sex +
                    (1|cycle) + (1+ppb_ave_diff_sc|cycle_pair),
                  data = incu_new2, na.action=na.fail, REML=FALSE,
                  control=lmerControl (optimizer="optimx",optCtrl=list(method='nlminb')))

summary(co_ml_incu)
hist(resid(co_ml_incu))

## @knitr in-dredge
# dredging to create model set
# linking age variables with their quadratic counterpart so either both or neither are retained
dr_cmin<-dredge(co_ml_incu, subset = (dc(age_sc, age_sq_sc) && 
                                        dc(p_age_sc, p_age_sq_sc) && 
                                        dc(age_sq_sc, age_sc) && 
                                        dc(p_age_sq_sc, p_age_sc)))
dr_cmin # view full model list


## @knitr mod-sel

sub_m1_incu<-subset(dr_cmin, delta<2) ##subsets models delta <2
sub_m1_incu # view subsetted models
no_m1_incu<-dim(as.data.frame(sub_m1_incu))[1] # how many models are in the subsetted model set
no_m1_incu # number of models retained

## @knitr in-nest
nest_m1_incu <- subset(sub_m1_incu, !nested(.)) # removes nested models
nest_m1_incu # view retained models
no_m12_incu<-dim(as.data.frame(nest_m1_incu))[1] # how many models retained when nested models removed
no_m12_incu # number of models

# see all coefficients for the top model set
in_coeff <- coef(nest_m1_incu, full=TRUE)
in_coeff

## @knitr in-mod-ave
modavm1_incu<-model.avg(nest_m1_incu, delta<2, fit = TRUE) # model averaging
modavm1_incu
Weights(modavm1_incu)
tablem1a_incu<-coefTable(modavm1_incu, full=TRUE)
tablem1a_incu # final coefficient table 

# confidence intervals
br_CI <- confint(modavm1_incu, full=TRUE)
br_CI

## @knitr bolk-brood

#bolker suggested linear model BROODING
lbr_model <- lm(no_hours_sqrt ~ 
                  sex + 
                  pers_sc +
                  p_pers_sc+
                  age_sc +
                  age_sq_sc +
                  p_age_sc+
                  p_age_sq_sc+
                  num_days_sc+
                  new_part+
                  ppb_ave_diff_sc+
                  age_sc:pers_sc+
                  age_sq_sc:pers_sc+
                  age_sc:ppb_ave_diff_sc+
                  age_sq_sc:ppb_ave_diff_sc+
                  pers_sc:ppb_ave_diff_sc+
                  new_part:ppb_ave_diff_sc+
                  p_age_sc:p_pers_sc+
                  p_age_sq_sc:p_pers_sc+
                  p_age_sc:ppb_ave_diff_sc+
                  p_age_sq_sc:ppb_ave_diff_sc+
                  p_pers_sc:sex+
                  pers_sc:sex +
                  p_pers_sc:ppb_ave_diff_sc,
                  data = brood_new2)
summary(lbr_model)

## @knitr coml-brood

co_ml_brood<- lmer(no_hours_sqrt ~
                     sex + 
                     pers_sc +
                     p_pers_sc+
                     age_sc +
                     age_sq_sc +
                     p_age_sc+
                     p_age_sq_sc+
                     num_days_sc+
                     new_part+
                     ppb_ave_diff_sc+
                     age_sc:pers_sc+
                     age_sq_sc:pers_sc+
                     age_sc:ppb_ave_diff_sc+
                     age_sq_sc:ppb_ave_diff_sc+
                     pers_sc:ppb_ave_diff_sc+
                     new_part:ppb_ave_diff_sc+
                     p_age_sc:p_pers_sc+
                     p_age_sq_sc:p_pers_sc+
                     p_age_sc:ppb_ave_diff_sc+
                     p_age_sq_sc:ppb_ave_diff_sc+
                     p_pers_sc:ppb_ave_diff_sc+
                     p_pers_sc:sex +
                     pers_sc:sex +
                     (1|cycle) + (1+ppb_ave_diff_sc|cycle_pair),
                   data = brood_new2, na.action=na.fail, REML=FALSE,
                   control=lmerControl (optimizer="optimx",optCtrl=list(method='nlminb')))

summary(co_ml_brood)
hist(resid(co_ml_brood))

## @knitr br-dredge
#dredge creating model set 
# linking age variables to their equivalent quadratic version so they can only be discarded or retained together 
dr_cmbr<-dredge(co_ml_brood, subset = (dc(age_sc, age_sq_sc) && 
                                       dc(p_age_sc, p_age_sq_sc) && 
                                       dc(age_sq_sc, age_sc) && 
                                       dc(p_age_sq_sc, p_age_sc)))

dr_cmbr # view full model list

## @knitr br-sel2 
sub_m1_brood<-subset(dr_cmbr, delta<2) ##subsets models delta <2
no_m1_brood<-dim(as.data.frame(sub_m1_brood))[1] # how many models are in the subsetted model
no_m1_brood

## @knitr br-nest
nest_m1_brood <- subset(sub_m1_brood, !nested(.))# remove nested models
nest_m1_brood
no_m12_brood<-dim(as.data.frame(nest_m1_brood))[1] # how many models when when nested models removed
no_m12_brood


# see all coefficients for the top model set
br_coeff <- coef(nest_m1_brood, full=TRUE)
br_coeff


## @knitr br-ave
modavm1_brood<-model.avg(nest_m1_brood, fit = TRUE) # model averaging
modavm1_brood
Weights(modavm1_brood)
tablem1a_brood<-coefTable(modavm1_brood, full=TRUE)
tablem1a_brood # final coefficients table 

# confidence intervals
br_CI <- confint(modavm1_brood, full=TRUE)
br_CI

save.image("New_model.RData")


# problem: back transforming the regression line from the model so that the graphs' Y axes can be the raw values


####---- setup----####

## @knitr graph-setup

library(MASS)
library(lme4)
library(sjPlot)
library(ggplot2)
library(optimx)
library(MuMIn)
library(dplyr)

total_df4<-read.csv("total_df4.csv")
incu_new2<-subset(total_df4, breed_stage == "incu") # incubation subset, 260 observations
brood_new2<-subset(total_df4, breed_stage== "brood") # brooding subset, 611 observations

## @knitr incu-model

# run the incubation model without scaled variables to get regression line, note sqrt response variable 
co_ml_incu_r<- lmer(no_hours_sqrt ~
                    sex + 
                    #pers +
                    p_pers+
                    age+
                    age_sq +
                    p_age+
                    p_age_sq+
                    NumDays+
                   # new_part+
                    ppb_ave_diff+
                    #age:pers+
                    #age_sq:pers+
                    #age:ppb_ave_diff+
                   # age_sq:ppb_ave_diff+
                  #  pers:ppb_ave_diff+
                    #new_part:ppb_ave_diff+
                   # p_age:p_pers+
                  #  p_age_sq:p_pers+
                  #  p_age:ppb_ave_diff+
                    #p_age_sq:ppb_ave_diff+
                    #p_pers:ppb_ave_diff+
                    (1|cycle) + (1+ppb_ave_diff|cycle_pair),
                  data = incu_new2, na.action=na.fail, REML=FALSE,
                  control=lmerControl (optimizer="optimx",optCtrl=list(method='nlminb')))

summary(co_ml_incu_r)

## @knitr incu-IT

# dredging to create model set
# linking age variables with their quadratic counterpart so either both or neither are retained
dr_cmin_r<-dredge(co_ml_incu_r, subset = (dc(age, age_sq) && 
                                        dc(p_age, p_age_sq) && 
                                        dc(age_sq, age) && 
                                        dc(p_age_sq, p_age)))
dr_cmin_r # view full model list

# confidence set creation 
sub_m1_incu_r<-subset(dr_cmin_r, delta<2) ##subsets models delta >2

# remove nested models
nest_m1_incur <- subset(sub_m1_incu_r, !nested(.)) # removes nested models

#model averaging 
modavm1_incur<-model.avg(nest_m1_incur, delta<2, fit = TRUE, revised.var = TRUE) # model averaging, this object is used to get the regression line

## @knitr brood-model

# do exactly the same thing again for the brooding model
co_ml_broodr<- lmer(no_hours_sqrt ~
                    sex + 
                    pers +
                    p_pers+
                    age +
                    age_sq +
                    p_age+
                    p_age_sq+
                    #NumDays+
                    new_part+
                    ppb_ave_diff+
                    #age:pers+
                    #age_sq:pers+
                    #age:ppb_ave_diff+
                    #age_sq:ppb_ave_diff+
                    #pers:ppb_ave_diff+
                    #new_part:ppb_ave_diff+
                    #p_age:p_pers+
                    #p_age_sq:p_pers+
                    #p_age:ppb_ave_diff+
                    #p_age_sq:ppb_ave_diff+
                    #p_pers:ppb_ave_diff+
                    (1|cycle) + (1+ppb_ave_diff|cycle_pair),
                  data = brood_new2, na.action=na.fail, REML=FALSE,
                  control=lmerControl (optimizer="optimx",optCtrl=list(method='nlminb')))

summary(co_ml_broodr)

## @knitr brood-IT

#dredge creating model set 
# linking age variables to their equivalent quadratic version so they can only be discarded or retained together 
dr_cmbrr<-dredge(co_ml_broodr, subset = (dc(age, age_sq) && 
                                         dc(p_age, p_age_sq) && 
                                         dc(age_sq, age) && 
                                         dc(p_age_sq, p_age)))
dr_cmbrr # view full model list

# creating confidence set
sub_m1_broodr<-subset(dr_cmbrr, delta<2) ##subsets models delta >2

# removing nested models
nest_m1_broodr <- subset(sub_m1_broodr, !nested(.))# remove nested models

# model averaging
modavm1_broodr<-model.avg(nest_m1_broodr, delta<2, fit = TRUE, revised.var = TRUE) # model averaging, this object will be used to get the regression line for the graphs

## @knitr mod-est
# objects will be used to extract regression lines
modavm1_incur # model averaged coefficients for incubation model
modavm1_broodr # model averaged coefficients for brooding model 

## @knitr co-graph

# coordination graph: y = no_hours (foraging trip, hrs), x = partner previous trip (difference from mean trip duration) 

## @knitr co-setup
# coordination graph setup
# getting regression lines from models

mA_inr<-summary(modavm1_incur) # summary incu model
mA_brsq<-summary(modavm1_broodr) #summary brooding model

inDatr <- plot_model(mA_inr, type = "pred", terms = c("ppb_ave_diff")) # extract regression line for incubation, identify predictor variable of interest
inDatr <- data.frame(inDatr$data)

brDatsq <- plot_model(mA_brsq, type = "pred", terms = c("ppb_ave_diff")) # extract regression line for brooding, identify predictor variable of interest
brDatsq <- data.frame(brDatsq$data)

## @knitr order-incu
# start process of making incubation and brooding appear on the graph in a logical order
total_df4$breed_stage<- as.character(total_df4$breed_stage)

for (a in 1:(nrow(total_df4))) {
  if (total_df4$breed_stage[a] == "incu")
  {total_df4$breed_stage[a] <- "Incubation"}
  else {total_df4$breed_stage[a] <- "Brooding"}
}

total_df4<-total_df4%>% arrange(total_df4) %>% 
  mutate(breed_stage = factor(breed_stage, levels=c("Incubation", "Brooding")))
## @knitr co-sq-root

# Compute the non-square-rooted data for plotting
inDatr.unsqr<- data.frame(x=inDatr$x, predicted=inDatr$predicted^2)
brDatsq.unsqr<- data.frame(x=brDatsq$x, predicted=brDatsq$predicted^2)

## @knitr co-new-df

# Create a single data frame with both predictions, labeled by the "breed_stage" factor
brDatsq.unsqr $ breed_stage<- "Brooding"
inDatr.unsqr $breed_stage <- "Incubation"
all_preds<- rbind(inDatr.unsqr, brDatsq.unsqr)

#finish process of making incubation and brooding appear in a logical order 
all_preds$breed_stage <- factor(all_preds$breed_stage, levels=c("Incubation", "Brooding"), ordered=TRUE) # ordered so that "incu" panel appears on left

## @knitr co-graph-ggplot

ppb_graph_all<-ggplot( aes(x = x, y = predicted), data = all_preds) +# regression line
  geom_line(size = 1.25)+ 
  geom_point(aes(x = ppb_ave_diff, y = no_hours), data = total_df4) + # raw data, not using square root
  theme_bw(base_size =15) +
  xlab("Partner's deviation from average trip duration (hrs)")+
  ylab("Individual trip duration (hrs)")+
  facet_wrap(.~breed_stage)+
  xlim(-181, 300)+
  ylim (0, 500)+ 
  theme_classic()+
  theme(axis.text = element_text(size = 15))+
  theme(axis.title= element_text(size=18))

ppb_graph_all


## @knitr pers-setup

# X axis = boldness score (collective, both focal and partner), Y axis =individual trip duration, brooding only, colour = focal or partner 

mA_brsq<-summary(modavm1_broodr) #summary brooding model

brDatpers <- plot_model(mA_brsq, type = "pred", terms = c("pers")) # extract regression line for pers
brDatpers <- data.frame(brDatpers$data)

brDatp_pers <- plot_model(mA_brsq, type = "pred", terms = c("p_pers")) # extract regression line for p_pers
brDatp_pers <- data.frame(brDatp_pers$data)

## @knitr pers-sq-root

# Compute the non-square-rooted pers data for plotting
brpers_unsqr<- data.frame(x=brDatpers$x, predicted=brDatpers$predicted^2,se=brDatpers$std.error, cih = brDatpers$conf.high^2, cil =brDatpers$conf.low^2)
brp_pers_unsqr<- data.frame(x=brDatp_pers$x, predicted=brDatp_pers$predicted^2, se=brDatp_pers$std.error, cih = brDatp_pers$conf.high^2, cil =brDatp_pers$conf.low^2)

## @knitr pers-graph-code

# create pers brooding graph
pers_graph_com<-ggplot(aes(x = x, y = predicted), data = brpers_unsqr) +# regression line for pers variable
  geom_line(size = 1.5, colour = "red")+
  geom_errorbar(aes(ymin=cil, ymax=cih), width=.1, colour = "red") +
  geom_line(aes(x = x, y = predicted), data = brp_pers_unsqr, size =1.5, colour = "dark blue", alpha = 0.7) +# regression line for p_pers variable
  geom_point(aes(x = pers, y = no_hours), data = brood_new2, alpha = 0.0) + # hide data point to validate x axis, using pers variable creates correct scale for both variables but will not feature in final plot 
  geom_errorbar(data =brp_pers_unsqr, aes(ymin=cil, ymax=cih), width=.1, colour = "dark blue") +
  theme_bw(base_size =15) +
  xlab("Boldness Score")+
  ylab("Individual trip duration (hrs)")+
  xlim(-1, 5)+
  ylim (0, 327)+ 
  theme_classic()+
  scale_x_continuous(breaks=c(-1, 0, 1, 2, 3, 4, 5))+
  theme(axis.text = element_text(size = 15))+
  theme(axis.title= element_text(size=18))
  
pers_graph_com

## @knitr age-setup

# x axis = partner age, y axis = individual trip duration, incubation only 

# extract regression line
mA_inr<-summary(modavm1_incur)
age_dat <- plot_model(mA_inr, type = "pred", terms = c("p_age_sq"))
age_dat<- data.frame(age_dat$data)

## @knitr un-sq-age

# non-square root data for plotting
age_dat<- age_dat%>% mutate(age_dat, age_new = (sqrt(x))) # calculate square root of age
age_dat.unsqr<- data.frame(x=age_dat$x, age_dat$age_new, predicted=age_dat$predicted^2) # untransform response variable 

## @knitr age-graph-code

p_ageincu_graph<-ggplot( aes(x = age_dat.age_new, y = predicted), data = age_dat.unsqr)+# regression line
  geom_line(size = 1.5)+ 
  geom_point(aes(x = p_age, y = no_hours), data = incu_new2, alpha = 0.0)+ # hides data points 
  theme_bw(base_size =15) +
  xlab("Partner age (years)")+
  ylab("Individual trip duration (hrs)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=c(10, 15, 20, 25, 30, 35, 40))+
  theme_classic()+
  theme(axis.text = element_text(size = 15))+
  theme(axis.title= element_text(size=18))


p_ageincu_graph


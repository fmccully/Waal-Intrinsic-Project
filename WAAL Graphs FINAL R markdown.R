####---- setup----####

## @knitr graph-setup

library(MASS)
library(knitr)
library(lme4)
library(sjPlot)
library(ggplot2)
library(optimx)
library(MuMIn)
library(dplyr)
library(here)

total_df4<-read.csv("anon_waal_dataset.csv")
incu_new2<-subset(total_df4, breed_stage == "incu") # incubation subset, 260 observations
brood_new2<-subset(total_df4, breed_stage== "brood") # brooding subset, 611 observations

## @knitr us-incu-mod

# run the incubation model without scaled variables to get p_age regression line
co_ml_incu_r<- lmer(no_hours_sqrt ~
                      sex + 
                      pers +
                      p_pers+
                      age+
                      age_sq +
                      p_age+
                      p_age_sq+
                      NumDays+
                      new_part+
                      ppb_ave_diff+
                      age:pers+
                      age_sq:pers+
                      age:ppb_ave_diff+
                      age_sq:ppb_ave_diff+
                      pers:ppb_ave_diff+
                      new_part:ppb_ave_diff+
                      p_age:p_pers+
                      p_age_sq:p_pers+
                      p_age:ppb_ave_diff+
                      p_age_sq:ppb_ave_diff+
                      p_pers:ppb_ave_diff+
                      p_pers:sex+
                      pers:sex+
                      (1|cycle) + (1+ppb_ave_diff|cycle_pair),
                    data = incu_new2, na.action=na.fail, REML=FALSE,
                    control=lmerControl (optimizer="optimx",optCtrl=list(method='nlminb')))

summary(co_ml_incu_r)

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

#model averaging this object is used to get the regression line
modavm1_incur<-model.avg(nest_m1_incur, delta<2, fit = TRUE, revised.var = TRUE) 


## @knitr s-incu-mod 

# run original, scaled  incu model
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

# dredging to create model set
# linking age variables with their quadratic counterpart so either both or neither are retained
dr_cmin<-dredge(co_ml_incu, subset = (dc(age_sc, age_sq_sc) && 
                                        dc(p_age_sc, p_age_sq_sc) && 
                                        dc(age_sq_sc, age_sc) && 
                                        dc(p_age_sq_sc, p_age_sc)))
dr_cmin # view full model list

sub_m1_incu<-subset(dr_cmin, delta<2) ##subsets models delta <2

nest_m1_incu <- subset(sub_m1_incu, !nested(.)) # removes nested models

# model averaging, this object will be used in graph creation
modavm1_incu<-model.avg(nest_m1_incu, delta<2, fit = TRUE) 


## @knitr us-brood-mod 

# run unscaled brooding model
co_ml_broodr<- lmer(no_hours_sqrt ~
                     sex + 
                     pers +
                     p_pers+
                     age_sq+
                     age +
                     p_age+
                     p_age_sq+
                     NumDays+
                     new_part+
                     ppb_ave_diff+
                     age:pers+
                     age_sq:pers+
                     age:ppb_ave_diff+
                     age_sq:ppb_ave_diff+
                     pers:ppb_ave_diff+
                     new_part:ppb_ave_diff+
                     p_age:p_pers_sc+
                     p_age_sq:p_pers+
                     p_age:ppb_ave_diff+
                     p_age_sq:ppb_ave_diff+
                     p_pers:ppb_ave_diff+
                     p_pers:sex +
                     pers:sex +
                     (1|cycle) + (1+ppb_ave_diff_sc|cycle_pair),
                   data = brood_new2, na.action=na.fail, REML=FALSE,
                   control=lmerControl (optimizer="optimx",optCtrl=list(method='nlminb')))

summary(co_ml_broodr)
hist(resid(co_ml_broodr))

#dredge creating model set 
# linking age variables to their equivalent quadratic version so they can only be discarded or retained together 
dr_cmbr_r<-dredge(co_ml_broodr, subset = (dc(age, age_sq) && 
                                         dc(p_age, p_age_sq) && 
                                         dc(age_sq, age) && 
                                         dc(p_age_sq, p_age)))

dr_cmbr_r # view full model list

sub_m1_broodr<-subset(dr_cmbr_r, delta<2) ##subsets models delta <2

nest_m1_broodr <- subset(sub_m1_broodr, !nested(.))# remove nested models

## model averaging, this object will provide the graph's regression lines
modavm1_broodr<-model.avg(nest_m1_broodr, fit = TRUE) 

## @knitr s-brood-mod

# run original brooding model
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

#dredge creating model set 
# linking age variables to their equivalent quadratic version so they can only be discarded or retained together 
dr_cmbr<-dredge(co_ml_brood, subset = (dc(age_sc, age_sq_sc) && 
                                         dc(p_age_sc, p_age_sq_sc) && 
                                         dc(age_sq_sc, age_sc) && 
                                         dc(p_age_sq_sc, p_age_sc)))

dr_cmbr # view full model list

sub_m1_brood<-subset(dr_cmbr, delta<2) ##subsets models delta <2

nest_m1_brood <- subset(sub_m1_brood, !nested(.))# remove nested models

 # model averaging, this object will be used in graph creation
modavm1_brood<-model.avg(nest_m1_brood, fit = TRUE)

## @knitr 1a-reg-line

#y axis = individual trip duration / no hours, x axis = scaled pers score 
#(to match other pers graphs), regression line taken from scaled model

# extract regression line
mA_in<-summary(modavm1_incu)
ppers_dat <- plot_model(mA_in, type = "pred", terms = c("p_pers_sc"))
ppers_dat<- data.frame(ppers_dat$data)

## knitr 1a-sqrt
# non-square root data for plotting
ppers_dat_unsqr<- data.frame(x=ppers_dat$x, predicted=ppers_dat$predicted^2)

## @knitr p-pers-incu

p_pers_graph_incu<-ggplot(aes(x = x, y = predicted), data = ppers_dat_unsqr) +# regression line for pers variable
  geom_line(size = 1.5, colour = "black")+
  geom_point(aes(x = p_pers_sc, y = no_hours), data = incu_new2) + 
  theme_bw(base_size =15) +
  xlab("Scaled Partner Boldness Score")+
  ylab("Individual trip duration (hrs)")+
  ylim (0, 500)+ 
  theme_classic()+
  theme(axis.text = element_text(size = 15))+
  theme(axis.title= element_text(size=18))

p_pers_graph_incu


## @knitr age-setup

# x axis = partner age, y axis = individual trip duration, incubation only, 
#regression line taken from unscaled model to match x axis 

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
  geom_point(aes(x = p_age, y = no_hours), data = incu_new2)+ # hides data points 
  theme_bw(base_size =15) +
  xlab("Partner age (years)")+
  ylab("Individual trip duration (hrs)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=c(10, 15, 20, 25, 30, 35, 40))+
  theme_classic()+
  theme(axis.text = element_text(size = 15))+
  theme(axis.title= element_text(size=18))


p_ageincu_graph


## @knitr pers-setup
# personality graphs set up

# X axis = boldness score focal female, focal male and partner, Y axis =individual trip duration/ no hours, brooding only
# regression line taken from scaled model

mA_brsq<-summary(modavm1_brood) #summary brooding model

# extract regression line for pers:sex, pers:sex model averaged coefficients not used
brDatpers <- plot_model(mA_brsq, type = "pred", terms = c("pers_sc", "sex")) 
brDatpers <- data.frame(brDatpers$data)

#extract regression line for p_pers
brDatp_pers <- plot_model(mA_brsq, type = "pred", terms = c("p_pers_sc")) 
brDatp_pers <- data.frame(brDatp_pers$data)

## @knitr pers-sq-root

# Compute the non-square-rooted pers data for plotting
brpers_unsqr<- data.frame(x=brDatpers$x, predicted=brDatpers$predicted^2,se=brDatpers$std.error, cih = brDatpers$conf.high^2, cil =brDatpers$conf.low^2, group = brDatpers$group, group_col =brDatpers$group_col)
brp_pers_unsqr<- data.frame(x=brDatp_pers$x, predicted=brDatp_pers$predicted^2, se=brDatp_pers$std.error, cih = brDatp_pers$conf.high^2, cil =brDatp_pers$conf.low^2)

## @knitr male-female-pers
# male and female data are subsetted
brpers_unsqr$group<- as.factor(brpers_unsqr$group)
brpers_unsqr_males<-subset(brpers_unsqr, group == "Male")
brpers_unsqr_females<-subset(brpers_unsqr, group == "Female")

## @knitr pers-graph-code

#separate graphs for three lines 
#p-pers
pers_graph_p_pers<-ggplot(aes(x = x, y = predicted), data = brp_pers_unsqr) +# regression line for pers variable
  geom_line(size = 1.5, colour = "black")+
  geom_ribbon (fill= "grey", alpha=0.3,aes(ymin=cil,ymax=cih))+
  geom_point(aes(x = p_pers_sc, y = no_hours), data = brood_new2) +  
  theme_bw(base_size =15) +
  xlab("Scaled Boldness Score")+
  ylab("Individual trip duration (hrs)")+
  ylim (0, 200)+ 
  theme_classic()+
  theme(axis.text = element_text(size = 15))+
  theme(axis.title= element_text(size=18))

pers_graph_p_pers


#females
pers_graph_f<-ggplot(aes(x = x, y = predicted), data = brpers_unsqr) +# regression line for pers variable
  geom_line(aes(x = x, y = predicted), data = brpers_unsqr_females, size =1.5, colour = "black") +# regression line for female variable
  geom_ribbon (fill= "grey", alpha=0.25,data = brpers_unsqr_females, aes(ymin=cil,ymax=cih))+
  geom_point(aes(x = pers_sc, y = no_hours), data = brood_new2) +  
  theme_bw(base_size =15) +
  xlab("Scaled Boldness Score")+
  ylab("Individual trip duration (hrs)")+
  ylim (0, 200)+ 
  theme_classic()+
  theme(axis.text = element_text(size = 15))+
  theme(axis.title= element_text(size=18))

pers_graph_f


#males
pers_graph_m<-ggplot(aes(x = x, y = predicted), data = brpers_unsqr) +# regression line for pers variable
  geom_line(aes(x = x, y = predicted), data = brpers_unsqr_males, size =1.5, colour = "black") +
  geom_ribbon (fill= "grey", alpha=0.25,data = brpers_unsqr_males, aes(ymin=cil,ymax=cih))+
  geom_point(aes(x = pers_sc, y = no_hours), data = brood_new2) + 
  theme_bw(base_size =15) +
  xlab("Scaled Boldness Score")+
  ylab("Individual trip duration (hrs)")+
  ylim (0, 200)+ 
  theme_classic()+
  theme(axis.text = element_text(size = 15))+
  theme(axis.title= element_text(size=18))

pers_graph_m


## @knitr co-setup
#  x axis = partner previous trip duration (deviation from average trip duration), y axis = individual trip duration/ no hours
#regression lines taken from unscaled models to match x axis

# getting regression lines from models

mA_inr<-summary(modavm1_incur) # summary incu model
mA_brsq<-summary(modavm1_broodr) #summary brooding model

inDat <- plot_model(mA_inr, type = "pred", terms = c("ppb_ave_diff")) # extract regression line for incubation, identify predictor variable of interest
inDat <- data.frame(inDat$data)

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
inDatr.unsqr<- data.frame(x=inDat$x, predicted=inDat$predicted^2)
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
  xlab("Partner's previous trip duration")+
  ylab("Individual trip duration (hrs)")+
  facet_wrap(.~breed_stage)+
  ylim (0, 500)+ 
  theme_classic()+
  theme(axis.text = element_text(size = 15))+
  theme(axis.title= element_text(size=18))

ppb_graph_all

## @knitr save-image

#save.image("WAAL_graph_final.RData")

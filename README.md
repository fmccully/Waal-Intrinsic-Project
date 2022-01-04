# Waal-Intrinsic-Project
# Code and data for Waal Intrinsic Coordination Study 
This repository contaisn the data, code and r markdown file for the project on intrinsic variables and their effects on parental care 
coordination in wandering albatrosses.

GENERAL INFORMATION: waal_dataset.csv

Dataset description:

Details on 871 foraging trips made by breeding wandering albatrosses (Diomedea exulans) between 2008-2014. Trip durations were gathered from salt water immersion loggers attached to 71 individuals. The foraging trips of these individuals’ partners were estimated from these data (see below). Intrinsic traits (sex, age, boldness, partnership tenure) of both partners are provided. 

Current use:
To test the effect of intrinsic variables (sex, age, boldness, partnership tenure) on individual trip duration and strength of coordinated parental care in wandering albatrosses.  

Date of data collection: 
16th Dec 2008- 11th April 2014

Geographic location of data collection: 
Crozet archipelago (46.8 °S, 51.8 °E) 

Information about funding sources that supported the collection of the data: 
Institut Polaire Français Paul Emile Victor (IPEV, programme 109) 
Terres Australes and Antarctique Françaises (TAAF) 
European Research Council Advanced Grant under the European Community's Seven Framework Program FP7/2007–2013 (grant agreement ERC-2012-ADG_20120314) 
The Prince Albert II de Monaco Foundation


DATA & FILE OVERVIEW

File List: 
The original data file (waal_dataset.csv)

The subsettted data files ready for analysis (incu_new2.csv, brood_new2.csv, total_df4.csv)

The r script for the cleaning and analysis of the data (Waal model R markdown script.R)

The r markdown script for the cleaning and analysis of the data (Waal Model script.Rmd)

The r markdown file for the cleaning and analysis of the data (Waal Intrinsic Variables and Parental Care Coordination Model Script.pdf). METHODOLOGICAL INFORMATION

Description of methods used for collection/generation of data: 

Intrinsic variables: this population has been monitored since the 1960s and so data on the sex, age and partnership histories were available for all individuals. 

Trip durations: 71 wandering albatrosses were tagged with saltwater immersion loggers between 2008-2014. The loggers distinguish between dry periods (land or flying) and wet periods (immersed in saltwater). 

Boldness:  Responses of incubating birds to an approaching human (from a 5m distance) were quantified using an ordinal scale from 0-5 (0 = no response; 1 = raises head; 2 = rises onto tarsus; 3 = vocalises 4 = stands up; 5 = vacates nest). Higher scores  = bolder birds.

Partner previous bout (part_prev_bout): the trip duration of each bird's partner immediately prior to their own foraging trip.


Methods for processing the data: 

Trip durations: Periods > 12 hours dry were considered to be nest attendance bouts. Periods in between these > 12 hour dry bouts were considered foraging trips. The length of these is the response variable individual trip duration (no_hours). Partner (unloggered birds) trip durations were estimated from the loggered birds nest attendance bouts. Incubation trips occurred between 16th Dec- Mid march (exact date varied depending on patterns for individual pairs). Brooding occurred between mid-march and April 11th each year. 

Boldness scores: Observation number, observer identity, and bird ID were fitted as fixed effects in a generalized linear model. Individual parameter estimates were  mean-centred at the population level (see Patrick et al., 2013 for a detailed description). This created a boldness score of each individual.


Instrument- or software-specific information needed to interpret the data: 
Data processed in R, packages used:
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


DATA-SPECIFIC INFORMATION FOR: waal_dataset.csv

Number of variables: 
26

Number of cases/rows: 
Total: 871
Incubation: 260
Brooding: 611

Variable List: 
X: row number
tag: focal bird ID number (numbered leg ring) 
p_tag: ID number (numbered leg ring) of focal bird’s partner
sex: sex of focal bird (M/F)
partner_type: if focal bird is logger (i.e. carried a tag) or a partner (did not carry a tag). Partner bird trip durations will be estimated from logger bird data. 
Age: age of focal bird. Unit = years
p_age: age of focal bird’s partner. Unit = years 
pers: Focal bird’s boldness score. Unit = unspecified scale generated from a generalized linear model. Higher scores = increased boldness.
p_pers: Boldness score of focal bird’s partner. Unit = unspecified scale generated from a generalized linear model. Higher scores = increased boldness. 
pair_ID: indexing variable, combination of ‘tag’ and ‘p_tag’ to identify observations related to individual pairs across whole dataset
cycle: breeding year. As one breeding cycle lasts 12 months Nov-Nov, the December of each breeding year is labelled as belonging to the next calendar year. I.e. Dec 2008 will be labelled in ‘cycle’ as 2009. 
cycle_tag: indexing variable, combination of ‘tag’ and ‘cycle’ to identify observations relating to specific individuals in specific years
cycle_pair: indexing variable, combination of ‘pair_ID’ and ‘cycle’ to identify observations relating to specific pairs in specific years
new_part: partnership history. Binary variable either 1 = new partnership that cycle or 0= established pair which had bred together for at least one year prior to the observed trip duration
NumDays: control variable for the passage of time throughout the breeding season. Number of days since start of breeding season (16th Dec) each year. Unit = days
day_date: calendar date (day only) at start time of trip duration
breed_act: calendar month at start time of trip duration
breed_stage: either incubation (incu) (16th Dec until that pair showed signs of reduced trip durations in mid-march) or brooding(brood) (mid-march – 11th April). Used to separate breeding stage subsets.
start_date_time: date and time at the start of the focal bird’s foraging trip
start_numeric_date_time: date and time at the start of the focal bird’s foraging trip in numeric form
end_date_time: date and time at the end of the focal bird’s foraging trip
end_numeric_date_time: date and time at the end of the focal bird’s foraging trip in numeric form
timeduration: original variable representing length of foraging tip (trip duration) of focal bird. Unit = seconds
no_hours: principle response variable in the study. Length of foraging tip (trip duration) of focal bird. Unit = hours 
no_days: Length of foraging tip (trip duration) of focal bird. Unit = days 
part_prev_bout: Partner’s previous trip duration. the trip duration of each bird's partner immediately prior to their own foraging trip. Required to create measures of coordination for further analysis. Unit = hours



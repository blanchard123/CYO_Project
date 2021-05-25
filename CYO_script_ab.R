
### Adam Blanchard
### Choose Your Own (CYO) Capstone Project
### HarvardX: PH125.9x - Capstone Project
### https://github.com/blanchard123/CYO_Project

###########################################################
############## Choose Your Own Project Code ###############
###########################################################

# Heart failure prediction data: 
# https://www.kaggle.com/andrewmvd/heart-failure-clinical-data



###########################################################
##################### LOAD LIBRARIES ######################
###########################################################

# Install and load libraries as needed 
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(caretEnsemble)) install.packages("caretEnsemble", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(gtsummary)) install.packages("gtsummary", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(markdown)) install.packages("markdown", repos = "http://cran.us.r-project.org")
if(!require(party)) install.packages("party", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(rcompanion)) install.packages("rcompanion", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rstatix)) install.packages("rstatix", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")

library(caret)
library(caretEnsemble)
library(corrplot)
library(data.table)
library(gam)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(gtsummary)
library(knitr)
library(kableExtra)
library(markdown)
library(party)
library(randomForest)
library(rcompanion)
library(rpart)
library(rstatix)
library(scales)
library(tidyverse)
library(dplyr)

# set digits to 6 and stop scientific notation 
options(digits = 3)
options(scipen = 999)



###########################################################
#################### DATA DESCRIPTION #####################
###########################################################

# create a dataframe describing the data
heart_variables <- 
  data.frame(Feature = c("Age", "Anaemia", "Creatinine Phosphokinase", "Diabetes",
                         "Ejection Fraction", "High Blood Pressure", "Platelets", 
                         "Serum Creatinine", "Serum Sodium", "Sex", "Smoking", 
                         "Time", "Death Event"),
             Description = c("Age of patients in years",
                             "Decrease in red blood cells",
                             "Level of CPK in the blood",
                             "Presence of diabetes",
                             "Percentage of blood leaving heart",
                             "Presence of hypertension",
                             "Level of platelets in the blood",
                             "Level of creatinine in the blood",
                             "Level of sodium in the blood",
                             "Biological sex - man or woman",
                             "Presence of smoking",
                             "Number of days to follow-up",
                             "Death of patient during follow-up"),
             Measurement = c("Numeric - years", "Boolean", "Numeric - mcg/L", "Boolean", 
                             "Boolean", "Numeric - percentage", "Numeric - kp/mL",
                             "Numeric - mg/dL", "Numeric - mEq/L", "Binary","Boolean", 
                             "Numeric - days", "Boolean"),
             Range = c("40 - 95", "0, 1", "23 - 7,861", "0, 1", "14 - 80", "0, 1",
                       "25.01 - 850.00", "0.50 - 9.40", "114 - 148", "0, 1", "0, 1", 
                       "4 - 285", "0, 1"))

# convert the dataframe to a table describing the data 
heart_variables %>% 
  kbl(caption = "Variable Description, Measurement, and Range", align = "llcc") %>%
  row_spec(0, bold = T) %>%
  column_spec(1, bold = T) %>%
  kable_classic(full_width = F) %>%
  kable_styling(latex_options = "hold_position", font_size = 10) %>%
  kable_styling(latex_options = "scale_down") %>%
  footnote(general = c("Adapted from Chicco & Jurman, 2020", 
                       "mcg/L = micrograms per liter; kp/mL = kiloplatelets/microliter; 
                       mEq/L = milliequivalents per litre"),
           general_title = "")


###########################################################
##################### DATA WRANGLING ######################
###########################################################

# Note: this process could take a minute

# data stored on my github account 
data_url <- "https://raw.githubusercontent.com/blanchard123/CYO_Project/main/heart_failure_clinical_records_dataset.csv"

# download the csv file from github
download.file(data_url, "heart_data.csv")

# read the file into R and create numeric and logical variables (as appropriate)
heart_data_l <- read_csv("heart_data.csv", col_types = cols(
  age = col_double(),
  anaemia = col_logical(),
  creatinine_phosphokinase = col_double(),
  diabetes = col_logical(),
  ejection_fraction = col_double(),
  high_blood_pressure = col_logical(),
  platelets = col_double(),
  serum_creatinine = col_double(),
  serum_sodium = col_double(),
  sex = col_logical(),
  smoking = col_logical(),
  time = col_integer(),
  DEATH_EVENT = col_logical()))

# create a separate file with numeric and factor variables (as appropriate)
# converting from logical to factor variables can create issues, so two datasets are created 
# read the file into R and correct the column types 
heart_data_f <- read_csv("heart_data.csv", col_types = cols(
  age = col_double(),
  anaemia = col_factor(),
  creatinine_phosphokinase = col_double(),
  diabetes = col_factor(),
  ejection_fraction = col_double(),
  high_blood_pressure = col_factor(),
  platelets = col_double(),
  serum_creatinine = col_double(),
  serum_sodium = col_double(),
  sex = col_factor(),
  smoking = col_factor(),
  time = col_integer(),
  DEATH_EVENT = col_factor()))

# add value labels to the factor variables 
# note only run once or will result in all NA to the selected variables 
heart_data_f$anaemia <- factor(heart_data_f$anaemia, levels = c(0,1), labels = c("No","Yes"))
heart_data_f$diabetes <- factor(heart_data_f$diabetes, levels = c(0,1), labels = c("No","Yes"))
heart_data_f$high_blood_pressure <- factor(heart_data_f$high_blood_pressure, levels = c(0,1), labels = c("No","Yes"))
heart_data_f$sex <- factor(heart_data_f$sex, levels = c(0,1), labels = c("Female","Male"))
heart_data_f$smoking <- factor(heart_data_f$smoking, levels = c(0,1), labels = c("No","Yes"))
heart_data_f$DEATH_EVENT <- factor(heart_data_f$DEATH_EVENT, levels = c(0,1), labels = c("No","Yes"))

# convert age to integer variable in both datasets
heart_data_f <- heart_data_f %>% mutate(age = as.integer(age))
heart_data_l <- heart_data_l %>% mutate(age = as.integer(age))

# remove uneccesary feature from environment 
rm(data_url)

# Save the datasets
save(heart_data_l, file = "heart_data_l.RData")
save(heart_data_f, file = "heart_data_f.RData")



###########################################################
##################### DATA INSPECTION #####################
###########################################################

# check for NA values in all variables
any(is.na(heart_data_f$age))
any(is.na(heart_data_f$anaemia))
any(is.na(heart_data_f$creatinine_phosphokinase))
any(is.na(heart_data_f$diabetes))
any(is.na(heart_data_f$ejection_fraction))
any(is.na(heart_data_f$high_blood_pressure))
any(is.na(heart_data_f$platelets))
any(is.na(heart_data_f$serum_creatinine))
any(is.na(heart_data_f$serum_sodium))
any(is.na(heart_data_f$sex))
any(is.na(heart_data_f$smoking))
any(is.na(heart_data_f$time))
any(is.na(heart_data_f$DEATH_EVENT))

# another check for complete data - identify rows woth any missing values
heart_data_f[!complete.cases(heart_data_f),]

# number of rows and columns
dim(heart_data_f)

# basic identification of data and variable types 
str(heart_data_l, strict.width="cut")
str(heart_data_f, strict.width="cut")

# examine the heart dataset as a table
as_tibble(heart_data_f) %>% 
  slice(1:10) %>% 
  kbl(caption = "Examination of the Heart Data Structure", align = "c", 
      col.names = c("Age", "Anaemia", "Creatinine Phosphokinase", "Diabetes", 
                    "Ejection Fraction", "High Blood Pressure", "Platelets", 
                    "Serum Creatinine", "Serum Sodium", "Sex", "Smoking", "Time", 
                    "Death Event")) %>%
  row_spec(0, bold = T) %>%
  kable_classic(full_width = F) %>%
  kable_styling(latex_options = "hold_position", font_size = 10) %>%
  kable_styling(latex_options = "scale_down")

# basic descriptive information about the variables 
summary(heart_data_f)



###########################################################
################# BASIC DATA EXPLORATION ##################
###########################################################


### basic frequency distributions of binary variables ###
### all plots stored as separate objects ###

# frequency distribution of anaemia
fd_anm <- heart_data_f %>%
  ggplot(aes(anaemia)) +
  geom_bar(color = "Black", fill = "#a6cee3") +
  geom_text(aes(label = percent((..count..)/sum(..count..))), 
            stat = "count", position = position_stack(vjust = 0.5)) +
  xlab("Anaemia") +
  theme_light() +
  theme(axis.title.x = element_text()) +
  theme(axis.title.y = element_blank()) +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# frequency distribution of diabetes
fd_db <- heart_data_f %>%
  ggplot(aes(diabetes)) +
  geom_bar(color = "Black", fill = "#a6cee3") +
  geom_text(aes(label = percent((..count..)/sum(..count..))), 
            stat = "count", position = position_stack(vjust = 0.5)) +
  xlab("Diabetes") +
  theme_light() +
  theme(axis.title.x = element_text()) +
  theme(axis.title.y = element_blank()) +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# frequency distribution of high blood pressure
fd_hbp <- heart_data_f %>%
  ggplot(aes(high_blood_pressure)) +
  geom_bar(color = "Black", fill = "#a6cee3") +
  geom_text(aes(label = percent((..count..)/sum(..count..))), 
            stat = "count", position = position_stack(vjust = 0.5)) +
  xlab("High Blood Pressure") +
  theme_light() +
  theme(axis.title.x = element_text()) +
  theme(axis.title.y = element_blank()) +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# frequency distribution of sex
fd_sex <- heart_data_f %>%
  ggplot(aes(sex)) +
  geom_bar(color = "Black", fill = "#a6cee3") +
  geom_text(aes(label = percent((..count..)/sum(..count..))), 
            stat = "count", position = position_stack(vjust = 0.5)) +
  xlab("Sex") +
  theme_light() +
  theme(axis.title.x = element_text()) +
  theme(axis.title.y = element_blank()) +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# frequency distribution of smoking
fd_smk <- heart_data_f %>%
  ggplot(aes(smoking)) +
  geom_bar(color = "Black", fill = "#a6cee3") +
  geom_text(aes(label = percent((..count..)/sum(..count..))), 
            stat = "count", position = position_stack(vjust = 0.5)) +
  xlab("Smoking") +
  theme_light() +
  theme(axis.title.x = element_text()) +
  theme(axis.title.y = element_blank()) +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# frequency distribution of death events 
fd_death <- heart_data_f %>%
  ggplot(aes(DEATH_EVENT)) +
  geom_bar(color = "Black", fill = "#a6cee3") +
  geom_text(aes(label = percent((..count..)/sum(..count..))), 
            stat = "count", position = position_stack(vjust = 0.5)) +
  xlab("Death") +
  theme_light() +
  theme(axis.title.x = element_text()) +
  theme(axis.title.y = element_blank()) +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# arrange plots together in a grid for presentation 
grid.arrange(fd_death, fd_anm, fd_db, fd_hbp, fd_sex, fd_smk,
             ncol = 2,
             top = "Figure 1: Frequency Distributions of the Dichotomous Variables",
             left = "Frequency")


### basic frequency distributions of continuous variables ###
### all plots stored as separate objects ###

# frequency distribution of age
fd_age <- heart_data_f %>%
  ggplot(aes(age)) +
  geom_histogram(bins = 20, color = "Black", fill = "#a6cee3") +
  geom_vline(xintercept = mean(heart_data_f$age), color = "black") +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  xlab("Age") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# frequency distribution of creatinine phosphokinase levels
fd_cp <- heart_data_f %>%
  ggplot(aes(creatinine_phosphokinase)) +
  geom_histogram(bins = 20, color = "Black", fill = "#a6cee3") +
  geom_vline(xintercept = mean(heart_data_f$creatinine_phosphokinase), color = "black") +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  scale_x_log10(labels = comma) +
  xlab("Creatinine Phosphokinase (log)") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# frequency distribution of ejection fraction levels
fd_ef <- heart_data_f %>%
  ggplot(aes(ejection_fraction)) +
  geom_histogram(bins = 20, color = "Black", fill = "#a6cee3") +
  geom_vline(xintercept = mean(heart_data_f$ejection_fraction), color = "black") +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  xlab("Ejection Fraction") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# frequency distribution of platelet levels
fd_pl <- heart_data_f %>%
  ggplot(aes(platelets)) +
  geom_histogram(bins = 20, color = "Black", fill = "#a6cee3") +
  geom_vline(xintercept = mean(heart_data_f$platelets), color = "black") +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  scale_x_continuous(labels = comma, breaks = pretty_breaks(n = 10)) +
  xlab("Platelets") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# frequency distribution of serum creatinine levels 
fd_sc <- heart_data_f %>%
  ggplot(aes(serum_creatinine)) +
  geom_histogram(bins = 20, color = "Black", fill = "#a6cee3") +
  geom_vline(xintercept = mean(heart_data_f$serum_creatinine), color = "black") +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  scale_x_log10() +
  xlab("Serum Creatinine (log)") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# frequency distribution of serum sodium levels 
fd_ss <- heart_data_f %>%
  ggplot(aes(serum_sodium)) +
  geom_histogram(bins = 20, color = "Black", fill = "#a6cee3") +
  geom_vline(xintercept = mean(heart_data_f$serum_sodium), color = "black") +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  xlab("Serum Sodium") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# frequency distribution of time levels 
fd_tm <- heart_data_f %>%
  ggplot(aes(time)) +
  geom_histogram(bins = 20, color = "Black", fill = "#a6cee3") +
  geom_vline(xintercept = mean(heart_data_f$time), color = "black") +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  xlab("Length of Follow-up") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# arrange plots together in a grid for presentation 
grid.arrange(fd_age, fd_cp, fd_ef, fd_pl, fd_sc, fd_ss, fd_tm,
             ncol = 2, layout_matrix = rbind(c(1,1,2,2), c(3,3,4,4), c(5,5,6,6), c(NA,7,7,NA)),
             top = "Figure 2: Frequency Distributions of the Continuous Variables",
             left = "Frequency")



### bar graphs by death event ###
### all plots stored as separate objects ###

# bar graph of anaemia by death event 
bg_anm <- heart_data_f %>%
  ggplot(aes(anaemia, fill = DEATH_EVENT)) +
  geom_bar(position = "fill", colour = "black") +
  geom_text(aes(label = ..count..), stat = "count", position = position_fill(.5)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_fill_brewer(name = "Death", palette = "Paired") +
  xlab("Anaemia") +
  theme_light() +
  theme(axis.title.x = element_text()) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# bar graph of diabetes by death event 
bg_db <- heart_data_f %>%
  ggplot(aes(diabetes, fill = DEATH_EVENT)) +
  geom_bar(position = "fill", colour = "black") +
  geom_text(aes(label = ..count..), stat = "count", position = position_fill(.5)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_fill_brewer(name = "Death", palette = "Paired") +
  xlab("Diabetes") +
  theme_light() +
  theme(axis.title.x = element_text()) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# bar graph of high blood pressure by death event 
bg_hbp <- heart_data_f %>%
  ggplot(aes(high_blood_pressure, fill = DEATH_EVENT)) +
  geom_bar(position = "fill", colour = "black") +
  geom_text(aes(label = ..count..), stat = "count", position = position_fill(.5)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_fill_brewer(name = "Death", palette = "Paired") +
  xlab("High Blood Pressure") +
  theme_light() +
  theme(axis.title.x = element_text()) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# bar graph of sex by death event 
bg_sex <- heart_data_f %>%
  ggplot(aes(sex, fill = DEATH_EVENT)) +
  geom_bar(position = "fill", colour = "black") +
  geom_text(aes(label = ..count..), stat = "count", position = position_fill(.5)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_fill_brewer(name = "Death", palette = "Paired") +
  xlab("Sex") +
  theme_light() +
  theme(axis.title.x = element_text()) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# bar graph of smoking by death event 
bg_smk <- heart_data_f %>%
  ggplot(aes(smoking, fill = DEATH_EVENT)) +
  geom_bar(position = "fill", colour = "black") +
  geom_text(aes(label = ..count..), stat = "count", position = position_fill(.5)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_fill_brewer(name = "Death", palette = "Paired") +
  xlab("Smoking") +
  theme_light() +
  theme(axis.title.x = element_text()) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# create a common legend for the plots
# need to create an entire dummy plot
bg_smk_legend <- heart_data_f %>%
  ggplot(aes(smoking, fill = DEATH_EVENT)) +
  geom_bar(position = "fill", colour = "black") +
  geom_text(aes(label = ..count..), stat = "count", position = position_fill(.5)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_fill_brewer(name = "Death", palette = "Paired") +
  xlab("Smoking") +
  theme_light() +
  theme(axis.title.x = element_text()) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "right") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# function to extract the legend from a plot
extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}

# extract the legend as an object 
shared_legend_3 <- extract_legend(bg_smk_legend)

# arrange plots together in a grid for presentation 
grid.arrange(bg_anm, bg_db, bg_hbp, bg_smk, bg_sex, shared_legend_3,
             ncol = 3, layout_matrix = rbind(c(1,1,2,2,3,3), c(NA,4,4,5,5,6)),
             top = "Figure 3: Bar Graphs of the Dichotomous Variables by Patient Death",
             left = "Proportion")


# table of descriptive statistics of the dichotomous variables by death event
heart_data_f %>% select(anaemia, high_blood_pressure, diabetes, sex, smoking, DEATH_EVENT) %>%
  tbl_summary(by = DEATH_EVENT,
              type = all_categorical() ~ "categorical",
              digits = all_categorical() ~ 2,
              label = list(anaemia ~ "Anaemia", high_blood_pressure ~ "High Blood Pressure",
                           diabetes ~ "Diabetes", sex ~ "Sex", smoking ~ "Smoking")) %>% 
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>%
  add_overall() %>%
  as_kable_extra(caption = "Descriptive Statistics for the Dichotomous Variables: Overall and by Death Event", align = "lcccc") %>%
  add_header_above(c("", "", "Death Event" = 3), bold =T) %>%
  row_spec(0, bold = T) %>%
  column_spec(1, bold = T) %>%
  kable_classic(full_width = F) %>%
  kable_styling(latex_options = "hold_position", font_size = 10) %>%
  kable_styling(latex_options = "scale_down")



### boxplots of continuous variables by death event ###
### all plots stored as separate objects ###

# boxplot of age by death event
bp_age <- heart_data_f %>%
  ggplot(aes(DEATH_EVENT, age, fill = DEATH_EVENT)) +
  geom_boxplot(color = "Black") +
  geom_point(position = position_jitter(h = 0.25, w = 0.25), size = 2.5, alpha = 0.5) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_fill_brewer(name = "Death", palette = "Paired") +
  ylab("Age") +
  theme_light() +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(angle = 90, vjust = 2)) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.5,0.25,0.5), "cm"))

# boxplot of creatinine phosphokinase levels by death event
bp_cp <- heart_data_f %>%
  ggplot(aes(DEATH_EVENT, creatinine_phosphokinase, fill = DEATH_EVENT)) +
  geom_boxplot(color = "Black") +
  geom_point(position = position_jitter(h = 0.25, w = 0.25), size = 2.5, alpha = 0.5) +
  scale_y_log10(labels = comma) +
  scale_fill_brewer(name = "Death", palette = "Paired") +
  ylab("Creatinine Phosph. (log)") +
  theme_light() +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(angle = 90, vjust = 2)) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.5,0.25,0.5), "cm"))

# boxplot of ejection fraction levels by death event 
bp_ef <- heart_data_f %>%
  ggplot(aes(DEATH_EVENT, ejection_fraction, fill = DEATH_EVENT)) +
  geom_boxplot(color = "Black") +
  geom_point(position = position_jitter(h = 0.25, w = 0.25), size = 2.5, alpha = 0.5) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_fill_brewer(name = "Death", palette = "Paired") +
  ylab("Ejection Fraction") +
  theme_light() +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(angle = 90, vjust = 2)) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.5,0.25,0.5), "cm"))

# boxplot of platelet levels by death event 
bp_pl <- heart_data_f %>%
  ggplot(aes(DEATH_EVENT, platelets, fill = DEATH_EVENT)) +
  geom_boxplot(color = "Black") +
  geom_point(position = position_jitter(h = 0.25, w = 0.25), size = 2.5, alpha = 0.5) +
  scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 10)) +
  scale_fill_brewer(name = "Death", palette = "Paired") +
  ylab("Platelets") +
  theme_light() +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(angle = 90, vjust = 2)) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.5,0.25,0.5), "cm"))

# boxplot of serum creatinine levels by death event
bp_sc <- heart_data_f %>%
  ggplot(aes(DEATH_EVENT, serum_creatinine, fill = DEATH_EVENT)) +
  geom_boxplot(color = "Black") +
  geom_point(position = position_jitter(h = 0.25, w = 0.25), size = 2.5, alpha = 0.5) +
  scale_y_log10() +
  scale_fill_brewer(name = "Death", palette = "Paired") +
  ylab("Serum Creatinine (log)") +
  theme_light() +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(angle = 90, vjust = 2)) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.5,0.25,0.5), "cm"))

# boxplot of serum sodium levels by death event
bp_ss <- heart_data_f %>%
  ggplot(aes(DEATH_EVENT, serum_sodium, fill = DEATH_EVENT)) +
  geom_boxplot(color = "Black") +
  geom_point(position = position_jitter(h = 0.25, w = 0.25), size = 2.5, alpha = 0.5) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_fill_brewer(name = "Death", palette = "Paired") +
  ylab("Serum Sodium") +
  theme_light() +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(angle = 90, vjust = 2)) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.5,0.25,0.5), "cm"))

# boxplot of time levels by death event
bp_tm <- heart_data_f %>%
  ggplot(aes(DEATH_EVENT, time, fill = DEATH_EVENT)) +
  geom_boxplot(color = "Black") +
  geom_point(position = position_jitter(h = 0.25, w = 0.25), size = 2.5, alpha = 0.5) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_fill_brewer(name = "Death", palette = "Paired") +
  xlab("Death Event") +
  ylab("Length of Follow-up") +
  theme_light() +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(angle = 90, vjust = 2)) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.5,0.25,0.5), "cm"))

# create a common legend for the plots
# need to create an entire dummy plot
bp_tm_legend <- heart_data_f %>%
  ggplot(aes(DEATH_EVENT, time, fill = DEATH_EVENT)) +
  geom_boxplot(color = "Black") +
  geom_point(position = position_jitter(h = 0.25, w = 0.25), size = 2.5, alpha = 0.5) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_fill_brewer(name = "Death", palette = "Paired") +
  xlab("Death Event") +
  ylab("Length of Follow-up") +
  theme_light() +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(angle = 90, vjust = 2)) +
  theme(legend.position = "right") +
  theme(plot.margin = unit(c(0.5,0.5,0.25,0.5), "cm"))

# extract the legend as an object 
shared_legend_4 <- extract_legend(bp_tm_legend)

# arrange plots together in a grid for presentation 
grid.arrange(bp_age, bp_cp, bp_ef, bp_pl, bp_sc, bp_ss, bp_tm, shared_legend_4,
             ncol = 3, layout_matrix = rbind(c(1,2,3), c(4,5,6), c(NA,7,8)), 
             top = "Figure 4: Boxplots of the Continuous Variables by Patient Death")


# table of descriptive statistics of the continuous variables by death event
heart_data_f %>% select(age, creatinine_phosphokinase, ejection_fraction, platelets, 
                        serum_creatinine, serum_sodium, time, DEATH_EVENT) %>%
  tbl_summary(by = DEATH_EVENT,
              type = all_continuous() ~ "continuous2",
              statistic = all_continuous() ~  c("{mean} ({sd})", "{median} ({p25}, {p75})"), 
              digits = all_continuous() ~ 2,
              label = list(age ~ "Age", creatinine_phosphokinase ~ "Creatinine Phosphokinase",
                           ejection_fraction ~ "Ejection Fraction", platelets ~ "Platelets", 
                           serum_creatinine ~ "Serum Creatinine", serum_sodium ~ "Serum Sodium", 
                           time ~ "Length of Follow-up")) %>% 
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>%
  add_overall() %>%
  modify_footnote(all_stat_cols() ~ "Mean (SD) or Median (IQR)") %>%
  as_kable_extra(caption = "Descriptive Statistics for the Continuous Variables: Overall and by Death Event", align = "lcccc") %>%
  add_header_above(c("", "", "Death Event" = 3), bold =T) %>%
  row_spec(0, bold = T) %>%
  column_spec(1, bold = T) %>%
  kable_classic(full_width = F) %>%
  kable_styling(latex_options = "hold_position", font_size = 10) %>%
  kable_styling(latex_options = "scale_down")



###########################################################
################## INFERENTIAL ANALYSIS ###################
###########################################################

### some analyses already presented in tables above ###

# more details about the chi-square tests presented in table above
chisq_test(heart_data_f$anaemia, heart_data_f$DEATH_EVENT)
chisq_test(heart_data_f$high_blood_pressure, heart_data_f$DEATH_EVENT)
chisq_test(heart_data_f$diabetes, heart_data_f$DEATH_EVENT)
chisq_test(heart_data_f$sex, heart_data_f$DEATH_EVENT)
chisq_test(heart_data_f$smoking, heart_data_f$DEATH_EVENT)

# more details about the Mann-Whitney U tests in table above 
wilcox.test(heart_data_f$age ~ heart_data_f$DEATH_EVENT) 
wilcox.test(heart_data_f$creatinine_phosphokinase ~ heart_data_f$DEATH_EVENT) 
wilcox.test(heart_data_f$ejection_fraction ~ heart_data_f$DEATH_EVENT) 
wilcox.test(heart_data_f$platelets ~ heart_data_f$DEATH_EVENT) 
wilcox.test(heart_data_f$serum_creatinine ~ heart_data_f$DEATH_EVENT) 
wilcox.test(heart_data_f$serum_sodium ~ heart_data_f$DEATH_EVENT) 
wilcox.test(heart_data_f$time ~ heart_data_f$DEATH_EVENT) 

# alternative analysis (to the Mann-Whitney) using t-tests
t.test(age ~ DEATH_EVENT, data = heart_data_f)
t.test(creatinine_phosphokinase ~ DEATH_EVENT, data = heart_data_f)
t.test(ejection_fraction ~ DEATH_EVENT, data = heart_data_f)
t.test(platelets ~ DEATH_EVENT, data = heart_data_f)
t.test(serum_creatinine ~ DEATH_EVENT, data = heart_data_f)
t.test(serum_sodium ~ DEATH_EVENT, data = heart_data_f)
t.test(time ~ DEATH_EVENT, data = heart_data_f)


### correlations ###

# create correlation matrix 
cmat <- cor(heart_data_l)
colnames(cmat) <- c("Age", "Anaemia", "Creatinine Phosphokinase", "Diabetes",
                    "Ejection Fraction", "High Blood Pressure", "Platelets", 
                    "Serum Creatinine", "Serum Sodium", "Sex", "Smoking", 
                    "Time", "Death Event")
rownames(cmat) <- c("Age", "Anaemia", "Creatinine Phosphokinase", "Diabetes",
                    "Ejection Fraction", "High Blood Pressure", "Platelets", 
                    "Serum Creatinine", "Serum Sodium", "Sex", "Smoking", 
                    "Time", "Death Event")

# create correlation test data
res1 <- cor.mtest(heart_data_l, conf.level = 0.95)

# create heatmap of correlation matrix between all variables with extras
corrplot::corrplot(cmat,
                   type = "lower",
                   method = "square",
                   tl.col = "black",
                   tl.cex = 0.7,
                   title = "Figure 5: Correlation Matrix",
                   p.mat = res1$p,
                   insig = "label_sig",
                   sig.level = c(.001, .01, .05),
                   pch.cex = 0.9, 
                   pch.col = "white",
                   mar = c(1,1,3,1))

# correlations between death event and other variables - dataframe
correlations <- cor(heart_data_l, heart_data_l$DEATH_EVENT)
r_square <- (correlations)^2
temp_data <- data.frame(r_square = r_square, cor = correlations, p = res1$p[,13])
temp_data <- temp_data[-c(13), ]
rownames(temp_data) <- c("Age", "Anaemia", "Creatinine Phosphokinase", "Diabetes",
                         "Ejection Fraction", "High Blood Pressure", "Platelets", 
                         "Serum Creatinine", "Serum Sodium", "Sex", "Smoking", "Time")

# create table of correlations data
temp_data[order(temp_data$r_square, decreasing = T),] %>%
  kbl(caption = "Correlations with Death Event", 
      col.names = c("r squared", "r", "p-value"), 
      align = "lccc",
      digits = 3) %>%
  row_spec(0, bold = T) %>%
  column_spec(1, bold = T) %>%
  kable_classic(full_width = F) %>%
  kable_styling(latex_options = "hold_position", font_size = 10)


### logistic regressions ###

# logistic regression with all features including time
fit_glm <- glm(DEATH_EVENT ~ ., data = heart_data_l, family = "binomial")

# results of logistic regression
summary(fit_glm)

# more results of logistic regression - overall model fit
fit_glm_r2 <- nagelkerke(fit_glm)$Pseudo.R.squared 
colnames(fit_glm_r2) <- c("All Features")

fit_glm_r2 %>%
  kbl(caption = "Logistic Regression - Overall Model Fit", align = "c", col.names = "Pseudo R Squared") %>%
  row_spec(0, bold = T) %>%
  column_spec(1, bold = T) %>%
  kable_classic(full_width = F) %>%
  kable_styling(latex_options = "hold_position", font_size = 10) %>%
  kable_styling(latex_options = "scale_down")

# more results of logistic regression - coefficients table 
fit_glm_co <- summary(fit_glm)$coefficients 
rownames(fit_glm_co) <- c("(Intercept)", "Age", "Anaemia", "Creatinine Phosphokinase", "Diabetes",
                          "Ejection Fraction", "High Blood Pressure", "Platelets", 
                          "Serum Creatinine", "Serum Sodium", "Sex", "Smoking", "Time")

fit_glm_co %>%
  kbl(caption = "Logistic Regression Coefficients - All Features", 
      align = "lccc", col.names = c("Estimate", "Std. Error", "Z", "p")) %>%
  row_spec(0, bold = T) %>%
  row_spec(2, bold = T) %>%
  row_spec(6, bold = T) %>%
  row_spec(9, bold = T) %>%
  row_spec(13, bold = T) %>%
  kable_classic(full_width = F) %>%
  kable_styling(latex_options = "hold_position", font_size = 10) %>%
  kable_styling(latex_options = "scale_down")

# more results of logistic regression - odds ratios 
exp(coef(fit_glm))


# logistic regression without the TIME variable 
fit_glm2 <- glm(DEATH_EVENT ~ age + anaemia + creatinine_phosphokinase + diabetes +
                  ejection_fraction +  high_blood_pressure + platelets + serum_creatinine + 
                  serum_sodium + sex + smoking, data = heart_data_l)

# results of logistic regression
summary(fit_glm2)

# more results of logistic regression - overall model fit - comparing both models 
fit_glm_r2 <- fit_glm_r2 %>% cbind(nagelkerke(fit_glm2)$Pseudo.R.squared)
colnames(fit_glm_r2) <- c("All Features", "No Time Feature")

fit_glm_r2 %>%
  kbl(caption = "Logistic Regression - Overall Model Fit", align = "cc") %>%
  row_spec(0, bold = T) %>%
  column_spec(1, bold = T) %>%
  kable_classic(full_width = F) %>%
  kable_styling(latex_options = "hold_position", font_size = 10) %>%
  kable_styling(latex_options = "scale_down")


# more results of logistic regression - coefficients table 
fit_glm_co2 <- summary(fit_glm2)$coefficients 
rownames(fit_glm_co2) <- c("(Intercept)", "Age", "Anaemia", "Creatinine Phosphokinase", "Diabetes",
                           "Ejection Fraction", "High Blood Pressure", "Platelets", 
                           "Serum Creatinine", "Serum Sodium", "Sex", "Smoking")

fit_glm_co2 %>%
  kbl(caption = "Logistic Regression Coefficients - No Time Feature", 
      align = "lccc", col.names = c("Estimate", "Std. Error", "Z", "p")) %>%
  row_spec(0, bold = T) %>%
  row_spec(2, bold = T) %>%
  row_spec(6, bold = T) %>%
  row_spec(9, bold = T) %>%
  kable_classic(full_width = F) %>%
  kable_styling(latex_options = "hold_position", font_size = 10) %>%
  kable_styling(latex_options = "scale_down")

# more results of logistic regression - odds ratios 
exp(coef(fit_glm2))




###########################################################
################### MODELING APPROACHES ###################
###########################################################

########## Data preparation for algorithm training ########## 

# partition the heart dataset into training and test datasets


# partition the heart dataset into training and test datasets
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = heart_data_f$DEATH_EVENT, 
                                  times = 1, p = 0.2, list = FALSE)

heart_train <- heart_data_f[-test_index,]
heart_test <- heart_data_f[test_index,]

# save the training and test datasets
save(heart_train, file = "heart_train.RData")
save(heart_test, file = "heart_test.RData")

# examine frequency of outcome in both datasets 
table(heart_train$DEATH_EVENT)
table(heart_test$DEATH_EVENT)

# create data frame describing the accuracy metrics
model_acc <- 
  data.frame(Feature = c("Accuracy", "Kappa", "Sensitivity", "Specificity", "PPV", "NPV", "Precision", "Recall", 
                         "F1", "Prevalence", "Detection Rate", "Detection Prevalence", "Balanced Accuracy"),
             Description = c("Proportion of true positives and true negatives over all instances",
                             "Measure of agreement accounting for random chance*",
                             "Proportion of true positives over actual positives",
                             "Proportion of true negatives over actual negatives",
                             "Proportion of true positives over predicted positives*",
                             "Proportion of true negatives over predicted negatives*",
                             "Proportion of true positives over predicted positives",
                             "Proportion of true positives over actual positives",
                             "Harmonic average of precision and recall*",
                             "Proportion of actual positives over total",
                             "Proportion of true positives over total",
                             "Proportion of predicted positives over total",
                             "(sensitivity + specificity)/2"))

# convert the dataframe to a table describing the accuracy metrics
model_acc %>% 
  kbl(caption = "Model Accuracy Metrics", align = "ll") %>%
  row_spec(0, bold = T) %>%
  column_spec(1, bold = T) %>%
  kable_classic(full_width = F) %>%
  kable_styling(latex_options = "hold_position", font_size = 10) %>%
  kable_styling(latex_options = "scale_down") %>%
  footnote(general = c("Adapted from Irizarry (2019) and the caret package description."),
           symbol = c("These metrics are calculated using more complex definitions in the caret package."),
           general_title = "")


# examine information about the models - cforest
getModelInfo("cforest")
modelLookup("cforest")

# examine information about the models - knn
getModelInfo("knn")
modelLookup("knn")

# examine information about the models - knn
getModelInfo("glm")
modelLookup("glm")

# examine information about the models - gamLoess
getModelInfo("gamLoess")
modelLookup("gamLoess")

# examine information about the models - gamLoess
getModelInfo("rf")
modelLookup("rf")

# examine information about the models - gamLoess
getModelInfo("rpart")
modelLookup("rpart")


# set the seed for reproducibility 
set.seed(1, sample.kind = "Rounding")

# set cross-validation with 100 samples 
my_control <- trainControl(method = "cv", number = 100, p = .9,
                           savePredictions = "all",
                           classProbs = TRUE,
                           allowParallel = TRUE,
                           index = createResample(heart_train$DEATH_EVENT, 10))


# set the seed for reproducibility 
set.seed(1, sample.kind = "Rounding")

# train multiple models at the same time - all features 
train_models <- caretList(DEATH_EVENT ~ age + anaemia + creatinine_phosphokinase + diabetes +
                            ejection_fraction +  high_blood_pressure + platelets + serum_creatinine + 
                            serum_sodium + sex + smoking,
                          data = heart_train,
                          trControl = my_control,
                          methodList = c("cforest", "glm", "knn", "gamLoess", "rf", "rpart"),
                          continue_on_fail = FALSE, 
                          preProcess = c("center", "scale"))


# set the seed for reproducibility 
set.seed(1, sample.kind = "Rounding")

# train multiple models at the same time - select features (age, ejection fraction, and serum creatinine)
train_models2 <- caretList(DEATH_EVENT ~ age + ejection_fraction + serum_creatinine,
                           data = heart_train,
                           trControl = my_control,
                           methodList = c("cforest", "glm", "knn", "gamLoess", "rf", "rpart"),
                           continue_on_fail = FALSE, 
                           preProcess = c("center", "scale"))


# report model results in the training dataset - cforest
train_models$cforest$results
train_models$cforest$bestTune

# report model results in the training dataset - glm
train_models$glm$results

# report model results in the training dataset - knn
train_models$knn$results
train_models$knn$bestTune

# report model results in the training dataset - gamLoess
train_models$gamLoess$results

# report model results in the training dataset - rf
train_models$rf$results
train_models$rf$bestTune

# report model results in the training dataset - rpart
train_models$rpart$results
train_models$rpart$bestTune


# plot the accuracy of the models in the training set cross-validation - all features
resamples <- resamples(train_models)
dotplot(resamples, metric = "Accuracy", 
        main = "Figure 6: Accuracy across Models - All Features (Cross-Validation)") 

# plot the accuracy of the models in the training set cross-validation - select features 
resamples2 <- resamples(train_models2)
dotplot(resamples2, metric = "Accuracy", 
        main = "Figure 8: Accuracy across Models - Select Features (Cross-Validation)") 


# dataframe of results of all models in the training cross-validation
train_results <- data.frame(
  Model = c("cForest", "GLM", "KNN", "Loess", "RF", "rpart"),
  Accuracy1 = c(max(train_models$cforest$results$Accuracy), 
                max(train_models$glm$results$Accuracy),
                max(train_models$knn$results$Accuracy),
                max(train_models$gamLoess$results$Accuracy), 
                max(train_models$rf$results$Accuracy), 
                max(train_models$rpart$results$Accuracy)),
  AccuracySD1 = c(min(train_models$cforest$results$AccuracySD), 
                  min(train_models$glm$results$AccuracySD),
                  min(train_models$knn$results$AccuracySD),
                  min(train_models$gamLoess$results$AccuracySD), 
                  min(train_models$rf$results$AccuracySD), 
                  min(train_models$rpart$results$AccuracySD)),
  Kappa1 = c(max(train_models$cforest$results$Kappa), 
             max(train_models$glm$results$Kappa),
             max(train_models$knn$results$Kappa),
             max(train_models$gamLoess$results$Kappa), 
             max(train_models$rf$results$Kappa), 
             max(train_models$rpart$results$Kappa)),
  KappaSD1 =  c(min(train_models$cforest$results$KappaSD), 
                min(train_models$glm$results$KappaSD),
                min(train_models$knn$results$KappaSD),
                min(train_models$gamLoess$results$KappaSD), 
                min(train_models$rf$results$KappaSD), 
                min(train_models$rpart$results$KappaSD)),
  Accuracy2 = c(max(train_models2$cforest$results$Accuracy), 
                max(train_models2$glm$results$Accuracy),
                max(train_models2$knn$results$Accuracy),
                max(train_models2$gamLoess$results$Accuracy), 
                max(train_models2$rf$results$Accuracy), 
                max(train_models2$rpart$results$Accuracy)),
  AccuracySD2 = c(min(train_models2$cforest$results$AccuracySD), 
                  min(train_models2$glm$results$AccuracySD),
                  min(train_models2$knn$results$AccuracySD),
                  min(train_models2$gamLoess$results$AccuracySD), 
                  min(train_models2$rf$results$AccuracySD), 
                  min(train_models2$rpart$results$AccuracySD)),
  Kappa2 = c(max(train_models2$cforest$results$Kappa), 
             max(train_models2$glm$results$Kappa),
             max(train_models2$knn$results$Kappa),
             max(train_models2$gamLoess$results$Kappa), 
             max(train_models2$rf$results$Kappa), 
             max(train_models2$rpart$results$Kappa)),
  KappaSD2 =  c(min(train_models2$cforest$results$KappaSD), 
                min(train_models2$glm$results$KappaSD),
                min(train_models2$knn$results$KappaSD),
                min(train_models2$gamLoess$results$KappaSD), 
                min(train_models2$rf$results$KappaSD), 
                min(train_models2$rpart$results$KappaSD)))

# table of accruacy and kappa in the cross-validation - all models 
train_results %>%
  kbl(caption = "Accuracy across Models (Cross-Validation)", 
      align = "lclclclcl", col.names = c("Model", "Accuracy", "(SD)", "Kappa", "(SD)", "Accuracy", "(SD)", "Kappa", "(SD)")) %>%
  add_header_above(c("", "All Features" = 4, "Select Features" = 4), bold =T) %>%
  row_spec(0, bold = T) %>%
  column_spec(1, bold = T) %>%
  kable_classic(full_width = F) %>%
  kable_styling(latex_options = "hold_position", font_size = 10) %>%
  kable_styling(latex_options = "scale_down")

# plot of tuning parameters - cforest 
tuning_cf <- ggplot(train_models$cforest, highlight = TRUE) +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  xlab("Number of Randomly Selected Predictors") +
  ylab("Accuracy in Cross-Validation") +
  ggtitle("cforest") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_text(vjust = 2)) +
  theme(plot.title = element_text(size = 12, vjust = 2, hjust = 0.5)) +
  theme(plot.margin = unit(c(0.5,0.25,0.5,0.5), "cm"))

# plot of tuning parameters - knn 
tuning_knn <- ggplot(train_models$knn, highlight = TRUE) +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  xlab("Number of Neighbours") +
  ylab("Accuracy in Cross-Validation") +
  ggtitle("KNN") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_text(vjust = 2)) +
  theme(plot.title = element_text(size = 12, vjust = 2, hjust = 0.5)) +
  theme(plot.margin = unit(c(0.5,0.25,0.5,0.5), "cm"))

# plot of tuning parameters - rf 
tuning_rf <- ggplot(train_models$rf, highlight = TRUE) +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  xlab("Number of Randomly Selected Predictors") +
  ylab("Accuracy in Cross-Validation") +
  ggtitle("Random Forest") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_text(vjust = 2)) +
  theme(plot.title = element_text(size = 12, vjust = 2, hjust = 0.5)) +
  theme(plot.margin = unit(c(0.5,0.25,0.5,0.5), "cm"))

# plot of tuning parameters - rpart
tuning_rpart <- ggplot(train_models$rpart, highlight = TRUE) +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  xlab("Number of Randomly Selected Predictors") +
  ylab("Accuracy in Cross-Validation") +
  ggtitle("rpart") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_text(vjust = 2)) +
  theme(plot.title = element_text(size = 12, vjust = 2, hjust = 0.5)) +
  theme(plot.margin = unit(c(0.5,0.25,0.5,0.5), "cm"))

# arrange tuning plots together in a grid for presentation 
grid.arrange(tuning_cf, tuning_knn, tuning_rf, tuning_rpart,
             ncol = 2, 
             top = "Figure A8: Tuning Parameters across Models - All Features")

# plot variable importance for each relevant model 
imp1 <- plot(varImp(train_models$cforest), xlab = "cforest")
imp2 <- plot(varImp(train_models$glm), xlab = "GLM")
imp3 <- plot(varImp(train_models$gamLoess), xlab = "gamLoess")
imp4 <- plot(varImp(train_models$rf), xlab = "Random Forest")
imp5 <- plot(varImp(train_models$rpart), xlab = "rpart")

# arrange variable importance plots together in a grid for presentation 
grid.arrange(imp1, imp2, imp3, imp4, imp5,
             ncol = 2, layout_matrix = rbind(c(1,1,2,2), c(3,3,4,4), c(NA,5,5,NA)), 
             top = "Figure 7: Variable Importance across Models (Cross-Validation)")




###########################################################
################## RESULTS IN TEST DATASET ################
###########################################################

### using all features 
# predict in the test datatset - cforest
pred_cforest <- predict(train_models$cforest, heart_test, type = "raw")

# predict in the test datatset - loess
pred_gamLoess <- predict(train_models$gamLoess, heart_test, type = "raw")

# predict in the test datatset - random forest
pred_rf <- predict(train_models$rf, heart_test, type = "raw")

# predict in the test datatset - random forest
pred_rpart <- predict(train_models$rpart, heart_test, type = "raw")

### using select features 
# predict in the test datatset - cforest
pred_cforest2 <- predict(train_models2$cforest, heart_test, type = "raw")

# predict in the test datatset - loess
pred_gamLoess2 <- predict(train_models2$gamLoess, heart_test, type = "raw")

# predict in the test datatset - random forest
pred_rf2 <- predict(train_models2$rf, heart_test, type = "raw")

# predict in the test datatset - random forest
pred_rpart2 <- predict(train_models2$rpart, heart_test, type = "raw")

# examine the performance of the models in the test set
mat_results <- as.data.frame(confusionMatrix(pred_cforest, heart_test$DEATH_EVENT, positive = "Yes")$byClass)
names(mat_results)[1] <- "cForest"
mat_results <- mat_results %>% bind_cols(confusionMatrix(pred_gamLoess, heart_test$DEATH_EVENT, positive = "Yes")$byClass)
names(mat_results)[2] <- "Loess"
mat_results <- mat_results %>% bind_cols(confusionMatrix(pred_rf, heart_test$DEATH_EVENT, positive = "Yes")$byClass)
names(mat_results)[3] <- "RF"
mat_results <- mat_results %>% bind_cols(confusionMatrix(pred_rpart, heart_test$DEATH_EVENT, positive = "Yes")$byClass)
names(mat_results)[4] <- "rpart"
mat_results <- mat_results %>% bind_cols(confusionMatrix(pred_cforest2, heart_test$DEATH_EVENT, positive = "Yes")$byClass)
names(mat_results)[5] <- "cForest2"
mat_results <- mat_results %>% bind_cols(confusionMatrix(pred_gamLoess2, heart_test$DEATH_EVENT, positive = "Yes")$byClass)
names(mat_results)[6] <- "Loess2"
mat_results <- mat_results %>% bind_cols(confusionMatrix(pred_rf2, heart_test$DEATH_EVENT, positive = "Yes")$byClass)
names(mat_results)[7] <- "RF2"
mat_results <- mat_results %>% bind_cols(confusionMatrix(pred_rpart2, heart_test$DEATH_EVENT, positive = "Yes")$byClass)
names(mat_results)[8] <- "rpart2"

# examine results in a table 
mat_results %>%
  kbl(caption = "Model Results in the Test Dataset", align = "lcccccccc",
      col.names = c("cForest", "Loess", "RF", "rpart", "cForest", "Loess", "RF", "rpart")) %>%
  add_header_above(c("", "All Features" = 4, "Select Features" = 4), bold =T) %>%
  row_spec(0, bold = T) %>%
  column_spec(1, bold = T) %>%
  kable_classic(full_width = F) %>%
  kable_styling(latex_options = "hold_position", font_size = 10) %>%
  kable_styling(latex_options = "scale_down")




###########################################################
######################## APPENDIX A #######################
###########################################################

### SUPPLEMENTAL FIGURES AND TABLES ###

### examine continuous variables across ANAEMIA ###
### all plots stored as separate objects ###

# plot of age across anaemia by death event 
pp_anm_age <- heart_data_f %>%
  ggplot(aes(age, anaemia, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Age") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of creatinine phosphokinase levels across anaemia by death event 
pp_anm_cp <- heart_data_f %>%
  ggplot(aes(creatinine_phosphokinase, anaemia, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_log10(labels = comma) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Creatinine Phosphokinase (log)") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of ejection fraction levels across anaemia by death event 
pp_anm_ef <- heart_data_f %>%
  ggplot(aes(ejection_fraction, anaemia, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Ejection Fraction") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of platelet levels across anaemia by death event 
pp_anm_pl <- heart_data_f %>%
  ggplot(aes(platelets, anaemia, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_continuous(labels = comma, breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Platelets") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of serum creatinine levels across anaemia by death event 
pp_anm_sc <- heart_data_f %>%
  ggplot(aes(serum_creatinine, anaemia, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_log10() +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Serum Creatinine (log)") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of serum sodium levels across anaemia by death event 
pp_anm_ss <- heart_data_f %>%
  ggplot(aes(serum_sodium, anaemia, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Serum Sodium") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of time across anaemia by death event 
pp_anm_tm <- heart_data_f %>%
  ggplot(aes(time, anaemia, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Length of Follow-up") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# create a common legend for the plots
# need to create an entire dummy plot
pp_anm_tm_legend <- heart_data_f %>%
  ggplot(aes(time, anaemia, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 3, alpha = 0.75) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  xlab("Length of Follow-up") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -3)) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "right") +
  theme(plot.margin = unit(c(0.5,0.25,0.5,0.25), "cm"))

# extract the legend as an object 
shared_legend_5 <- extract_legend(pp_anm_tm_legend)

# arrange plots together in a grid for presentation 
grid.arrange(pp_anm_age, pp_anm_cp, pp_anm_ef, pp_anm_pl, pp_anm_sc, pp_anm_ss, pp_anm_tm, shared_legend_5,
             ncol = 2, layout_matrix = rbind(c(1,1,2,2), c(3,3,4,4), c(5,5,6,6), c(NA,7,7,8)),
             top = "Figure A1: Plots of the Continuous Variables by Anaemia and Patient Death",
             left = "Anaemia")



### examine continuous variables across DIABETES ###
### all plots stored as separate objects ###

# plot of age across diabetes by death event 
pp_db_age <- heart_data_f %>%
  ggplot(aes(age, diabetes, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Age") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of creatinine phosphokinase levels across diabetes by death event 
pp_db_cp <- heart_data_f %>%
  ggplot(aes(creatinine_phosphokinase, diabetes, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_log10(labels = comma) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Creatinine Phosphokinase (log)") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of ejection fraction levels across diabetes by death event 
pp_db_ef <- heart_data_f %>%
  ggplot(aes(ejection_fraction, diabetes, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Ejection Fraction") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of platelet levels across diabetes by death event 
pp_db_pl <- heart_data_f %>%
  ggplot(aes(platelets, diabetes, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_continuous(labels = comma, breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Platelets") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of serum creatinine levels across diabetes by death event 
pp_db_sc <- heart_data_f %>%
  ggplot(aes(serum_creatinine, diabetes, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_log10() +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Serum Creatinine (log)") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of serum sodium levels across diabetes by death event 
pp_db_ss <- heart_data_f %>%
  ggplot(aes(serum_sodium, diabetes, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Serum Sodium") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of time across diabetes by death event 
pp_db_tm <- heart_data_f %>%
  ggplot(aes(time, diabetes, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Length of Follow-up") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# arrange plots together in a grid for presentation 
grid.arrange(pp_db_age, pp_db_cp, pp_db_ef, pp_db_pl, pp_db_sc, pp_db_ss, pp_db_tm, shared_legend_5,
             ncol = 2, layout_matrix = rbind(c(1,1,2,2), c(3,3,4,4), c(5,5,6,6), c(NA,7,7,8)),
             top = "Figure A2: Plots of the Continuous Variables by Diabetes and Patient Death",
             left = "Diabetes")



### examine continuous variables across HIGH BLOOD PRESSURE ###
### all plots stored as separate objects ###

# plot of age across high blood pressure  by death event 
pp_hbp_age <- heart_data_f %>%
  ggplot(aes(age, high_blood_pressure, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Age") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of creatinine phosphokinase levels across high blood pressure  by death event 
pp_hbp_cp <- heart_data_f %>%
  ggplot(aes(creatinine_phosphokinase, high_blood_pressure, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_log10(labels = comma) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Creatinine Phosphokinase (log)") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of ejection fraction levels across high blood pressure  by death event 
pp_hbp_ef <- heart_data_f %>%
  ggplot(aes(ejection_fraction, high_blood_pressure, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Ejection Fraction") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of platelet levels across high blood pressure  by death event 
pp_hbp_pl <- heart_data_f %>%
  ggplot(aes(platelets, high_blood_pressure, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_continuous(labels = comma, breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Platelets") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of serum creatinine levels across high blood pressure  by death event 
pp_hbp_sc <- heart_data_f %>%
  ggplot(aes(serum_creatinine, high_blood_pressure, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_log10() +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Serum Creatinine (log)") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of serum sodium levels across high blood pressure  by death event 
pp_hbp_ss <- heart_data_f %>%
  ggplot(aes(serum_sodium, high_blood_pressure, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Serum Sodium") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of time across high blood pressure  by death event 
pp_hbp_tm <- heart_data_f %>%
  ggplot(aes(time, high_blood_pressure, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Length of Follow-up") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# arrange plots together in a grid for presentation 
grid.arrange(pp_hbp_age, pp_hbp_cp, pp_hbp_ef, pp_hbp_pl, pp_hbp_sc, pp_hbp_ss, pp_hbp_tm, shared_legend_5,
             ncol = 2, layout_matrix = rbind(c(1,1,2,2), c(3,3,4,4), c(5,5,6,6), c(NA,7,7,8)),
             top = "Figure A3: Plots of the Continuous Variables by High Blood Pressure and Patient Death",
             left = "High Blood Pressure ")


### examine continuous variables across SEX ###
### all plots stored as separate objects ###

# plot of age across sex by death event 
pp_sex_age <- heart_data_f %>%
  ggplot(aes(age, sex, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Age") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of creatinine phosphokinase levels across sex by death event 
pp_sex_cp <- heart_data_f %>%
  ggplot(aes(creatinine_phosphokinase, sex, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_log10(labels = comma) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Creatinine Phosphokinase (log)") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of ejection fraction levels across sex by death event 
pp_sex_ef <- heart_data_f %>%
  ggplot(aes(ejection_fraction, sex, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Ejection Fraction") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of platelet levels across sex by death event 
pp_sex_pl <- heart_data_f %>%
  ggplot(aes(platelets, sex, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_continuous(labels = comma, breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Platelets") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of serum creatinine levels across sex by death event 
pp_sex_sc <- heart_data_f %>%
  ggplot(aes(serum_creatinine, sex, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_log10() +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Serum Creatinine (log)") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of serum sodium levels across sex by death event 
pp_sex_ss <- heart_data_f %>%
  ggplot(aes(serum_sodium, sex, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Serum Sodium") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of time across sex by death event 
pp_sex_tm <- heart_data_f %>%
  ggplot(aes(time, sex, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Length of Follow-up") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# arrange plots together in a grid for presentation 
grid.arrange(pp_sex_age, pp_sex_cp, pp_sex_ef, pp_sex_pl, pp_sex_sc, pp_sex_ss, pp_sex_tm, shared_legend_5,
             ncol = 2, layout_matrix = rbind(c(1,1,2,2), c(3,3,4,4), c(5,5,6,6), c(NA,7,7,8)),
             top = "Figure A4: Plots of the Continuous Variables by Sex and Patient Death",
             left = "Sex")


### examine continuous variables across SMOKING ###
### all plots stored as separate objects ###

# plot of age across smoking by death event 
pp_smk_age <- heart_data_f %>%
  ggplot(aes(age, smoking, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Age") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of creatinine phosphokinase levels across smoking by death event 
pp_smk_cp <- heart_data_f %>%
  ggplot(aes(creatinine_phosphokinase, smoking, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_log10(labels = comma) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Creatinine Phosphokinase (log)") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of ejection fraction levels across smoking by death event 
pp_smk_ef <- heart_data_f %>%
  ggplot(aes(ejection_fraction, smoking, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Ejection Fraction") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of platelet levels across smoking by death event 
pp_smk_pl <- heart_data_f %>%
  ggplot(aes(platelets, smoking, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_continuous(labels = comma, breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Platelets") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of serum creatinine levels across smoking by death event 
pp_smk_sc <- heart_data_f %>%
  ggplot(aes(serum_creatinine, smoking, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_log10() +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Serum Creatinine (log)") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of serum sodium levels across smoking by death event 
pp_smk_ss <- heart_data_f %>%
  ggplot(aes(serum_sodium, smoking, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Serum Sodium") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of time across smoking by death event 
pp_smk_tm <- heart_data_f %>%
  ggplot(aes(time, smoking, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.5), size = 2, alpha = 0.75) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  stat_summary(aes(group = DEATH_EVENT), fun = "mean", geom = "point", size = 3, colour = "black") +
  xlab("Length of Follow-up") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# arrange plots together in a grid for presentation 
grid.arrange(pp_smk_age, pp_smk_cp, pp_smk_ef, pp_smk_pl, pp_smk_sc, pp_smk_ss, pp_smk_tm, shared_legend_5,
             ncol = 2, layout_matrix = rbind(c(1,1,2,2), c(3,3,4,4), c(5,5,6,6), c(NA,7,7,8)),
             top = "Figure A5: Plots of the Continuous Variables by Smoking and Patient Death",
             left = "Smoking")


### looking at interactions between continuous variables - TIME ###
### all plots stored as separate objects ###

# plot of age and time by death event
pp_tm_age <- heart_data_f %>%
  ggplot(aes(age, time, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.25), size = 2, alpha = 0.75) +
  stat_ellipse(lwd = 1) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  xlab("Age") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y =  element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of ejection fraction and time by death event
pp_tm_ef <- heart_data_f %>%
  ggplot(aes(ejection_fraction, time, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.25), size = 2, alpha = 0.75) +
  stat_ellipse(lwd = 1) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  xlab("Ejection Fraction") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y =  element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of serum creatinine and time by death event
pp_tm_sc <- heart_data_f %>%
  ggplot(aes(serum_creatinine, time, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.25), size = 2, alpha = 0.75) +
  stat_ellipse(lwd = 1) +
  scale_x_log10() +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  xlab("Serum Creatinine (log)") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y =  element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of serum sodium and time by death event
pp_tm_ss <- heart_data_f %>%
  ggplot(aes(serum_sodium, time, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.25), size = 2, alpha = 0.75) +
  stat_ellipse(lwd = 1) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  xlab("Serum Sodium") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y =  element_blank()) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# create a common legend for the plots
# need to create an entire dummy plot
pp_tm_ss_legend <- heart_data_f %>%
  ggplot(aes(serum_sodium, time, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.25), size = 2, alpha = 0.75) +
  stat_ellipse(lwd = 1) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  xlab("Serum Sodium") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y =  element_blank()) +
  theme(legend.position = "top") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# extract the legend as an object 
shared_legend_10 <- extract_legend(pp_tm_ss_legend)

# arrange plots together in a grid for presentation 
grid.arrange(shared_legend_10,
             arrangeGrob(pp_tm_age, pp_tm_ef, pp_tm_sc, pp_tm_ss, 
                         ncol = 2), 
             nrow = 2, heights = c(1,20),
             top = "Figure A6: Plots of the Continuous Variables by Days to Follow-up and Patient Death",
             left = "Length of Follow-up")


###################################################################################################

### looking at interactions between continuous variables - MIXED ###
### all plots stored as separate objects ###

# plot of ejection fraction and age by death event
pp_age_ef <- heart_data_f %>%
  ggplot(aes(age, ejection_fraction, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.25), size = 2, alpha = 0.75) +
  stat_ellipse(lwd = 1) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  xlab("Age") +
  ylab("Ejection Fraction") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y =  element_text(vjust = 2)) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of secrum creatinine and age by death event
pp_age_sc <- heart_data_f %>%
  ggplot(aes(age, serum_creatinine, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.25), size = 2, alpha = 0.75) +
  stat_ellipse(lwd = 1) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_log10() +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  xlab("Age") +
  ylab("Serum Creatinine (log)") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y =  element_text(vjust = 2)) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of serum sodium and age by death event
pp_age_ss <- heart_data_f %>%
  ggplot(aes(age, serum_sodium, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.25), size = 2, alpha = 0.75) +
  stat_ellipse(lwd = 1) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  xlab("Age") +
  ylab("Serum Sodium") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y =  element_text(vjust = 2)) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of ejection_fraction and secrum creatinine by death event
pp_ef_sc <- heart_data_f %>%
  ggplot(aes(ejection_fraction, serum_creatinine, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.25), size = 2, alpha = 0.75) +
  stat_ellipse(lwd = 1) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_log10() +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  xlab("Ejection Fraction") +
  ylab("Serum Creatinine (log)") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y =  element_text(vjust = 2)) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of ejection_fraction and secrum sodium by death event
pp_ef_ss <- heart_data_f %>%
  ggplot(aes(ejection_fraction, serum_sodium, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.25), size = 2, alpha = 0.75) +
  stat_ellipse(lwd = 1) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  xlab("Ejection Fraction") +
  ylab("Serum Sodium") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y =  element_text(vjust = 2)) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# plot of serum sodium and secrum creatinine by death event
pp_ss_sc <- heart_data_f %>%
  ggplot(aes(serum_sodium, serum_creatinine, color = DEATH_EVENT, shape = DEATH_EVENT)) + 
  geom_point(position = position_jitter(h = 0.25, w = 0.25), size = 2, alpha = 0.75) +
  stat_ellipse(lwd = 1) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_log10() +
  scale_color_brewer(name = "Death", palette = "Paired") +
  scale_shape_manual(name = "Death", values = c(15, 17)) +
  xlab("Serum Sodium") +
  ylab("Serum Creatinine (log)") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -1)) +
  theme(axis.title.y =  element_text(vjust = 2)) +
  theme(legend.position = "blank") +
  theme(plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm"))

# arrange plots together in a grid for presentation 
grid.arrange(shared_legend_10,
             arrangeGrob(pp_age_ef, pp_age_sc, pp_age_ss, pp_ef_sc, pp_ef_ss, pp_ss_sc, 
                         ncol = 2), 
             nrow = 2, heights = c(1,20),
             top = "Figure A7: Plots of Bivariate Continuous Variables by Patient Death")





###########################################################
######################## APPENDIX B #######################
###########################################################

### ENVIRONMENT ###

# print operating system and R version
version



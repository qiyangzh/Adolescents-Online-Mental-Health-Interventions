########################################################################################################
# Student Internet Mental Health Interventions
########################################################################################################
# Authors: Qiyang Zhang
# Contact: qiyang39@nus.edu.sg
# Created: 2024/11/19

# This file analyzes the included studies in the School-based Mental Health Interventions systematic 
# review, including preparing the data for analysis and meta-regressions.

########################################################################################################
# Initial Set-up
########################################################################################################
# Clear workspace
setwd("~/Desktop/Research2025/Student_Internet_mentalhealth")

# Load packages
test<-require(googledrive)   #all gs_XXX() functions for reading data from Google
if (test == FALSE) {
  install.packages("googledrive")
  require(googledrive)
}
test<-require(googlesheets4)   #all gs_XXX() functions for reading data from Google
if (test == FALSE) {
  install.packages("googlesheets4")
  require(googlesheets4)
}
test<-require(plyr)   #rename()
if (test == FALSE) {
  install.packages("plyr")
  require(plyr)
}
test<-require(metafor)   #escalc(); rma();
if (test == FALSE) {
  install.packages("metafor")
  require(metafor)
}
test<-require(robumeta)
if (test == FALSE) {
  install.packages("robumeta")
  require(robumeta)
}
test<-require(weightr) #selection modeling
if (test == FALSE) {
  install.packages("weightr")
  require(weightr)
}
test<-require(clubSandwich) #coeftest
if (test == FALSE) {
  install.packages("clubSandwich")
  require(clubSandwich)
}
test<-require(tableone)   #CreateTableOne()
if (test == FALSE) {
  install.packages("tableone")
  require(tableone)
}
test<-require(flextable)   
if (test == FALSE) {
  install.packages("flextable")
  require(flextable)
}
test<-require(officer)   
if (test == FALSE) {
  install.packages("officer")
  require(officer)
}
test<-require(tidyverse)   
if (test == FALSE) {
  install.packages("tidyverse")
  require(tidyverse)
}
test<-require(ggrepel)   
if (test == FALSE) {
  install.packages("ggrepel")
  require(ggrepel)
}
test<-require(readxl)   
if (test == FALSE) {
  install.packages("readxl")
  require(readxl)
}
test<-require(robumeta)   
if (test == FALSE) {
  install.packages("robumeta")
  require(robumeta)
}
test<-require(dplyr)   
if (test == FALSE) {
  install.packages("dplyr")
  require(dplyr)
}
test<-require(pimeta)   
if (test == FALSE) {
  install.packages("pimeta")
  require(pimeta)
}
rm(test)
########################################################################################################
# Load data
########################################################################################################
# load findings and studies
#gs4_auth(email = "zhangqiyang0329@gmail.com")
# gs4_auth(email = "aneitzel@gmail.com")
#findings <- read_sheet(id, sheet = "Findings", col_types = "c")
#studies <- read_sheet(id, sheet = "Studies", col_types = "c")   # includes separate effect sizes for each finding from a study
findings <- read_excel("Students_Coding.xlsx", sheet = "Findings")
studies <- read_excel("Students_Coding.xlsx", sheet = "Studies")

rm(id)

########################################################################################################
# Clean data
########################################################################################################
# remove any empty rows & columns
studies <- subset(studies, is.na(studies$Study)==FALSE)
findings <- subset(findings, is.na(findings$Study)==FALSE)

studies <- subset(studies, is.na(studies$Drop)==TRUE)
findings <- subset(findings, is.na(findings$Drop)==TRUE)

# merge dataframes
full <- merge(studies, findings, by = c("Study"), all = TRUE, suffixes = c(".s", ".f"))
full <- subset(full, is.na(full$Drop.s)==TRUE)

# format to correct variable types
nums <- c("Treatment.N.original", "Control.N.original", "Sample size", 
          "Treatment.Cluster", "Control.Cluster", 
          "T_Mean_Pre", "T_SD_Pre", "C_Mean_Pre", 
          "C_SD_Pre", "T_Mean_Post", "T_SD_Post", 
          "C_Mean_Post", "C_SD_Post", "Randomized", 
          "CBT","Clustered", "Number.of.sessionsORmodules",
          "Clinical population", "Duration.weeks",
          "Female", "Age.mean", "Self.guided",
          "Follow-up")

full[nums] <- lapply(full[nums], as.numeric)
rm(nums)

###############################################################
#Create unique identifiers (ES, study, program)
###############################################################
full$ESId <- as.numeric(rownames(full))
full$StudyID <- as.numeric(as.factor(full$Study))
summary(full$StudyID)
########################################################################################################
# Prep data
########################################################################################################
##### Calculate ESs #####
# calculate pretest ES, SMD is standardized mean difference
full <- escalc(measure = "SMD", m1i = T_Mean_Pre, sd1i = T_SD_Pre, n1i = Treatment.N.original,
               m2i = C_Mean_Pre, sd2i = C_SD_Pre, n2i = Control.N.original, data = full)
full$vi <- NULL
full <- plyr::rename(full, c("yi" = "ES_Pre"))

# calculate posttest ES
full <- escalc(measure = "SMD", m1i = T_Mean_Post, sd1i = T_SD_Post, n1i = Treatment.N.original,
               m2i = C_Mean_Post, sd2i = C_SD_Post, n2i = Control.N.original, data = full)
full$vi <- NULL
full <- plyr::rename(full, c("yi" = "ES_Post"))

# calculate DID (post ES - pre ES)
full$ES_DID <- full$ES_Post - full$ES_Pre

full$ES_DID[which(is.na(full$ES_DID)==TRUE & is.na(full$ES)==FALSE)] <- full$ES[which(is.na(full$ES_DID)==TRUE & is.na(full$ES)==FALSE)]
full$Effect.Size <- as.numeric(full$ES_DID)
full$Effect.Size <- as.numeric(full$Effect.Size)

###############################################################
#Calculate meta-analytic variables: Sample sizes
###############################################################
#create full sample/total clusters variables
full$Sample <- full$Treatment.N.original + full$Control.N.original
full$Cluster_Total <- full$Treatment.Cluster+full$Control.Cluster

###################################
#Create dummies
###################################
full$Smallsample <- 0
full$Smallsample[which(full$Sample<=250)] <- 1

#12 weeks was the threshold, but the mean here is 7.02, so we used 7
full$Longduration <- 0
full$Longduration[which(full$Duration.weeks>=7)] <- 1

#####################
#Centering, when there is missing value, this won't work
#####################
full$Intensity <- full$Number.of.sessionsORmodules/full$Duration.weeks
full$Intensity.c <- full$Intensity - mean(full$Intensity)

full$HighIntensity <- 0
full$HighIntensity[which(full$`Intensity.c` > 0)] <- 1
  
full$FemalePercent <- full$Female/full$`Sample.size`
full$FiftyPercentFemale <- 0
full$FiftyPercentFemale[which(full$`FemalePercent` > 0.50)] <- 1

full$CBT.c <- full$CBT - mean(full$CBT)
full$Self.guided.c <- full$Self.guided - mean(full$Self.guided)
full$Smallsample.c <- full$Smallsample - mean(full$Smallsample)

full$Follow.up.c <- full$Follow.up - mean(full$Follow.up)
full$Clinical.population.c <- full$Clinical.population - mean(full$Clinical.population)
full$Longduration.c <- full$Longduration - mean(full$Longduration)
full$Website.basedORInternet.based.c <- full$Website.basedORInternet.based - mean(full$Website.basedORInternet.based)
full$Elementary <- 0
full$Elementary[which(full$`Grade.levels` == "Elementary")] <- 1
full$Elementary.c <- full$Elementary - mean(full$Elementary)
full$Depression <- 0
full$Depression[which(full$`Outcomes` == "Depression")] <- 1
full$Depression.c <- full$Depression - mean(full$Depression)

full$Passive <- 0
full$Passive[which(full$`Control` == "Passive")] <- 1
full$Passive.c <- full$Passive - mean(full$Passive)

full$TextBased <- 0
full$TextBased[which(full$`Modality` == "text")] <- 1
full$TextBased.c <- full$TextBased - mean(full$TextBased)

###############################################################
#Calculate meta-analytic variables: Correct ES for clustering (Hedges, 2007, Eq.19)
###############################################################
#first, create an assumed ICC
full$icc <- NA
full$icc[which(full$Clustered == 1)] <- 0.2

#find average students/cluster
full$Treatment.Cluster.n <- NA
full$Treatment.Cluster.n[which(full$Clustered == 1)] <- round(full$Treatment.N.original[which(full$Clustered == 1)]/full$Treatment.Cluster[which(full$Clustered == 1)], 0)
full$Control.Cluster.n <- NA
full$Control.Cluster.n[which(full$Clustered == 1)] <- round(full$Control.N.original[which(full$Clustered == 1)]/full$Control.Cluster[which(full$Clustered == 1)], 0)
# find other parts of equation
full$n.TU <- NA
full$n.TU <- ((full$Treatment.N.original * full$Treatment.N.original) - (full$Treatment.Cluster * full$Treatment.Cluster.n * full$Treatment.Cluster.n))/(full$Treatment.N.original * (full$Treatment.Cluster - 1))
full$n.CU <- NA
full$n.CU <- ((full$Control.N.original * full$Control.N.original) - (full$Control.Cluster * full$Control.Cluster.n * full$Control.Cluster.n))/(full$Control.N.original * (full$Control.Cluster - 1))

# next, calculate adjusted ES, save the originals, then replace only the clustered ones
full$Effect.Size.adj <- full$Effect.Size * (sqrt(1-full$icc*(((full$Sample.size-full$n.TU*full$Treatment.Cluster - full$n.CU*full$Control.Cluster)+full$n.TU + full$n.CU - 2)/(full$Sample.size-2))))

# save originals, replace for clustered
full$Effect.Size.orig <- full$Effect.Size
full$Effect.Size[which(full$Clustered==1)] <- full$Effect.Size.adj[which(full$Clustered==1)]

num <- c("Effect.Size")
full[num] <- lapply(full[num], as.numeric)
rm(num)
################################################################
# Calculate meta-analytic variables: Variances (Lipsey & Wilson, 2000, Eq. 3.23)
################################################################
#calculate standard errors
full$se<-sqrt(((full$Treatment.N.original+full$Control.N.original)/(full$Treatment.N.original*full$Control.N.original))+((full$Effect.Size*full$Effect.Size)/(2*(full$Treatment.N.original+full$Control.N.original))))

#calculate variance
full$var<-full$se*full$se
full$Effect.Size[which(full$Negative.Outcomes==1)] <- full$Effect.Size[which(full$Negative.Outcomes==1)]*-1

########################################
#meta-regression
########################################
#Null Model
#Positive Outcome Model
Positive <- subset(full, full$Positive.Outcomes==1)

V_listPositive <- impute_covariance_matrix(vi=Positive$var, cluster=Positive$StudyID, r=0.8)

MVnullPositive <- rma.mv(yi=Effect.Size,
                 V=V_listPositive,
                 random=~1 | StudyID/ESId,
                 test="t",
                 data=Positive,
                 method="REML")
MVnullPositive

# #t-test of each covariate#
MVnull.coefPositive <- coef_test(MVnullPositive, cluster=Positive$StudyID, vcov="CR2")
MVnull.coefPositive

#Negative Outcome Model
Negative <- subset(full, full$Negative.Outcomes==1)

V_listNegative <- impute_covariance_matrix(vi=Negative$var, cluster=Negative$StudyID, r=0.8)

MVnullNegative <- rma.mv(yi=Effect.Size,
                         V=V_listNegative,
                         random=~1 | StudyID/ESId,
                         test="t",
                         data=Negative,
                         method="REML")
MVnullNegative

# #t-test of each covariate#
MVnull.coefNegative <- coef_test(MVnullNegative, cluster=Negative$StudyID, vcov="CR2")
MVnull.coefNegative

View(Negative[c("Study","Effect.Size", "Outcomes", "CBT")])
### Output prediction interval ###
dat_clean <- data.frame(yi = Negative$Effect.Size, se_g = Negative$se)
dat_clean <- na.omit(dat_clean)

yi_clean <- dat_clean$yi
se_g_clean <- dat_clean$se_g
install.packages("pimeta")
library(pimeta)

pima_result <- pima(yi_clean, se_g_clean, method = "HK")  # Using the Hartung-Knapp method

print(pima_result)

#########################
#DepressionandAnxiety
#########################
DepressionandAnxiety <- subset(full, full$Outcomes=="Depression" | full$Outcomes=="Anxiety" )

V_listDepressionandAnxiety <- impute_covariance_matrix(vi=DepressionandAnxiety$var, cluster=DepressionandAnxiety$StudyID, r=0.8)

MVnullDepressionandAnxiety <- rma.mv(yi=Effect.Size,
                         V=V_listDepressionandAnxiety,
                         random=~1 | StudyID/ESId,
                         test="t",
                         data=DepressionandAnxiety,
                         method="REML")
MVnullDepressionandAnxiety

# #t-test of each covariate#
MVnull.coefDepressionandAnxiety <- coef_test(MVnullDepressionandAnxiety, cluster=DepressionandAnxiety$StudyID, vcov="CR2")
MVnull.coefDepressionandAnxiety

### Output prediction interval ###
dat_clean <- data.frame(yi = DepressionandAnxiety$Effect.Size, se_g = DepressionandAnxiety$se)
dat_clean <- na.omit(dat_clean)

yi_clean <- dat_clean$yi
se_g_clean <- dat_clean$se_g

pima_result <- pima(yi_clean, se_g_clean, method = "HK")  # Using the Hartung-Knapp method

print(pima_result)
###########################
#forest plot
###########################
#robumeta
study_averages <- DepressionandAnxiety %>%
  group_by(StudyID) %>%
  summarise(avg_effect_size = mean(Effect.Size, na.rm = TRUE),
            across(everything(), ~ first(.)))

MVnull <- robu(formula = Effect.Size ~ 1, studynum = StudyID, data = study_averages, var.eff.size = var)
install.packages("meta")
library(meta)
m.gen <- meta::metagen(TE = Effect.Size,
                 seTE = se,
                 studlab = Study,
                 data = study_averages,
                 sm = "SMD",
                 fixed = FALSE,
                 random = TRUE,
                 method.tau = "REML",
                 method.random.ci = "HK")
summary(m.gen)
png(file = "forestplot.png", width = 3600, height = 3000, res = 300)

meta::forest(m.gen,
             sortvar = TE,
             prediction = TRUE,
             print.tau2 = FALSE,
             leftlabs = c("Author", "g", "SE"))
dev.off()
# 95% prediction intervals
print(PI_upper <- MVnullDepressionandAnxiety$b[1] + (1.96*sqrt(MVnullDepressionandAnxiety$sigma2[1] + MVnullDepressionandAnxiety$sigma2[2])))
print(PI_lower <- MVnullDepressionandAnxiety$b[1] - (1.96*sqrt(MVnullDepressionandAnxiety$sigma2[1] + MVnullDepressionandAnxiety$sigma2[2])))

#Comparison model
terms <- c("Depression.c", "CBT.c", 
           "Longduration.c", "Smallsample.c",
           "Elementary.c", "Clinical.population.c")
interact <- c("Depression.c*CBT.c")
formula <- reformulate(termlabels = c(terms, interact))

V_listDepressionandAnxiety <- impute_covariance_matrix(vi=DepressionandAnxiety$var, cluster=DepressionandAnxiety$StudyID, r=0.8)

MVfullDepressionandAnxiety <- rma.mv(yi=Effect.Size,
                 V=V_listDepressionandAnxiety,
                 mods=formula,
                 random=~1 | StudyID/ESId,
                 test="t",
                 data=DepressionandAnxiety,
                 method="REML")
MVfullDepressionandAnxiety

#t-test of each covariate#
MVfull.coefDepressionandAnxiety <- coef_test(MVfullDepressionandAnxiety, cluster=DepressionandAnxiety$StudyID, vcov="CR2")
MVfull.coefDepressionandAnxiety

#############################
# Method:"Control"
# Unit:"Smallsample", "Age.mean", "FiftyPercentFemale","Longduration","Grade.levels"， "Clinical.population"
# Outcome: "Follow.up", "Outcomes", "Age.mean", "Duration.weeks"
# Setting: "Continent", , ,"Website.basedORInternet.based","CBT","Self.guided", "Modality"

# swaps signs for reverse-coded outcomes (so positive is good and negative is bad)
#  "Self.guided","Outcomes", "Follow.up.c", "Website.basedORInternet.based.c", "Self.guided", "Longduration.c",
#terms <- c("Directionality..twoway..oneway.","Passive.c", "Smallsample.c", 
#            "Clinical.population.c",
#           "CBT.c", "Elementary.c", "Longduration.c")
#"Clinical.population.c",

terms <- c("Directionality..twoway..oneway.","Longduration.c",
           "Passive.c", "Smallsample.c", "Self.guided",
           "CBT.c", "Elementary.c")
terms <- c("Modality")
formula <- reformulate(termlabels = c(terms))

V_list_Positive <- impute_covariance_matrix(vi=Positive$var, cluster=Positive$StudyID, r=0.8)

MVfull_Positive <- rma.mv(yi=Effect.Size,
                 V=V_list_Positive,
                 mods=formula,
                 random=~1 | StudyID/ESId,
                 test="t",
                 data=Positive,
                 method="REML")
MVfull_Positive

#t-test of each covariate#
MVfull.coef_Positive <- coef_test(MVfull_Positive, cluster=Positive$StudyID, vcov="CR2")
MVfull.coef_Positive

##############
#Negative
#############
V_list_Negative <- impute_covariance_matrix(vi=Negative$var, cluster=Negative$StudyID, r=0.8)

MVfull_Negative <- rma.mv(yi=Effect.Size,
                          V=V_list_Negative,
                          mods=formula,
                          random=~1 | StudyID/ESId,
                          test="t",
                          data=Negative,
                          method="REML")
MVfull_Negative

#t-test of each covariate#
MVfull.coef_Negative <- coef_test(MVfull_Negative, cluster=Negative$StudyID, vcov="CR2")
MVfull.coef_Negative
#################################################################################
# Marginal Means
#################################################################################
# Positive, set up table to store results
means <- data.frame(moderator = character(0), group = character(0), beta = numeric(0), SE = numeric(0), 
                    tstat = numeric(0), df = numeric(0), p_Satt = numeric(0))

mods <- c("as.factor(CBT)","as.factor(Directionality..twoway..oneway.)",
          "as.factor(Passive)", "as.factor(Smallsample)", 
          "as.factor(Elementary)", "as.factor(Clinical.population)", 
          "as.factor(Longduration)", "as.factor(Self.guided)")
for(i in 1:length(mods)){
  # i <- 1
  formula <- reformulate(termlabels = c(mods[i], terms, "-1"))   # Worth knowing - if you duplicate terms, it keeps the first one
  mod_means <- rma.mv(yi=Effect.Size,
                      V=V_list_Positive,
                      mods=formula,
                      random=~1 | StudyID/ESId,
                      test="t",
                      data=Positive,
                      method="REML") #estimate variances using REML
  coef_mod_means <- as.data.frame(coef_test(mod_means,#estimation model above
                                            cluster=Positive$StudyID, #define cluster IDs
                                            vcov = "CR2")) #estimation method (CR2 is best)
  # limit to relevant rows (the means you are interested in)
  coef_mod_means$moderator <- gsub(x = mods[i], pattern = "as.factor", replacement = "")
  coef_mod_means$group <- rownames(coef_mod_means)
  rownames(coef_mod_means) <- c()
  coef_mod_means <- subset(coef_mod_means, substr(start = 1, stop = nchar(mods[i]), x = coef_mod_means$group)== mods[i])
  coef_mod_means$group <- substr(x = coef_mod_means$group, start = nchar(mods[i])+1, stop = nchar(coef_mod_means$group))
  means <- dplyr::bind_rows(means, coef_mod_means)
}
means

# Negative, set up table to store results
means_Negative <- data.frame(moderator = character(0), group = character(0), beta = numeric(0), SE = numeric(0), 
                    tstat = numeric(0), df = numeric(0), p_Satt = numeric(0))

mods_Negative <- c("as.factor(CBT)","as.factor(Directionality..twoway..oneway.)",
          "as.factor(Passive)", "as.factor(Smallsample)", 
          "as.factor(Elementary)", "as.factor(Clinical.population)", 
          "as.factor(Longduration)", "as.factor(Self.guided)")
for(i in 1:length(mods_Negative)){
  # i <- 1
  formula <- reformulate(termlabels = c(mods_Negative[i], terms, "-1"))   # Worth knowing - if you duplicate terms, it keeps the first one
  mod_means_Negative <- rma.mv(yi=Effect.Size,
                      V=V_list_Negative,
                      mods=formula,
                      random=~1 | StudyID/ESId,
                      test="t",
                      data=Negative,
                      method="REML") #estimate variances using REML
  coef_mod_means_Negative <- as.data.frame(coef_test(mod_means_Negative,#estimation model above
                                            cluster=Negative$StudyID, #define cluster IDs
                                            vcov = "CR2")) #estimation method (CR2 is best)
  # limit to relevant rows (the means you are interested in)
  coef_mod_means_Negative$moderator <- gsub(x = mods_Negative[i], pattern = "as.factor", replacement = "")
  coef_mod_means_Negative$group <- rownames(coef_mod_means_Negative)
  rownames(coef_mod_means_Negative) <- c()
  coef_mod_means_Negative <- subset(coef_mod_means_Negative, substr(start = 1, stop = nchar(mods_Negative[i]), x = coef_mod_means_Negative$group)== mods_Negative[i])
  coef_mod_means_Negative$group <- substr(x = coef_mod_means_Negative$group, start = nchar(mods_Negative[i])+1, stop = nchar(coef_mod_means_Negative$group))
  means_Negative <- dplyr::bind_rows(means_Negative, coef_mod_means_Negative)
}
means_Negative
#################################################################################
# Heterogeneity
#################################################################################
# 95% prediction intervals, Positive
print(PI_upper <- MVfull_Positive$b[1] + (1.96*sqrt(MVfull_Positive$sigma2[1] + MVfull_Positive$sigma2[2])))
print(PI_lower <- MVfull_Positive$b[1] - (1.96*sqrt(MVfull_Positive$sigma2[1] + MVfull_Positive$sigma2[2])))

# 95% prediction intervals, Negative
print(PI_upper <- MVfull_Negative$b[1] + (1.96*sqrt(MVfull_Negative$sigma2[1] + MVfull_Negative$sigma2[2])))
print(PI_lower <- MVfull_Negative$b[1] - (1.96*sqrt(MVfull_Negative$sigma2[1] + MVfull_Negative$sigma2[2])))

#################################################################################
#Create Descriptives Table
#################################################################################
# identify variables for descriptive tables (study-level and outcome-level)
vars_study <- c("Smallsample", "Directionality..twoway..oneway.",
                "Clinical.population",
                "CBT", "Elementary", "Longduration")
vars_outcome <- c("Outcomes", "Passive" , "Follow.up")

# To make this work, you will need a df that is at the study-level for study-level 
# variables (such as research design) you may have already created this (see above, with study-level ESs), but if you didn't, here is an easy way:
# 1) make df with *only* the study-level variables of interest and studyIDs in it, "Intensity"
study_level_full <- full[c("StudyID", "Smallsample", "Directionality..twoway..oneway.",
                           "Clinical.population",
                           "CBT", "Elementary", "Longduration")]
# 2) remove duplicated rows
study_level_full <- unique(study_level_full)
# 3) make sure it is the correct number of rows (should be same number of studies you have)
length(study_level_full$StudyID)==length(unique(study_level_full$StudyID))
# don't skip step 3 - depending on your data structure, some moderators can be
# study-level in one review, but outcome-level in another

# create the table "chunks"
table_study_df <- as.data.frame(print(CreateTableOne(vars = vars_study, data = study_level_full, 
                                                     includeNA = TRUE, 
                                                     factorVars = c("Smallsample", "Directionality..twoway..oneway.",
                                                                    "Clinical.population",
                                                                    "CBT", "Elementary", "Longduration")), 
                                      showAllLevels = TRUE))
table_outcome_df <- as.data.frame(print(CreateTableOne(vars = vars_outcome, data = full, includeNA = TRUE,
                                                       factorVars = c("Outcomes", "Passive" , "Follow.up")), 
                                        showAllLevels = TRUE))
rm(vars_study, vars_outcome)

################################
# Descriptives Table Formatting
################################
table_study_df$Category <- row.names(table_study_df)
rownames(table_study_df) <- c()
table_study_df <- table_study_df[c("Category", "level", "Overall")]
table_study_df$Category[which(substr(table_study_df$Category, 1, 1)=="X")] <- NA
table_study_df$Category <- gsub(pattern = "\\..mean..SD..", replacement = "", x = table_study_df$Category)
table_study_df$Overall <- gsub(pattern = "\\( ", replacement = "\\(", x = table_study_df$Overall)
table_study_df$Overall <- gsub(pattern = "\\) ", replacement = "\\)", x = table_study_df$Overall)
table_study_df$Category <- gsub(pattern = "\\.", replacement = "", x = table_study_df$Category)
table_study_df$Category[which(table_study_df$Category=="n")] <- "Total Studies"
table_study_df$level[which(table_study_df$level=="1")] <- "Yes"
table_study_df$level[which(table_study_df$level=="0")] <- "No                                                                              "
# fill in blank columns (to improve merged cells later)
for(i in 1:length(table_study_df$Category)) {
  if(is.na(table_study_df$Category[i])) {
    table_study_df$Category[i] <- table_study_df$Category[i-1]
  }
}

table_outcome_df$Category <- row.names(table_outcome_df)
rownames(table_outcome_df) <- c()
table_outcome_df <- table_outcome_df[c("Category", "level", "Overall")]
table_outcome_df$Category[which(substr(table_outcome_df$Category, 1, 1)=="X")] <- NA
table_outcome_df$Overall <- gsub(pattern = "\\( ", replacement = "\\(", x = table_outcome_df$Overall)
table_outcome_df$Overall <- gsub(pattern = "\\) ", replacement = "\\)", x = table_outcome_df$Overall)
table_outcome_df$Category <- gsub(pattern = "\\.", replacement = "", x = table_outcome_df$Category)
table_outcome_df$Category[which(table_outcome_df$Category=="n")] <- "Total Effect Sizes"
# fill in blank columns (to improve merged cells later)
for(i in 1:length(table_outcome_df$Category)) {
  if(is.na(table_outcome_df$Category[i])) {
    table_outcome_df$Category[i] <- table_outcome_df$Category[i-1]
  }
}

########################
#Output officer
########################
myreport<-read_docx()

# Descriptive Table DepressionandAnxiety
myreport <- body_add_par(x = myreport, value = "Table 4: Descriptive Statistics Depression and Anxiety", style = "Normal")
descriptives_studyDepressionandAnxiety <- flextable(head(table_study_dfDepressionandAnxiety, n=nrow(table_study_dfDepressionandAnxiety)))
descriptives_studyDepressionandAnxiety <- add_header_lines(descriptives_studyDepressionandAnxiety, values = c("Study Level"), top = FALSE)
descriptives_studyDepressionandAnxiety <- theme_vanilla(descriptives_studyDepressionandAnxiety)
descriptives_studyDepressionandAnxiety <- merge_v(descriptives_studyDepressionandAnxiety, j = c("Category"))
myreport <- body_add_flextable(x = myreport, descriptives_studyDepressionandAnxiety)

descriptives_outcomeDepressionandAnxiety <- flextable(head(table_outcome_dfDepressionandAnxiety, n=nrow(table_outcome_dfDepressionandAnxiety)))
descriptives_outcomeDepressionandAnxiety <- delete_part(descriptives_outcomeDepressionandAnxiety, part = "header")
descriptives_outcomeDepressionandAnxiety <- add_header_lines(descriptives_outcomeDepressionandAnxiety, values = c("Outcome Level"))
descriptives_outcomeDepressionandAnxiety <- theme_vanilla(descriptives_outcomeDepressionandAnxiety)
descriptives_outcomeDepressionandAnxiety <- merge_v(descriptives_outcomeDepressionandAnxiety, j = c("Category"))
myreport <- body_add_flextable(x = myreport, descriptives_outcomeDepressionandAnxiety)
myreport <- body_add_par(x = myreport, value = "", style = "Normal")

# Descriptive Table
myreport <- body_add_par(x = myreport, value = "Table 4: Descriptive Statistics", style = "Normal")
descriptives_study <- flextable(head(table_study_df, n=nrow(table_study_df)))
descriptives_study <- add_header_lines(descriptives_study, values = c("Study Level"), top = FALSE)
descriptives_study <- theme_vanilla(descriptives_study)
descriptives_study <- merge_v(descriptives_study, j = c("Category"))
myreport <- body_add_flextable(x = myreport, descriptives_study)

descriptives_outcome <- flextable(head(table_outcome_df, n=nrow(table_outcome_df)))
descriptives_outcome <- delete_part(descriptives_outcome, part = "header")
descriptives_outcome <- add_header_lines(descriptives_outcome, values = c("Outcome Level"))
descriptives_outcome <- theme_vanilla(descriptives_outcome)
descriptives_outcome <- merge_v(descriptives_outcome, j = c("Category"))
myreport <- body_add_flextable(x = myreport, descriptives_outcome)
myreport <- body_add_par(x = myreport, value = "", style = "Normal")

########################
# MetaRegression Table
########################
MVnull.coefPositive
str(MVnull.coefPositive)
MVnull.coefPositive$coef <- row.names(as.data.frame(MVnull.coefPositive))
row.names(MVnull.coefPositive) <- c()
MVnull.coefPositive <- MVnull.coefPositive[c("Coef", "beta", "SE", "tstat", "df_Satt", "p_Satt")]
MVnull.coefPositive
str(MVnull.coefPositive)

MVnull.coefNegative
str(MVnull.coefNegative)
MVnull.coefNegative$coef <- row.names(as.data.frame(MVnull.coefNegative))
row.names(MVnull.coefNegative) <- c()
MVnull.coefNegative <- MVnull.coefNegative[c("Coef", "beta", "SE", "tstat", "df_Satt", "p_Satt")]
MVnull.coefNegative
str(MVnull.coefNegative)

MVnull.coefDepressionandAnxiety
str(MVnull.coefDepressionandAnxiety)
MVnull.coefDepressionandAnxiety$coef <- row.names(as.data.frame(MVnull.coefDepressionandAnxiety))
row.names(MVnull.coefDepressionandAnxiety) <- c()
MVnull.coefDepressionandAnxiety <- MVnull.coefDepressionandAnxiety[c("Coef", "beta", "SE", "tstat", "df_Satt", "p_Satt")]
MVnull.coefDepressionandAnxiety
str(MVnull.coefDepressionandAnxiety)

MVfull.coefDepressionandAnxiety$coef <- row.names(as.data.frame(MVfull.coefDepressionandAnxiety))
row.names(MVfull.coefDepressionandAnxiety) <- c()
MVfull.coefDepressionandAnxiety <- MVfull.coefDepressionandAnxiety[c("Coef", "beta", "SE", "tstat", "df_Satt", "p_Satt")]

# MetaRegression Table
model_nullpositive <- flextable(head(MVnull.coefPositive, n=nrow(MVnull.coefPositive)))
colkeys <- c("beta", "SE", "tstat", "df_Satt")
model_nullpositive <- colformat_double(model_nullpositive,  j = colkeys, digits = 2)
model_nullpositive <- colformat_double(model_nullpositive,  j = c("p_Satt"), digits = 3)
model_nullpositive <- add_header_lines(model_nullpositive, values = c("Null Model Positive"), top = FALSE)
model_nullpositive <- theme_vanilla(model_nullpositive)

myreport <- body_add_par(x = myreport, value = "Table 5: Model Results Positive", style = "Normal")
myreport <- body_add_flextable(x = myreport, model_nullpositive)

model_full_Positive <- flextable(head(MVfull.coef_Positive, n=nrow(MVfull.coef_Positive)))
model_full_Positive <- colformat_double(model_full_Positive,  j = c("beta"), digits = 2)
model_full_Positive <- colformat_double(model_full_Positive,  j = c("p_Satt"), digits = 3)
#model_full <- autofit(model_full)
model_full_Positive <- delete_part(model_full_Positive, part = "header")
model_full_Positive <- add_header_lines(model_full_Positive, values = c("Meta-Regression"))
model_full_Positive <- theme_vanilla(model_full_Positive)

myreport <- body_add_flextable(x = myreport, model_full_Positive)
myreport <- body_add_par(x = myreport, value = "", style = "Normal")

#Negative
model_null <- flextable(head(MVnull.coefNegative, n=nrow(MVnull.coefNegative)))
colkeys <- c("beta", "SE", "tstat", "df_Satt")
model_null <- colformat_double(model_null,  j = colkeys, digits = 2)
model_null <- colformat_double(model_null,  j = c("p_Satt"), digits = 3)
model_null <- add_header_lines(model_null, values = c("Null Model Negative"), top = FALSE)
model_null <- theme_vanilla(model_null)

myreport <- body_add_par(x = myreport, value = "Table 5: Model Results Negative", style = "Normal")
myreport <- body_add_flextable(x = myreport, model_null)

model_full_Negative <- flextable(head(MVfull.coef_Negative, n=nrow(MVfull.coef_Negative)))
model_full_Negative <- colformat_double(model_full_Negative,  j = c("beta"), digits = 2)
model_full_Negative <- colformat_double(model_full_Negative,  j = c("p_Satt"), digits = 3)
#model_full <- autofit(model_full)
model_full_Negative <- delete_part(model_full_Negative, part = "header")
model_full_Negative <- add_header_lines(model_full_Negative, values = c("Meta-Regression"))
model_full_Negative <- theme_vanilla(model_full_Negative)

myreport <- body_add_flextable(x = myreport, model_full_Negative)
myreport <- body_add_par(x = myreport, value = "", style = "Normal")
#DepressionandAnxiety
model_nullDepressionandAnxiety <- flextable(head(MVnull.coefDepressionandAnxiety, n=nrow(MVnull.coefDepressionandAnxiety)))
colkeys <- c("beta", "SE", "tstat", "df_Satt")
model_nullDepressionandAnxiety <- colformat_double(model_nullDepressionandAnxiety,  j = colkeys, digits = 2)
model_nullDepressionandAnxiety <- colformat_double(model_nullDepressionandAnxiety,  j = c("p_Satt"), digits = 3)
model_nullDepressionandAnxiety <- add_header_lines(model_nullDepressionandAnxiety, values = c("Null Model Depression and Anxiety"), top = FALSE)
model_nullDepressionandAnxiety <- theme_vanilla(model_nullDepressionandAnxiety)

myreport <- body_add_par(x = myreport, value = "Table 5: Model Results Depression and Anxiety", style = "Normal")
myreport <- body_add_flextable(x = myreport, model_nullDepressionandAnxiety)

model_fullDepressionandAnxiety <- flextable(head(MVfull.coefDepressionandAnxiety, n=nrow(MVfull.coefDepressionandAnxiety)))
model_fullDepressionandAnxiety <- colformat_double(model_fullDepressionandAnxiety,  j = c("beta"), digits = 2)
model_fullDepressionandAnxiety <- colformat_double(model_fullDepressionandAnxiety,  j = c("p_Satt"), digits = 3)
model_fullDepressionandAnxiety <- delete_part(model_fullDepressionandAnxiety, part = "header")
model_fullDepressionandAnxiety <- add_header_lines(model_fullDepressionandAnxiety, values = c("Meta-Regression"))
model_fullDepressionandAnxiety <- theme_vanilla(model_fullDepressionandAnxiety)

myreport <- body_add_flextable(x = myreport, model_fullDepressionandAnxiety)
myreport <- body_add_par(x = myreport, value = "", style = "Normal")

# Marginal Means Table
marginalmeans <- flextable(head(means, n=nrow(means)))
colkeys <- c("moderator", "group", "SE", "tstat", "df")
marginalmeans <- colformat_double(marginalmeans,  j = colkeys, digits = 2)
marginalmeans <- colformat_double(marginalmeans,  j = c("p_Satt"), digits = 3)
rm(colkeys)
marginalmeans <- theme_vanilla(marginalmeans)
marginalmeans <- merge_v(marginalmeans, j = c("moderator"))
myreport <- body_add_par(x = myreport, value = "Table: Marginal Means Positive", style = "Normal")
myreport <- body_add_flextable(x = myreport, marginalmeans)

#Negative
marginalmeans_Negative <- flextable(head(means_Negative, n=nrow(means)))
colkeys <- c("moderator", "group", "SE", "tstat", "df")
marginalmeans_Negative <- colformat_double(marginalmeans_Negative,  j = colkeys, digits = 2)
marginalmeans_Negative <- colformat_double(marginalmeans_Negative,  j = c("p_Satt"), digits = 3)
rm(colkeys)
marginalmeans_Negative <- theme_vanilla(marginalmeans_Negative)
marginalmeans_Negative <- merge_v(marginalmeans_Negative, j = c("moderator"))
myreport <- body_add_par(x = myreport, value = "Table: Marginal Means Negative", style = "Normal")
myreport <- body_add_flextable(x = myreport, marginalmeans_Negative)


# Write to word doc
file = paste("TableResults.docx", sep = "")
print(myreport, file)


#publication bias , selection modeling
full_y <- full$Effect.Size
full_v <- full$var
weightfunct(full_y, full_v)
weightfunct(full_y, full_v, steps = c(.025, .50, 1))

# set sample sizes based on median sample size (331) and existing % weights for bar plot
#            w_j = meta-analytic weight for study j (before rescaling)
#            w_j_perc = percent weight allocated to study j

########################
#schoolbased
#######################
findings <- read_excel("School_based.xlsx", sheet = "Findings")
studies <- read_excel("School_based.xlsx", sheet = "Studies")

########################################################################################################
# Clean data
########################################################################################################
# remove any empty rows & columns
studies <- subset(studies, is.na(studies$Study)==FALSE)
studies <- subset(studies, studies$Drop==0)
findings <- subset(findings, is.na(findings$Study)==FALSE)
findings <- subset(findings, findings$Drop==0)

# remove irrelevant columns
studies <- studies[c("Study", "Program name", "Age", "Grades", "Country", 
                     "Type", "N (T, C)", "Duration (weeks)", "Design", 
                     "Randomized", "Clustered", "Control", "Placebo", "Features","CBT", 
                     "Targeted Outcomes", "Program delivery", "Teacher","Notes", "WaitlistorBAU")]
findings <- findings[c("Drop", "Study", "Program name", "GradeLevel", 
                       "Universal", "Treatment.N", "Control.N", "Total.N",
                       "Treatment.Cluster", "Control.Cluster", "Unit", "Group", "Construct (D, A)", 
                       "Measure", "T_Mean_Pre", "T_SD_Pre", "C_Mean_Pre", 
                       "C_SD_Pre", "T_Mean_Post", "T_SD_Post", "C_Mean_Post", "C_SD_Post", 
                       "ReverseCoded (lower scores are better)", "Effect.Size", "Notes")]

# merge dataframes
full <- merge(studies, findings, by = c("Study", "Program name"), all = TRUE, suffixes = c(".s", ".f"))

# rename some cols to shorten
full <- plyr::rename(full, c("Construct (D, A)" = "Construct", "ReverseCoded (lower scores are better)" = "ReverseCoded", "Duration (weeks)" = "Duration.weeks"))

# recode outcomes (to eliminate internalizing)
full$Construct[which(full$Construct=="Internalizing Problems")]<-"D+A"

# remove ? from reverse-coding (go with assumed value for now while coding)
full$ReverseCoded <- gsub("\\?", "", full$ReverseCoded)

# format to correct variable types
nums <- c("Treatment.N", "Control.N", "Total.N", 
          "Treatment.Cluster", "Control.Cluster", 
          "T_Mean_Pre", "T_SD_Pre", "C_Mean_Pre", 
          "C_SD_Pre", "T_Mean_Post", "T_SD_Post", 
          "C_Mean_Post", "C_SD_Post", "ReverseCoded", 
          "Duration.weeks", "Teacher", "CBT", "Universal", 
          "Randomized", "Clustered", "WaitlistorBAU")
full[nums] <- lapply(full[nums], as.numeric)
rm(nums)

###############################################################
#Create unique identifiers (ES, study, program)
###############################################################
full$ESId <- as.numeric(rownames(full))
full$StudyID <- as.numeric(as.factor(full$Study))

########################################################################################################
# Prep data
########################################################################################################
##### Calculate ESs #####
# calculate pretest ES, SMD is standardized mean difference
full <- escalc(measure = "SMD", m1i = T_Mean_Pre, sd1i = T_SD_Pre, n1i = Treatment.N,
               m2i = C_Mean_Pre, sd2i = C_SD_Pre, n2i = Control.N, data = full)
full$vi <- NULL
full <- plyr::rename(full, c("yi" = "ES_Pre"))

# calculate posttest ES
full <- escalc(measure = "SMD", m1i = T_Mean_Post, sd1i = T_SD_Post, n1i = Treatment.N,
               m2i = C_Mean_Post, sd2i = C_SD_Post, n2i = Control.N, data = full)
full$vi <- NULL
full <- plyr::rename(full, c("yi" = "ES_Post"))

# calculate DID (post ES - pre ES)
full$ES_DID <- full$ES_Post - full$ES_Pre

# put various ES together.  Options:
## 1) used reported ES (so it should be in the Effect.Size column, nothing to do)
## 2) Effect.Size is NA, and DID not missing, replace with that
full$Effect.Size[which(is.na(full$Effect.Size)==TRUE & is.na(full$ES_DID)==FALSE)] <- full$ES_DID[which(is.na(full$Effect.Size)==TRUE & is.na(full$ES_DID)==FALSE)]
## 3) Effect.Size and DID is missing, used adjusted means (from posttest ES col), replace with that
full$Effect.Size[which(is.na(full$Effect.Size)==TRUE & is.na(full$ES_DID)==TRUE)] <- full$ES_Post[which(is.na(full$Effect.Size)==TRUE & is.na(full$ES_DID)==TRUE)]
full$Effect.Size <- as.numeric(full$Effect.Size)

###############################################################
#Calculate meta-analytic variables: Sample sizes
###############################################################
#create full sample/total clusters variables
full$Sample <- full$Treatment.N + full$Control.N
full$Cluster_Total <- full$Treatment.Cluster+full$Control.Cluster

###############################################################
#Calculate meta-analytic variables: Correct ES for clustering (Hedges, 2007, Eq.19)
###############################################################
#first, create an assumed ICC
full$icc <- NA
full$icc[which(full$Clustered == 1)] <- 0.2

#find average students/cluster
full$Treatment.Cluster.n <- NA
full$Treatment.Cluster.n[which(full$Clustered == 1)] <- round(full$Treatment.N[which(full$Clustered == 1)]/full$Treatment.Cluster[which(full$Clustered == 1)], 0)
full$Control.Cluster.n <- NA
full$Control.Cluster.n[which(full$Clustered == 1)] <- round(full$Control.N[which(full$Clustered == 1)]/full$Control.Cluster[which(full$Clustered == 1)], 0)
# find other parts of equation
full$n.TU <- NA
full$n.TU <- ((full$Treatment.N * full$Treatment.N) - (full$Treatment.Cluster * full$Treatment.Cluster.n * full$Treatment.Cluster.n))/(full$Treatment.N * (full$Treatment.Cluster - 1))
full$n.CU <- NA
full$n.CU <- ((full$Control.N * full$Control.N) - (full$Control.Cluster * full$Control.Cluster.n * full$Control.Cluster.n))/(full$Control.N * (full$Control.Cluster - 1))

# next, calculate adjusted ES, save the originals, then replace only the clustered ones
full$Effect.Size.adj <- full$Effect.Size * (sqrt(1-full$icc*(((full$Total.N-full$n.TU*full$Treatment.Cluster - full$n.CU*full$Control.Cluster)+full$n.TU + full$n.CU - 2)/(full$Total.N-2))))

# save originals, replace for clustered
full$Effect.Size.orig <- full$Effect.Size
full$Effect.Size[which(full$Clustered==1)] <- full$Effect.Size.adj[which(full$Clustered==1)]

num <- c("Effect.Size")
full[num] <- lapply(full[num], as.numeric)
rm(num)
################################################################
# Calculate meta-analytic variables: Variances (Lipsey & Wilson, 2000, Eq. 3.23)
################################################################
#calculate standard errors
full$se<-sqrt(((full$Treatment.N+full$Control.N)/(full$Treatment.N*full$Control.N))+((full$Effect.Size*full$Effect.Size)/(2*(full$Treatment.N+full$Control.N))))

#calculate variance
full$var<-full$se*full$se

################################################################
# Calculate meta-analytic variables: Correct variance for clustering (Hedges, 2007, Eq. 20)
################################################################
# first, create an assumed ICC
full$icc <- NA
full$icc[which(full$Clustered == 1)] <- 0.2   # could look this up and specify

# find average students/cluster
full$Treatment.Cluster.n <- NA
full$Treatment.Cluster.n[which(full$Clustered == 1)] <- round(full$Treatment.N[which(full$Clustered == 1)]/full$Treatment.Cluster[which(full$Clustered == 1)], 0)
full$Control.Cluster.n <- NA
full$Control.Cluster.n[which(full$Clustered == 1)] <- round(full$Control.N[which(full$Clustered == 1)]/full$Control.Cluster[which(full$Clustered == 1)], 0)

# calculate the different parts of the formula (see Hedges 2007, equation)
full$ntilde <- NA
full$ntilde <- ((full$Control.N * full$Treatment.Cluster * full$Treatment.Cluster.n * full$Treatment.Cluster.n)/(full$Treatment.N * full$Sample)) +
  ((full$Treatment.N * full$Control.Cluster * full$Control.Cluster.n * full$Control.Cluster.n)/(full$Control.N * full$Sample))

full$n.TU <- NA
full$n.TU <- ((full$Treatment.N * full$Treatment.N) - (full$Treatment.Cluster * full$Treatment.Cluster.n * full$Treatment.Cluster.n))/(full$Treatment.N * (full$Treatment.Cluster - 1))
full$n.CU <- NA
full$n.CU <- ((full$Control.N * full$Control.N) - (full$Control.Cluster * full$Control.Cluster.n * full$Control.Cluster.n))/(full$Control.N * (full$Control.Cluster - 1))

full$AT <- NA
full$AT <-((full$Treatment.N^2 * full$Treatment.Cluster * full$Treatment.Cluster.n^2) + ((full$Treatment.Cluster * full$Treatment.Cluster.n^2)^2) - (2 * full$Treatment.N * full$Treatment.Cluster.n^3 * full$Treatment.Cluster))/(full$Treatment.N^2)

full$AC <- NA
full$AC <-((full$Control.N^2 * full$Control.Cluster * full$Control.Cluster.n^2) + ((full$Control.Cluster * full$Control.Cluster.n^2)^2) - (2 * full$Control.N * full$Control.Cluster.n^3 * full$Control.Cluster))/(full$Control.N^2)

full$A <- full$AT + full$AC

full$B <- NA
full$B <- full$n.TU * (full$Treatment.Cluster - 1) + full$n.CU * (full$Control.Cluster -1)

full$var.adj <- NA
full$var.adj <- ((full$Treatment.N + full$Control.N)/(full$Treatment.N * full$Control.N)) * (1 + (full$ntilde - 1) * full$icc) + ((((full$Sample - 2)*(1-full$icc)^2) + full$A * full$icc^2 + 2*full$icc*(1-full$icc))*full$Effect.Size^2)/(2*(full$Sample-2)*((full$Sample - 2)-full$icc*(full$Sample-2-full$B)))
full$var.old <- full$var
full$var[which(full$Clustered == 1)] <- full$var.adj[which(full$Clustered == 1)]

# swaps signs for reverse-coded outcomes (so positive is good and negative is bad)
full$Effect.Size[which(is.na(full$Effect.Size))] <- full$Effect.Size.orig #This is for Lewis study
full$Effect.Size[which(full$ReverseCoded==1)] <- full$Effect.Size[which(full$ReverseCoded==1)]*-1

########################################
#meta-regression
########################################
#Null Model
V_list <- impute_covariance_matrix(vi=full$var, cluster=full$StudyID, r=0.8)

MVnull <- rma.mv(yi=Effect.Size,
                 V=V_list,
                 random=~1 | StudyID/ESId,
                 test="t",
                 data=full,
                 method="REML")
MVnull

# #t-test of each covariate#
MVnull.coef <- coef_test(MVnull, cluster=full$StudyID, vcov="CR2")
MVnull.coef

study_averages <- full %>%
  group_by(StudyID) %>%
  summarise(avg_effect_size = mean(Effect.Size, na.rm = TRUE),
            across(everything(), ~ first(.)))

forest <- robu(formula = Effect.Size ~ 1, studynum = StudyID, data = full, var.eff.size = var, rho = 0.8, small = TRUE)
forestplot <- forest.robu(forest, es.lab = "Construct", study.lab = "Study", "Effect.Size"=Effect.Size)
forestplot
metafor::forest.rma(MVnull, slab = full$Study, header = "Study", cex = 1.1, font = 6)
###########################
#forest plot
###########################
#robumeta

MVnull <- robu(formula = Effect.Size ~ 1, studynum = StudyID, data = study_averages, var.eff.size = var)
install.packages("meta")
library(meta)
m.gen <- meta::metagen(TE = Effect.Size,
                       seTE = se,
                       studlab = Study,
                       data = study_averages,
                       sm = "SMD",
                       fixed = FALSE,
                       random = TRUE,
                       method.tau = "REML",
                       method.random.ci = "HK")
summary(m.gen)
png(file = "forestplot.png", width = 3600, height = 3000, res = 300)

meta::forest(m.gen,
             sortvar = TE,
             prediction = TRUE,
             print.tau2 = FALSE,
             leftlabs = c("Author", "g", "SE"))
dev.off()
#############
#merge
DepressionandAnxiety$Online <- 1
full$Online <- 0

# Ensure both datasets have the same columns
all_columns <- union(names(DepressionandAnxiety), names(full))

# Add any missing columns to 'dataset1'
for (col in setdiff(all_columns, names(DepressionandAnxiety))) {
  DepressionandAnxiety[[col]] <- NA
}
for (col in setdiff(all_columns, names(full))) {
  full[[col]] <- NA
}
combined_dataset <- rbind(DepressionandAnxiety, full)



terms <- c("Online")
formula <- reformulate(termlabels = c(terms))

V_list <- impute_covariance_matrix(vi=combined_dataset$var, cluster=combined_dataset$StudyID, r=0.8)

MVfull <- rma.mv(yi=Effect.Size,
                          V=V_list,
                          mods=formula,
                          random=~1 | StudyID/ESId,
                          test="t",
                          data=combined_dataset,
                          method="REML")
MVfull

#t-test of each covariate#
MVfull.coef <- coef_test(MVfull, cluster=combined_dataset$StudyID, vcov="CR2")
MVfull.coef


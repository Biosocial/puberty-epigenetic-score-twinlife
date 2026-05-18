#### Title: Testing MPS puberty in Twinlife data
#### author: Yayouk Willems
#### date: 2024-2025
#### Published in Scientific Reports: https://www.nature.com/articles/s41598-025-19588-1

#### Install Packages ####
install.packages("haven")
install.packages("psych")
install.packages("ltm")
install.packages("foreign")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("openxlsx")
install.packages("tidyverse")
install.packages("ggcorrplot")
install.packages("psych")
install.packages("apaTables")
install.packages("gee")   
install.packages("geepack")
install.packages("lme4")
install.packages('lmerTest')
install.packages('apaTables')
install.packages("MuMIn")

library(MuMIn)
library(lme4)
library(lmerTest)
library(geepack)
library(gee) 
library(apaTables)
library(tidyverse)
library(ggcorrplot)
library(openxlsx)
library(haven)
library(psych)
library(ltm)
library(foreign)
library(ggplot2)
library(dplyr)

#### Read in the data ####

DataTL <- read_dta("/Users/willems/Desktop/Projects/Puberty Sister Paper/Analysis October/TwinLife_MPIB_transfer_puberty_V2.0.0_06112024.dta")
colnames(DataTL)

#rename fid_v2 and pid_v2 to fid and pid 

DataTL <- DataTL %>%
  rename(fid = fid_v2)

DataTL <- DataTL %>%
  rename(pid = pid_v2)

#check sample size MPS-puberty
#this is in the full sample, but we are mostly interested in adolescents (see descriptives below)
describe(DataTL$dnampubage_wid5)
describe(DataTL$dnampubage_wid12)

### Check Sample ####

#In Twinlife, also parents and other family members participate. 
#This datafile only includes twins
#1= first born twin, 2= second born twin
#firstborn  n=572  
#secondborn n=528
table(DataTL$ptyp)

### Check variables of interest  ####
## Create dataframe with variables of interest

df <-         DataTL[,c("wav0100",              #subsample (gen)
                        "fid",                  #family ID (scientific use file)
                        "pid",                  #person ID (scientific use file)
                        "zyg0112",              #zyg0112 recommended as zygosity variable as this is updated by the results of theDNAanalysis
                        "cgr",                  #twin birth cohort
                        "ptyp",                 #type of respondent
                        "epic_age_wid5",        #age at saliva sampling (gen)
                        "epic_age_wid12",       
                        "epic_array_wid5",      #technical covariate - array (gen)
                        "epic_array_wid12",
                        "epic_cellepi00_wid5",  #proportion epithelial cells (gen)
                        "epic_cellepi00_wid12", 
                        "epic_cellepi01_wid5",  #proportion leukocyte cells (gen)
                        "epic_cellepi01_wid12",
                        "epic_cellfib00_wid5",  #proportion fibroblast cells (gen)
                        "epic_cellfib00_wid12",
                        "epic_cellic00_wid5",   #proportion immune cell (gen)
                        "epic_cellic00_wid12",
                        "epic_cellic01_wid5",   #proportion buccal cells (gen)
                        "epic_cellic01_wid12",
                        "epic_celltyp00_wid5",  #cell type composition (gen)
                        "epic_celltyp00_wid12",
                        "epic_plate_wid5",      #technical covariate - plate (gen)
                        "epic_plate_wid12",
                        "epic_qc_wid5",         #results of quality control in epigenetic sample (gen)
                        "epic_qc_wid12",
                        "epic_tmdiff_wid5",     #years between saliva sample 1 and 2 (gen)
                        "epic_tmdiff_wid12",    #years between saliva sample 1 and 2 (gen)
                        "dnampubage_wid5",      #5 dnampubage_
                        "dnampubage_wid12",     #12 dnampubage_
                        "epic_clk00_wid5",      # Horvath multi tissue
                        "epic_clk20_wid5",      # Horvath skinblood
                        "epic_clk70_wid5",      # Phenoage
                        "epic_clk80_wid5",      # GrimAge
                        "epic_clk81_wid5",      # GrimAgev2
                        "epic_pac00_wid5",      # DunedinPace
                        "epic_clk00_wid12",      # Horvath multi tissue
                        "epic_clk20_wid12",      # Horvath skinblood
                        "epic_clk70_wid12",      # Phenoage
                        "epic_clk80_wid12",      # GrimAge
                        "epic_clk81_wid12",      # GrimAgev2
                        "epic_pac00_wid12",       # DunedinPace
                        "Epi_wid5",             #Epithelial cells
                        "Epi_wid12",
                        "Fib_wid5",             #Fibroblasts
                        "Fib_wid12",
                        "IC_wid5",              #Immune cells
                        "IC_wid12",
                        "age0101_wid1",         #age in months on the date of the family questionnaire (gen)
                        "age0101_wid3",
                        "age0101_wid5",
                        "age0101_wid7",
                        "age0101_wid12",
                        "age0201_wid1",         #age in months on 01 January of survey year of subsample (gen)
                        "age0201_wid3",
                        "age0201_wid5",
                        "age0201_wid7",
                        "age0201_wid12",
                        "bdy0200_wgt_wid1",     #weight
                        "bdy0200_wgt_wid3",
                        "bdy0200_wgt_wid5",
                        "bdy0200_wgt_wid7",
                        "bdy0100_hgt_wid1",     #height
                        "bdy0100_hgt_wid3",
                        "bdy0100_hgt_wid5",
                        "bdy0100_hgt_wid7",
                        "bdy0300_wid1",         #BMI
                        "bdy0300_wid3",        
                        "bdy0300_wid5",
                        "bdy0300_wid7",
                        "pub0100_wid3",         #puberty started: physical change (t,u,s, > 10 & <= 16 yr)
                        "pub0100_wid5",         #note! Data also contains _wid1, _wid7, _wid12, but this has no data
                        "pub0400_wid5",         #menstruation - already started (t, u, s, >= 11 & <= 16 yr), #note! Data also contains _wid1 _wid3, _wid12 but has no data
                        "pub0401_wid5",         #menstruation - age (t, u, s, >= 11) #note! Data also contains _wid1 _wid3, _wid12 but has no data
                        "pub0300_wid3",         #have you noticed changes in how you think and feel (interests / mood swings) due to puberty changes
                        "pub0300_wid5",
                        "sex",                  #sex
                        "sex_genotype")]        #sex_genotype_en


## Set missings to NA
# Note! -87 to -99 = missing for most variables. Set to NA!
# Check if this is the case for variables of interest, and set to NA if necessary

desc_df <- describe(df)  
print(desc_df)
#write.xlsx(desc_df, file = "/Users/willems/Desktop/Projects/Puberty Sister Paper/Twinlife_MPIB_transfer_16th May/Descriptives_99.xlsx", rowNames = TRUE)

# Replace missings TL to NA (see codebook for specifics for every missings coding)
# Codebook: https://www.twin-life.de/documentation/downloads

df2 <- mutate_all(df, ~replace(., . %in% c(-80, -81, -82, -83, -87, -90, -92, -93, -94, -95, -98, -99), NA))
desc_df2 <- describe(df2)  
print(desc_df2)
#write.xlsx(desc_df2, file = "/Users/willems/Desktop/Projects/Puberty Sister Paper/Twinlife_MPIB_transfer_16th May/Descriptives.xlsx", rowNames = TRUE)

#### check descriptives overall sample ####
colnames(df2)

#how many families
#n=558
unique_fid_count <- length(unique(df2$fid))
print(unique_fid_count)

#age full sample
#note that age ranges are different for puberty analyses as these have not been filled out by everyone
#note that we use data from _wid5 as this is closest to puberty measure
describe(df2$epic_age_wid12) #mean =18,2, min=10,75 max = 31,83
describe(df2$epic_age_wid5) #mean =15,87, min=8,58 max = 29,17


####  check puberty variables #### 
#all puberty items only asked in reasonable age range

#pub0100_wid3 was filled out by people between age 12.75 and 14.17, n=349, all from cohort 2 (n=349)
#puberty started: physical change 
#1: has not started yet
#2: is slowly starting
#3: has definitely started
#4: I already have the body of an adult woman/an adult man
df_wid3 <- df2[!is.na(df2$pub0100_wid3), ]
describe(df_wid3$age0101_wid3)
table(df2$pub0100_wid3)
table(df_wid3$cgr)
table(df2$pub0100_wid3)

#pub0100_wid5 was filled out by people between age 14.83 and 16.25, n=324, all from cohort 2 (n=349) 
#puberty started: physical change 
#1: has not started yet
#2: is slowly starting
#3: has definitely started
#4: I already have the body of an adult woman/an adult man
df_wid5 <- df2[!is.na(df2$pub0100_wid5), ]
describe(df_wid5$age0101_wid5)
table(df2$pub0100_wid5)
table(df_wid5$cgr)
table(df2$pub0100_wid5)

#pub0400_wid5 was filled out by people between age 14.83 and 16.25, n=199, all from cohort 2 (n=206)
#menstruation - already started
#1: yes
#2: no, I have not had a menstrual period yet
df_wid5_m <- df2[!is.na(df2$pub0400_wid5), ]
describe(df_wid5_m$age0101_wid5)
table(df_wid5_m$cgr)
table(df_wid5_m$pub0400_wid5)


#pub0401_wid5 was filled out by people between age 14.83 and 29.17, n=307, n=163 cohort 2, n=73 cohort 3, n=71 cohort 4
#menstruation - age when it started
df_wid5_m <- df2[!is.na(df2$pub0401_wid5), ]
describe(df_wid5_m$age0101_wid5)
describe(df_wid5_m$pub0401_wid5)
table(df_wid5_m$cgr)
table(df_wid5_m$pub0401_wid5)
table(df_wid5_m$pub0400_wid5)



describe(df_wid5_m %>% filter(cgr == 2) %>% pull(pub0401_wid5))

#pub0300_wid3 was filled out by people between age 14.83 and 16.2, n=249 all from cohort 2
#thoughts/emotions changed
#1: yes
#2: no
df_wid5_m <- df2[!is.na(df2$pub0300_wid3), ]
describe(df_wid5_m$age0101_wid5)
table(df_wid5_m$cgr)
table(df_wid5_m$pub0300_wid3)

#bdy0200_wgt_wid7 , n=335 in in cohort 2
df_wid5_m <- df2[!is.na(df2$bdy0200_wgt_wid7), ]
describe(df_wid5_m$age0101_wid5)


#NOTE: 
# 1) we will only use data from cohort 2 for puberty analyses as they have puberty data available
# 2) we will use _wid5 as this is the closest to DNAm measurement (also at _wid5)
# 3) for age at menarche, we will use data from cohort 2-4 as they have this measure available in girls/women



### Creating datafiles seperatly for Boys and Girls ####

#only select participants with available puberty variables:
#not using this, but just to check overall sample size
filtered_df2 <- subset(df2, !is.na(pub0100_wid3) | !is.na(pub0100_wid5) | !is.na(pub0400_wid5) | !is.na(pub0401_wid5))
check <-filtered_df2[,c("pub0100_wid3", "pub0100_wid5", "pub0400_wid5", "pub0401_wid5")]

#1=male, 2=female

# Create a data frame for boys (sex=1)
#n=166 with at least one puberty measure available
#n=494 in the total sample
#boys_df <- filtered_df2[filtered_df2$sex == 1, ]
boys_df <- df2[df2$sex == 1, ]

# Create a data frame for girls (sex=2)
#n=352 girls with at least one puberty measure available
#n=561 girls in total
#girls_df <- filtered_df2[filtered_df2$sex == 2, ]
girls_df <- df2[df2$sex == 2, ]


#create dataframe for wid5 and wid12 (cause different rows, and does not work for regressions later on)
#although later we only use _wid5 as this is closest to puberty measurement

describe(girls_df$dnampubage_wid5)
describe(boys_df$dnampubage_wid5)

girls_df_wid5  <- girls_df[!is.na(girls_df$dnampubage_wid5), ]
girls_df_wid12 <- girls_df[!is.na(girls_df$dnampubage_wid12), ]

boys_df_wid5  <- boys_df[!is.na(boys_df$dnampubage_wid5), ]
boys_df_wid12 <- boys_df[!is.na(boys_df$dnampubage_wid12), ]


#### Residualizing DNAm scores for cell composition and standardizing ####

colnames(df2)

#### For GIRLS ####

#for wave 5

#Puberty
#Residualize for cell composition and standardize dnampubertalage 
reg = lm (dnampubage_wid5 ~ 
            IC_wid5 + Epi_wid5, data=girls_df_wid5) ;
girls_df_wid5$dnampubertalage_res_cell_wid5 = residuals(reg) 
girls_df_wid5$dnampubertalage_res_cellZ_wid5 = as.numeric(scale(girls_df_wid5$dnampubertalage_res_cell_wid5))

##dnampubertalageAccel##
girls_df_wid5$dnampubertalageAccel_res_cellZ_wid5 = scale(resid(lm(girls_df_wid5$dnampubertalage_res_cellZ_wid5 ~ girls_df_wid5$epic_age_wid5)))

describe(girls_df_wid5$dnampubage_wid5)
describe(girls_df_wid5$dnampubertalage_res_cell_wid5)
describe(girls_df_wid5$dnampubertalage_res_cellZ_wid5)
describe(girls_df_wid5$dnampubertalageAccel_res_cellZ_wid5)

#Horvath Multi-Tissue: epic_clk00_wid5
reg = lm(epic_clk00_wid5 ~ IC_wid5 + Epi_wid5, data = girls_df_wid5)
girls_df_wid5$HovarthMultiTis_res_cell_wid5 = residuals(reg)
girls_df_wid5$HovarthMultiTis_res_cellZ_wid5 = as.numeric(scale(girls_df_wid5$HovarthMultiTis_res_cell_wid5))
girls_df_wid5$HovarthMultiTisAccel_res_cellZ_wid5 = scale(resid(lm(girls_df_wid5$HovarthMultiTis_res_cellZ_wid5 ~ girls_df_wid5$epic_age_wid5)))

describe(girls_df_wid5$epic_clk00_wid5)
describe(girls_df_wid5$HovarthMultiTis_res_cell_wid5)
describe(girls_df_wid5$HovarthMultiTis_res_cellZ_wid5)
describe(girls_df_wid5$HovarthMultiTisAccel_res_cellZ_wid5)

#Horvath Skin Blood:epic_clk20_wid5
reg = lm(epic_clk20_wid5 ~ IC_wid5 + Epi_wid5, data = girls_df_wid5)
girls_df_wid5$HorvatSkinBlood_res_cell_wid5 = residuals(reg)
girls_df_wid5$HorvatSkinBlood_res_cellZ_wid5 = as.numeric(scale(girls_df_wid5$HorvatSkinBlood_res_cell_wid5))
girls_df_wid5$HorvatSkinBloodAccel_res_cellZ_wid5 = scale(resid(lm(girls_df_wid5$HorvatSkinBlood_res_cellZ_wid5 ~ girls_df_wid5$epic_age_wid5)))

describe(girls_df_wid5$epic_clk20_wid5)
describe(girls_df_wid5$HorvatSkinBlood_res_cell_wid5)
describe(girls_df_wid5$HorvatSkinBlood_res_cellZ_wid5)
describe(girls_df_wid5$HorvatSkinBloodAccel_res_cellZ_wid5)

#PhenoAge:epic_clk70_wid5
reg = lm(epic_clk70_wid5 ~ IC_wid5 + Epi_wid5, data = girls_df_wid5)
girls_df_wid5$PhenoAge_res_cell_wid5 = residuals(reg)
girls_df_wid5$PhenoAge_res_cellZ_wid5 = as.numeric(scale(girls_df_wid5$PhenoAge_res_cell_wid5))
girls_df_wid5$PhenoAgeAccel_res_cellZ_wid5 = scale(resid(lm(girls_df_wid5$PhenoAge_res_cellZ_wid5 ~ girls_df_wid5$epic_age_wid5)))

describe(girls_df_wid5$epic_clk70_wid5)
describe(girls_df_wid5$PhenoAge_res_cell_wid5)
describe(girls_df_wid5$PhenoAge_res_cellZ_wid5)
describe(girls_df_wid5$PhenoAgeAccel_res_cellZ_wid5)

#GrimAge : epic_clk80_wid5
reg = lm(epic_clk80_wid5 ~ IC_wid5 + Epi_wid5, data = girls_df_wid5)
girls_df_wid5$GrimAge_res_cell_wid5 = residuals(reg)
girls_df_wid5$GrimAge_res_cellZ_wid5 = as.numeric(scale(girls_df_wid5$GrimAge_res_cell_wid5))
girls_df_wid5$GrimAgeAccel_res_cellZ_wid5 = scale(resid(lm(girls_df_wid5$GrimAge_res_cellZ_wid5 ~ girls_df_wid5$epic_age_wid5)))

describe(girls_df_wid5$epic_clk80_wid5)
describe(girls_df_wid5$GrimAge_res_cell_wid5)
describe(girls_df_wid5$GrimAge_res_cellZ_wid5)
describe(girls_df_wid5$GrimAgeAccel_res_cellZ_wid5)

#GrimAge v2 : epic_clk81_wid5
reg = lm(epic_clk81_wid5 ~ IC_wid5 + Epi_wid5, data = girls_df_wid5)
girls_df_wid5$GrimAgev2_res_cell_wid5 = residuals(reg)
girls_df_wid5$GrimAgev2_res_cellZ_wid5 = as.numeric(scale(girls_df_wid5$GrimAgev2_res_cell_wid5))
girls_df_wid5$GrimAgev2Accel_res_cellZ_wid5 = scale(resid(lm(girls_df_wid5$GrimAgev2_res_cellZ_wid5 ~ girls_df_wid5$epic_age_wid5)))

describe(girls_df_wid5$epic_clk81_wid5)
describe(girls_df_wid5$GrimAgev2_res_cell_wid5)
describe(girls_df_wid5$GrimAgev2_res_cellZ_wid5)
describe(girls_df_wid5$GrimAgev2Accel_res_cellZ_wid5)

#DunedinPace:epic_pac00_wid5
reg = lm(epic_pac00_wid5 ~ IC_wid5 + Epi_wid5, data = girls_df_wid5)
girls_df_wid5$DunedinPace_res_cell_wid5 = residuals(reg)
girls_df_wid5$DunedinPace_res_cellZ_wid5 = as.numeric(scale(girls_df_wid5$DunedinPace_res_cell_wid5))
girls_df_wid5$DunedinPaceAccel_res_cellZ_wid5 = scale(resid(lm(girls_df_wid5$DunedinPace_res_cellZ_wid5 ~ girls_df_wid5$epic_age_wid5)))

describe(girls_df_wid5$epic_pac00_wid5)
describe(girls_df_wid5$DunedinPace_res_cell_wid5)
describe(girls_df_wid5$DunedinPace_res_cellZ_wid5)
describe(girls_df_wid5$DunedinPaceAccel_res_cellZ_wid5)


#for wave 12
#Residualize for cell composition and standardize dnampubertalage 
reg = lm (dnampubage_wid12 ~ 
            IC_wid12 + Epi_wid12, data=girls_df_wid12) ;
girls_df_wid12$dnampubertalage_res_cell_wid12 = residuals(reg) 
girls_df_wid12$dnampubertalage_res_cellZ_wid12 = as.numeric(scale(girls_df_wid12$dnampubertalage_res_cell_wid12))

##dnampubertalageAccel##
girls_df_wid12$dnampubertalageAccel_res_cellZ_wid12 = scale(resid(lm(girls_df_wid12$dnampubertalage_res_cellZ_wid12 ~ girls_df_wid12$epic_age_wid12)))

describe(girls_df_wid12$dnampubage_wid12)
describe(girls_df_wid12$dnampubertalage_res_cell_wid12)
describe(girls_df_wid12$dnampubertalage_res_cellZ_wid12)
describe(girls_df_wid12$dnampubertalageAccel_res_cellZ_wid12)

# HovarthMultiTis
reg = lm(epic_clk00_wid12 ~ IC_wid12 + Epi_wid12, data = girls_df_wid12)
girls_df_wid12$HovarthMultiTis_res_cell_wid12 = residuals(reg)
girls_df_wid12$HovarthMultiTis_res_cellZ_wid12 = as.numeric(scale(girls_df_wid12$HovarthMultiTis_res_cell_wid12))
girls_df_wid12$HovarthMultiTisAccel_res_cellZ_wid12 = scale(resid(lm(girls_df_wid12$HovarthMultiTis_res_cellZ_wid12 ~ girls_df_wid12$epic_age_wid12)))

describe(girls_df_wid12$epic_clk00_wid12)
describe(girls_df_wid12$HovarthMultiTis_res_cell_wid12)
describe(girls_df_wid12$HovarthMultiTis_res_cellZ_wid12)
describe(girls_df_wid12$HovarthMultiTisAccel_res_cellZ_wid12)

# HorvatSkinBlood
reg = lm(epic_clk20_wid12 ~ IC_wid12 + Epi_wid12, data = girls_df_wid12)
girls_df_wid12$HorvatSkinBlood_res_cell_wid12 = residuals(reg)
girls_df_wid12$HorvatSkinBlood_res_cellZ_wid12 = as.numeric(scale(girls_df_wid12$HorvatSkinBlood_res_cell_wid12))
girls_df_wid12$HorvatSkinBloodAccel_res_cellZ_wid12 = scale(resid(lm(girls_df_wid12$HorvatSkinBlood_res_cellZ_wid12 ~ girls_df_wid12$epic_age_wid12)))

describe(girls_df_wid12$epic_clk20_wid12)
describe(girls_df_wid12$HorvatSkinBlood_res_cell_wid12)
describe(girls_df_wid12$HorvatSkinBlood_res_cellZ_wid12)
describe(girls_df_wid12$HorvatSkinBloodAccel_res_cellZ_wid12)

# PhenoAge
reg = lm(epic_clk70_wid12 ~ IC_wid12 + Epi_wid12, data = girls_df_wid12)
girls_df_wid12$PhenoAge_res_cell_wid12 = residuals(reg)
girls_df_wid12$PhenoAge_res_cellZ_wid12 = as.numeric(scale(girls_df_wid12$PhenoAge_res_cell_wid12))
girls_df_wid12$PhenoAgeAccel_res_cellZ_wid12 = scale(resid(lm(girls_df_wid12$PhenoAge_res_cellZ_wid12 ~ girls_df_wid12$epic_age_wid12)))

describe(girls_df_wid12$epic_clk70_wid12)
describe(girls_df_wid12$PhenoAge_res_cell_wid12)
describe(girls_df_wid12$PhenoAge_res_cellZ_wid12)
describe(girls_df_wid12$PhenoAgeAccel_res_cellZ_wid12)

# GrimAge
reg = lm(epic_clk80_wid12 ~ IC_wid12 + Epi_wid12, data = girls_df_wid12)
girls_df_wid12$GrimAge_res_cell_wid12 = residuals(reg)
girls_df_wid12$GrimAge_res_cellZ_wid12 = as.numeric(scale(girls_df_wid12$GrimAge_res_cell_wid12))
girls_df_wid12$GrimAgeAccel_res_cellZ_wid12 = scale(resid(lm(girls_df_wid12$GrimAge_res_cellZ_wid12 ~ girls_df_wid12$epic_age_wid12)))

describe(girls_df_wid12$epic_clk80_wid12)
describe(girls_df_wid12$GrimAge_res_cell_wid12)
describe(girls_df_wid12$GrimAge_res_cellZ_wid12)
describe(girls_df_wid12$GrimAgeAccel_res_cellZ_wid12)

# GrimAgev2
reg = lm(epic_clk81_wid12 ~ IC_wid12 + Epi_wid12, data = girls_df_wid12)
girls_df_wid12$GrimAgev2_res_cell_wid12 = residuals(reg)
girls_df_wid12$GrimAgev2_res_cellZ_wid12 = as.numeric(scale(girls_df_wid12$GrimAgev2_res_cell_wid12))
girls_df_wid12$GrimAgev2Accel_res_cellZ_wid12 = scale(resid(lm(girls_df_wid12$GrimAgev2_res_cellZ_wid12 ~ girls_df_wid12$epic_age_wid12)))

describe(girls_df_wid12$epic_clk81_wid12)
describe(girls_df_wid12$GrimAgev2_res_cell_wid12)
describe(girls_df_wid12$GrimAgev2_res_cellZ_wid12)
describe(girls_df_wid12$GrimAgev2Accel_res_cellZ_wid12)

# DunedinPace
reg = lm(epic_pac00_wid12 ~ IC_wid12 + Epi_wid12, data = girls_df_wid12)
girls_df_wid12$DunedinPace_res_cell_wid12 = residuals(reg)
girls_df_wid12$DunedinPace_res_cellZ_wid12 = as.numeric(scale(girls_df_wid12$DunedinPace_res_cell_wid12))
girls_df_wid12$DunedinPaceAccel_res_cellZ_wid12 = scale(resid(lm(girls_df_wid12$DunedinPace_res_cellZ_wid12 ~ girls_df_wid12$epic_age_wid12)))

describe(girls_df_wid12$epic_pac00_wid12)
describe(girls_df_wid12$DunedinPace_res_cell_wid12)
describe(girls_df_wid12$DunedinPace_res_cellZ_wid12)
describe(girls_df_wid12$DunedinPaceAccel_res_cellZ_wid12)



#create one datafile for girls (we are not using _wid12, so I did not include all the one above in further analyses)


girls_df_wid12_sel <- girls_df_wid12[,c("pid",
                                        "dnampubertalage_res_cell_wid12",
                                        "dnampubertalage_res_cellZ_wid12",
                                        "dnampubertalageAccel_res_cellZ_wid12")]

girls_df_all <- left_join(girls_df_wid5, girls_df_wid12_sel, by = "pid")

colnames(girls_df_all)




#check age of different cohorts
girls_df_all_Nomens <- subset(girls_df_all, !is.na(pub0100_wid3) | !is.na(pub0100_wid5) | !is.na(pub0400_wid5) )

describe(girls_df_all_Nomens$age0101_wid3)  #13.40
describe(girls_df_all_Nomens$age0101_wid5)  #15.44
describe(girls_df_all_Nomens$age0101_wid12) #17.68
describe(girls_df_all_Nomens$epic_age_wid12) #17.69
describe(girls_df_all_Nomens$epic_age_wid5) #15.46


girls_df_all_mens <- girls_df_all[!is.na(girls_df_all$pub0401_wid5), ]

describe(girls_df_all_mens$age0101_wid5) #19.70
describe(girls_df_all_mens$age0101_wid12) #22
describe(girls_df_all_mens$epic_age_wid12) #19.70
describe(girls_df_all_mens$epic_age_wid5) #22


#developmental covariates
girls.corr <- girls_df_all[,c("dnampubage_wid5",
                              "dnampubertalage_res_cell_wid5",
                              "dnampubertalage_res_cellZ_wid5",
                              "dnampubertalageAccel_res_cellZ_wid5",
                              "dnampubage_wid12",
                              "dnampubertalage_res_cell_wid12",
                              "dnampubertalage_res_cellZ_wid12",
                              "dnampubertalageAccel_res_cellZ_wid12",
                              "epic_age_wid5",
                              "epic_age_wid12",
                              "pub0100_wid3",
                              "pub0100_wid5",
                              "pub0400_wid5", 
                              "pub0401_wid5", 
                              "pub0300_wid3",
                              "pub0300_wid5",
                              "bdy0200_wgt_wid3",    #weight
                              "bdy0200_wgt_wid7",
                              "bdy0100_hgt_wid3",    #height
                              "bdy0300_wid3",        #BMI
                              "bdy0300_wid7")]

##### Calculate correlation matrix ####
# calculate significance levels
cormatrix <- corr.test(girls.corr, use = "pairwise.complete.obs", adjust = "none", ci = TRUE)
print(cormatrix, short=FALSE)
lowertable <- lowerCor(girls.corr, use = "pairwise.complete.obs")
print(lowertable)
#safe as apa.cor table from apaTables
#tablecor <- apa.cor.table(girls.corr, "/Users/willems/Desktop/Projects/Puberty Sister Paper/Analysis October/Correlations_girls")  



##### Main Analyses ####

#For the analyses: We regress Puberty outcome on DNAm measure, We select the DNAm puberty measure closest to the puberty measure, we correct for family membership
#We select _wid5 because: 1) these are closest to DNAm measure, 2) we do not want future DNAm to predict past Puberty


#### Physical Pubertal changes and MPS Puberty ####
#Body changes wave 5 and DNAm puberty at wave 5

model <- lmer(scale(pub0100_wid5) ~ scale(dnampubertalage_res_cell_wid5)
               + (1 |fid), data = girls_df_all) 
summary(model)
r.squaredGLMM(model)




#Body changes wave 5 and DNAm puberty acceleration at wave 5

model <- lmer(scale(pub0100_wid5) ~ dnampubertalageAccel_res_cellZ_wid5
               + (1 |fid), data = girls_df_all) 

summary(model)
r.squaredGLMM(model)

# Install the ordinal package if you haven't already
install.packages("ordinal")

# Load the ordinal package
library(ordinal)

#not sure whether it is better to fit ordinal logistic model? doing it, shows also significant effect:
# Fit the ordinal logistic mixed model
model <- clmm(as.factor(pub0100_wid5) ~ dnampubertalage_res_cell_wid5 + (1 | fid), data = girls_df_all)

# View the summary
summary(model)

#for acceleration
model <- clmm(as.factor(pub0100_wid5) ~ dnampubertalageAccel_res_cellZ_wid5 + (1 | fid), data = girls_df_all)

# View the summary
summary(model)

#in case we want to include that in the text, we report as following:
#We conducted an ordinal logistic regression to examine the relationship between dnampubertalage_res_cellZ_wid5 and the ordinal outcome pub0100_wid5, 
#controlling for random intercepts by fid. Results indicated that dnampubertalage_res_cellZ_wid5 was significantly associated with higher levels of 
#pub0100_wid5 (log odds coefficient = 0.798, SE = 0.406, p = 0.049). Specifically, for each one-unit increase in dnampubertalage_res_cellZ_wid5, 
#the odds of being in a higher severity category of pub0100_wid5 were approximately 2.22 times greater (95% CI [lower CI, upper CI], p = 0.049).






##### Age at Menstruation and MPS Puberty ####

#DNAm puberty Wave5

model <- lmer(scale(pub0401_wid5) ~ dnampubertalage_res_cellZ_wid5
              + (1 |fid), data = girls_df_all) 

summary(model)
r.squaredGLMM(model)

#DNAm puberty acceleration Wave5
model <- lmer(scale(pub0401_wid5) ~ dnampubertalageAccel_res_cellZ_wid5
              + (1 |fid), data = girls_df_all) 

summary(model)
r.squaredGLMM(model)

colnames(girls_df_all)
#Puberty and other clocks: phenoAccel
model <- lmer(scale(pub0401_wid5) ~ PhenoAgeAccel_res_cellZ_wid5
              + (1 |fid), data = girls_df_all) 

summary(model)

#Puberty and other clocks: GrimAgeAccel
model <- lmer(scale(pub0401_wid5) ~ GrimAgeAccel_res_cellZ_wid5
              + (1 |fid), data = girls_df_all) 

summary(model)

#Puberty and other clocks:DunedinPace
model <- lmer(scale(pub0401_wid5) ~ DunedinPace_res_cellZ_wid5
              + (1 |fid), data = girls_df_all) 

summary(model)


#Puberty and other clocks:Horvath1
model <- lmer(scale(pub0401_wid5) ~ HovarthMultiTisAccel_res_cellZ_wid5
              + (1 |fid), data = girls_df_all) 

summary(model)


#Puberty and other clocks:Horvath2
model <- lmer(scale(pub0401_wid5) ~ HorvatSkinBloodAccel_res_cellZ_wid5
              + (1 |fid), data = girls_df_all) 

summary(model)

#### Correlate clocks with one-another

colnames(girls_df_all)

cor.test(boys_df_wid5_noNA$PhenoAgeAccel_res_cellZ_wid5, boys_df_wid5_noNA$dnampubertalageAccel_res_cellZ_wid5)

# Load the required package
if (!requireNamespace("Hmisc", quietly = TRUE)) install.packages("Hmisc")
library(Hmisc)

# Select the variables of interest
variables <- c("dnampubertalageAccel_res_cellZ_wid5", "PhenoAgeAccel_res_cellZ_wid5", 
               "GrimAgeAccel_res_cellZ_wid5", "HovarthMultiTisAccel_res_cellZ_wid5", 
               "HorvatSkinBloodAccel_res_cellZ_wid5", "DunedinPace_res_cellZ_wid5")

# Subset the dataframe to only these variables
correlation_data <- girls_df_all[variables]

# Calculate correlation matrix with p-values
correlation_results <- rcorr(as.matrix(correlation_data), type = "pearson")

# Extract correlation coefficients
correlation_matrix <- correlation_results$r

# Extract p-values
p_values_matrix <- correlation_results$P

# Function to format correlations with significance stars
format_correlation <- function(r, p) {
  if (is.na(r)) return("")
  if (p < 0.001) {
    return(sprintf("%.2f***", r))
  } else if (p < 0.01) {
    return(sprintf("%.2f**", r))
  } else if (p < 0.05) {
    return(sprintf("%.2f*", r))
  } else {
    return(sprintf("%.2f", r))
  }
}

# Apply the formatting function to both matrices
formatted_matrix <- matrix("", nrow = nrow(correlation_matrix), ncol = ncol(correlation_matrix),
                           dimnames = dimnames(correlation_matrix))
for (i in 1:nrow(correlation_matrix)) {
  for (j in 1:ncol(correlation_matrix)) {
    if (i != j) {  # Skip self-correlations
      formatted_matrix[i, j] <- format_correlation(correlation_matrix[i, j], p_values_matrix[i, j])
    } else {
      formatted_matrix[i, j] <- "1.00***"  # Self-correlation always 1.00***
    }
  }
}

# Convert the formatted matrix to a dataframe for display
formatted_table <- as.data.frame(formatted_matrix)

# Rename the columns and rows for a cleaner look
colnames(formatted_table) <- variables
rownames(formatted_table) <- variables

# Print the final formatted table
print(formatted_table, right = FALSE, row.names = TRUE)






### Plot the relation with age of menarche
plot <- ggplot(data = girls_df_all, aes(x = dnampubertalageAccel_res_cellZ_wid5, y = pub0401_wid5)) +
  geom_point(color = "deeppink4") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Adds the line of best fit
  labs(x = "DNAmPubertalAgeAcceleration", y = "Menarche Age") +
  theme_minimal() +
  xlim(-3, 3) +
  ylim(8, 15.5)


ggsave("/Users/willems/Desktop/Projects/Puberty Sister Paper/Analysis October/plot_TwinLife.png", plot = plot, width = 9, height = 7)
ggsave("/Users/willems/Desktop/Projects/Puberty Sister Paper/Analysis October/plot_TwinLife.pdf", plot = plot, width = 9, height = 7)

#### Puberty predicted by MPS puberty and other clocks ####

#DNAm puberty acceleration Wave5
model <- lmer(scale(pub0401_wid5) ~ dnampubertalageAccel_res_cellZ_wid5 + DunedinPaceAccel_res_cellZ_wid5
              + (1 |fid), data = girls_df_all) 

summary(model)


#### Height / Weight and BMI only in the cohort that has puberty data available (that is cohort 2)
#### there is no data available for _wid5 weight/height etc so we take _wid7 (so _wid5 DNAm predicts _wid7 physical outcomes)

#Height and DNAm puberty 
# Fit the model for rows where cgr is 2
model <- lmer(scale(bdy0100_hgt_wid7) ~ dnampubertalage_res_cellZ_wid5 + (1 | fid), 
              data = subset(girls_df_all, cgr == 2))

model <- lmer(scale(bdy0100_hgt_wid7) ~ dnampubertalageAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(girls_df_all, cgr == 2))

# Display the summary of the model
summary(model)
r.squaredGLMM(model)


#Weight and DNAm puberty 
# Fit the model for rows where cgr is 2
model <- lmer(scale(bdy0200_wgt_wid7) ~ dnampubertalage_res_cellZ_wid5 + (1 | fid), 
              data = subset(girls_df_all, cgr == 2))

model <- lmer(scale(bdy0200_wgt_wid7) ~ dnampubertalageAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(girls_df_all, cgr == 2))

# Display the summary of the model
summary(model)


describe(subset(girls_df_all, cgr == 2)$bdy0200_wgt_wid7)

#BMI and DNAm puberty 
# Fit the model for rows where cgr is 2
model <- lmer(scale(bdy0300_wid7) ~ dnampubertalage_res_cellZ_wid5 + (1 | fid), 
              data = subset(girls_df_all, cgr == 2))

model <- lmer(scale(bdy0300_wid7) ~ dnampubertalageAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(girls_df_all, cgr == 2))

# Display the summary of the model
summary(model)
r.squaredGLMM(model)

describe(subset(girls_df_all, cgr == 2)$bdy0300_wid7)

#### Clocks acceleration only in the cohort that has puberty data available (that is cohort 2)

#Horvath Multi-tissue & Pubertal Accceleration
# Fit the model only in cohort 2

model <- lmer(HovarthMultiTisAccel_res_cellZ_wid5 ~ dnampubertalageAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(girls_df_all, cgr == 2))

# Display the summary of the model
summary(model)


#Horvath SkinBlood & Pubertal Accceleration
# Fit the model only in cohort 2

model <- lmer(HorvatSkinBloodAccel_res_cellZ_wid5 ~ dnampubertalageAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(girls_df_all, cgr == 2))

# Display the summary of the model
summary(model)

#GrimAge  & Pubertal Accceleration
# Fit the model only in cohort 2

model <- lmer(GrimAgeAccel_res_cellZ_wid5 ~ dnampubertalageAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(girls_df_all, cgr == 2))

# Display the summary of the model
summary(model)


#GrimAge V2  & Pubertal Accceleration
# Fit the model only in cohort 2

model <- lmer(GrimAgev2Accel_res_cellZ_wid5 ~ dnampubertalageAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(girls_df_all, cgr == 2))

# Display the summary of the model
summary(model)

#Phenoage  & Pubertal Accceleration
# Fit the model only in cohort 2

model <- lmer(PhenoAgeAccel_res_cellZ_wid5 ~ dnampubertalageAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(girls_df_all, cgr == 2))

# Display the summary of the model
summary(model)


#Dunedin  & Pubertal Accceleration
# Fit the model only in cohort 2

model <- lmer(DunedinPaceAccel_res_cellZ_wid5 ~ dnampubertalageAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(girls_df_all, cgr == 2))

# Display the summary of the model
summary(model)

#### Run associations across clocks  ####

#Horvath multitissue & with all other clocks

model <- lmer(HorvatSkinBloodAccel_res_cellZ_wid5  ~ HovarthMultiTisAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(girls_df_all, cgr == 2))

summary(model)

model <- lmer(GrimAgeAccel_res_cellZ_wid5  ~ HovarthMultiTisAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(girls_df_all, cgr == 2))

summary(model)

model <- lmer(PhenoAgeAccel_res_cellZ_wid5 ~ HovarthMultiTisAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(girls_df_all, cgr == 2))

summary(model)

model <- lmer(DunedinPaceAccel_res_cellZ_wid5 ~ HovarthMultiTisAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(girls_df_all, cgr == 2))

summary(model)


#Horvath skinblood & with all other clocks

model <- lmer(GrimAgeAccel_res_cellZ_wid5  ~ HorvatSkinBloodAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(girls_df_all, cgr == 2))

summary(model)

model <- lmer(PhenoAgeAccel_res_cellZ_wid5 ~ HorvatSkinBloodAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(girls_df_all, cgr == 2))

summary(model)

model <- lmer(DunedinPaceAccel_res_cellZ_wid5 ~ HorvatSkinBloodAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(girls_df_all, cgr == 2))

summary(model)

#Grim age with other clocks
model <- lmer(PhenoAgeAccel_res_cellZ_wid5 ~ GrimAgeAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(girls_df_all, cgr == 2))

summary(model)

model <- lmer(DunedinPaceAccel_res_cellZ_wid5 ~ GrimAgeAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(girls_df_all, cgr == 2))

summary(model)

#PhenoAge with other clocks
model <- lmer(DunedinPaceAccel_res_cellZ_wid5 ~ PhenoAgeAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(girls_df_all, cgr == 2))

summary(model)


#### For BOYS ####

#for wave 5
#Residualize for cell composition and standardize dnampubertalage 
reg = lm (dnampubage_wid5 ~ 
            IC_wid5 + Epi_wid5, data=boys_df_wid5) ;
boys_df_wid5$dnampubertalage_res_cell_wid5 = residuals(reg) 
boys_df_wid5$dnampubertalage_res_cellZ_wid5 = as.numeric(scale(boys_df_wid5$dnampubertalage_res_cell_wid5))

##dnampubertalageAccel##
boys_df_wid5$dnampubertalageAccel_res_cellZ_wid5 = scale(resid(lm(boys_df_wid5$dnampubertalage_res_cellZ_wid5 ~ boys_df_wid5$epic_age_wid5)))

describe(boys_df_wid5$dnampubage_wid5)
describe(boys_df_wid5$dnampubertalage_res_cell_wid5)
describe(boys_df_wid5$dnampubertalage_res_cellZ_wid5)
describe(boys_df_wid5$dnampubertalageAccel_res_cellZ_wid5)


#the clocks below for boys are computed in 493 and not 494 like the puberty clock. So we analyze them in 493
boys_df_wid5_noNA <- boys_df_wid5[!is.na(boys_df_wid5$epic_clk00_wid5), ]


# HovarthMultiTis
reg = lm(epic_clk00_wid5 ~ IC_wid5 + Epi_wid5, data = boys_df_wid5_noNA)
boys_df_wid5_noNA$HovarthMultiTis_res_cell_wid5 = residuals(reg)
boys_df_wid5_noNA$HovarthMultiTis_res_cellZ_wid5 = as.numeric(scale(boys_df_wid5_noNA$HovarthMultiTis_res_cell_wid5))
boys_df_wid5_noNA$HovarthMultiTisAccel_res_cellZ_wid5 = scale(resid(lm(boys_df_wid5_noNA$HovarthMultiTis_res_cellZ_wid5 ~ boys_df_wid5_noNA$epic_age_wid5)))

describe(boys_df_wid5_noNA$epic_clk00_wid5)
describe(boys_df_wid5_noNA$HovarthMultiTis_res_cell_wid5)
describe(boys_df_wid5_noNA$HovarthMultiTis_res_cellZ_wid5)
describe(boys_df_wid5_noNA$HovarthMultiTisAccel_res_cellZ_wid5)

# HorvatSkinBlood
reg = lm(epic_clk20_wid5 ~ IC_wid5 + Epi_wid5, data = boys_df_wid5_noNA)
boys_df_wid5_noNA$HorvatSkinBlood_res_cell_wid5 = residuals(reg)
boys_df_wid5_noNA$HorvatSkinBlood_res_cellZ_wid5 = as.numeric(scale(boys_df_wid5_noNA$HorvatSkinBlood_res_cell_wid5))
boys_df_wid5_noNA$HorvatSkinBloodAccel_res_cellZ_wid5 = scale(resid(lm(boys_df_wid5_noNA$HorvatSkinBlood_res_cellZ_wid5 ~ boys_df_wid5_noNA$epic_age_wid5)))

describe(boys_df_wid5_noNA$epic_clk20_wid5)
describe(boys_df_wid5_noNA$HorvatSkinBlood_res_cell_wid5)
describe(boys_df_wid5_noNA$HorvatSkinBlood_res_cellZ_wid5)
describe(boys_df_wid5_noNA$HorvatSkinBloodAccel_res_cellZ_wid5)

# PhenoAge
reg = lm(epic_clk70_wid5 ~ IC_wid5 + Epi_wid5, data = boys_df_wid5_noNA)
boys_df_wid5_noNA$PhenoAge_res_cell_wid5 = residuals(reg)
boys_df_wid5_noNA$PhenoAge_res_cellZ_wid5 = as.numeric(scale(boys_df_wid5_noNA$PhenoAge_res_cell_wid5))
boys_df_wid5_noNA$PhenoAgeAccel_res_cellZ_wid5 = scale(resid(lm(boys_df_wid5_noNA$PhenoAge_res_cellZ_wid5 ~ boys_df_wid5_noNA$epic_age_wid5)))

describe(boys_df_wid5_noNA$epic_clk70_wid5)
describe(boys_df_wid5_noNA$PhenoAge_res_cell_wid5)
describe(boys_df_wid5_noNA$PhenoAge_res_cellZ_wid5)
describe(boys_df_wid5_noNA$PhenoAgeAccel_res_cellZ_wid5)

# GrimAge
reg = lm(epic_clk80_wid5 ~ IC_wid5 + Epi_wid5, data = boys_df_wid5_noNA)
boys_df_wid5_noNA$GrimAge_res_cell_wid5 = residuals(reg)
boys_df_wid5_noNA$GrimAge_res_cellZ_wid5 = as.numeric(scale(boys_df_wid5_noNA$GrimAge_res_cell_wid5))
boys_df_wid5_noNA$GrimAgeAccel_res_cellZ_wid5 = scale(resid(lm(boys_df_wid5_noNA$GrimAge_res_cellZ_wid5 ~ boys_df_wid5_noNA$epic_age_wid5)))

describe(boys_df_wid5_noNA$epic_clk80_wid5)
describe(boys_df_wid5_noNA$GrimAge_res_cell_wid5)
describe(boys_df_wid5_noNA$GrimAge_res_cellZ_wid5)
describe(boys_df_wid5_noNA$GrimAgeAccel_res_cellZ_wid5)

# GrimAgev2
reg = lm(epic_clk81_wid5 ~ IC_wid5 + Epi_wid5, data = boys_df_wid5_noNA)
boys_df_wid5_noNA$GrimAgev2_res_cell_wid5 = residuals(reg)
boys_df_wid5_noNA$GrimAgev2_res_cellZ_wid5 = as.numeric(scale(boys_df_wid5_noNA$GrimAgev2_res_cell_wid5))
boys_df_wid5_noNA$GrimAgev2Accel_res_cellZ_wid5 = scale(resid(lm(boys_df_wid5_noNA$GrimAgev2_res_cellZ_wid5 ~ boys_df_wid5_noNA$epic_age_wid5)))

describe(boys_df_wid5_noNA$epic_clk81_wid5)
describe(boys_df_wid5_noNA$GrimAgev2_res_cell_wid5)
describe(boys_df_wid5_noNA$GrimAgev2_res_cellZ_wid5)
describe(boys_df_wid5_noNA$GrimAgev2Accel_res_cellZ_wid5)

# DunedinPace
reg = lm(epic_pac00_wid5 ~ IC_wid5 + Epi_wid5, data = boys_df_wid5_noNA)
boys_df_wid5_noNA$DunedinPace_res_cell_wid5 = residuals(reg)
boys_df_wid5_noNA$DunedinPace_res_cellZ_wid5 = as.numeric(scale(boys_df_wid5_noNA$DunedinPace_res_cell_wid5))
boys_df_wid5_noNA$DunedinPaceAccel_res_cellZ_wid5 = scale(resid(lm(boys_df_wid5_noNA$DunedinPace_res_cellZ_wid5 ~ boys_df_wid5_noNA$epic_age_wid5)))

describe(boys_df_wid5_noNA$epic_pac00_wid5)
describe(boys_df_wid5_noNA$DunedinPace_res_cell_wid5)
describe(boys_df_wid5_noNA$DunedinPace_res_cellZ_wid5)
describe(boys_df_wid5_noNA$DunedinPaceAccel_res_cellZ_wid5)


#### regressions with clocks

#### Clocks acceleration only in the cohort that has puberty data available (that is cohort 2)

# Horvath Multi-tissue & Pubertal Acceleration
# Fit the model only in cohort 2

model <- lmer(HovarthMultiTisAccel_res_cellZ_wid5 ~ dnampubertalageAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(boys_df_wid5_noNA, cgr == 2))

# Display the summary of the model
summary(model)


# Horvath SkinBlood & Pubertal Acceleration
# Fit the model only in cohort 2

model <- lmer(HorvatSkinBloodAccel_res_cellZ_wid5 ~ dnampubertalageAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(boys_df_wid5_noNA, cgr == 2))

# Display the summary of the model
summary(model)


# GrimAge & Pubertal Acceleration
# Fit the model only in cohort 2

model <- lmer(GrimAgeAccel_res_cellZ_wid5 ~ dnampubertalageAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(boys_df_wid5_noNA, cgr == 2))

# Display the summary of the model
summary(model)


# GrimAge V2 & Pubertal Acceleration
# Fit the model only in cohort 2

model <- lmer(GrimAgev2Accel_res_cellZ_wid5 ~ dnampubertalageAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(boys_df_wid5_noNA, cgr == 2))

# Display the summary of the model
summary(model)


# PhenoAge & Pubertal Acceleration
# Fit the model only in cohort 2

model <- lmer(PhenoAgeAccel_res_cellZ_wid5 ~ dnampubertalageAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(boys_df_wid5_noNA, cgr == 2))

# Display the summary of the model
summary(model)


# DunedinPace & Pubertal Acceleration
# Fit the model only in cohort 2

model <- lmer(DunedinPaceAccel_res_cellZ_wid5 ~ dnampubertalageAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(boys_df_wid5_noNA, cgr == 2))

# Display the summary of the model
summary(model)

#### Run associations across clocks ####

#Horvath multitissue & with all other clocks

model <- lmer(HorvatSkinBloodAccel_res_cellZ_wid5  ~ HovarthMultiTisAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(boys_df_wid5_noNA, cgr == 2))

summary(model)

model <- lmer(GrimAgeAccel_res_cellZ_wid5  ~ HovarthMultiTisAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(boys_df_wid5_noNA, cgr == 2))

summary(model)

model <- lmer(PhenoAgeAccel_res_cellZ_wid5 ~ HovarthMultiTisAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(boys_df_wid5_noNA, cgr == 2))

summary(model)

model <- lmer(DunedinPaceAccel_res_cellZ_wid5 ~ HovarthMultiTisAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(boys_df_wid5_noNA, cgr == 2))

summary(model)


#Horvath skinblood & with all other clocks

model <- lmer(GrimAgeAccel_res_cellZ_wid5  ~ HorvatSkinBloodAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(boys_df_wid5_noNA, cgr == 2))

summary(model)

model <- lmer(PhenoAgeAccel_res_cellZ_wid5 ~ HorvatSkinBloodAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(boys_df_wid5_noNA, cgr == 2))

summary(model)

model <- lmer(DunedinPaceAccel_res_cellZ_wid5 ~ HorvatSkinBloodAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(boys_df_wid5_noNA, cgr == 2))

summary(model)

#Grim age with other clocks
model <- lmer(PhenoAgeAccel_res_cellZ_wid5 ~ GrimAgeAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(boys_df_wid5_noNA, cgr == 2))

summary(model)

model <- lmer(DunedinPaceAccel_res_cellZ_wid5 ~ GrimAgeAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(boys_df_wid5_noNA, cgr == 2))

summary(model)

#PhenoAge with other clocks
model <- lmer(DunedinPaceAccel_res_cellZ_wid5 ~ PhenoAgeAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(boys_df_wid5_noNA, cgr == 2))

summary(model)











#for wave 12
#Residualize for cell composition and standardize dnampubertalage 
reg = lm (dnampubage_wid12 ~ 
            IC_wid12 + Epi_wid12, data=boys_df_wid12) ;
boys_df_wid12$dnampubertalage_res_cell_wid12 = residuals(reg) 
boys_df_wid12$dnampubertalage_res_cellZ_wid12 = as.numeric(scale(boys_df_wid12$dnampubertalage_res_cell_wid12))

##dnampubertalageAccel##
boys_df_wid12$dnampubertalageAccel_res_cellZ_wid12 = scale(resid(lm(boys_df_wid12$dnampubertalage_res_cellZ_wid12 ~ boys_df_wid12$epic_age_wid12)))

describe(boys_df_wid12$dnampubage_wid12)
describe(boys_df_wid12$dnampubertalage_res_cell_wid12)
describe(boys_df_wid12$dnampubertalage_res_cellZ_wid12)
describe(boys_df_wid12$dnampubertalageAccel_res_cellZ_wid12)
colnames(boys_df_wid12)

# HovarthMultiTis
reg = lm(epic_clk00_wid12 ~ IC_wid12 + Epi_wid12, data = boys_df_wid12)
boys_df_wid12$HovarthMultiTis_res_cell_wid12 = residuals(reg)
boys_df_wid12$HovarthMultiTis_res_cellZ_wid12 = as.numeric(scale(boys_df_wid12$HovarthMultiTis_res_cell_wid12))
boys_df_wid12$HovarthMultiTisAccel_res_cellZ_wid12 = scale(resid(lm(boys_df_wid12$HovarthMultiTis_res_cellZ_wid12 ~ boys_df_wid12$epic_age_wid12)))

describe(boys_df_wid12$epic_clk00_wid12)
describe(boys_df_wid12$HovarthMultiTis_res_cell_wid12)
describe(boys_df_wid12$HovarthMultiTis_res_cellZ_wid12)
describe(boys_df_wid12$HovarthMultiTisAccel_res_cellZ_wid12)

# HorvatSkinBlood
reg = lm(epic_clk20_wid12 ~ IC_wid12 + Epi_wid12, data = boys_df_wid12)
boys_df_wid12$HorvatSkinBlood_res_cell_wid12 = residuals(reg)
boys_df_wid12$HorvatSkinBlood_res_cellZ_wid12 = as.numeric(scale(boys_df_wid12$HorvatSkinBlood_res_cell_wid12))
boys_df_wid12$HorvatSkinBloodAccel_res_cellZ_wid12 = scale(resid(lm(boys_df_wid12$HorvatSkinBlood_res_cellZ_wid12 ~ boys_df_wid12$epic_age_wid12)))

describe(boys_df_wid12$epic_clk20_wid12)
describe(boys_df_wid12$HorvatSkinBlood_res_cell_wid12)
describe(boys_df_wid12$HorvatSkinBlood_res_cellZ_wid12)
describe(boys_df_wid12$HorvatSkinBloodAccel_res_cellZ_wid12)

# PhenoAge
reg = lm(epic_clk70_wid12 ~ IC_wid12 + Epi_wid12, data = boys_df_wid12)
boys_df_wid12$PhenoAge_res_cell_wid12 = residuals(reg)
boys_df_wid12$PhenoAge_res_cellZ_wid12 = as.numeric(scale(boys_df_wid12$PhenoAge_res_cell_wid12))
boys_df_wid12$PhenoAgeAccel_res_cellZ_wid12 = scale(resid(lm(boys_df_wid12$PhenoAge_res_cellZ_wid12 ~ boys_df_wid12$epic_age_wid12)))

describe(boys_df_wid12$epic_clk70_wid12)
describe(boys_df_wid12$PhenoAge_res_cell_wid12)
describe(boys_df_wid12$PhenoAge_res_cellZ_wid12)
describe(boys_df_wid12$PhenoAgeAccel_res_cellZ_wid12)

# GrimAge
reg = lm(epic_clk80_wid12 ~ IC_wid12 + Epi_wid12, data = boys_df_wid12)
boys_df_wid12$GrimAge_res_cell_wid12 = residuals(reg)
boys_df_wid12$GrimAge_res_cellZ_wid12 = as.numeric(scale(boys_df_wid12$GrimAge_res_cell_wid12))
boys_df_wid12$GrimAgeAccel_res_cellZ_wid12 = scale(resid(lm(boys_df_wid12$GrimAge_res_cellZ_wid12 ~ boys_df_wid12$epic_age_wid12)))

describe(boys_df_wid12$epic_clk80_wid12)
describe(boys_df_wid12$GrimAge_res_cell_wid12)
describe(boys_df_wid12$GrimAge_res_cellZ_wid12)
describe(boys_df_wid12$GrimAgeAccel_res_cellZ_wid12)

# GrimAgev2
reg = lm(epic_clk81_wid12 ~ IC_wid12 + Epi_wid12, data = boys_df_wid12)
boys_df_wid12$GrimAgev2_res_cell_wid12 = residuals(reg)
boys_df_wid12$GrimAgev2_res_cellZ_wid12 = as.numeric(scale(boys_df_wid12$GrimAgev2_res_cell_wid12))
boys_df_wid12$GrimAgev2Accel_res_cellZ_wid12 = scale(resid(lm(boys_df_wid12$GrimAgev2_res_cellZ_wid12 ~ boys_df_wid12$epic_age_wid12)))

describe(boys_df_wid12$epic_clk81_wid12)
describe(boys_df_wid12$GrimAgev2_res_cell_wid12)
describe(boys_df_wid12$GrimAgev2_res_cellZ_wid12)
describe(boys_df_wid12$GrimAgev2Accel_res_cellZ_wid12)

# DunedinPace
reg = lm(epic_pac00_wid12 ~ IC_wid12 + Epi_wid12, data = boys_df_wid12)
boys_df_wid12$DunedinPace_res_cell_wid12 = residuals(reg)
boys_df_wid12$DunedinPace_res_cellZ_wid12 = as.numeric(scale(boys_df_wid12$DunedinPace_res_cell_wid12))
boys_df_wid12$DunedinPaceAccel_res_cellZ_wid12 = scale(resid(lm(boys_df_wid12$DunedinPace_res_cellZ_wid12 ~ boys_df_wid12$epic_age_wid12)))

describe(boys_df_wid12$epic_pac00_wid12)
describe(boys_df_wid12$DunedinPace_res_cell_wid12)
describe(boys_df_wid12$DunedinPace_res_cellZ_wid12)
describe(boys_df_wid12$DunedinPaceAccel_res_cellZ_wid12)




#Merging data _wid5 and_wid12 (we mostly focus on _wid5 so the _wid12 scales are not included here)



boys_df_wid12_sel <- boys_df_wid12[,c("pid",
                                      "dnampubertalage_res_cell_wid12",
                                      "dnampubertalage_res_cellZ_wid12",
                                      "dnampubertalageAccel_res_cellZ_wid12")]

boys_df_all <- left_join(boys_df_wid5, boys_df_wid12_sel, by = "pid")


describe(boys_df_all$dnampubage_wid5)

describe(girls_df_all$dnampubage_wid5)

#developmental covariates
boys.corr <- boys_df_all[,c("dnampubage_wid5",
                            "dnampubertalage_res_cell_wid5",
                            "dnampubertalage_res_cellZ_wid5",
                            "dnampubertalageAccel_res_cellZ_wid5",
                            "dnampubage_wid12",
                            "dnampubertalage_res_cell_wid12",
                            "dnampubertalage_res_cellZ_wid12",
                            "dnampubertalageAccel_res_cellZ_wid12",
                            "epic_age_wid5",
                            "epic_age_wid12",
                            "pub0100_wid3",
                            "pub0100_wid5",
                            "pub0300_wid3",
                            "pub0300_wid5",
                            "bdy0200_wgt_wid3",    #weight
                            "bdy0200_wgt_wid7",
                            "bdy0100_hgt_wid3",    #height
                            "bdy0300_wid3",        #BMI
                            "bdy0300_wid7")]

# calculate correlation matrix
# calculate significance levels
cormatrix <- corr.test(boys.corr, use = "pairwise.complete.obs", adjust = "none", ci = TRUE)
print(cormatrix, short=FALSE)
lowertable <- lowerCor(boys.corr, use = "pairwise.complete.obs")
print(lowertable)
#safe as apa.cor table from apaTables
#tablecor <- apa.cor.table(boys.corr, "/Users/willems/Desktop/Projects/Puberty Sister Paper/Twinlife_MPIB_transfer_16th May/Correlations_boys") 



#### Physical Pubertal changes and MPS Puberty ####

#Body changes wave 5 and DNAm puberty at wave 5

model <- lmer(scale(pub0100_wid5) ~ dnampubertalage_res_cellZ_wid5
              + (1 |fid), data = boys_df_all) 
summary(model)
r.squaredGLMM(model)

#Body changes wave 5 and DNAm puberty acceleration at wave 5

model <- lmer(scale(pub0100_wid5) ~ dnampubertalageAccel_res_cellZ_wid5
              + (1 |fid), data = boys_df_all) 

summary(model)
r.squaredGLMM(model)


#### Height / Weight and BMI only in the cohort that has puberty data available (that is cohort 2)
#### there is no data available for _wid5 weight/height etc so we take _wid7 (so _wid5 DNAm predicts _wid7 physical outcomes)

#Height and DNAm puberty 
# Fit the model for rows where cgr is 2
model <- lmer(scale(bdy0100_hgt_wid7) ~ dnampubertalage_res_cellZ_wid5 + (1 | fid), 
              data = subset(boys_df_all, cgr == 2))

model <- lmer(scale(bdy0100_hgt_wid7) ~ dnampubertalageAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(boys_df_all, cgr == 2))

# Display the summary of the model
summary(model)
r.squaredGLMM(model)


#Weight and DNAm puberty 
# Fit the model for rows where cgr is 2
model <- lmer(scale(bdy0200_wgt_wid7) ~ dnampubertalage_res_cellZ_wid5 + (1 | fid), 
              data = subset(boys_df_all, cgr == 2))

model <- lmer(scale(bdy0200_wgt_wid7) ~ dnampubertalageAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(boys_df_all, cgr == 2))

# Display the summary of the model
summary(model)




#BMI and DNAm puberty 
# Fit the model for rows where cgr is 2
model <- lmer(scale(bdy0300_wid7) ~ dnampubertalage_res_cellZ_wid5 + (1 | fid), 
              data = subset(boys_df_all, cgr == 2))

model <- lmer(scale(bdy0300_wid7) ~ dnampubertalageAccel_res_cellZ_wid5 + (1 | fid), 
              data = subset(boys_df_all, cgr == 2))

# Display the summary of the model
summary(model)
r.squaredGLMM(model)



#### rerun in total sample?

# Fit the model for rows where for all adolsecnets (>9 and <18)
model <- lmer(scale(bdy0300_wid7) ~ dnampubertalage_res_cellZ_wid5 + (1 | fid), 
              data = subset(girls_df_all, epic_age_wid5 <= 18 & epic_age_wid5 > 9 ))

# Fit the model for rows where for all adolsecnets (>9 and <18)
model <- lmer(scale(bdy0300_wid7) ~ dnampubertalage_res_cellZ_wid5 + (1 | fid), 
              data = subset(boys_df_all, epic_age_wid5 <= 18 & epic_age_wid5 > 9 ))

# Display the summary of the model
summary(model)



#### Run correlations between clocks for boys
colnames(boys_df_wid5_noNA)



# Select the variables of interest
variables <- c("dnampubertalageAccel_res_cellZ_wid5", "PhenoAgeAccel_res_cellZ_wid5", 
               "GrimAgeAccel_res_cellZ_wid5", "HovarthMultiTisAccel_res_cellZ_wid5", 
               "HorvatSkinBloodAccel_res_cellZ_wid5", "DunedinPace_res_cellZ_wid5")

# Subset the dataframe to only these variables
correlation_data <- boys_df_wid5_noNA[variables]

# Calculate correlation matrix with p-values
correlation_results <- rcorr(as.matrix(correlation_data), type = "pearson")

# Extract correlation coefficients
correlation_matrix <- correlation_results$r

# Extract p-values
p_values_matrix <- correlation_results$P

# Function to format correlations with significance stars
format_correlation <- function(r, p) {
  if (is.na(r)) return("")
  if (p < 0.001) {
    return(sprintf("%.2f***", r))
  } else if (p < 0.01) {
    return(sprintf("%.2f**", r))
  } else if (p < 0.05) {
    return(sprintf("%.2f*", r))
  } else {
    return(sprintf("%.2f", r))
  }
}

# Apply the formatting function to both matrices
formatted_matrix <- matrix("", nrow = nrow(correlation_matrix), ncol = ncol(correlation_matrix),
                           dimnames = dimnames(correlation_matrix))
for (i in 1:nrow(correlation_matrix)) {
  for (j in 1:ncol(correlation_matrix)) {
    if (i != j) {  # Skip self-correlations
      formatted_matrix[i, j] <- format_correlation(correlation_matrix[i, j], p_values_matrix[i, j])
    } else {
      formatted_matrix[i, j] <- "1.00***"  # Self-correlation always 1.00***
    }
  }
}

# Convert the formatted matrix to a dataframe for display
formatted_table <- as.data.frame(formatted_matrix)

# Rename the columns and rows for a cleaner look
colnames(formatted_table) <- variables
rownames(formatted_table) <- variables

# Print the final formatted table
print(formatted_table, right = FALSE, row.names = TRUE)






#### Descriptives Full sample, and Boys and Girls, for all measures of interest ####

#Overall descriptives of Cohort 2 (boys and girls together)
describe(subset(df2, cgr == 2)$epic_age_wid5)
table(subset(df2, cgr == 2)$sex)
describe(subset(df2, cgr == 2)$dnampubage_wid5)

#Overall descriptives of Cohort 2,3 and 4 (boys and girls together)
describe(subset(df2, cgr %in% c(2, 3, 4))$epic_age_wid5)
table(subset(df2, cgr %in% c(2, 3, 4))$sex)
describe(subset(df2, cgr %in% c(2, 3, 4))$dnampubage_wid5)

#Descriptives of measures of interest of Cohort 2 (Body changes, Height,Weight,BMI) only in Cohort 2

#girls
describe(subset(df2, cgr == 2 & sex == 2)$epic_age_wid5) #age
describe(subset(df2, cgr == 2 & sex == 2)$pub0100_wid5)  #physical changes
describe(subset(df2, cgr == 2 & sex == 2)$bdy0100_hgt_wid7) #height
describe(subset(df2, cgr == 2 & sex == 2)$bdy0200_wgt_wid7) #weight
describe(subset(df2, cgr == 2 & sex == 2)$bdy0300_wid7) #bmi

describe(subset(df2, cgr == 2 & sex == 2)$dnampubage_wid5) #DNAm puberty
describe(subset(df2, cgr == 2 & sex == 2)$epic_clk00_wid5) #Horvath multi tissue
describe(subset(df2, cgr == 2 & sex == 2)$epic_clk20_wid5) #HorvatSkinBlood
describe(subset(df2, cgr == 2 & sex == 2)$epic_clk80_wid5) #GrimAge
describe(subset(df2, cgr == 2 & sex == 2)$epic_clk81_wid5) #GrimAgeV2
describe(subset(df2, cgr == 2 & sex == 2)$epic_clk70_wid5) #PhenoAge
describe(subset(df2, cgr == 2 & sex == 2)$epic_pac00_wid5) #DunedinPACE


describe(subset(girls_df_all, cgr == 2)$dnampubertalage_res_cell_wid5) #DNAm residualized for cell type
describe(subset(girls_df_all, cgr == 2)$dnampubertalageAccel_res_cellZ_wid5) #DNAm residualized for cell type and age
describe(subset(girls_df_all, cgr == 2)$dnampubertalageAccel_res_cellZ_wid5) #DNAm residualized for cell type and age
describe(subset(girls_df_all, cgr == 2)$HovarthMultiTisAccel_res_cellZ_wid5) #Horvath multi tissue 
describe(subset(girls_df_all, cgr == 2)$HorvatSkinBloodAccel_res_cellZ_wid5) #HorvatSkinBlood
describe(subset(girls_df_all, cgr == 2)$GrimAgeAccel_res_cellZ_wid5) #GrimAge
describe(subset(girls_df_all, cgr == 2)$GrimAgev2Accel_res_cellZ_wid5) #GrimAgeV2
describe(subset(girls_df_all, cgr == 2)$PhenoAgeAccel_res_cellZ_wid5) #PhenoAge
describe(subset(girls_df_all, cgr == 2)$DunedinPaceAccel_res_cellZ_wid5) #DunedinPACE


#Descriptives of measures Cohort 2-4 (Age of Menarche)
describe(subset(df2, cgr %in% c(2, 3, 4) & !is.na(pub0401_wid5))$epic_age_wid5)
describe(subset(df2, cgr %in% c(2, 3, 4) & !is.na(pub0401_wid5))$pub0401_wid5)
describe(subset(df2, cgr %in% c(2, 3, 4) & !is.na(pub0401_wid5))$dnampubage_wid5)
describe(subset(girls_df_all, cgr %in% c(2, 3, 4) & !is.na(pub0401_wid5))$dnampubertalage_res_cell_wid5)
describe(subset(girls_df_all, cgr %in% c(2, 3, 4) & !is.na(pub0401_wid5))$dnampubertalageAccel_res_cellZ_wid5)
describe(subset(girls_df_all, cgr %in% c(2, 3, 4) & !is.na(pub0401_wid5))$bdy0100_hgt_wid7)
describe(subset(girls_df_all, cgr %in% c(2, 3, 4) & !is.na(pub0401_wid5))$bdy0300_wid7)

#boys
describe(subset(df2, cgr == 2 & sex == 1)$epic_age_wid5) #age
describe(subset(df2, cgr == 2 & sex == 1)$pub0100_wid5)  #physical changes
describe(subset(df2, cgr == 2 & sex == 1)$bdy0100_hgt_wid7) #height
describe(subset(df2, cgr == 2 & sex == 1)$bdy0200_wgt_wid7) #weight
describe(subset(df2, cgr == 2 & sex == 1)$bdy0300_wid7) #bmi

describe(subset(df2, cgr == 2 & sex == 1)$dnampubage_wid5) #DNAm
describe(subset(df2, cgr == 2 & sex == 1)$epic_clk00_wid5) #Horvath multi tissue
describe(subset(df2, cgr == 2 & sex == 1)$epic_clk20_wid5) #HorvatSkinBlood
describe(subset(df2, cgr == 2 & sex == 1)$epic_clk80_wid5) #GrimAge
describe(subset(df2, cgr == 2 & sex == 1)$epic_clk81_wid5) #GrimAgeV2
describe(subset(df2, cgr == 2 & sex == 1)$epic_clk70_wid5) #PhenoAge
describe(subset(df2, cgr == 2 & sex == 1)$epic_pac00_wid5) #DunedinPACE


describe(subset(boys_df_all, cgr == 2)$dnampubertalage_res_cell_wid5) #DNAm residualized for cell type
describe(subset(boys_df_all, cgr == 2)$dnampubertalageAccel_res_cellZ_wid5) #DNAm residualized for cell type and age
describe(subset(boys_df_wid5_noNA, cgr == 2)$HovarthMultiTisAccel_res_cellZ_wid5) #Horvath multi tissue 
describe(subset(boys_df_wid5_noNA, cgr == 2)$HorvatSkinBloodAccel_res_cellZ_wid5) #HorvatSkinBlood
describe(subset(boys_df_wid5_noNA, cgr == 2)$GrimAgeAccel_res_cellZ_wid5) #GrimAge
describe(subset(boys_df_wid5_noNA, cgr == 2)$GrimAgev2Accel_res_cellZ_wid5) #GrimAgeV2
describe(subset(boys_df_wid5_noNA, cgr == 2)$PhenoAgeAccel_res_cellZ_wid5) #PhenoAge
describe(subset(boys_df_wid5_noNA, cgr == 2)$DunedinPaceAccel_res_cellZ_wid5) #DunedinPACE



##### Safe all datasets #####

write.csv(df2, "/Users/willems/Desktop/Projects/Puberty Sister Paper/Analysis October/fulldata.csv", row.names = FALSE)
write.csv(girls_df_all, "/Users/willems/Desktop/Projects/Puberty Sister Paper/Analysis October/girls_pubertyclocks.csv", row.names = FALSE)
write.csv(boys_df_all, "/Users/willems/Desktop/Projects/Puberty Sister Paper/Analysis October/boys_pubertyclocks.csv", row.names = FALSE)
write.csv(boys_df_wid5_noNA, "/Users/willems/Desktop/Projects/Puberty Sister Paper/Analysis October/boys_epiclocks.csv", row.names = FALSE)

girls_df_all <- read.csv("/Users/willems/Desktop/Projects/Puberty Sister Paper/Analysis October/girls_pubertyclocks.csv", header = TRUE)
boys_df_all <- read.csv("/Users/willems/Desktop/Projects/Puberty Sister Paper/Analysis October/boys_pubertyclocks.csv", header = TRUE)
boys_df_wid5_noNA <- read.csv("/Users/willems/Desktop/Projects/Puberty Sister Paper/Analysis October/boys_epiclocks.csv", header = TRUE)
  
colnames(girls_df_all)  

#### Twin Correlations ####

#Previously, I created data seperatly for boys and girls. We either re-create residualized DNAm measure on full sample (option 1) or merge boys and girls sample (option 2)

# Option 1:

#Residualize for cell composition and standardize dnampubertalage 
reg = lm (dnampubage_wid5 ~ 
            IC_wid5 + Epi_wid5, data=df2) ;
df2$dnampubertalage_res_cell_wid5 = residuals(reg) 
df2$dnampubertalage_res_cellZ_wid5 = as.numeric(scale(df2$dnampubertalage_res_cell_wid5))

##dnampubertalageAccel##
df2$dnampubertalageAccel_res_cellZ_wid5 = scale(resid(lm(df2$dnampubertalage_res_cellZ_wid5 ~ df2$epic_age_wid5)))

describe(df2$dnampubage_wid5)
describe(df2$dnampubertalage_res_cell_wid5)
describe(df2$dnampubertalage_res_cellZ_wid5)
describe(df2$dnampubertalageAccel_res_cellZ_wid5)

# Select rows where cgr == 2 (cause that is what we base our analyses on)
data_pretwin <- df2 %>%
  filter(cgr == 2)

#data_pretwin <- df2

#create wide dataset to correlate values twin 1 and twin 2

zygo_dat_wide <- data_pretwin %>%
  pivot_wider(
    id_cols = fid,          # Group by family ID
    names_from = ptyp,           # Use twin_id to create new columns
    values_from = c(zyg0112,sex, dnampubertalage_res_cellZ_wid5, dnampubertalageAccel_res_cellZ_wid5),      
    names_prefix = ""           # Prefix for the new columns
  ) %>%
  as.data.frame()


#version 2

girls_pretwin <- girls_df_all[,c("fid",
                                 "pid",
                                 "zyg0112",
                                 "cgr",
                                 "ptyp",
                                 "sex",
                                 "dnampubertalage_res_cellZ_wid5",
                                 "dnampubertalageAccel_res_cellZ_wid5")]
                                 
                                 
boys_pretwin <- boys_df_all[,c("fid",
                                 "pid",
                                 "zyg0112",
                                 "cgr",
                                 "ptyp",
                                 "sex",
                                 "dnampubertalage_res_cellZ_wid5",
                                 "dnampubertalageAccel_res_cellZ_wid5")]                               

data_pretwin <- rbind(girls_pretwin, boys_pretwin)

# Select rows where cgr == 2 (cause that is what we base our analyses on)
data_pretwin <- data_pretwin %>%
  filter(cgr == 2)

data_pretwin <- data_pretwin %>%
  filter(cgr %in% c(2, 3, 4))

zygo_dat_wide <- data_pretwin %>%
  pivot_wider(
    id_cols = fid,          # Group by family ID
    names_from = ptyp,           # Use twin_id to create new columns
    values_from = c(zyg0112,sex, dnampubertalage_res_cellZ_wid5, dnampubertalageAccel_res_cellZ_wid5),      
    names_prefix = ""           # Prefix for the new columns
  ) %>%
  as.data.frame()


#version 3 , do it seperate for boys and girls


data_pretwin <- girls_df_all %>%
  filter(cgr == 2)

#data_pretwin <- girls_df_all %>%
#  filter(cgr %in% c(2, 3, 4))

zygo_dat_wide <- data_pretwin %>%
  pivot_wider(
    id_cols = fid,          # Group by family ID
    names_from = ptyp,           # Use twin_id to create new columns
    values_from = c(zyg0112, sex, dnampubertalage_res_cellZ_wid5, dnampubertalageAccel_res_cellZ_wid5),      
    names_prefix = ""           # Prefix for the new columns
  ) %>%
  as.data.frame()

# Calculate the correlation for zyg0112_1 == 1
MZ <- zygo_dat_wide %>%
  filter(zyg0112_1 == 1) %>%
  summarise(cor = cor(dnampubertalage_res_cellZ_wid5_1, dnampubertalage_res_cellZ_wid5_2, use = "complete.obs")) %>%
  pull(cor)

# Calculate the correlation for zyg0112_1 == 2
DZ <- zygo_dat_wide %>%
  filter(zyg0112_1 == 2) %>%
  summarise(cor = cor(dnampubertalage_res_cellZ_wid5_1, dnampubertalage_res_cellZ_wid5_2, use = "complete.obs")) %>%
  pull(cor)

# Print correlations
print(MZ)
print(DZ)

data_pretwin <- boys_df_all %>%
  filter(cgr == 2)

zygo_dat_wide <- data_pretwin %>%
  pivot_wider(
    id_cols = fid,          # Group by family ID
    names_from = ptyp,           # Use twin_id to create new columns
    values_from = c(zyg0112,sex, dnampubertalage_res_cellZ_wid5, dnampubertalageAccel_res_cellZ_wid5),      
    names_prefix = ""           # Prefix for the new columns
  ) %>%
  as.data.frame()


# Calculate the correlation for zyg0112_1 == 1
MZ <- zygo_dat_wide %>%
  filter(zyg0112_1 == 1) %>%
  summarise(cor = cor(dnampubertalage_res_cellZ_wid5_1, dnampubertalage_res_cellZ_wid5_2, use = "complete.obs")) %>%
  pull(cor)

# Calculate the correlation for zyg0112_1 == 2
DZ <- zygo_dat_wide %>%
  filter(zyg0112_1 == 2) %>%
  summarise(cor = cor(dnampubertalage_res_cellZ_wid5_1, dnampubertalage_res_cellZ_wid5_2, use = "complete.obs")) %>%
  pull(cor)

# Print correlations
print(MZ)
print(DZ)






#### Calculate twin correlations

# Function to calculate correlation, p-value, and confidence intervals for each subgroup
calculate_correlation <- function(data, zyg_val, sex_val) {
  # Filter the dataset for the given zyg_val and sex_val
  filtered_data <- data %>%
    filter(zyg0112_1 == zyg_val, sex_1 == sex_val)
  
  # Check if there are at least two rows to compute correlation
  if (nrow(filtered_data) > 1) {
    # Perform correlation test
    test <- cor.test(
      filtered_data$dnampubertalageAccel_res_cellZ_wid5_1,
      filtered_data$dnampubertalageAccel_res_cellZ_wid5_2,
      use = "complete.obs"
    )
    # Return correlation, p-value, and confidence interval
    return(list(
      correlation = test$estimate,
      p_value = test$p.value,
      conf_int = test$conf.int
    ))
  } else {
    # Return NA if insufficient data
    return(list(
      correlation = NA,
      p_value = NA,
      conf_int = c(NA, NA)
    ))
  }
}

# Calculate correlations, p-values, and confidence intervals for each subgroup
MZm <- calculate_correlation(zygo_dat_wide, 1, 1) # zyg0112_1 == 1, sex_1 == 1
MZf <- calculate_correlation(zygo_dat_wide, 1, 2) # zyg0112_1 == 1, sex_1 == 2
DZm <- calculate_correlation(zygo_dat_wide, 2, 1) # zyg0112_1 == 2, sex_1 == 1
DZf <- calculate_correlation(zygo_dat_wide, 2, 2) # zyg0112_1 == 2, sex_1 == 2

# Print results for each group
print(MZm)
print(DZm)

print(MZf)
print(DZf)


#double check MZm and DZm cors

#MZM

filtered_data <- zygo_dat_wide %>%
  filter(zyg0112_1 == 1 & sex_1 == 1 & zyg0112_2 == 1 & sex_2 == 1)

cor.test(filtered_data$dnampubertalageAccel_res_cellZ_wid5_1, filtered_data$dnampubertalageAccel_res_cellZ_wid5_2)

#DZM
filtered_data <- zygo_dat_wide %>%
  filter(zyg0112_1 == 2 & sex_1 == 1 & zyg0112_2 == 2 & sex_2 == 1)

cor.test(filtered_data$dnampubertalageAccel_res_cellZ_wid5_1, filtered_data$dnampubertalageAccel_res_cellZ_wid5_2)

#MZF
filtered_data <- zygo_dat_wide %>%
  filter(zyg0112_1 == 1 & sex_1 == 2 & zyg0112_2 == 1 & sex_2 == 2)

cor.test(filtered_data$dnampubertalageAccel_res_cellZ_wid5_1, filtered_data$dnampubertalageAccel_res_cellZ_wid5_2)

#DZF
filtered_data <- zygo_dat_wide %>%
  filter(zyg0112_1 == 2 & sex_1 == 2 & zyg0112_2 == 2 & sex_2 == 2)

cor.test(filtered_data$dnampubertalageAccel_res_cellZ_wid5_1, filtered_data$dnampubertalageAccel_res_cellZ_wid5_2)


### Association Age and Puberty MPSs with Age at Menarche ####

install.packages("readxl")  # Install the package if not already installed
library(readxl)

#### Plots effect sizes
#### Create plots for associations Aging and Puberty MPSs with Age at Menarche ####

install.packages("readxl")  # Install the package if not already installed
library(readxl)

#### Plots effect sizes

load("~/Desktop/Projects/Puberty Sister Paper/Analysis October/ttp_menarche_mps.rda")  #ttp
load("~/Desktop/Projects/Puberty Sister Paper/Analysis October/ffcw_menarche_mps.rda") #ffcw
TwinLife_results <- read_excel("~/Desktop/Projects/Puberty Sister Paper/Analysis October/associations clocks and age at menarche.xlsx") #twinlife
TwinLife_results <- as.data.frame(TwinLife_results)

#Check and change datafiles so they have same names and same columns 

#for Texas Twins
colnames(ttp_results)
head(ttp_results)

colnames(ttp_results)[1] <- "Clock"
ttp_merge <- ttp_results[,c("Clock", "Estimate", "CI_lower", "CI_upper")]
ttp_merge$Cohort <- "TTP"

#for Future Families
colnames(ffcw_results)
head(ffcw_results)

colnames(ffcw_results)[1] <- "Clock"
FFCW_merge <- ffcw_results[, c("Clock", "Estimate", "CI_lower", "CI_upper")]
FFCW_merge$Cohort <- "FFCW"

# Define the desired order of the Clock variable
right_order <- c("DNAmPubertalAgeAccel", "GrimAgeAccel", "PhenoAgeAccel",
                 "DunedinPACE", "Horvath1Accel", "Horvath2Accel")

# Reorder the Clock column in FFCW_merge
FFCW_merge <- FFCW_merge %>%
  mutate(Clock = factor(Clock, levels = right_order)) %>%
  arrange(Clock)

# View the reordered dataset
print(FFCW_merge)

#for TwinLife
colnames(TwinLife_results)
head(TwinLife_results)

TwinLife_merge <- TwinLife_results[, c("Clock", "Estimate", "CI_lower", "CI_upper")]
TwinLife_merge$Cohort <- "TwinLife"

# Define the desired order of the Clock variable
right_order <- c("DNAmPubertalAgeAccel", "GrimAgeAccel", "PhenoAgeAccel",
                 "DunedinPACE", "Horvath1Accel", "Horvath2Accel")

# Reorder the Clock column in TwinLife_merge
TwinLife_merge <- TwinLife_merge %>%
  mutate(Clock = factor(Clock, levels = right_order)) %>%
  arrange(Clock)


# Combine datasets
combined_data <- bind_rows(ttp_merge, FFCW_merge, TwinLife_merge)

#change Horvath1 and 2 to actual names
combined_data <- combined_data %>%
  mutate(Clock = case_when(
    Clock == "Horvath1Accel" ~ "Horvath Multi-Tissue Accel",
    Clock == "Horvath2Accel" ~ "Horvath Skin-and-Blood Accel",
    TRUE ~ Clock  # Keep other values unchanged
  ))


#Order of clocks in plot
desired_order <- c("Horvath Multi-Tissue Accel", "Horvath Skin-and-Blood Accel", "PhenoAgeAccel",
                   "GrimAgeAccel", "DunedinPACE", "DNAmPubertalAgeAccel")


# Update the Clock column to follow the new order
combined_data <- combined_data %>%
  mutate(Clock = factor(Clock, levels = desired_order))


# Add an offset for each cohort to stagger lines
combined_data <- combined_data %>%
  mutate(Cohort_Offset = case_when(
    Cohort == "FFCW" ~ 0.2,   # Offset for Cohort 1
    Cohort == "TTP" ~ 0.0,     # No offset for Cohort 2
    Cohort == "TwinLife" ~ -0.2  # Offset for Cohort 3
  )) %>%
  mutate(Clock_Position = as.numeric(factor(Clock)) + Cohort_Offset)

# Ensure x-axis covers the desired range
ggplot(combined_data, aes(x = Estimate, y = Clock_Position, color = Cohort)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_y_continuous(
    breaks = unique(as.numeric(factor(combined_data$Clock))),
    labels = unique(combined_data$Clock)
  ) +
  scale_x_continuous(
    limits = c(-0.5, 0.2)  # Set x-axis range
  ) +
  labs(
    title = "Associations of Aging and Puberty MPSs with Age at Menarche",
    x = "Standardized Beta (95% CI)",
    y = "Methylation Profile Score",
    color = "Cohort"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12, face = "bold", color = "black"),  # Axis tick labels
    axis.title = element_text(size = 14, face = "bold", color = "black"), # Axis titles
    legend.text = element_text(size = 12, color = "black"),  # Legend text
    legend.title = element_text(size = 14, face = "bold", color = "black"), # Legend title
    plot.title = element_text(size = 16, face = "bold", color = "black", hjust = 0.5), # Title
    panel.grid = element_blank()  # Remove gray grid lines for a cleaner look
  ) +
  scale_color_manual(
    values = c("deeppink4", "darkolivegreen", "darkgoldenrod"),  # Custom colors
    name = "Cohort"  # Legend title
  ) 


print(combined_data %>% select(Clock, Clock_Position, Estimate, CI_lower, CI_upper))

#### Running multiple-regression models ####
girls_df_all <- read.csv("/Users/willems/Desktop/Projects/Puberty Sister Paper/Analysis October/girls_pubertyclocks.csv", header = TRUE)


#be sure only using 1st timepoint, that MPS are adjusted for IC and Epi, and standardized 
#also be sure using non age-residualized DunedinPACE

colnames(girls_df_all)
#create postmenarchial dataframe
girls_df_postmenarche <- girls_df_all[!is.na(girls_df_all$pub0401_wid5), ]

# 5 clocks predicting menarche age
model1 <- lm(pub0401_wid5 ~  PhenoAgeAccel_res_cellZ_wid5 + GrimAgeAccel_res_cellZ_wid5 + HovarthMultiTisAccel_res_cellZ_wid5 + HorvatSkinBloodAccel_res_cellZ_wid5 + DunedinPaceAccel_res_cellZ_wid5, data = girls_df_all) 
summary(model1)

# 5 clocks + dnampubertalageaccel predicting menarche age
model2 <- lm(pub0401_wid5 ~  dnampubertalageAccel_res_cellZ_wid5 + PhenoAgeAccel_res_cellZ_wid5 + GrimAgeAccel_res_cellZ_wid5 + HovarthMultiTisAccel_res_cellZ_wid5 + HorvatSkinBloodAccel_res_cellZ_wid5 + DunedinPaceAccel_res_cellZ_wid5, data = girls_df_all) 
summary(model2)

# 5 clocks + dnampubertalageaccel + chronologial age predicting menarche
model3 <- lm(pub0401_wid5 ~  dnampubertalageAccel_res_cellZ_wid5 + PhenoAgeAccel_res_cellZ_wid5 + GrimAgeAccel_res_cellZ_wid5 + HovarthMultiTisAccel_res_cellZ_wid5 + HorvatSkinBloodAccel_res_cellZ_wid5 + DunedinPaceAccel_res_cellZ_wid5 + epic_age_wid5, data = girls_df_all) 
summary(model3)

# 5 clocks + chronological age predicting menarche age
model4 <- lm(pub0401_wid5 ~ PhenoAgeAccel_res_cellZ_wid5 + GrimAgeAccel_res_cellZ_wid5 + HovarthMultiTisAccel_res_cellZ_wid5 + HorvatSkinBloodAccel_res_cellZ_wid5 + DunedinPaceAccel_res_cellZ_wid5 + epic_age_wid5, data = girls_df_all) 
summary(model4)

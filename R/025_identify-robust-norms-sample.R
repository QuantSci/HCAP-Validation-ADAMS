# clear environment
rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
source(here::here("R","002_libraries.R"))
source(here::here("R","005_folder-paths-and-options.R"))

####
# The robust norms sample definitions are:
# CN in ADAMS Wave A OR ADAMS Wave C
# No indicators of memory-related disease or stroke in HRS 2002, 2004, and 2006
####

####
## Load data
####

# ADAMS
ADAMS1AD_R <- readRDS(here::here(RDS_path, "010_ADAMS1AD_R.RDS"))
ADAMS1CD_R <- readRDS(here::here(RDS_path, "010_ADAMSWAVEC.RDS"))
ADAMS1TRK_R <- readRDS(here::here(RDS_path, "010_ADAMS1TRK_R.RDS"))
# HRS
HRS_2002 <- readRDS(here::here(RDS_path, "010_HRS_2002.RDS"))
HRS_2004 <- readRDS(here::here(RDS_path, "010_HRS_2004.RDS"))
HRS_2006 <- readRDS(here::here(RDS_path, "010_HRS_2006.RDS"))

###
# Prep ID Variable
###

ADAMS_HRS_ID <- ADAMS1TRK_R %>% 
  dplyr::select(HHID, PN, ADAMSSID) %>% 
  tidyr::unite(HRS_ID, c("HHID", "PN"))

####
# Get HRS Vars of interest
####

HRS_2002_data <- HRS_2002 %>% 
  tidyr::unite(HRS_ID, c("HHID", "PN")) %>% 
  dplyr::filter(HRS_ID %in% ADAMS_HRS_ID$HRS_ID) %>% 
  dplyr::select(HRS_ID, HC053, HC069) %>%  #HC053 is stroke, HC069 is memory-related problems
  dplyr::mutate(strokeY10N_2002 = dplyr::case_when(HC053 == 1 ~ 1, # 1 = yes, left as yes
                                              HC053 == 2 ~ 1, # Possible stroke or TIA = 2, recoded to yes
                                              HC053 == 3 ~ 1, # 3 = disputes previous wave, but now has condition, recoded as yes
                                              HC053 == 4 ~ 1, # 4 = disputes previous wave, but does NOT have condition, recoded as yes
                                              HC053 == 5 ~ 0, # 5 = no, recoded to no
                                              HC053 == 8 ~ NA_real_, # 8 = don't know, 9 = refused, recoded to missing
                                              HC053 == 9 ~ NA_real_),
                memprobsY10N_2002 = dplyr::case_when(HC069 == 1 ~ 1, # 1= yes, left as yes
                                                HC069 == 5 ~ 0, # 5 = no, recoded to no
                                                HC069 == 8 ~ NA_real_, # 8 = don't know, 9 = refused, recoded to missing
                                                HC069 == 9 ~ NA_real_)) %>% 
  dplyr::select(-HC053, -HC069)

HRS_2004_data <- HRS_2004 %>% 
  tidyr::unite(HRS_ID, c("HHID", "PN")) %>% 
  dplyr::filter(HRS_ID %in% ADAMS_HRS_ID$HRS_ID) %>% 
  dplyr::select(HRS_ID, JC053, JC069) %>%  #JC053 is stroke, JC069 is memory-related problems
  dplyr::mutate(strokeY10N_2004 = dplyr::case_when(JC053 == 1 ~ 1, # 1 = yes, left as yes
                                                   JC053 == 2 ~ 1, # Possible stroke or TIA = 2, recoded to yes
                                                   JC053 == 3 ~ 1, # 3 = disputes previous wave, but now has condition, recoded as yes
                                                   JC053 == 4 ~ 1, # 4 = disputes previous wave, but does NOT have condition, recoded as yes
                                                   JC053 == 5 ~ 0, # 5 = no, recoded to no
                                                   JC053 == 8 ~ NA_real_, # 8 = don't know, 9 = refused, recoded to missing
                                                   JC053 == 9 ~ NA_real_),
                memprobsY10N_2004 = dplyr::case_when(JC069 == 1 ~ 1, # 1= yes, left as yes
                                                     JC069 == 5 ~ 0, # 5 = no, recoded to no
                                                     JC069 == 8 ~ NA_real_, # 8 = don't know, 9 = refused, recoded to missing
                                                     JC069 == 9 ~ NA_real_)) %>% 
  dplyr::select(-JC053, -JC069)

HRS_2006_data <- HRS_2006 %>% 
  tidyr::unite(HRS_ID, c("HHID", "PN")) %>% 
  dplyr::filter(HRS_ID %in% ADAMS_HRS_ID$HRS_ID) %>% 
  dplyr::select(HRS_ID, KC053, KC069) %>%  #KC053 is stroke, KC069 is memory-related problems
  dplyr::mutate(strokeY10N_2006 = dplyr::case_when(KC053 == 1 ~ 1, # 1 = yes, left as yes
                                                   KC053 == 2 ~ 1, # Possible stroke or TIA = 2, recoded to yes
                                                   KC053 == 3 ~ 1, # 3 = disputes previous wave, but now has condition, recoded as yes
                                                   KC053 == 4 ~ 1, # 4 = disputes previous wave, but does NOT have condition, recoded as yes
                                                   KC053 == 5 ~ 0, # 5 = no, recoded to no
                                                   KC053 == 8 ~ NA_real_, # 8 = don't know, 9 = refused, recoded to missing
                                                   KC053 == 9 ~ NA_real_),
                memprobsY10N_2006 = dplyr::case_when(KC069 == 1 ~ 1, # 1= yes, left as yes
                                                     KC069 == 5 ~ 0, # 5 = no, recoded to no
                                                     KC069 == 8 ~ NA_real_, # 8 = don't know, 9 = refused, recoded to missing
                                                     KC069 == 9 ~ NA_real_)) %>% 
  dplyr::select(-KC053, -KC069)

## Merge together

ADAMS_stroke_memprob <- ADAMS_HRS_ID %>% 
  dplyr::left_join(HRS_2002_data, by = "HRS_ID") %>% 
  dplyr::left_join(HRS_2004_data, by = "HRS_ID") %>% 
  dplyr::left_join(HRS_2006_data, by = "HRS_ID") %>% 
  dplyr::mutate(no_stroke = dplyr::if_else((strokeY10N_2002 == 0 & strokeY10N_2004 == 0 & strokeY10N_2006 == 0), 1, 0), # create "no stroke at any time point" variable
                no_memprob = dplyr::if_else((memprobsY10N_2002 == 0 & memprobsY10N_2004 == 0 & memprobsY10N_2004 == 0), 1, 0)) # create no "memprob at any time point" variable
                                               

# Check data

QSPtools::checkvar(ADAMS_stroke_memprob, no_memprob, memprobsY10N_2002, memprobsY10N_2004, memprobsY10N_2006) %>% print(n = 30)

# any_memprob memprobsY10N_2002 memprobsY10N_2004 memprobsY10N_2006     n
# 14          1                 0                 0                 0   678   ## This 678 is the normative reference


QSPtools::checkvar(ADAMS_stroke_memprob, no_stroke, strokeY10N_2002, strokeY10N_2004, strokeY10N_2006) %>% print(n = 30)

# any_stroke strokeY10N_2002 strokeY10N_2004 strokeY10N_2006     n
# 14         1               0               0               0   694   694  # This 694 is the normative reference


###
# Get CN in ADAMS A and C
###

ADAMS_CN_A <- ADAMS1AD_R %>% 
  dplyr::select(ADAMSSID, HHID, PN, ADFDX1) %>% 
  dplyr::filter(ADFDX1 == 31) # 31 = cognitively normal

ADAMS_CN_C <- ADAMS1CD_R %>% 
  dplyr::select(ADAMSSID, HHID, PN, CDFDX1) %>% 
  dplyr::filter(CDFDX1 == 31) # 31 = cognitively normal

ADAMS_CN <- left_join(ADAMS_CN_A, ADAMS_CN_C, by = c("ADAMSSID", "HHID", "PN")) %>% 
  tidyr::unite(HRS_ID, c("HHID", "PN"))

###
# Add in HRS data to ADAMS CN data
###

get_robust_norms_sample <- ADAMS_CN %>% 
  dplyr::left_join(ADAMS_stroke_memprob, by = c("ADAMSSID", "HRS_ID")) %>% 
  dplyr::filter(no_stroke == 1) %>% # only retain folks with NO stroke at any wave 
  dplyr::filter(no_memprob == 1) %>%  # only retain folks with NO stroke at any wave
  dplyr::select(ADAMSSID, HRS_ID)


flowchart_data <- ADAMS_CN %>% 
  dplyr::left_join(ADAMS_stroke_memprob, by = c("ADAMSSID", "HRS_ID"))

table(flowchart_data$no_memprob) # 287

flowchart_data_pt2 <- flowchart_data %>% 
  filter(no_memprob == 1)

table(flowchart_data_pt2$no_stroke)
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
                                              HC053 == 2 ~ 0, # Possible stroke or TIA = 2, recoded to no
                                              HC053 == 3 ~ 1, # 3 = disputes previous wave, but now has condition, recoded as yes
                                              HC053 == 4 ~ 0, # 4 = disputes previous wave, but does NOT have condition, recoded as no
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
                                                   JC053 == 2 ~ 0, # Possible stroke or TIA = 2, recoded to no
                                                   JC053 == 3 ~ 1, # 3 = disputes previous wave, but now has condition, recoded as yes
                                                   JC053 == 4 ~ 0, # 4 = disputes previous wave, but does NOT have condition, recoded as no
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
                                                   KC053 == 2 ~ 0, # Possible stroke or TIA = 2, recoded to no
                                                   KC053 == 3 ~ 1, # 3 = disputes previous wave, but now has condition, recoded as yes
                                                   KC053 == 4 ~ 0, # 4 = disputes previous wave, but does NOT have condition, recoded as no
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
  dplyr::mutate(any_stroke = dplyr::case_when(strokeY10N_2002 == 1 ~ 1, # create "stroke at any time point" variable
                                              strokeY10N_2004 == 1 ~ 1,
                                              strokeY10N_2006 == 1 ~ 1,
                                              TRUE ~ 0),
                any_memprob = dplyr::case_when(memprobsY10N_2002 == 1 ~ 1, # create "memprob at any time point" variable
                                               memprobsY10N_2004 == 1 ~ 1,
                                               memprobsY10N_2006 == 1 ~ 1,
                                               TRUE ~ 0))

# Check data

QSPtools::checkvar(ADAMS_stroke_memprob, any_memprob, memprobsY10N_2002, memprobsY10N_2004, memprobsY10N_2006) %>% print(n = 30)

# any_memprob memprobsY10N_2002 memprobsY10N_2004 memprobsY10N_2006     n
#  1           0                 0                 0                 0   678
#  2           0                 0                 0                NA   204
#  3           0                 0                NA                 0    17
#  4           0                 0                NA                NA   244
#  5           0                NA                 0                 0     4
#  6           0                NA                NA                 0     2
#  7           0                NA                NA                NA   259
#  8           1                 0                 0                 1    51
#  9           1                 0                 1                 0     3
# 10           1                 0                 1                 1    16
# 11           1                 0                 1                NA    80
# 12           1                 0                NA                 1     1
# 13           1                 1                 0                 0     4
# 14           1                 1                 0                 1     1
# 15           1                 1                 0                NA     2
# 16           1                 1                 1                 1     2
# 17           1                 1                 1                NA    11
# 18           1                 1                NA                 0     1
# 19           1                 1                NA                 1    11
# 20           1                 1                NA                NA   167
# 21           1                NA                 0                 1     1
# 22           1                NA                 1                 1     2
# 23           1                NA                 1                NA     5
# 24           1                NA                NA                 1     4

QSPtools::checkvar(ADAMS_stroke_memprob, any_stroke, strokeY10N_2002, strokeY10N_2004, strokeY10N_2006) %>% print(n = 30)

# any_stroke strokeY10N_2002 strokeY10N_2004 strokeY10N_2006     n
#  1          0               0               0               0   721
#  2          0               0               0              NA   221
#  3          0               0              NA               0    18
#  4          0               0              NA              NA   275
#  5          0              NA               0               0     5
#  6          0              NA               0              NA     2
#  7          0              NA              NA               0     2
#  8          0              NA              NA              NA   214
#  9          1               0               0               1    32
# 10          1               0               1               0     4
# 11          1               0               1               1    21
# 12          1               0               1              NA     9
# 13          1               0              NA               1     1
# 14          1               1               0               0     9
# 15          1               1               0               1     2
# 16          1               1               0              NA     6
# 17          1               1               1               0     7
# 18          1               1               1               1    80
# 19          1               1               1              NA    55
# 20          1               1              NA               1     2
# 21          1               1              NA              NA    84


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
  dplyr::filter(any_stroke == 0) %>% # only retain stroke = 0
  dplyr::filter(any_memprob == 0) %>%  # only retain memprobs = 0
  dplyr::select(ADAMSSID, HRS_ID)
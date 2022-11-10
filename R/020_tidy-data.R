# clear environment
rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
source(here::here("R","002_libraries.R"))
source(here::here("R","005_folder-paths-and-options.R"))

ADAMS1AN_R <- readRDS(here::here(RDS_path, "010_ADAMS1AN_R.RDS"))
ADAMS1TRK_R <- readRDS(here::here(RDS_path, "010_ADAMS1TRK_R.RDS"))
ADAMS1AD_R <- readRDS(here::here(RDS_path,  "010_ADAMS1AD_R.RDS"))

# https://github.com/rnj0nes/HCAP22-CFA-HCAP/blob/main/-110-create-variables.do
# Pull variables to import from here

# Cognition variables

## ORIENTATION

cognition_1 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANMSE1:ANMSE10) %>% 
  dplyr::mutate(across(c(ANMSE1:ANMSE10), na_if, 97)) %>%  # mark 97s as missing
  dplyr::rowwise() %>% 
  dplyr::mutate(vdori1 = sum(c_across(ANMSE1:ANMSE10))) %>%  # create vdori1
  dplyr::select(ADAMSSID, vdori1)
#  0    1    2    3    4    5    6    7    8    9   10 <NA> 
# 21   17   35   31   28   30   53   49   83  142  323   44 

cognition_2 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANPRES) %>% 
  dplyr::mutate(ANPRES = na_if(ANPRES, 97), # mark 97-99 as missing
                ANPRES = na_if(ANPRES, 98),
                ANPRES = na_if(ANPRES, 99),
                vdori2 = ANPRES) %>% 
  dplyr::select(ADAMSSID, vdori2)
  
orientation <- cognition_1 %>% 
  left_join(cognition_2, by = "ADAMSSID")

## MEMORY: IMMEDIATE EPISODIC

cognition_3 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANIMMCR1, ANIMMCR2, ANIMMCR3) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(across(c(ANIMMCR1:ANIMMCR3), na_if, 97), # mark 97-99 as missing
                across(c(ANIMMCR1:ANIMMCR3), na_if, 98),
                across(c(ANIMMCR1:ANIMMCR3), na_if, 99),
                vdmie1 = sum(c_across(ANIMMCR1:ANIMMCR3))) %>% 
  dplyr::select(ADAMSSID, vdmie1)

cognition_4 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANMSE11S) %>% 
  dplyr::mutate(ANMSE11S = na_if(ANMSE11S, 97)) %>%  # mark 97 as missing
  dplyr::rename(vdmie2 = ANMSE11S)

cognition_5 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANIMMCR1) %>% 
  dplyr::mutate(ANIMMCR1 = na_if(ANIMMCR1, 97), # mark 97-99 as missing
                ANIMMCR1 = na_if(ANIMMCR1, 98),
                ANIMMCR1 = na_if(ANIMMCR1, 99),
                vdmie3 = ANIMMCR1) %>% 
  dplyr::select(ADAMSSID, vdmie3)

########
## vdmie4 IS BASED ON THE BRAVE MAN STORY, UNSURE WHICH TO USE IN ADAMS
########

## MEMORY: DELAYED EPISODIC

cognition_7 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANDELCOR) %>% 
  dplyr::mutate(ANDELCOR = na_if(ANDELCOR, 97),  # mark 97 as missing
                vdmde1 = ANDELCOR) %>% 
  dplyr::select(ADAMSSID, vdmde1)

cognition_8 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANWM2TOT) %>% 
  dplyr::mutate(ANWM2TOT = na_if(ANWM2TOT, 97),  # mark 97 as missing
                vdmde2 = ANWM2TOT) %>% 
  dplyr::select(ADAMSSID, vdmde2)
  
## MEMORY: RECOGNITION

## VISUOSPATIAL

## EXECUTIVE FUNCTION

## ATTENTION/SPEED

## LANGUAGE/FLUENCY

## MERGE COGNITION DATA
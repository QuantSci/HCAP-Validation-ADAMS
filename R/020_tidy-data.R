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
  dplyr::select(ADAMSSID, ANMSE1:ANMSE10) %>%              # MMSE items 0-10, all (0-1)
  dplyr::mutate(across(c(ANMSE1:ANMSE10), na_if, 97)) %>%  # mark 97s as missing
  dplyr::rowwise() %>% 
  dplyr::mutate(vdori1 = sum(c_across(ANMSE1:ANMSE10))) %>%  # create vdori1
  dplyr::select(ADAMSSID, vdori1)
#  0    1    2    3    4    5    6    7    8    9   10 <NA> 
# 21   17   35   31   28   30   53   49   83  142  323   44 

cognition_2 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANPRES) %>%       # TICS name of president (0-1)
  dplyr::mutate(ANPRES = na_if(ANPRES, 97), # mark 97-99 as missing
                ANPRES = na_if(ANPRES, 98),
                ANPRES = na_if(ANPRES, 99),
                vdori2 = ANPRES) %>% 
  dplyr::select(ADAMSSID, vdori2)
  
orientation <- cognition_1 %>% 
  left_join(cognition_2, by = "ADAMSSID")

## MEMORY: IMMEDIATE EPISODIC

cognition_3 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANIMMCR1, ANIMMCR2, ANIMMCR3) %>% # immediate word list recall trials 1-3, all (0-10) ###### DIFFERENT FROM HCAP
  dplyr::rowwise() %>% 
  dplyr::mutate(across(c(ANIMMCR1:ANIMMCR3), na_if, 97), # mark 97-99 as missing
                across(c(ANIMMCR1:ANIMMCR3), na_if, 98),
                across(c(ANIMMCR1:ANIMMCR3), na_if, 99),
                vdmie1 = sum(c_across(ANIMMCR1:ANIMMCR3))) %>% 
  dplyr::select(ADAMSSID, vdmie1)

cognition_4 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANMSE11S) %>%              # MMSE item 11 score (0-3)
  dplyr::mutate(ANMSE11S = na_if(ANMSE11S, 97)) %>%  # mark 97 as missing
  dplyr::rename(vdmie2 = ANMSE11S)

cognition_5 <- ADAMS1AN_R %>%  
  dplyr::select(ADAMSSID, ANIMMCR1) %>%         # immediate word list recall trial 1 (0-10)                    ###### DIFFERENT FROM HCAP
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
  dplyr::select(ADAMSSID, ANDELCOR) %>%          # delayed word list recall (0-10)
  dplyr::mutate(ANDELCOR = na_if(ANDELCOR, 97),  # mark 97 as missing
                vdmde1 = ANDELCOR) %>% 
  dplyr::select(ADAMSSID, vdmde1)

cognition_8 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANWM2TOT) %>%          # Weschler Log Mem II for stories (0-37)  ###### DIFFERENT FROM HCAP
  dplyr::mutate(ANWM2TOT = na_if(ANWM2TOT, 97),  # mark 97 as missing
                vdmde2 = ANWM2TOT) %>% 
  dplyr::select(ADAMSSID, vdmde2)

cognition_9 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANMSE13) %>%          # MMSE Item 13 (0-1)                       ###### DIFFERENT FROM HCAP
  dplyr::mutate(ANMSE13 = na_if(ANMSE13, 97),  # mark 97 as missing
                vdmde3 = ANMSE13) %>% 
  dplyr::select(ADAMSSID, vdmde3)

cognition_10 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANDCPTOT) %>%          # Delayed Constructional PRAXIS Total (0-11)                   
  dplyr::mutate(ANDCPTOT = na_if(ANDCPTOT, 97),  # mark 97 as missing
                vdmde4 = ANDCPTOT) %>% 
  dplyr::select(ADAMSSID, vdmde4)

########
## vdmie5 IS BASED ON THE BRAVE MAN STORY, UNSURE WHICH TO USE IN ADAMS
########

## MEMORY: RECOGNITION

cognition_12 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANRECYES, ANRECNO) %>%  # sum of word list recognition, yes and no (0-20)
  dplyr::rowwise() %>% 
  dplyr::mutate(across(c(ANRECYES, ANRECNO), na_if, 97), # mark 97 as missing
                vdmre1 = sum(c_across(ANRECYES:ANRECNO))) %>% 
  dplyr::select(ADAMSSID, vdmre1)

# UNSURE WHAT TO USE FOR VDMRE2 - LOGICAL MEMORY RECOGNITION

## VISUOSPATIAL

cognition_14 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANCPTOT) %>%  # constructional praxis immediate (0-11)
  dplyr::mutate(ANCPTOT = na_if(ANCPTOT, 97),  # mark 97 as missing
                vdvis1 = ANCPTOT) %>% 
  dplyr::select(ADAMSSID, vdvis1)

cognition_15 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANMSE22) %>%  # MMSE ITEM 22
  dplyr::mutate(ANMSE22 = na_if(ANMSE22, 97),  # mark 97 as missing
                vdvis2 = ANMSE22) %>% 
  dplyr::select(ADAMSSID, vdvis2)

visuospatial <- cognition_14 %>% 
  left_join(cognition_15, by = "ADAMSSID")

## EXECUTIVE FUNCTION

# Unsure what to use for Raven's progressive matrices

cognition_17 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANTMBSEC) %>% # Trails Btime to complete (0-727)
  dplyr::mutate(ANTMBSEC = na_if(ANTMBSEC, 995),  # mark 995-997 as missing
                ANTMBSEC = na_if(ANTMBSEC, 996),
                ANTMBSEC = na_if(ANTMBSEC, 997),
                vdefx2 = ANTMBSEC) %>% 
  dplyr::select(ADAMSSID, vdefx2)

# Unsure what to use for vdexf3 - vdexf4

cognition_20 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANSDMERR) %>% # Symbol digit modality - total errors (0-12)
  dplyr::mutate(ANSDMERR = na_if(ANSDMERR, 97),  # mark 97 as missing
                vdexf5 = ANSDMERR) %>% 
  dplyr::select(ADAMSSID, vdexf5)


# Unsure what to use for vdexf6 - vdexf7

## ATTENTION/SPEED

cognition_23 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANSDMTOT) %>% # Symbol digit modality - total score (0-63)
  dplyr::mutate(ANSDMTOT = na_if(ANSDMTOT, 97),  # mark 97 as missing
                vdasp1 = ANSDMTOT) %>% 
  dplyr::select(ADAMSSID, vdasp1)

cognition_24 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANTMASEC) %>% # Trails A - time to complete (0-373)
  dplyr::mutate(ANTMASEC = na_if(ANTMASEC, 995),  # mark 995-997 as missing
                ANTMASEC = na_if(ANTMASEC, 995),
                ANTMASEC = na_if(ANTMASEC, 997),
                vdasp2 = ANTMASEC) %>% 
  dplyr::select(ADAMSSID, vdasp2)

cognition_25 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANMSE12) %>% # MMSE Item 12 (0-5)
  dplyr::mutate(ANMSE12 = na_if(ANMSE12, 97),  # mark 97 as missing
                vdasp3 = ANMSE12) %>% 
  dplyr::select(ADAMSSID, vdasp3)

# Unsure what to use for vdasp4 -- could use TICS backwards 20 or 86

# Unsure what to use for vdasp5 

## LANGUAGE/FLUENCY

cognition_28 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANAFTOT) %>%  # Animal Fluency total score (0-33)
  dplyr::mutate(ANAFTOT = na_if(ANAFTOT, 97),  # mark 97 as missing
                vdlfl1 = ANAFTOT) %>% 
  dplyr::select(ADAMSSID, vdlfl1)

cognition_29 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANSCISOR, ANCACTUS) %>%  # TICS name scissors, cactus (0-1)
  dplyr::rowwise() %>% 
  dplyr::mutate(across(c(ANSCISOR:ANCACTUS), na_if, 97), # mark 97-99 as missing
                across(c(ANSCISOR:ANCACTUS), na_if, 98),
                across(c(ANSCISOR:ANCACTUS), na_if, 99),
                vdlfl2 = sum(c_across(ANSCISOR:ANCACTUS))) %>% 
  dplyr::select(ADAMSSID, vdlfl2)

cognition_30 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANMSE14, ANMSE15) %>%  # MMSE items 14 and 15 (0-1)
  dplyr::rowwise() %>% 
  dplyr::mutate(across(c(ANMSE14:ANMSE15), na_if, 97), # mark 97 as missing
                vdlfl3 = sum(c_across(ANMSE14:ANMSE15))) %>% 
  dplyr::select(ADAMSSID, vdlfl3)

cognition_31 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANMSE21) %>%  # MMSE item 21 (0-1)
  dplyr::mutate(ANMSE21 = na_if(ANMSE21, 97), # mark 97 as missing
                ANMSE21 = dplyr::case_when(ANMSE21 == 0 ~ 0,
                                           ANMSE21 == 1 ~ 1,
                                           ANMSE21 == 2 ~ 1), # recode so 2 (correct, wrote name) is marked as correct 
                vdlfl4 = ANMSE21) %>% 
  dplyr::select(ADAMSSID, vdlfl4)

cognition_32 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANMSE20F, ANMSE20L, ANMSE20R) %>%  # MMSE paper folds all (0-1)
  dplyr::rowwise() %>% 
  dplyr::mutate(across(c(ANMSE20F, ANMSE20L, ANMSE20R), na_if, 97), # mark 97 as missing
                vdlfl5 = sum(c_across(ANMSE20F:ANMSE20R))) %>% 
  dplyr::select(ADAMSSID, vdlfl5)

cognition_33 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANMSE17) %>%  # MMSE item 17 (0-1)
  dplyr::mutate(ANMSE17 = na_if(ANMSE17, 97), # mark 97 as missing
                ANMSE17 = dplyr::case_when(ANMSE17 == 0 ~ 0,
                                           ANMSE17 == 1 ~ 1,
                                           ANMSE17 == 2 ~ 1), # recode so 2 (correct, tactile stimuli) is marked as correct 
                vdlfl6 = ANMSE17) %>% 
  dplyr::select(ADAMSSID, vdlfl6)

# Unsure what to use for vdlfl7

# Possible typo in github -- two versions of vdlfl5 and vdlfl6. intentional write-over?

## MERGE COGNITION DATA
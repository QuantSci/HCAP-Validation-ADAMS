# clear environment
rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
source(here::here("R","002_libraries.R"))
source(here::here("R","005_folder-paths-and-options.R"))

ADAMS1AN_R <- readRDS(here::here(RDS_path, "010_ADAMS1AN_R.RDS"))
ADAMS1TRK_R <- readRDS(here::here(RDS_path, "010_ADAMS1TRK_R.RDS"))
ADAMS1AD_R <- readRDS(here::here(RDS_path,  "010_ADAMS1AD_R.RDS"))

## Obtain variables for analysis


ModelVars <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, # ID
                # Orientation items
                ANMSE1:ANMSE10, # Orientation
                # Memory items
                ANDELCOR, # DEL WORD LIST MEM TOTAL CORRECT -- Analysis Name vdmde1
                ANMSE13:ANMSE15, # 3-word delayed recall (sum for analysis) -- analysis name vdmde3
                ANWM2A, # Logical memory delayed A -- analysis name vdmde2
                ANWM2B, # Logical memory delayed B -- analayis name vdmde?
                ANDCPTOT, # DELAYED CONSTRUCTIONAL PRAXIS TOTAL -- analysis name vdmde4
                ANRECYES, # WORD LIST RECOG - TOTAL CORRECT YES -- analysis name vdmre1 (sum w/ no for analysis)
                ANRECNO, # WORD LIST RECOG - TOTAL CORRECT NO
               
                # Exec. Func items
                ANTMBSEC, # trails B -- vdefx2
                ANDSSBT, # digit span backwards -- vdefx8
                ANDSSFT, # digit span forwards -- vdefx9
                ANTMASEC, # trails A -- vdasp2
                ANSDMTOT, # symbol digit modality -- vdasp1
                ANMSE12, # backwards spelling -- vdasp3
                
                # Language/Fluency items
                ANAFTOT, # Animal naming  -- vdlfl1
                ANSCISOR, # name two objects (TICS) -- vdlfl2
                ANCACTUS,
                ANMSE16, # name two objects (MMSE) -- vdlfl3
                ANMSE17,
                ANMSE21, # write a sentence -- vdlfl4
                ANMSE19, # read and follow command -- vdlfl5
                ANBNTTOT, # boston naming test -- vdlfl7
                ANCOWATO, # controlled oral word assoc. -- vdlfl8
                # Visuospatial items
                ANCPTOT
                ) %>% 
  dplyr::mutate(ANMSE16_R = dplyr::case_when(ANMSE16 == 2 ~ 1, # Recode Vars that need it
                                             ANMSE16 == 1 ~ 1,
                                             ANMSE16 == 0 ~ 0),
                ANMSE17_R = dplyr::case_when(ANMSE17 == 2 ~ 1,
                                             ANMSE17 == 1 ~ 1,
                                             ANMSE17 == 0 ~ 0),
                ANMSE21_R = dplyr::case_when(ANMSE21 == 2 ~ 1,
                                             ANMSE21 == 1 ~ 1,
                                             ANMSE21 == 0 ~ 0)) %>% 
  dplyr::select(-ANMSE16, -ANMSE17, -ANMSE21) %>% 
  dplyr::mutate(across(c(ANMSE1:ANMSE15, ANMSE12, ANMSE19,
                         ANSCISOR, ANCACTUS, ANRECYES, ANRECNO, ANWM2A,
                         ANWM2B, ANDCPTOT, ANDELCOR, ANDSSBT,
                         ANDSSFT, ANSDMTOT, ANAFTOT,
                         ANBNTTOT, ANCOWATO, ANCPTOT), na_if, 97), # mark 97s as missing
                across(c(ANSCISOR, ANCACTUS), na_if, 98), # mark 98s as missing
                across(c(ANSCISOR, ANCACTUS), na_if, 99), # mark 99s as missing
                across(c(ANTMBSEC, ANTMASEC), na_if, 995), # mark 995-997 as missing (trails A/B)
                across(c(ANTMBSEC, ANTMASEC), na_if, 996),
                across(c(ANTMBSEC, ANTMASEC), na_if, 997)) 

## Examine raw variables

# ModelVars %>%
#   dplyr::select(-ADAMSSID) %>%
#   gtsummary::tbl_summary(
#     statistic = list(
#     c(ANRECYES, ANRECNO, ANWM2A, ANWM2B, ANDCPTOT, ANTMBSEC, ANTMASEC,
#       ANDSSBT, ANDSSFT, ANSDMTOT, ANAFTOT, ANBNTTOT, ANCOWATO, ANCPTOT) ~ "{mean} ({sd})")) %>%
#   gtsummary::as_gt() %>%
#   gt::gtsave("HCAP-ADAMS-MODELVARS.rtf")


## Make variables to be used in analaysis
## Pull from this document https://github.com/rnj0nes/HCAP22/blob/main/CFA-HCAP/NEW-CFA-HCAP_a4.pdf

####
## ORIENTATION
####

vdori1 <- ModelVars %>% 
    dplyr::select(ADAMSSID, ANMSE1:ANMSE10) %>%  # MMSE items 0-10, all (0-1)
    dplyr::rowwise() %>%
    dplyr::mutate(vdori1 = sum(c_across(ANMSE1:ANMSE10))) %>%  # create vdori1
    dplyr::select(ADAMSSID, vdori1)

# vdori1 is a direct match to Jones et al. 

####
## MEMORY
####

vdmre1 <- ModelVars %>% 
  dplyr::select(ADAMSSID, ANRECYES, ANRECNO) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(vdmre1 = sum(c_across(ANRECYES:ANRECNO))) %>% # create vdmre1 by summing
  dplyr::select(ADAMSSID, vdmre1)

# vdmre1 is a direct match to Jones et al.

vdmde3 <- ModelVars %>% 
  dplyr::select(ADAMSSID, ANMSE13:ANMSE15) %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(mmse_delay_sum = sum(c_across(ANMSE13:ANMSE15)),
                vdmde3 = dplyr::case_when(mmse_delay_sum == 3 ~ 2,
                                          mmse_delay_sum == 2 ~ 1,
                                          mmse_delay_sum == 1 ~ 0,
                                          mmse_delay_sum == 0 ~ 0)) %>%  # create vdmde3
  dplyr::select(ADAMSSID, vdmde3)

# vdmde3 is a direct match to Jones et al.

vdmde4 <- ModelVars %>% 
  dplyr::select(ADAMSSID, ANDCPTOT) %>% 
  dplyr::rename(vdmde4 = ANDCPTOT) # rename ANDCPTOT to vdmde4

# vdmre1 is a direct match to Jones et al.

vdmde1 <- ModelVars %>% 
  dplyr::select(ADAMSSID, ANDELCOR) %>% 
  dplyr::rename(vdmde1 = ANDELCOR) # rename ANDELCOR to vdmde1

# vdmde1 is a direct match to Jones et al.

vdmde6 <- ModelVars %>% 
  dplyr::select(ADAMSSID, ANWM2A) %>% 
  dplyr::rename(vdmde6 = ANWM2A) # rename AMWM2A to vdmde6

# vdmde6 is NOT in Jones et al.

vdmde7<- ModelVars %>% 
  dplyr::select(ADAMSSID, ANWM2B) %>% 
  dplyr::rename(vdmde7 = ANWM2B) # rename AMWM2B to vdmde7

# vdmde7 is NOT in Jones et al.

memory <- vdmre1 %>% 
  left_join(vdmde3, by = "ADAMSSID") %>% 
  left_join(vdmde4, by = "ADAMSSID") %>% 
  left_join(vdmde1, by = "ADAMSSID") %>% 
  left_join(vdmde6, by = "ADAMSSID") %>% 
  left_join(vdmde7, by = "ADAMSSID")

####
## EXECUTIVE FUNCTIONING
####

vdefx2 <- ModelVars %>% 
  dplyr::select(ADAMSSID, ANTMBSEC) %>% 
  dplyr::mutate(
    trailsb_recode = dplyr::if_else(ANTMBSEC > 300, 300, ANTMBSEC),
    vdefx2 = 1 - (log(trailsb_recode)/log(300))) %>%  # follow Jones et al. scoring convention to obtain vdefx2
  dplyr::select(-ANTMBSEC, -trailsb_recode)

# vdefx2 is a match to Jones et al.

vdasp1 <- ModelVars %>% 
  dplyr::select(ADAMSSID, ANSDMTOT) %>% 
  dplyr::rename(vdasp1 = ANSDMTOT) # rename ANSDMTOT to vdasp1

# vdasp1 is a match to Jones et al.

vdasp2 <- ModelVars %>% 
  dplyr::select(ADAMSSID, ANTMASEC) %>% 
  dplyr::mutate(
    trailsa_recode = dplyr::if_else(ANTMASEC > 300, 300, ANTMASEC),
    vdasp2 = 1 - (log(trailsa_recode)/log(300))) %>%  # follow Jones et al. scoring convention to obtain vdasp2
  dplyr::select(-ANTMASEC, -trailsa_recode)

# vdasp2 is a match to Jones et al.

vdasp3 <- ModelVars %>% 
  dplyr::select(ADAMSSID, ANMSE12) %>% 
  dplyr::rename(vdasp3 = ANMSE12) # rename ANMSE12 to vdasp3

# vdasp3 is a match to Jones et al.

vdefx8 <- ModelVars %>% 
  dplyr::select(ADAMSSID, ANDSSBT) %>% 
  dplyr::rename(vdefx8 = ANDSSBT) # rename ANDSSBT to vdefx3

# vdefx8 is NOT in Jones et al.

vdefx9 <- ModelVars %>% 
  dplyr::select(ADAMSSID, ANDSSFT) %>% 
  dplyr::rename(vdefx9 = ANDSSFT) # rename ANDSSFT to vdefx4

# vdefx9 is NOT in Jones et al.


execfunc <- vdefx2 %>% 
  left_join(vdasp1, by = "ADAMSSID") %>% 
  left_join(vdasp2, by = "ADAMSSID") %>% 
  left_join(vdasp3, by = "ADAMSSID") %>% 
  left_join(vdefx8, by = "ADAMSSID") %>% 
  left_join(vdefx9, by = "ADAMSSID")

####
## LANGUAGE FLUENCY
####

vdlfl1 <- ModelVars %>% 
  dplyr::select(ADAMSSID, ANAFTOT) %>% 
  dplyr::rename(vdlfl1 = ANAFTOT) # rename ANAFTOT to vdlfl1

# vdlfl1 is a match to Jones et al.

vdlfl2 <- ModelVars %>% 
  dplyr::select(ADAMSSID, ANSCISOR, ANCACTUS) %>%  # TICS name scissors, cactus (0-1)
  dplyr::mutate(tics_sum = ANSCISOR + ANCACTUS,
                vdlfl2 = dplyr::if_else(tics_sum == 2, 1, 0)) %>% # get vdlfl2 
  dplyr::select(ADAMSSID, vdlfl2)

# vdlfl2 is a match to Jones et al.

vdlfl3 <- ModelVars %>% 
  dplyr::select(ADAMSSID, ANMSE16_R, ANMSE17_R) %>% 
  dplyr::mutate(mmse_sum = ANMSE16_R + ANMSE17_R,
                vdlfl3 = dplyr::if_else(mmse_sum == 2, 1, 0)) %>% #  get vdlfl3
  dplyr::select(ADAMSSID, vdlfl3)

# vdlfl3 is a match to Jones et al.

vdlfl4 <- ModelVars %>% 
  dplyr::select(ADAMSSID, ANMSE21_R) %>% 
  dplyr::rename(vdlfl4 = ANMSE21_R) # rename

# vdlfl4 is a match to Jones et al.

vdlfl5 <- ModelVars %>% 
  dplyr::select(ADAMSSID, ANMSE19) %>%  # MMSE read and follow command
  dplyr::mutate(vdlfl5 = dplyr::if_else(ANMSE19 > 0, 1, 0)) %>% # rescore so > 0 (i.e., 1 or 2)  == 1, otherwise 0 to get vdlfl5
  dplyr::select(ADAMSSID, vdlfl5)

# vdlfl5 is a match to Jones et al.

vdlfl7 <- ModelVars %>% 
  dplyr::select(ADAMSSID, ANBNTTOT) %>% 
  dplyr::rename(vdlfl7 = ANBNTTOT) # rename

# vdlfl7 is NOT in Jones et al.

vdlfl8 <- ModelVars %>% 
  dplyr::select(ADAMSSID, ANCOWATO) %>% 
  dplyr::rename(vdlfl8 = ANCOWATO) #rename

# vdlfl8 is NOT in Jones et al.

language <- vdlfl1 %>% 
  left_join(vdlfl2, by = "ADAMSSID") %>% 
  left_join(vdlfl3, by = "ADAMSSID") %>% 
  left_join(vdlfl4, by = "ADAMSSID") %>% 
  left_join(vdlfl5, by = "ADAMSSID") %>% 
  left_join(vdlfl7, by = "ADAMSSID") %>% 
  left_join(vdlfl8, by = "ADAMSSID")

####
## VISUOSPATIAL
####

vdvis1 <- ModelVars %>% 
  dplyr::select(ADAMSSID, ANCPTOT) %>% 
  dplyr::rename(vdvis1 = ANCPTOT)

####
## MERGE ALL
####

cognition_scored <- vdori1 %>% 
  left_join(memory, by = "ADAMSSID") %>% 
  left_join(execfunc, by = "ADAMSSID") %>% 
  left_join(language, by = "ADAMSSID") %>% 
  left_join(vdvis1, by = "ADAMSSID") %>% 
  ungroup() %>% 
  labelled::remove_labels()

####
## MIN-MAX NORMALIZATION
####

minmax <- function(x){
  
  c_n <- (5 / (8*1000)) * sd(x, na.rm = T)
  c_d <- (10/ (8*1000)) * sd(x, na.rm = T)
  
  (x - min(x, na.rm = T) + c_n) / (max(x, na.rm = T) - min(x, na.rm = T) + c_d)
  
}

minmax(cognition_scored$vdmre1)


cognition_normalized <- cognition_scored %>% 
  dplyr::mutate(vdmre1z = minmax(vdmre1),
                vdmde4z = minmax(vdmde4),
                vdmde6z = minmax(vdmde6),
                vdmde7z = minmax(vdmde7),
                vdasp1z = minmax(vdasp1),
                vdefx8z = minmax(vdefx8),
                vdefx9z = minmax(vdefx9),
                vdlfl1z = minmax(vdlfl1),
                vdlfl7z = minmax(vdlfl7),
                vdlfl8z = minmax(vdlfl8),
                vdvis1z = minmax(vdvis1)) %>% 
  dplyr::select(-vdmre1,
                -vdmde4,
                -vdmde6,
                -vdmde7,
                -vdasp1,
                -vdefx8,
                -vdefx9,
                -vdlfl1,
                -vdlfl7,
                -vdlfl8,
                -vdvis1)

## label data

labelled::var_label(cognition_normalized) <- list(
  vdori1 = "MMSE 10 items (number of correct 0-10)",
  vdmre1z = "CERAD word list recognition task (0-20)",
  vdmde3 = "MMSE 3 word delayed recall (0-3)",
  vdmde4z = "CERAD word list delayed (0-10)",
  vdmde1 = "10 word delayed recall (0-10)",
  vdmde6z = "Logical memory delayed A",
  vdmde7z = "Logical memory delayed B",
  vdefx2 = "Trails B time (observed 32-300 seconds)",
  vdasp1z = "Symbol Digit Modalities Test score",
  vdasp2 = "Trails A",
  vdasp3 = "MMSE spell world backwards",
  vdefx8z = "Digit span backwards",
  vdefx9z = "Digit span forwards", 
  vdlfl1z = "Category fluency (animals)",
  vdlfl2 = "Naming 2 items HRS TICS scissors cactus",
  vdlfl3 = "Naming 2 items MMSE",
  vdlfl4 = "MMSE write a sentence",
  vdlfl5 = "MMSE read and follow command",
  vdlfl7z = "Boston naming test",
  vdlfl8z = "Controlled oral word association",
  vdvis1z = "CERAD constructional praxis")

# Make table to review
# 
cognition_normalized %>%
  dplyr::select(-ADAMSSID) %>%
  gtsummary::tbl_summary(
    statistic = list(
      c(vdmre1z, vdmde4z, vdmde6z, vdmde7z,
        vdasp1z, vdefx8z, vdefx9z,
        vdlfl1z, vdlfl7z, vdlfl8z,
        vdvis1z) ~ "{mean} ({sd})")) %>%
  gtsummary::as_gt() %>%
  gt::gtsave("HCAP-ADAMS-MODELVARS-SCORED-update2023-11-16.rtf")

###
## prep for analysis
###

# add in weights

get_weights <- ADAMS1TRK_R %>% 
  select(ADAMSSID, SECLUST, AASAMPWT_F, SESTRAT)

analysis_data <- left_join(cognition_normalized, get_weights, by = "ADAMSSID")

saveRDS(cognition_normalized, here::here(RDS_path, "020_cognition-normalized.rds"))
saveRDS(analysis_data, here::here(RDS_path, "020_analysis_data.rds"))
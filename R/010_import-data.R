rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
source(here::here("R","002_libraries.R"))
source(here::here("R","005_folder-paths-and-options.R"))

## AN_R has cognitive data. 

ADAMS1AN_R <- haven::read_sav(fs::path(data_path, "ADAMS1AN_R.sav"))

## Loading tracker filer for demographics and weights

ADAMS1TRK_R <- haven::read_dta(fs::path(data_path, "ADAMS1TRK_R.dta"))

## AD_R has dementia information

ADAMS1AD_R <- haven::read_sav(fs::path(data_path, "ADAMS1AD_R.sav"))

## Get data for robust norm sample

ADAMS_WAVE_C <- haven::read_sav(fs::path(data_path, "ADAMS1CD_R.sav"))
HRS_2002 <- haven::read_sav(fs::path(hrs_data_2002_path, "H02C_R.sav"))
HRS_2004 <- haven::read_sav(fs::path(hrs_data_2004_path, "H04C_R.sav"))
HRS_2006 <- haven::read_sav(fs::path(hrs_data_2006_path, "H06C_R.sav"))
HRS_TRK <- haven::read_sav(fs::path(hrs_data_path, "2020", "trk2020tr_r.sav"))

## Save out files

saveRDS(ADAMS1AN_R, here::here(RDS_path, "010_ADAMS1AN_R.RDS"))
saveRDS(ADAMS1TRK_R, here::here(RDS_path, "010_ADAMS1TRK_R.RDS"))
saveRDS(ADAMS1AD_R, here::here(RDS_path, "010_ADAMS1AD_R.RDS"))

saveRDS(ADAMS_WAVE_C, here::here(RDS_path, "010_ADAMSWAVEC.RDS"))
saveRDS(HRS_2002, here::here(RDS_path, "010_HRS_2002.RDS"))
saveRDS(HRS_2004, here::here(RDS_path, "010_HRS_2004.RDS"))
saveRDS(HRS_2006, here::here(RDS_path, "010_HRS_2006.RDS"))
saveRDS(HRS_TRK, here::here(RDS_path, "010_HRS_TRK.RDS"))



cog_long <- UMNDataset_0606203 %>% 
  dplyr::select(sub_id, CompositeAttentionHughes, CompositeLanguageHughes, CompositeMemoryHughes, CompositeVisuospatialHughes,
                CompositeExecutiveHughes) %>% 
  tidyr::pivot_longer(names_to = "test", values_to = "cognition") %>% 
  group_by(sub_id) %>% 
  dplyr::summarize(iSD = sd(cognition, na.rm = T))

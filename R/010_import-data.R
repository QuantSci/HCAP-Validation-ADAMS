rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
source(here::here("R","002_libraries.R"))
source(here::here("R","005_folder-paths-and-options.R"))

## AN_R has cognitive data. 

ADAMS1AN_R <- haven::read_sav(fs::path(data_path, "ADAMS1AN_R.sav"))

## Loading tracker filer for demographics and weights

ADAMS1TRK_R <- haven::read_dta(fs::path(data_path, "ADAMS1TRK_R.dta"))

## AD_R has dementia information

ADAMS1AD_R <- haven::read_sav(fs::path(data_path, "ADAMS1AD_R.sav"))

## Save out files

saveRDS(ADAMS1AN_R, here::here(RDS_path, "010_ADAMS1AN_R.RDS"))
saveRDS(ADAMS1TRK_R, here::here(RDS_path, "010_ADAMS1TRK_R.RDS"))
saveRDS(ADAMS1AD_R, here::here(RDS_path, "010_ADAMS1AD_R.RDS"))

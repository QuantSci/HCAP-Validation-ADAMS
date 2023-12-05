# clear environment
rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
source(here::here("R","002_libraries.R"))
source(here::here("R","005_folder-paths-and-options.R"))

# read data

analysis_data <- readRDS(here::here(RDS_path, "020_analysis_data.rds")) %>% 
  mutate(ADAMSSID = as.numeric(ADAMSSID)) %>% 
  labelled::remove_labels() %>% 
  labelled::remove_attributes("label") %>% 
  labelled::remove_attributes("display_width") %>% 
  tidyr::unite(SECU_R, c("SECLUST", "SESTRAT"), sep = ".", remove = FALSE) %>% # Restructure strata so Mplus doesn't yell
  select(-SECLUST) %>% 
  select(ADAMSSID, SECU_R, vdefx2, vdefx8z, vdefx9z, vdasp1z, vdasp2, vdasp3, SESTRAT, AASAMPWT_F)

fs::dir_create(Mplus_path, "120_exfunc-model")
setwd(here::here(Mplus_path, "120_exfunc-model"))

exfunc_model <- mplusObject(
  MODEL = "
  efx BY vdefx2* vdefx8z vdefx9z vdasp1z vdasp2 vdasp3;
  
  efx@1;
  
  method BY vdefx2@1 vdasp2@1;
  
  method WITH efx@0;
  
  ",
  VARIABLE = "idvariable = ADAMSSID;
  CATEGORICAL ARE vdasp3;
  WEIGHT = AASAMPWT_F;
  CLUSTER = SECU_R;
  STRATIFICATION = SESTRAT;",
  ANALYSIS = "ESTIMATOR = WLSMV;
  TYPE = COMPLEX;",
  usevariables = colnames(analysis_data),
  rdata = analysis_data,
  SAVEDATA = "FILE IS scores.dat;
  SAVE is fscores;
  FORMAT is free;",
  OUTPUT = "SVALUES STANDARDIZED MODINDICES;"
)

exfunc <- mplusModeler(exfunc_model, modelout = "exfunc.inp", run = TRUE)

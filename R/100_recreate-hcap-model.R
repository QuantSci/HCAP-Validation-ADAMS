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
  select(-SECLUST)

fs::dir_create(Mplus_path, "100_recreate-hcap-nofixedloadings")
setwd(here::here(Mplus_path, "100_recreate-hcap-nofixedloadings"))

hcap_model_nofixedloadings <- mplusObject(
  MODEL = "
  !ori BY vdori1*;
  
  mem BY vdmre1* vdmde3 vdmde4 vdmde8 vdmde6 vdmde7;
  
  efx BY vdefx2* vdasp1 vdasp2 vdasp3 vdefx8 vdefx9;
  
  lfl BY vdlfl1* vdlfl2 vdlfl3 vdlfl4 vdlfl5 vdlfl7 vdlfl8;
  
  !vsp BY vdvis1*;
  
  mem@1;
  efx@1;
  lfl@1;
  
  vdori1 WITH mem efx lfl vdvis1;
  mem WITH efx lfl vdvis1;
  efx WITH lfl vdvis1;
  lfl WITH vdvis1;
  
  ! Methods factors
  vdmre1 WITH vdmde4;
  
  ",
  VARIABLE = "idvariable = ADAMSSID;
  CATEGORICAL ARE vdmde3 vdmde8 vdasp3 vdlfl2 vdlfl3 vdlfl4 vdlfl5;
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

hcap_nofixedloadings <- mplusModeler(hcap_model_nofixedloadings, modelout = "hcap_nofixedloadings.inp", run = TRUE)

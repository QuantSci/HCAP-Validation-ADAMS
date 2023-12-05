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
  select(ADAMSSID, SECU_R, vdmre1z, vdmde3, vdmde4z, vdmde1, vdmde6z, vdmde7z, SESTRAT, AASAMPWT_F)

fs::dir_create(Mplus_path, "110_memory-model")
setwd(here::here(Mplus_path, "110_memory-model"))

memory_model <- mplusObject(
  MODEL = "
  mem BY vdmre1z* vdmde3 vdmde4z vdmde1 vdmde6z vdmde7z;
  
  mem@1;
  
  method BY vdmre1z@1 vdmde1@1;
  
  method WITH mem@0;
  
  ",
  VARIABLE = "idvariable = ADAMSSID;
  CATEGORICAL ARE vdmde3;
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

memory <- mplusModeler(memory_model, modelout = "memory.inp", run = TRUE)



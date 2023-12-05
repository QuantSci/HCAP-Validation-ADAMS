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
  select(ADAMSSID, SECU_R, vdlfl1z, vdlfl2:vdlfl5, vdlfl7z, vdlfl8z, SESTRAT, AASAMPWT_F)

fs::dir_create(Mplus_path, "130_language-model")
setwd(here::here(Mplus_path, "130_language-model"))

language_model <- mplusObject(
  MODEL = "
  lfl BY vdlfl1z* vdlfl2 vdlfl3 vdlfl4 vdlfl5 vdlfl7z vdlfl8z;
  
  lfl@1;
  
  ",
  VARIABLE = "idvariable = ADAMSSID;
  CATEGORICAL ARE vdlfl2 vdlfl3 vdlfl4 vdlfl5;
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

language <- mplusModeler(language_model, modelout = "language.inp", run = TRUE)

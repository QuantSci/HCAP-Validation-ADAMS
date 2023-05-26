data_path <- fs::path(QSPtools::network_path(),
                      "STUDIES", "HRSADAMS", "data", "SOURCE")

hrs_data_path <- fs::path(QSPtools::network_path(),
                          "STUDIES", "HRS", "POSTED", "DATA", "SOURCE")

hrs_data_2002_path <- fs::path(hrs_data_path, "2002")
hrs_data_2004_path <- fs::path(hrs_data_path, "2004")
hrs_data_2006_path <- fs::path(hrs_data_path, "2006")

user<- Sys.getenv("USER")
if (user=="rnj") {
  data_path <- gsub("Volumes/Research/BM", "Volumes/BM", data_path)
}
data_path

data_dictionary_path <- fs::path(QSPtools::network_path(), "STUDIES", "HRSADAMS", "HCAP_VALIDATION")

fs::dir_create(here::here(QSPtools::network_path(), "STUDIES", "HRSADAMS", "HCAP_VALIDATION", "R", "Images"))
images_path <- here::here(QSPtools::network_path(), "STUDIES", "HRSADAMS", "HCAP_VALIDATION", "R", "Images")

fs::dir_create(here::here(QSPtools::network_path(), "STUDIES", "HRSADAMS", "HCAP_VALIDATION", "R", "Mplus"))
Mplus_path <- here::here(QSPtools::network_path(), "STUDIES", "HRSADAMS", "HCAP_VALIDATION", "R", "Mplus")

fs::dir_create(here::here(QSPtools::network_path(), "STUDIES", "HRSADAMS", "HCAP_VALIDATION", "R", "RDS"))
RDS_path <- here::here(QSPtools::network_path(), "STUDIES", "HRSADAMS", "HCAP_VALIDATION", "R","RDS")
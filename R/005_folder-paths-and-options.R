data_path <- fs::path(QSPtools::network_path(),
                      "STUDIES", "HRSADAMS", "data", "SOURCE")
user<- Sys.getenv("USER")
if (user=="rnj") {
  data_path <- gsub("Volumes/Research/BM", "Volumes/BM", data_path)
}
data_path

fs::dir_create(here::here(QSPtools::network_path(), "STUDIES", "HRSADAMS", "HCAP_VALIDATION", "R", "Images"))
images_path <- here::here(QSPtools::network_path(), "STUDIES", "HRSADAMS", "HCAP_VALIDATION", "R", "Images")

fs::dir_create(here::here(QSPtools::network_path(), "STUDIES", "HRSADAMS", "HCAP_VALIDATION", "R", "Mplus"))
Mplus_path <- here::here(QSPtools::network_path(), "STUDIES", "HRSADAMS", "HCAP_VALIDATION", "R", "Mplus")

fs::dir_create(here::here(QSPtools::network_path(), "STUDIES", "HRSADAMS", "HCAP_VALIDATION", "R", "RDS"))
RDS_path <- here::here(QSPtools::network_path(), "STUDIES", "HRSADAMS", "HCAP_VALIDATION", "R","RDS")
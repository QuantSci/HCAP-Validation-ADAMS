library(pacman)
pacman::p_load(tidyverse,
               haven,
               gtsummary,
               MplusAutomation,
               survey,
               srvyr,
               nnet,
               splitstackshape,
               rms,
               ggalt,
               mice,
               nnet,
               labelled)

label_vd_data <- function(data){
  labelled::var_label(data) <- list(
    vdori1 = "MMSE 10 items (number of correct 0-10)",
    vdmre1 = "CERAD word list recognition task (0-20)",
    vdmie2 = "MMSE 3 word recognition (0-3)",
    vdmde8 = "CERAD constructional praxis delayed (0-4)",
    vdmde1 = "CERAD word list delayed (0-10)",
    vdmde6 = "Logical memory delayed A",
    vdmde7 = "Logical memory delayed B",
    vdefx2 = "Trails B time (observed 32-300 seconds)",
    vdasp1 = "Symbol Digit Modalities Test score",
    vdasp2 = "Trails A",
    vdasp3 = "MMSE spell world backwards",
    vdefx8 = "Digit span backwards",
    vdefx9 = "Digit span forwards", 
    vdlfl1 = "Category fluency (animals)",
    vdlfl2 = "Naming 2 items HRS TICS scissors cactus",
    vdlfl3 = "Naming 2 items MMSE",
    vdlfl4 = "MMSE write a sentence",
    vdlfl5 = "MMSE read and follow command",
    vdlfl7 = "Boston naming test",
    vdlfl8 = "Controlled oral word association",
    vdvis1 = "CERAD constructional praxis"
  )
}


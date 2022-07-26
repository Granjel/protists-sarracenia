#load flow function
##loads the summary data from each run
##needs consistent naming in csv files and location
##needs complete data (no combination of protist, bacteria, rep and time missing)
##for example, if KL, BAll, rep = c(1, 2), time = c(1, 2), all csv must be present
##individual csv required (one csv per run, not multiple runs in a csv)
load_flow <- function(protist, bacteria, rep, time){
  
  #requires
  library(tidyverse); library(stringr)
  
  #data location (Google Drive)
  location <- c("Q:/My Drive/FlowCam Data") 
  
  #protist strain
  #protist <- c("KL")
  
  #bacteria strain or combination (BAll = *all bacteria*)
  #bacteria <- c("BAll")
  
  #define culture name (protist x bacteria; example: *KLxBAll*)
  culture <- apply(expand.grid(protist, bacteria), 1, paste, collapse = "x")
  
  #replicate
  #rep <- c(1) #change this
  rep <- paste("rep", rep, sep = "")
  
  #time
  #time <- c(1, 2, 5, 6) #change this
  time <- paste("time", time, sep = "")
  
  #file type
  f <- c("csv")
  
  #metadata
  meta <- apply(expand.grid(culture, rep, time), 1, paste, collapse = "_")
  
  #final files to load
  datafiles <- paste(paste(location, paste(meta, "summary", sep = "_"), sep = "/"), f, sep = ".")
  
  data <- NULL
  for (i in 1:length(datafiles)){
    #information about each sample
    header <- data.frame(cbind(meta[i],
                               str_split(str_split(meta[i], pattern = "_", simplify = TRUE)[1, 1], pattern = "x", simplify = TRUE),
                               str_split(meta[i], pattern = "_", simplify = TRUE)))
    colnames(header) <- c("code", "protist", "bacteria", "culture", "rep", "time")
    header$rep <- as.numeric(str_split_fixed(header$rep, pattern = "rep", n = 2)[,2])
    header$time <- as.numeric(str_split_fixed(header$time, pattern = "time", n = 2)[,2])
    
    #match with data
    data <- rbind(data, cbind(header, as.data.frame(t(read.csv(datafiles[i])))[,-1]))
  }
  
  data <- data[, 1:38]
  rownames(data) <- NULL
  colnames(data) <- c("code", "protist", "bacteria", "culture", "replicate", "time", "run", "mode", "priming_method",
                      "flow_rate_ml_min", "recalibrations", "stop_reason", "sample_volume_aspirated_ml", "sample_volume_processed_ml",
                      "fluid_volume_imaged_ml", "efficiency", "particle_count", "images", "total", "used", "percentage_used",
                      "particles_per_image", "frame_rate_fps", "background_intensity_mean", "background_intensity_min",
                      "background_intensity_max", "date_time", "start_time", "end_time", "sampling_time", "environment", "software",
                      "magnification", "calibration_factor", "serial_number", "number_of_processors", "pump", "syringe_size_ml")
  
  data$replicate <- as.character(data$replicate)
  data$recalibrations <- as.numeric(data$recalibrations)
  data$flow_rate_ml_min <- as.numeric(str_split_fixed(data$flow_rate_ml_min, pattern = " ", n = 4)[,3])
  data$sample_volume_aspirated_ml <- as.numeric(str_split_fixed(data$sample_volume_aspirated_ml, pattern = " ", n = 3)[,2])
  data$sample_volume_processed_ml <- as.numeric(str_split_fixed(data$sample_volume_processed_ml, pattern = " ", n = 3)[,2])
  data$fluid_volume_imaged_ml <- as.numeric(str_split_fixed(data$fluid_volume_imaged_ml, pattern = " ", n = 3)[,2])
  data$efficiency <- parse_number(data$efficiency)/ 100
  data$particle_count <- as.numeric(data$particle_count)
  data$total <- as.numeric(data$total)
  data$used <- as.numeric(data$used)
  data$percentage_used <- parse_number(data$percentage_used)/ 100
  data$particles_per_image <- as.numeric(data$particles_per_image)
  data$particles_per_image <- as.numeric(data$particles_per_image)
  data$frame_rate_fps <- as.numeric(str_split_fixed(data$frame_rate_fps, pattern = " ", n = 4)[,3])
  data$background_intensity_mean <- as.numeric(data$background_intensity_mean)
  data$background_intensity_min <- as.numeric(data$background_intensity_min)
  data$background_intensity_max <- as.numeric(data$background_intensity_max)
  data$calibration_factor <- as.numeric(data$calibration_factor)
  data$serial_number <- as.numeric(data$serial_number)
  data$number_of_processors <- as.numeric(data$number_of_processors)
  data$syringe_size_ml <- as.numeric(str_split_fixed(data$syringe_size_ml, pattern = " ", n = 4)[,3])
  data$particles_ml <- data$particle_count / data$sample_volume_processed_ml
  
  #STILL NEED TO DEAL WITH TIME TRANSFORMATIONS
  
  return(data)
  
}
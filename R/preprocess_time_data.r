# rm(list=ls())
# library(dplyr)
# source("validate.r")
# #dat <- time_data

preprocess_time_data <- function(dat) {

  # test if object of class be_monk
  stopifnot(inherits(dat, "be_monk"))
  
  cn_time_stamps <-   colnames(dat[, -c("ID", "INTERVIEWER")])
  
  mc_elements <- cn_time_stamps[grepl(paste0("_A", 1:9, collapse="|"), cn_time_stamps  )]
  mc_stems <- unique(gsub(paste0("_A", 1:9, collapse="|"), replacement = "", mc_elements))
  
  mc_stems %>%                 
    lapply(function(x) {
      mc_elements_belonging_to_stem <- mc_elements[which(gsub(paste0("_A", 1:9, collapse="|"), replacement = "", mc_elements) == x)] 
      dat[, c(paste0(x, "_MC") ) := rowSums(.SD, na.rm = T), .SDcols = c(mc_elements_belonging_to_stem)]
    }) %>%           
    invisible                                                     
  
  # aus 0en NAs machen - muss man allgemeiner lössen
  for(col in names(dat)) set(dat, i=which(dat[[col]] %in% 0), j=col, value=NA)

 
  time_var <- c("INTERVIEWER", "ID", 
                setdiff(cn_time_stamps, mc_elements),
                paste0(mc_stems, "_MC")
  )
  
  dat <- dat[, time_var, with=F]
  
return(dat)
  
}



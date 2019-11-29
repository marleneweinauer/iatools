# rm(list=ls())
# library(dplyr)
# source("validate.r")



preprocess_time_data <- function(dat) {

  dat <- copy(dat)
  
  # test if object of class be_monk
  stopifnot(inherits(dat, "monk_time"))
  
  cn_time_stamps <-   colnames(dat[, -c("ID", "INTERVIEWER")])
  
  mc_elements <- cn_time_stamps[grepl(paste0("_A", 1:9, collapse="|"), cn_time_stamps  )]
  
  if (length(mc_elements) > 0) {
  mc_stems <- unique(gsub(paste0("_A", 1:9, collapse="|"), replacement = "", mc_elements))
  
  mc_stems %>%                 
    lapply(function(x) {
      mc_elements_belonging_to_stem <- mc_elements[which(gsub(paste0("_A", 1:9, collapse="|"), replacement = "", mc_elements) == x)] 
    if ( x %in% colnames(dat)) {mc_elements_belonging_to_stem <- c(mc_elements_belonging_to_stem, x)}
        dat[, c(paste0(x, "_MC") ) := rowSums(.SD, na.rm = T), .SDcols = c(mc_elements_belonging_to_stem)]
    }) %>%           
    invisible      
  
  translate_table <- rbindlist(   # das schöner mit time process integrieren
    lapply(mc_stems, 
          function(x) {
            mc_elements_belonging_to_stem <- mc_elements[which(gsub(paste0("_A", 1:9, collapse="|"), replacement = "", mc_elements) == x)] 
            if ( x %in% colnames(dat)) {mc_elements_belonging_to_stem <- c(mc_elements_belonging_to_stem, x)}            
  data.table(orignial_name =  mc_elements_belonging_to_stem, 
            mc_name = paste0(x, "_MC") ) }
    )
  )
  
  # lapply( 
  #   unique(translate_table$mc_name), 
  #   function(x) {
  #     blibb <- translate_table[mc_name == x, orignial_name]
  #     dat[, c(x) := rowSums(.SD, na.rm = T), .SDcols = blibb]
  #     }
  #   )
  
  
  # aus 0en NAs machen - muss man allgemeiner lössen
  for(col in names(dat)) set(dat, i=which(dat[[col]] %in% 0), j=col, value=NA)

 
  time_var <- c("INTERVIEWER", "ID", 
                setdiff(cn_time_stamps, c(mc_stems, mc_elements)),
                paste0(mc_stems, "_MC")
  )
  

  

  
  dat <- dat[, time_var, with=F]
  
  setattr(dat, "translate_table", translate_table)
  }
  
  
  
return(dat)
  
}



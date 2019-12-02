# die Funktion muss man technisch noch viel besser durchdenken...wie man das mit der ARgument übergabe macht und so :) 

# TODO: methods!!!


# ist so noch keine gute Lösung !!! Mit Stefan besprechen 
# 
  #  survey_data = survey_data 
  #  time_data = time_data
  #  path = "~/mnt/user_home/Git/interviewer_analysis/iatools/inst/reports"
  #  interviewer = "T143@statistik.gv.at"
  #  tools = c("shares")
  # conspi = T

#' @export
render_specific_interviewer <- function(survey_data, 
                                        time_data,
                                        path = "~/mnt/user_home/Git/interviewer_analysis/iatools/inst/reports",
                                        interviewer,
                                        tools = c("q20","q", "shares"),
                                        conspi = T
) {
  
  survey_data <- copy(survey_data)
  time_data <- copy(time_data)
  
  translate_table <- NULL

  if ( any(c("q", "q20", "median") %in% tools) ) {
    
  #if (is.null(time_data)) {stop("In the tools methods to analyse time_data are indicated, but no time_data is available!
  #                         Please either provide time_data or set tools = 'shares' ")}  

# Time data      
prepro_time_data <- preprocess_time_data(dat = time_data)

time_short_dat_detailed <-  inv_time_data(data = prepro_time_data,
                                            method = c("q20","q", "median"),
                                            answer_options_to_exclude = 0 ,
                                            quantil_of_fastest_interviews = 0.05,
                                            quantil_of_fastest_interviewer= 0.9,
                                            min_of_fast_interviews = 10, 
                                            output = "detailed")  

time_short_dat_detailed_INT <- time_short_dat_detailed [INTERVIEWER == interviewer]

if ("translate_table" %in% names(attributes(prepro_time_data)) ) {
  translate_table <-  attr(prepro_time_data, "translate_table")
}
  

}

 
if ("shares" %in% tools) {
  
#  if (is.null(survey_data)) {stop("In the tools methods to analyse survey_data are indicated, but no survey_data is available!
#                           Please either provide survey_data or remove 'shares' in tools ")} 

 
  
shares_detailed_dat <- inv_answer_shares(
  survey_data = survey_data,
  answer_options_to_exclude = c(-1,-2,-3),
  alpha = 0.05, 
  translate_table = translate_table,
  output = "detailed"
)

# shares overview

shares_detailed_dat[, any_shares_conspi := max(shares_conspi, na.rm = T), by = c("INTERVIEWER", "variable")]
shares_detailed_dat_INT <- shares_detailed_dat[INTERVIEWER == interviewer,]
if (conspi) {
  shares_rmd <- shares_detailed_dat_INT[any_shares_conspi == 1]
} else {
  shares_rmd <- shares_detailed_dat_INT
}

}



  # q overview 

  if ("q" %in% tools) {

  if (conspi) {
    time_q <- time_short_dat_detailed_INT[q_conspi == 1 & value < q_threshold, ]
  } else {
    time_q <- time_short_dat_detailed_INT
  }  
 
    time_q <- time_q[,  c("INTERVIEWER", "variable", "ID",  "value",           # daugt mir noch nicht so arg
                          "median_INTERVIEWER", "median_est",  "q_threshold", "q_conspi")]
    setorderv(time_q, c("INTERVIEWER", "variable", "value"))  
  
    
    split_DT <- split( time_q  , f =  time_q$variable )
    split_DT <- split_DT[lapply(split_DT, nrow) > 0]
         
  }
 

  
  # 20%-Quantil 
  
  if ("q20" %in% tools ) {
  
 q20_rmd <-  time_short_dat_detailed_INT[, .SD[1], by=.(q20_conspi, variable)]
 if (conspi) {
  q20_conspi_rmd <- q20_rmd[q20_conspi == 1, ]
 } else {
   q20_conspi_rmd <- q20_rmd
 }
  q20_conspi_rmd <-  q20_conspi_rmd[, c("INTERVIEWER", "variable", "ID", "q20_INTERVIEWER",  "q20_est", "COUNT", "q20_conspi")]  # COUNT noch erstellen!!!
  
  }
  
  # Median
  
  if ("median" %in% tools ) {
  
  if (conspi) {
  median_conspi_rmd <- q20_rmd[median_conspi == 1, ]
  } else {
    median_conspi_rmd <- q20_rmd
  }
  median_conspi_rmd <-  median_conspi_rmd[, c("INTERVIEWER", "variable", "ID", "median_INTERVIEWER",  "median_est", "COUNT", "median_conspi")]  # COUNT noch erstellen!!!
  
  }
  
  
  if (conspi) {
  output_dir <- file.path(path, "conspi")
  } else {
    output_dir <- file.path(path, "full")
  }
  
  
  render("~/mnt/user_home/Git/interviewer_analysis/iatools/inst/reports/specific_interviewer.Rmd",
        output_dir = output_dir,
        output_file = paste0(substr(interviewer, 1, 4), "specific_interviewer.html")
  )
 # browseURL(file.path(path, paste0(substr(interviewer, 1, 4), "specific_interviewer.html") ) )
  
}


###===





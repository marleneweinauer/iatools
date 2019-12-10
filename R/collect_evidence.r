#time_data
#survey_data 
#tools = c("q20","q", "shares")

#' @export
collect_evidence <- function(
  time_data = NULL,
  survey_data = NULL, 
  tools = c("q20","q", "shares"), 
  quantil_of_fastest_interviews = 0.05,
  quantil_of_fastest_interviewer= 0.9,
  min_of_fast_interviews = 10, 
  answer_options_to_exclude_surv = c(-1,-2,-3),
  alpha = 0.05, 
  answer_options_to_exclude_time = 0
) {
  

if ( any(c("q", "q20", "median") %in% tools) ) {
  
  if (is.null(time_data)) {stop("In the tools methods to analyse time_data are indicated, but no time_data is available!
                           Please either provide time_data or set tools = 'shares' ")}
    
prepro_time_data <- preprocess_time_data(time_data)

time_short_dat <-  inv_time_data(data = prepro_time_data,
                                 method = setdiff(tools, "shares"),
                                 answer_options_to_exclude = answer_options_to_exclude_time ,
                                 quantil_of_fastest_interviews = quantil_of_fastest_interviews,
                                 quantil_of_fastest_interviewer= quantil_of_fastest_interviewer,
                                 min_of_fast_interviews =  min_of_fast_interviews )

  # Option 1: Time Data + Shares 
  if ("shares" %in% tools) {
    
  if (is.null(survey_data)) {stop("In the tools methods to analyse survey_data are indicated, but no survey_data is available!
                           Please either provide survey_data or remove 'shares' in tools ")}
      
  shares_short_dat <- inv_answer_shares(
  survey_data = survey_data,
  answer_options_to_exclude = c(-1,-2,-3),
  alpha = 0.05, 
  translate_table = attr(prepro_time_data, "translate_table")
  )

  conspi_DT <- 
  merge(time_short_dat, 
        shares_short_dat, 
        by = c("INTERVIEWER", "variable"), 
        all.x = T
  )


} else {
  
  # Option 2: Only Time Data
  conspi_DT <- time_short_dat
  
}

} else {
  
  if (is.null(survey_data)) {stop("In the tools methods to analyse survey_data are indicated, but no survey_data is available!
                           Please either provide survey_data or remove 'shares' in tools ")}
  
  # Option 3: Only Shares
  conspi_DT <- inv_answer_shares(
    survey_data = survey_data,
    answer_options_to_exclude =  answer_options_to_exclude_surv,
    alpha = alpha, 
    translate_table = NULL
  )
  
}


conspi_DT.m <- melt(conspi_DT, 
                    variable.name = "method_conspi")

conspi_DT.m[is.na(value), value := 0]

setattr(conspi_DT.m, 
        "key_variables", attr(survey_data, "key_variables")
        )  # das mit den key variables daugt mir noch gar ned 

return(conspi_DT.m)


}


#===================




#conspi_DT.m <- collect_evidence(
#  time_data = time_data, 
#  survey_data = survey_data, 
#  tools = c("shares", "q", "q20")
#)

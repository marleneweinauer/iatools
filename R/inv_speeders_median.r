
# data = time_data
# method = "q20"
# answer_options_to_exclude = 0 



#' Test for fast Quantiles in Answer Times 
#' 
#' This function investigates if an interviewer has a very low 0.2-quantile or median in any question. 
#' 
#' @param data data table of class **be_monk**
#' @param method "q20" if the 0.2 shall be compared; "median" if the median shall be compared. 
#' @param answer_options_to_exclude a vector of answer options that shall not be included in the analysis
#' @return data table with 0.2-quantiles/medians in times for each question-interviewer-combination
#' @details If an interviewer has many low medians, he/she could be a complete falsifier. If an interviewer has many low 0.2-quantiles, but not low medians, he/she could be a partly falifier. 
#'
#' @export
inv_speeders_median <- function(data, 
                               method = "q20", 
                             answer_options_to_exclude) {

  # test if object of class be_monk
  stopifnot(inherits(data, "be_monk"))
  
  # create long table
  dat_long <-  melt(data, id.vars = c("INTERVIEWER", "ID") )

  
  # Stefan fragen ob das eher schon in die S3 Definition - das ist halt nicht sehr gnerell 
  # eliminate unwanted answer_options
  if (!is.null(answer_options_to_exclude)) {
  for(col in names(dat_long)) set(dat_long, i=which(dat_long[[col]] %in% answer_options_to_exclude), j=col, value=NA)
  }
  
  # remove not asked questions
  dat_long <- na.omit(dat_long)
  
  if ( method == "q20") {
 dat_long[ , value_variable_INTERVIEWER := quantile(value, 0.2, na.rm = T),
           by = c("variable", "INTERVIEWER")]
   dat_long[, est_variable := quantile(value, 0.2, na.rm = T), by = "variable"]  # create estimates - noch besser durchdenken !!!!!!!!!!!!!!!!!!!!
 }

 if ( method == "median") {
dat_long[ , value_variable_INTERVIEWER := median(value, na.rm = T),
           by = c("variable", "INTERVIEWER")] 
   dat_long[, est_variable := median(value, na.rm = T), by = "variable"]         # create estimates - noch besser durchdenken !!!!!!!!!!!!!!!!!!!!
   } 
 

  #dat_long[, blubb := 10^(median(log(value))) - mad(log(value)) , 
  #         by = c("variable", "INTERVIEWER")]
  
 setkey(dat_long, variable, INTERVIEWER)

 dat_median <- dat_long[.(expand.grid(unique(variable),unique(INTERVIEWER)) ) , mult = "first"]  # konstistent mit inv_sppeders file machen - dort ist es mit marker
 dat_median <- dat_median[, ID := NULL]
 
 dat_median[, threshold := 0.5 * est_variable]  # steht jetzt nicht so im paper!! muss man korriegier 
 
 dat_median[, conspi := as.numeric(value_variable_INTERVIEWER <= threshold)]
 dat_median[, COUNT := .N, by = c("INTERVIEWER", "conspi")]
 
 return(dat_median)
 
}


 
 
 
 
 
 
 


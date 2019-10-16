# rm(list=ls())
# library(dplyr)
# source("validate.r")


# data <- time_data
#  quantil_of_fastest_interviews = 0.05;
#  quantil_of_fastest_interviewer= 0.9;
#  min_of_fast_interviews = 5;
#  answer_options_to_exclude = 0


#' Test for fastest answers
#' 
#' This function tests if an interviewer has provided conspicious many fast answer times within the fastest answer times of all interviews conducted by all interviewers. 
#' 
#' @param data data table of class **be_monk**
#' @param quantil_of_fastest_interviews quantil of fastest time stamps for a specific question within all interviewers
#' @param quantil_of_fastest_interviewer quantil of presence of specific interviewer with the fastest interval
#' @param quantil_of_fastest_interviewer quantil of presence of specific interviewer with the fastest interval
#' @param min_of_fast_interviews min of fast answers within the fastest quantil of all interviews that an invertviewer is regarded conspicious for this question
#' @return list containing quantil_speeders_dt (data.table with binary variable if interviewer was conspicious regarding the fastest interviews for each question-interviewer-combination) & dat_long (detailed information of fast interviews)
#' @details An interviewer conspicious for a specific question has produced many extreme results for this specific question. If this interviewer is also conspicious according to method "q20" in \code{\link{inv_speeders_median}}, this interviewer could partly falsify this question. On the other hand if an interviewer is not additionally conspicious due to method "q20", he or she has produced extreme results, but the majority of interviews was not salient. Extreme results could be therefore outcomes of other reasons, e.g. clicking through the questionnaire sessions to get acquainted with the questions. 
#'
#' @export
inv_speeders_q <- function(data, 
                          quantil_of_fastest_interviews = 0.05,
                          quantil_of_fastest_interviewer = 0.9,
                          min_of_fast_interviews = 5, 
                          answer_options_to_exclude) {

  
  # test if object of class be_monk
  stopifnot(inherits(data, "be_monk"))
  
  # create long table
  dat_long <-  melt(data, id.vars = c("INTERVIEWER", "ID") )

  # Stefan fragen ob das eher schon in die S3 Definition
  # eliminate unwanted answer_options
  if (!is.null(answer_options_to_exclude)) {
    for(col in names(dat_long)) {
      set(dat_long, 
          i=which(dat_long[[col]] %in% answer_options_to_exclude), 
          j=col, 
          value=NA)
    }
  }
  
  
  dat_long[, threshold_value := quantile(value, 
                                         quantil_of_fastest_interviews, 
                                         na.rm = T), 
           by = variable]

 #to_clust_Fragenlabel <- dat_long[ variable == Fragenlabel,]
 #threshold_value <- quantile(as.numeric(dat_long[ variable == Fragenlabel, value]), 
#                             quantil_of_fastest_interviews, 
#                             na.rm = T)
 

 dat_long[, 
          sum_values_below_threshold_by_var := sum(value <= threshold_value, na.rm = T),
          by = .(INTERVIEWER, variable)]
 
 
 dat_long[, marker := c(1L, rep(0L, .N-1L)), by=.(INTERVIEWER, variable)]
 #dat_long[, marker_i := c(1L, rep(0L, .N-1L)), by=.(INTERVIEWER)]
 
 quantil_speeders_dt <- dat_long[
   marker == 1 & sum_values_below_threshold_by_var > 0,  
   c("INTERVIEWER", "variable", "sum_values_below_threshold_by_var")
   ]  # soll man größer 0 da rausstreicehn
 
 #quantile(dat_long[marker == 1 & sum_values_below_threshold_by_var > 0, variable], 
#          quantil_of_fastest_interviews, 
#          na.rm = T
#          )
 

  quantil_speeders_dt[,
       threshold_quant_method_1 :=
       floor(quantile(sum_values_below_threshold_by_var,
                      quantil_of_fastest_interviewer, 
                       na.rm = T)),
       by = "variable"]
 
 quantil_speeders_dt[, question_speeded := as.numeric(sum_values_below_threshold_by_var >= threshold_quant_method_1 &
                                                     sum_values_below_threshold_by_var >= min_of_fast_interviews)]
 
 quantil_speeders_dt[, COUNT := sum(question_speeded), by = INTERVIEWER]
 
 #quantil_speeders_dt[, 
#                     number_of_speeded_questions_2 := sum( sum_values_below_threshold_by_var >= threshold_quant_method_1 &
#                                                   sum_values_below_threshold_by_var >= min_of_fast_interviews), 
#                     by = INTERVIEWER]
 
 # muss noch überlegen ob ich das so in der liste will
 quantil_speeders_dt <- quantil_speeders_dt[, c("INTERVIEWER", "variable", "question_speeded", "COUNT")]
 dat_long <- dat_long[, -"marker"]
 
 setattr(dat_long, "key_variables", attr(data, "key_variables"))
 setattr(quantil_speeders_dt, "key_variables", attr(data, "key_variables"))
 
 quantil_speeders_list <- 
   list(
     dat_long = dat_long,
     quantil_speeders_dt  = quantil_speeders_dt 
   )
 
 return(quantil_speeders_list)
}

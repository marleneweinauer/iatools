
# TODO NA-NULL-PROBLEM IN GRIFF BEKOMMEN
# da wird immer hin und her geschwankt und das ist nicht wirklich elegant

      #    data = time_data
       # data = prepro_time_data
       #    method = c("q20", "median", "q")
       #    answer_options_to_exclude = 0
       #    quantil_of_fastest_interviews = 0.05
       #   quantil_of_fastest_interviewer= 0.9
       # min_of_fast_interviews = 5
       # min_COUNT  = 20
      


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
inv_time_data<- function(data, 
                               method = "q20", 
                               quantil_of_fastest_interviews = 0.05,
                               quantil_of_fastest_interviewer= 0.9,
                               min_of_fast_interviews = 15,
                             answer_options_to_exclude, 
                             output = "conspi", 
                         min_COUNT  = 20) {
  
  data <- copy(data)  

  # test if object of class be_monk
  stopifnot(inherits(data, "monk_time"))
  
  # convert to numeric
  for (el in setdiff(colnames(data), c("INTERVIEWER", "ID"))) 
  {data[, (el) := as.numeric(get(el))] }
  
  # create long table
  time_long_dt <-  melt(data, id.vars = c("INTERVIEWER", "ID") )
 
  
  # eifnacher 
  
  
  # Stefan fragen ob das eher schon in die S3 Definition - das ist halt nicht sehr gnerell 
  # eliminate unwanted answer_options
  if (!is.null(answer_options_to_exclude)) {
  for(col in names(time_long_dt)) set(time_long_dt, i=which(time_long_dt[[col]] %in% answer_options_to_exclude), j=col, value=NA)
  }
  
  # remove not asked questions
  time_long_dt <- na.omit(time_long_dt)
  time_long_dt[, COUNT := .N, by = c("INTERVIEWER", "variable")]
 
  
  # quantilfunktion
   if ( "q20" %in% method) {
 time_long_dt[ , q20_INTERVIEWER := quantile(value, 0.2, na.rm = T),
           by = c("variable", "INTERVIEWER")]
   time_long_dt[, q20_est := quantile(value, 0.2, na.rm = T), by = "variable"]  # create estimates - noch besser durchdenken !!!!!!!!!!!!!!!!!!!!
 }

 if ( "median" %in% method) {
time_long_dt[ , median_INTERVIEWER := median(value, na.rm = T),
           by = c("variable", "INTERVIEWER")] 
   time_long_dt[, median_est := median(value, na.rm = T), by = "variable"]         # create estimates - noch besser durchdenken !!!!!!!!!!!!!!!!!!!!
   } 
 
  
  if ( "q" %in% method) {
    time_long_dt[, q_threshold := quantile(value, 
                                           quantil_of_fastest_interviews, 
                                           na.rm = T), 
             by = variable]
    
    time_long_dt[, 
             sum_values_below_threshold_by_var := sum(value <= q_threshold, na.rm = T),
             by = .(INTERVIEWER, variable)]
  } 


 # einschränken auf Interviewer X variable per Kommbination
 
 # time_long_dt[, .SD[1], by=.(INTERVIEWER, variable)] 
 
 time_long_dt[, marker := c(1L, rep(0L, .N-1L)), by=.(INTERVIEWER, variable)] 

 #time_short_dt <- copy(time_long_dt)
  time_short_dt <-  time_long_dt[marker == 1, ]
 #time_long_dt <- time_short_dt[, ID := NULL]
 
 setattr(time_long_dt, "key_variables", attr(data, "key_variables"))
 # steht jetzt nicht so im paper!! muss man korriegier 
 
 if ( "q20" %in% method) {
 time_short_dt[, q20_conspi := as.numeric(q20_INTERVIEWER <= 0.5 * q20_est &
                                            COUNT >= min_COUNT)]
 }
 
 if ( "median" %in% method) {
   time_short_dt[, median_conspi := as.numeric(median_INTERVIEWER <= 0.5 * median_est &
                                                 COUNT >= min_COUNT)]
 }
 
 if ("q" %in% method) {
  
#   time_short_dt[, threshold_quant_method_1 := NA_integer_]
#
#  time_short_dt[
#    sum_values_below_threshold_by_var  != 0,
#    threshold_quant_method_1 := floor(quantile(
#      sum_values_below_threshold_by_var,
#      quantil_of_fastest_interviewer, 
#      na.rm = TRUE
#    )),
#    by = "variable"
#  ]

   
   time_short_dt[sum_values_below_threshold_by_var  == 0 , sum_values_below_threshold_by_var  := NA]
   
   time_short_dt[,
                 threshold_quant_method_1 :=
                   floor(quantile(sum_values_below_threshold_by_var,
                                  quantil_of_fastest_interviewer, 
                                  na.rm = T)),
                 by = "variable"]
   
      
  time_short_dt[, q_conspi := as.numeric(sum_values_below_threshold_by_var >= threshold_quant_method_1 &
                                                  sum_values_below_threshold_by_var >= min_of_fast_interviews)]
  
  time_short_dt[is.na(q_conspi), q_conspi := 0]

 }
 
 

 time_long_dt <- merge(time_long_dt, 
                       time_short_dt[, c("INTERVIEWER", "variable", paste0(method, "_conspi")), with = F], 
                       by = c("INTERVIEWER", "variable"),
                       all.x = T)
 
 
 
 conspi_col <-  colnames(time_short_dt)[grepl("conspi", colnames(time_short_dt))]
 time_short_dt <- time_short_dt[, c("INTERVIEWER", "variable", conspi_col), with = F]
 
 #class(time_short_dt) <- c("monk_speeder", class(time_short_dt))
 
 setattr(time_short_dt, "key_variables", attr(data, "key_variables"))

 if (output == "conspi") {
   return(time_short_dt) } 
 
 if (output == "detailed") {
   return(time_long_dt) } 
 
 
}


# blubb <-  inv_time_data(data = time_data,
# method = c("q20", "median", "q"),
# answer_options_to_exclude = 0 ,
# quantil_of_fastest_interviews = 0.05,
# quantil_of_fastest_interviewer= 0.9,
# min_of_fast_interviews = 5)


 



"%!in%" <- function(x, y) !("%in%"(x, y))

binom_test_adapted <- function(x, n, p, alternative) {
  if (sum(is.na(c(x, n, p))) == 0) {
    ret <- binom.test(x, n, p, alternative=alternative)$p.value
  }  else {
      ret <- NA_real_
      }
  return(ret)
}

p_adjust_adapted <- function(p,n, method) {
  if (sum(is.na(c(p, n))) == 0 ) {
    ret <- p.adjust(p, n, method)
  }  else {
      ret <- NA_real_
      }
  return(ret)
}



# survey_data = survey_data
# answer_options_to_exclude = c(-1,-2,-3)
# alpha = 0.05
# max_answer_options_of_question = 5

#' Test answer distribution
#' 
#' This function investigates the answer shares by interviewer and detects deviances in shares.
#' 
#' @param survey_data data table of class **be_monk**
#' @param answer_options_to_exclude a vector of answer options that shall not be included in the analysis
#' @param alpha singnificance level on that shall be tested 
#' @param max_answer_options_of_question threshold of number of answer options that the question shall be analysed 
#' @return data table with unadjusted and adjusted p-values for each question-answer-interviewer-combination
#' @details Each answer option is treated as a binary variable.
#' To test whether the share of a specific answer option for a specific interviewer deviates from the expected probability,  estimated from all interviews, a binomial test is carried out. p-values are p-adjusted. Significances are calculated on #'level alpha.
#' @export
inv_answer_shares  <- function(
  survey_data,
  answer_options_to_exclude,
  alpha = 0.05, 
  max_answer_options_of_question =15
) {
  
  # test if object of class be_monk
  stopifnot(inherits(survey_data, "be_monk"))

  # Stefan fragen ob das eher schon in die S3 Definition
  # eliminate unwanted answer_options
  for(col in names(survey_data)) set(survey_data, i=which(survey_data[[col]] %in% answer_options_to_exclude), j=col, value=NA)

  survey_data[, ID := NULL] # brauchen wir das überhaupt je? sonst kann man sich das gleich sparen
  
  #colnames(survey_data) %>%       # ist das sehr dumm?          
  #  lapply(function(x) survey_data[, c(x) := as.factor(as.character(c(x)))]) %>%           
  #  invisible    
  
  # create long table
  binom_dt <-  melt(survey_data, 
                    id.vars = "INTERVIEWER")[,
               .N, by = .(INTERVIEWER, variable, value)]
  binom_dt <- binom_dt[!is.na(value), ] # na überdenken
  
  # remove variables with too many answer options (not optimal for binomal analyis)
  binom_dt[, answer_options_of_question := length(table(value)), by = variable]
  binom_dt <- binom_dt[answer_options_of_question <=   max_answer_options_of_question, ]
  
  
  # create variables necessary for binomial test
  binom_dt[,
           question_asked := sum(N),
           by = c("INTERVIEWER", "variable")]

  binom_dt[, ratio := N/question_asked]
  binom_dt[,
           estimate := sum(N)/sum(question_asked),
           by = c("variable", "value")] # feiner machen
  
  # carry out binomial test
  binom_dt[, p_value_binom_unadjusted := binom_test_adapted(
    x = N,
    n = question_asked,
    p = estimate,
    alternative = "two.sided"
  ),
  by = .(INTERVIEWER, variable, value)
  ]
  
  #anzahl_interviewer <- length(unique(binom_dt$INTERVIEWER))
  
  #binom_dt[,p_value_binom_adjusted := p.adjust.adapted(p = p_value_binom_unadjusted, 
  #                                   n = anzahl_interviewer), by=.(INTERVIEWER, variable, value)]
  
  # adjust p values 
  binom_dt[, 
          p_value_binom_adjusted := p.adjust(p_value_binom_unadjusted, method="fdr"), 
           by = .(variable, value)
           ]
  
  # test for significany on alpha level for each answer in each question 
  binom_dt[,
           sign := as.numeric(p_value_binom_adjusted < alpha),
           by=.(INTERVIEWER, variable, value)
           ]
  
  # check whether any answer option in whole question was signficant 
  binom_dt[, 
           any_sign_in_question := max(sign, na.rm = T), 
           by=.(INTERVIEWER, variable)
           ]
  
  setattr(binom_dt, "key_variables", attr(survey_data, "key_variables"))
  
  return(binom_dt)
 
}

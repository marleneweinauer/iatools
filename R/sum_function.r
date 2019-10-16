# dat <- dat_median
# dat$INTERVIEWER <- as.character(dat$INTERVIEWER) # wieder wegkriegen!!!!!!!!!!!!!!!!!! 

# method q20 median fastestquantile shares 

#' Summarize Function for RMD -Files 
#' 
#' This function summarizes results of the inv-functions for the Rmd:Report-Files
#' 
#' @param dat currently data.table but in construction for classes 
#' @param method currently method of inv function, but will be skipped as soon as classes are introduced
#' @return in construction: list of two data.tables (one with detailed information and one not)
#' @export
sum_function <- function(dat, 
                           method) {
  
  setkey(dat, INTERVIEWER)
  
  if (method %in% c("median", "q20")) { # ?berlgen ob man das so macht oder immer gleich benennt
    dat <- dat[conspi == 1, ]
  }
  if (method %in% "fastestquantile") {
    dat <- dat[question_speeded == 1, ]
  }
  
  if (method %in% "shares") {
    dat <- dat[any_sign_in_question == 1, ]
    dat[, marker := c(1L, rep(0L, .N-1L)), by=.(INTERVIEWER, variable)]
    dat <- dat[marker == 1, c("variable", "INTERVIEWER")]
    dat <- dat[, COUNT := .N, by = INTERVIEWER]
  }
  
  # summary all questions
  speeder_all_questions <- copy(dat)
  speeder_all_questions <- speeder_all_questions[, SIG_Q := paste(unique(variable), collapse = " "), by = INTERVIEWER]
  speeder_all_questions <- speeder_all_questions[unique(INTERVIEWER), mult = "first"]
  speeder_all_questions <- speeder_all_questions[, c("SIG_Q", "INTERVIEWER", "COUNT")]
  
  # summary filter questions
  speeder_filter_questions <- dat[variable %in%  attr(dat, "key_variables"), ]
  speeder_filter_questions[, sum_filter_questions_speeded := .N, by = INTERVIEWER]
  speeder_filter_questions <- speeder_filter_questions[, SIG_Q := paste(unique(variable), collapse = " "), by = INTERVIEWER]
  speeder_filter_questions <- speeder_filter_questions[unique(INTERVIEWER), mult = "first"]
  speeder_filter_questions <- speeder_filter_questions[, c("SIG_Q", "INTERVIEWER", "COUNT")]
  
  
  speeder_q_list <- list(
    speeder_all_questions = speeder_all_questions, 
    speeder_filter_questions = speeder_filter_questions 
  )
  
  return(speeder_q_list )
  
}

# blibb <-  sum_speeders_q(dat, 
#                          method = "q20")
# 
# blobb <- sum_speeders_q(binom_dt, 
#                         method = "shares")

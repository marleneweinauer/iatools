
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


# 
      #   survey_data = survey_list$survey_data
      #answer_options_to_exclude = c(-1,-2,-3)
      #   alpha = 0.05
      #  max_answer_options_of_question = 15
        
      #  prepro_time_data <- preprocess_time_data(survey_list$time_data)
      #  translate_table <- attr(prepro_time_data, "translate_table")
      #  questions_to_keep <- "JVS_OffeneStellen_F01_Anzahl"

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
inv_answer_shares <- function(
  survey_data,
  answer_options_to_exclude,
  alpha = 0.05, 
  max_answer_options_of_question =15, 
  translate_table = NULL, 
  output = "conspi"
  
) {
  
  survey_data <- copy(survey_data)
  
  # test if object of class be_monk
  stopifnot(inherits(survey_data, "monk_survey"))

  # Stefan fragen ob das eher schon in die S3 Definition
  # eliminate unwanted answer_options
  for(col in names(survey_data)) set(survey_data, i=which(survey_data[[col]] %in% answer_options_to_exclude), j=col, value=NA)

  survey_data[, ID := NULL] # brauchen wir das überhaupt je? sonst kann man sich das gleich sparen
  
  #colnames(survey_data) %>%       # ist das sehr dumm?          
  #  lapply(function(x) survey_data[, c(x) := as.factor(as.character(c(x)))]) %>%           
  #  invisible    
  
  
  
  # das kann sehr gefährlich sein!! bepsrechen!!
  h <- function(w) if( any( grepl( "are not all of the same type", w) ) ) invokeRestart( "muffleWarning" )
  
    # create long table
  shares_long_dt <-  withCallingHandlers(melt(survey_data, 
                    id.vars = "INTERVIEWER"), warning = h)[,
               .N, by = .(INTERVIEWER, variable, value)]
  shares_long_dt <- shares_long_dt[!is.na(value), ] # na überdenken
  
  # remove variables with too many answer options (not optimal for binomal analyis)
  shares_long_dt[, answer_options_of_question := length(table(value)), by = variable]
  
 # if (is.null(questions_to_keep)) {
  shares_long_dt <- shares_long_dt[answer_options_of_question <=   max_answer_options_of_question, ] 
  #} else {
  #shares_long_dt <- shares_long_dt[(answer_options_of_question <= max_answer_options_of_question) | 
  #                                   (variable %in% questions_to_keep), ]   
#  }
  
  
  
  # create variables necessary for binomial test
  shares_long_dt[,
           question_asked := sum(N),
           by = c("INTERVIEWER", "variable")]

  shares_long_dt[, ratio := N/question_asked]

  # create estimate without specific interviewer
  shares_long_dt[,
           estimate := (sum(N) - N)/(sum(question_asked) - question_asked),
           by = c("variable", "value")] 
  
  # carry out binomial test
  shares_long_dt[, p_value_binom_unadjusted := binom_test_adapted(
    x = N,
    n = question_asked,
    p = estimate,
    alternative = "two.sided"
  ),
  by = .(INTERVIEWER, variable, value)
  ]
  
  # adjusten for werte für die kein p-value gebildete werden konnte - russisch 
  shares_long_dt[is.na(p_value_binom_unadjusted), p_value_binom_unadjusted := 1]
  
  #anzahl_interviewer <- length(unique(shares_long_dt$INTERVIEWER))
  
  #shares_long_dt[,p_value_binom_adjusted := p.adjust.adapted(p = p_value_binom_unadjusted, 
  #                                   n = anzahl_interviewer), by=.(INTERVIEWER, variable, value)]
  
  # adjust p values 
  shares_long_dt[, 
          p_value_binom_adjusted := p.adjust(p_value_binom_unadjusted, method="fdr"), 
           by = .(variable, value)
           ]
  
  # test for significany on alpha level for each answer in each question 
  shares_long_dt[,
           sign := as.numeric(p_value_binom_adjusted < alpha),
           by=.(INTERVIEWER, variable, value)
           ]
  
  # check whether any answer option in whole question was signficant 
  shares_long_dt[, 
           shares_conspi := max(sign, na.rm = T), 
           by=.(INTERVIEWER, variable)
           ]
  
  class(shares_long_dt) <- c("monk_shares", class(shares_long_dt))
  setattr(shares_long_dt, "key_variables", attr(data, "key_variables"))
  
  shares_long_dt[, marker := c(1L, rep(0L, .N-1L)), by=.(INTERVIEWER, variable)] 
  #shares_short_dt <- copy(shares_long_dt)
  #shares_short_dt <- shares_short_dt[marker == 1,]
  shares_short_dt <- shares_long_dt[marker == 1, ]
  #shares_long_dt <- shares_short_dt[, ID := NULL]
  
  shares_short_dt <- shares_short_dt[, c("INTERVIEWER", "variable", "shares_conspi")]
  
  
  # ist an der stelle auch unlogisch und zu früh...gehört eins später aber ich machs jetzt mal
  # der einfachkeit halber so 
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # das ist super vorläufig programmiert
  
  if (("key_variables" %in% names(attributes(survey_data)) )) {
    shares_short_dt[,shares_filter_conspi := 
                      as.numeric(
                        shares_conspi == 1 &
                      variable %in% attr(survey_data, "key_variables")
                      )
                    ]
  }
  
  
  if (!is.null(translate_table)) {
    
    shares_short_dt <- merge(shares_short_dt , 
                            translate_table, 
                            by.x = "variable", 
                            by.y = "orignial_name", 
                            all.x = T)
    
    #shares_short_dt[, mc_variable := ifelse(is.na(mc_name), 1, 0)]
    shares_short_dt [is.na(mc_name),  mc_name := variable]
    shares_short_dt [, variable := mc_name]
    shares_short_dt [, mc_name := NULL]
    shares_long_dt[, 
                   shares_conspi := max(sign, na.rm = T), 
                   by=.(INTERVIEWER, variable)
                   ]
    shares_short_dt [, marker := c(1L, rep(0L, .N-1L)), by=.(INTERVIEWER, variable)] 
    shares_short_dt <- shares_short_dt [marker == 1, ]
    
    variables_short <- intersect(
      c("INTERVIEWER", "variable", "shares_conspi", "shares_filter_conspi"), 
      colnames(shares_short_dt)
    )
      
 
      
      shares_short_dt <- shares_short_dt[, variables_short, with = F]
    
  }
  
  

  setattr(shares_short_dt, "key_variables", attr(data, "key_variables"))

  
  if (output == "conspi") {
  return(shares_short_dt) } 
  
  if (output == "detailed") {
    return(shares_long_dt) } 
  
 
}

#' In-house read-in Function for Statistics Austria
#' 
#' This function is only relevant for users within Statisticis Austria 
#' 
#' @param file path to survey/time data 
#' @param condition condition of interviews to be analysed 
#' @param mode_variable name of the variable indicating the mode 
#' @param edge_items character vector of two items: first and last question to be analysed 
#' @param key_variables a character vector of variables of high importance e.g. gate_questions
#' @return object of class be_monk
#' @export
read_in_statsurv <- function(file, 
                            condition,
                            mode_variable,
                            edge_items,
                            key_variables, 
                            INT = NULL) {
  dat <- data.table(read.csv2(file,
                              header =T))
  dat <- dat[eval(parse(text = condition)) & get(mode_variable) == "CATI", ]
  
  variables <- colnames(dat)[which(colnames(dat ) == edge_items[1]) : 
                                       which(colnames(dat ) == edge_items[2])
                                     ]
  
  dat[, ID := FID]
  if (is.null(INT)) {dat[, INTERVIEWER := as.character(S_PARENTQRE_PORTALUSER)]}
  
  if (!is.null(INT)) {
  dat <- merge(dat, 
              INT,
              by = "ID",
              all.x = T)
    }
  
  dat <- create_iaclass(dat = dat ,
                        variables = variables, 
                        key_variables = key_variables)
  
}


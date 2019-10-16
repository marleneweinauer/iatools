

# survey_data must be a data.table in wide-table-format with observations in rows and variables in collumns
# the restriction to a specific mode is recommended! The tests are design for CATI- telephone interview
#
# the attribute "variables" belongs to the collumns that shall be analysed
# the attribute "key_variables" belongs to collumns of specific importance
#
# the ID collum of the observations must be named ID!
# the INTERVIEWER collumn of interviewer IDs must be named INTERVIEWER!


#dat <- survey_data




#' create_iaclass
#'
#' Most functions of the ia tools package need data to be of class be_monk. The create_iaclass function
#' transfers your data to this class.
#'
#'
#' @param dat survey data or time stamp data. dat must be a data.table or data.frame in wide-table-format with observations in rows and variables in collumns.
#'  The collum of IDs of observations must be named ID. The collum of interviewers must be named INTERVIEWER.
#' @param variables a character vector of variables you want to be tested in further tests
#' @param key_variables a character vector of variables of high importance e.g. gate_questions
#' @param min_intervies the treshold of minimum of interviews conducted by an interviewer to be analysed
#' @return object of class be_monk
#'
#' @details The restriction of dat to a specific mode is recommended! The tests are designed for CATI- telephone interviews
#' @examples \dontrun{a <- 1}
#' @export
create_iaclass <- function(dat,
                      variables,
                      key_variables = NULL,
                      min_interviews = 15
                      ){
  dat <- data.table(dat)
  #dat <- sticky(dat)

  if("ID" %!in% colnames(dat)) {
    stop("No ID variable was found! Please rename your ID variable \"ID\"! " )
  }

  if("ID" %!in% colnames(dat)) {
    stop("No INTERVIEWER variable was found! Please rename your variable \"INTERVIEWER\"! " )
  }


  # define Variables as attribute
  if( any(variables %!in% colnames(dat)) ) {
    stop("variables are indicated survey variables that are not in survey_data") # text umformenn
  }

  # select collumns to test for
  dat <- subset(dat,
              select = c("INTERVIEWER", "ID", variables))
 # class(  attr(dat, "variables") ) <- "avector"

  # remove data without interviewer information
  dat <- dat[!is.na(INTERVIEWER)]

  # remove interviewer with too less interviews
  dat[, COUNT := .N, by = INTERVIEWER]
  dat <- dat[COUNT >= min_interviews, ]
  dat[, COUNT := NULL]


  # define key variables as attribute
  if (!is.null(key_variables)) {
    if( any(key_variables %!in% variables) ) {
    stop("key_variables are indicated as attribute that are not in survey_data ") # text umformen
  }
  #attr(dat, "key_variables") <- key_variables
  setattr(dat, "key_variables", key_variables)
  }

  class(dat) <- c("be_monk", class(dat))
  return(dat)
}





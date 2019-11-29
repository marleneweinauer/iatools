

# conspi_DT.m <- conspi_DT.m
# filter <- NULL
# filter <- attr(survey_data, "key_variables")
# method = "q20_conspi"


create_overview <- 
  function(conspi_DT.m = conspi_DT.m, 
           filter = NULL, 
           method) { 

dat <- copy(conspi_DT.m)
dat <- dat[method_conspi == paste0(method, "_conspi") & value == 1, ]
setkey(dat, INTERVIEWER)

if (!is.null(filter)) {dat <- dat[variable %in% filter,]}
dat[, COUNT := .N, by = INTERVIEWER]
dat[, questions := paste(unique(variable), collapse = " "), by = INTERVIEWER]
dat <- dat[unique(INTERVIEWER), mult = "first"]
dat <- dat[, c("questions", "INTERVIEWER", "COUNT")]

return(dat)
  }

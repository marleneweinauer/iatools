# TODO: methods!!!


# ist so noch keine gute Lösung !!! Mit Stefan besprechen 

# detailed_answer_shares <- inv_answer_shares_nur_shorttable(
#   survey_data = survey_data,
#   answer_options_to_exclude = c(-1,-2,-3),
#   alpha = 0.05, 
#   translate_table = attr(prepro_time_data, "translate_table"), 
#   output = "detailed"
# )




render_overview <- function(conspi_DT.m, 
                            path = "~/mnt/user_home/Git/interviewer_analysis/iatools/inst/reports",
                            methods = NULL #,  # TODO !!!!!!!!!!!!!!
                            #detailed_answer_shares  = NULL
                            ) {

# create overview

overview_q <- create_overview(conspi_DT.m = conspi_DT.m, 
                              filter = NULL, 
                              method = "q")

overview_q_filter <- create_overview(conspi_DT.m = conspi_DT.m, 
                                     filter = attr(survey_data, "key_variables"), 
                                     method = "q")

overview_q20 <- create_overview(conspi_DT.m = conspi_DT.m, 
                                filter = NULL, 
                                method = "q20")

overview_q20_filter <- create_overview(conspi_DT.m = conspi_DT.m, 
                                       filter = attr(survey_data, "key_variables"), 
                                       method = "q20")



overview_shares <- create_overview(conspi_DT.m = conspi_DT.m, 
                                   filter = NULL, 
                                   method = "shares")

overview_shares_filter <- create_overview(conspi_DT.m = conspi_DT.m, 
                                          filter = attr(survey_data, "key_variables"), 
                                          method = "shares")


#if (exists("detailed_answer_shares")) {
#  shares_detailed_dat <- shares_detailed_dat 
#}


render("~/mnt/user_home/Git/interviewer_analysis/iatools/inst/reports/overview.Rmd", 
       output_dir = path)

}


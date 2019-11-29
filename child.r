rm(list=ls())

library(data.table)
library(dplyr)
library(scales)
library(ggplot2)
library(rmarkdown)
library(magrittr)
#library(ComplexHeatmap )

'%!in%' <- function(x,y)!('%in%'(x,y))

source("~/mnt/user_home/Git/interviewer_analysis/iatools/R/create_iaclass.r")
source("~/mnt/user_home/Git/interviewer_analysis/iatools/R/inv_time_data.r")
source("~/mnt/user_home/Git/interviewer_analysis/iatools/R/inv_answer_shares.r")
source("~/mnt/user_home/Git/interviewer_analysis/iatools/R/read_in_statsurv.R")
source("~/mnt/user_home/Git/interviewer_analysis/iatools/R/preprocess_time_data.r")
source("~/mnt/user_home/Git/interviewer_analysis/iatools/R/create_text2.r")
source("~/mnt/user_home/Git/interviewer_analysis/iatools/R/plot_function.R")
source("~/mnt/user_home/Git/interviewer_analysis/iatools/R/create_overview.R")
source("~/mnt/user_home/Git/interviewer_analysis/iatools/R/render_overview.R")
source("~/mnt/user_home/Git/interviewer_analysis/iatools/R/render_specific.R")
source("~/mnt/user_home/Git/interviewer_analysis/iatools/R/test_for_iid.R")



# read in data statsurv (vom Nutzer anzupassen)
#========================

# IKT
survey_data <- read_in_statsurv(
  file = "~/mnt/o/STATsurv/Erhebungen/MZ/MZ66/02_Erhebung/06_Daten_Reporting/01_Daten/Surveydata.csv",
  mode_variable = "S_MODE",
  condition = "IKTPIU %in% c(1,2,3,4)",
  edge_items = c("IKTPIU", "IKTPSEC_DBU"),
  key_variables = c("IKTPIU", "IKTPIFU2", "IKTPGOV_A3", "IKTPIBUY"),
  type = "survey"
)

time_data <-read_in_statsurv(
  file = "~/mnt/o/STATsurv/Erhebungen/MZ/MZ66/02_Erhebung/06_Daten_Reporting/02_Reports/Antwortdauern.csv",
  mode_variable = "MODE",
  condition = "IKTPIU > 0",
  edge_items = c("IKTPIU", "IKTPSEC_DBU"),
  key_variables = c("IKTPIU", "IKTPIFU2", "IKTPGOV_A3", "IKTPIBUY"),
  INT = survey_data[, c("ID", "INTERVIEWER")],
  type = "time"
)

#MZ 67
# survey_data <- read_in_statsurv(
#   file = "~/mnt/o/STATsurv/Erhebungen/MZ/MZ68/02_Erhebung/06_Daten_Reporting/01_Daten/Surveydata.csv",
#   mode_variable = "S_MODE",
#   condition = "C1 %in% c(1,2)",
#   edge_items = c("C1", "K13"),
#   key_variables = c("C1", "C2", "C3", "C4a", "C4", "C6", "C7", "C9", "D14", "D16", "D20", "D21", "D26", "E1", "E6", "E8", "H3", "H4b", "H5", "H10a", "H10b", "H13", "J1", "L4", "L6", "K1a", "K4" ),
#   type = "survey"
# )
# 
# time_data <-read_in_statsurv(
#   file = "~/mnt/o/STATsurv/Erhebungen/MZ/MZ68/02_Erhebung/06_Daten_Reporting/02_Reports/Antwortdauern.csv",
#   mode_variable = "MODE",
#   condition = "C1 > 0",
#    edge_items = c("C1", "K13"),
#   key_variables = c("C1", "C2", "C3", "C4a", "C4", "C6", "C7", "C9", "D14", "D16", "D20", "D21", "D26", "E1", "E6", "E8", "H3", "H4b", "H5", "H10a", "H10b", "H13", "J1", "L4", "L6", "K1a", "K4" ),
#   INT = survey_data[, c("ID", "INTERVIEWER")],
#   type = "time"
# )
# 
# control.dat <- create_control_dat_statsurv(
#   file = "~/mnt/o/STATsurv/Erhebungen/MZ/MZ68/02_Erhebung/06_Daten_Reporting/01_Daten/Surveydata.csv",
#   mode_variable = "S_MODE",
#   condition = "C1 %in% c(1,2)",
#   min_interviews = 15
# )

#SILC
# survey_data <- read_in_statsurv(
#   file = "~/mnt/o/STATsurv/Erhebungen/SC/SC19/02_Erhebung/06_Daten_Reporting/01_Daten/Surveydata.csv",
#   mode_variable = "S_MODE",
#   condition = "P102000 %in% 1:5",
#   edge_items = c("P001000", "K013030"),
#   key_variables = c("P001000", "P036000" ),
#   type = "survey"
# )
# 
# time_data <-read_in_statsurv(
#   file = "~/mnt/o/STATsurv/Erhebungen/SC/SC19/02_Erhebung/06_Daten_Reporting/02_Reports/Antwortdauern.csv",
#   mode_variable = "MODE",
#   condition = "P102000 > 0",
#   edge_items = c("P001000", "K013030"),
#   key_variables = c("P001000", "P036000" ),
#   INT = survey_data[, c("ID", "INTERVIEWER")],
#   type = "time"
# )
# 
# control.dat <- create_control_dat_statsurv(
#   file = "~/mnt/o/STATsurv/Erhebungen/SC/SC19/02_Erhebung/06_Daten_Reporting/01_Daten/Surveydata.csv",
#   mode_variable = "S_MODE",
#   condition = "P102000 > 0",
#   min_interviews = 15
# )




#JVS
# survey_data <- read_in_statsurv(
#   file = "~/mnt/o/STATsurv/Erhebungen/JVS/JV08/02_Erhebung/06_Daten_Reporting/01_Daten/Surveydata.csv",
#   mode_variable = "S_MODE",
#   condition = "JVS_OffeneStellen_F01_Anzahl >= 0",
#   edge_items =  c("JVS_OffeneStellen_F01_Anzahl", "JVS_OffeneStellen_F02_Teil2.100_A1"),
#   key_variables = NULL,
#   type = "survey"
# )
# 
# time_data <-read_in_statsurv(
#   file = "~/mnt/o/STATsurv/Erhebungen/JVS/JV08/02_Erhebung/06_Daten_Reporting/02_Reports/Antwortdauern.csv",
#   mode_variable = "MODE",
#   condition = "JVS_OffeneStellen_F01_Anzahl > 0",
#   edge_items =c("JVS_OffeneStellen_F01_Anzahl", "JVS_OffeneStellen_F02_Teil2.6"),
#   key_variables = NULL,
#   INT = survey_data[, c("ID", "INTERVIEWER")],
#   type = "time"
# )




#p <- create_control_plot(control.dat, 
#                         "Alter", 
#                         10)

# # # UG 
 # survey_data <- read_in_statsurv(
 #   file ="~/mnt/o/STATsurv/Erhebungen/UG/UG07/02_Erhebung/06_Daten_Reporting/01_Daten/Surveydata.csv",
 #   mode_variable = "S_MODE",
 #   condition = "A1 %in% c(1,2)",
 #   edge_items = c("A1", "T20.25"),
 #   key_variables = c("A1", "B1", "T1"),
 #   type = "survey"
 # )
 # 
 # time_data <-read_in_statsurv(
 #   file = "~/mnt/o/STATsurv/Erhebungen/UG/UG07/02_Erhebung/06_Daten_Reporting/02_Reports/Antwortdauern.csv",
 #   mode_variable = "MODE",
 #   condition = "A1 > 0",
 #   edge_items = c("A1", "B2_5_2.4"),
 #   key_variables = c("A1", "B1", "T1"),
 #   INT = survey_data[, c("ID", "INTERVIEWER")],
 #   type = "time"
 # )

# render specific
#==================

#mimi <- render_specific_interviewer(survey_data = survey_data, 
#                                                           time_data = time_data,
#                                                           path = "~/mnt/user_home/Git/interviewer_analysis#/iatools/inst/reports",
#                                                           interviewer = "T143@statistik.gv.at",
#                                                           methods = NULL #,  # TODO !!!!!!!!!!!!!!
#                                                           #detailed_answer_shares  = NULL
#)


# preprocess time data
#======================

prepro_time_data <- preprocess_time_data(time_data)


# create data for various methods
#=================================

time_short_dat <-  inv_time_data(data = prepro_time_data,
                        method = c("q20","q"),
                        answer_options_to_exclude = 0 ,
                        quantil_of_fastest_interviews = 0.05,
                        quantil_of_fastest_interviewer= 0.9,
                        min_of_fast_interviews = 5)

time_short_dat_detailed <-  inv_time_data(data = prepro_time_data,
                                                method = c("q20","q"),
                                                answer_options_to_exclude = 0 ,
                                                quantil_of_fastest_interviews = 0.05,
                                                quantil_of_fastest_interviewer= 0.9,
                                                min_of_fast_interviews = 5, 
                                                output = "detailed")



shares_short_dat <- inv_answer_shares(
  survey_data = survey_data,
  answer_options_to_exclude = c(-1,-2,-3),
  alpha = 0.05, 
  translate_table = attr(prepro_time_data, "translate_table")
)


survey_data <- survey_data[JVS_OffeneStellen_F01_Anzahl == 0, agg_Stellen := 0 ]
survey_data <- survey_data[JVS_OffeneStellen_F01_Anzahl %in% 1:5 , agg_Stellen := 1 ]
survey_data <- survey_data[JVS_OffeneStellen_F01_Anzahl %in% 6:10 , agg_Stellen := 2 ]
survey_data <- survey_data[JVS_OffeneStellen_F01_Anzahl > 10 , agg_Stellen := 3 ]

shares_detailed_dat <- inv_answer_shares(
   survey_data = survey_data,
   answer_options_to_exclude = c(-1,-2,-3),
   alpha = 0.05, 
   translate_table = attr(prepro_time_data, "translate_table"), 
   output = "detailed"
)


# create combined table 
#==========================


conspi_DT <- 
  merge(time_short_dat, 
        shares_short_dat, 
        by = c("INTERVIEWER", "variable"), 
        all.x = T
  )



for(col in names(conspi_DT)) set(conspi_DT, i=which(conspi_DT[[col]] %in% NA), j=col, value=0)

 
 conspi_DT.m <- melt(conspi_DT, 
                     variable.name = "method_conspi")
 
 conspi_DT.m[is.na(value), value := 0]
 
 setattr(conspi_DT.m, "key_variables", attr(survey_data, "key_variables"))  # das mit den key variables daugt mir noch gar ned 
 

 
# # create overview
# 
# overview_q <- create_overview(conspi_DT.m = conspi_DT.m, 
#    filter = NULL, 
#    method = "q")
# 
# overview_q_filter <- create_overview(conspi_DT.m = conspi_DT.m, 
#                 filter = attr(survey_data, "key_variables"), 
#                 method = "q")
# 
# overview_q20 <- create_overview(conspi_DT.m = conspi_DT.m, 
#                               filter = NULL, 
#                               method = "q20")
# 
# overview_q20_filter <- create_overview(conspi_DT.m = conspi_DT.m, 
#                                      filter = attr(survey_data, "key_variables"), 
#                                      method = "q20")
# 
# 
# 
# overview_shares <- create_overview(conspi_DT.m = conspi_DT.m, 
#                                           filter = NULL, 
#                                           method = "shares")
# 
# overview_shares_filter <- create_overview(conspi_DT.m = conspi_DT.m, 
#                 filter = attr(survey_data, "key_variables"), 
#                 method = "shares")


# Interviewer Texte
#=====================

 ( blubb <- create_text(conspi_DT.m = conspi_DT.m, 
                      interviewer = "T101@statistik.gv.at") )


 
# Plots
#==============

overview_plot(conspi_DT.m)
detailed_plot(conspi_DT.m)
interviewer_plot(conspi_DT.m,
                 interviewer = "T208@statistik.gv.at")

# RMD
#=====

render_specific_interviewer(survey_data = survey_data, 
                            time_data = time_data,
                            path = "~/mnt/user_home/Git/interviewer_analysis/iatools/inst/reports",
                            interviewer = "T208@statistik.gv.at",
                            methods = NULL #,  # TODO !!!!!!!!!!!!!!
                            #detailed_answer_shares  = NULL
)
 
#render_overview(conspi_DT.m, 
#                path = "~/mnt/user_home/Git/interviewer_analysis/iatools/inst/test")


# detailed RMD 


 interviewer = "T130@statistik.gv.at"

shares_detailed_dat[, any_shares_conspi := max(shares_conspi, na.rm = T), by = c("INTERVIEWER", "variable")]

shares_detailed_dat_INT <- shares_detailed_dat[INTERVIEWER == "T130@statistik.gv.at"]

shares_detailed_dat_INT[any_shares_conspi == 1]


# INTERVIEWER plots
#==================

ggplot(control.dat, aes(x = age)) +
  geom_histogram(binwidth = 10, color = "grey30", fill = "white") +  # binwidth überlgen!!
  geom_vline(xintercept = mean(control.dat$age), color = "red", linetype = "dashed") +
  facet_grid(INTERVIEWER ~ .)

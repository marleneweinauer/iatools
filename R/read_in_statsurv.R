 # file = "~/mnt/o/STATsurv/Erhebungen/JVS/JV08/02_Erhebung/06_Daten_Reporting/02_Reports/Antwortdauern.csv"
 # mode_variable = "MODE"
 # condition = "JVS_OffeneStellen_F01_Anzahl > 0"
 # edge_items =  c("JVS_OffeneStellen_F01_Anzahl", "JVS_OffeneStellen_F02_Teil2.6")
 # key_variables = NULL
 # INT = survey_data[, c("ID", "INTERVIEWER")]
 # type = "time"

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


    # file = "~/mnt/o/STATsurv/Erhebungen/MZ/MZ66/02_Erhebung/06_Daten_Reporting/02_Reports/Antwortdauern.csv"
    # mode_variable = "MODE"
    # condition = "IKTPIU > 0"
    # edge_items = c("IKTPIU", "IKTPSEC_DBU")
    # key_variables = c("IKTPIU", "IKTPIFU2", "IKTPGOV_A3", "IKTPIBUY")
    # INT = survey_data[, c("ID", "INTERVIEWER")]
    # type = "time"





read_in_statsurv <- function(file, 
                            condition,
                            mode_variable,
                            edge_items,
                            key_variables, 
                            type,
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
                        key_variables = key_variables, 
                        type = type)
  
}





#EID <- "JVS08"


read_in_shiny <- function(EID) {
 
  
  EID_info  <- data.table(read.csv2(
         paste0("~/mnt/user_home/Git/interviewer_analysis/iatools/inst/erhebungen/", EID, ".csv"), 
         header = T, 
         stringsAsFactors = F
     ))
  
  
  
  if (any(
    is.na(
      EID_info[Variable %in% c("Erhebungsname", "Pfad_Antworten", "Pfad_Zeit", 
               "condition_Daten", "condition_Zeit",  "edge_items_Antwort", "edge_items_Zeit"), 
               Auspraegung ])
    )
  ) {
    stop("Eine der Variablen 'Erhebungsname', 'Pfad_Antworten', 'Pfad_Zeit', 
               'condition_Daten', 'condition_Zeit',  'edge_items_Antwort', 'edge_items_Zeit' 
               wurde nicht im csv angegeben! Alle diese Variablen sind zur Durchführung der App notwendig!")
               
  }
  
  #EID_info$Auspraegung <- as.character(EID_info$Auspraegung)
  
  #EID_info[Variable %in% c("edge_items_Antwort", 
  #                         "edge_items_Zeit", 
  #                         "key_variables"), "Auspraegung"] <-  gsub("\"", "", EID_info[Variable %in% c("edge_items_Antwort", 
  #                        "edge_items_Zeit", 
  #                        "key_variables"), "Auspraegung"] )
  
 # EID_info[Variable == "edge_items_Antwort", "Auspraegung"] <-  gsub("\"", "", EID_info[Variable %in% "edge_items_Antwort", Auspraegung] )
#  EID_info[Variable == "edge_items_Zeit", "Auspraegung"] <-  gsub("\"", "", EID_info[Variable %in% "edge_items_Zeit", Auspraegung] )
#  EID_info[Variable == "key_variables", "Auspraegung"] <-  gsub("\"", "", EID_info[Variable %in% "key_variables", Auspraegung] )
  #EID_info[Variable == "edge_items_Antwort", Auspraegung] <-  gsub("\"", "", EID_info[Variable %in% "edge_items_Antwort", Auspraegung] )
  
  # PFAD anpassen je nachdem wo das dann liegt!!!!!!!!!!!!!!!!!!!!!!!
  

  
      
  if (!is.na(EID_info[Variable == "key_variables", Auspraegung]) ) {
    key_variables <- eval(parse( text = EID_info[Variable == "key_variables", Auspraegung] ))
  } else {
    key_variables <- NULL 
  }
    
  survey_data <- read_in_statsurv(
    file = EID_info[Variable == "Pfad_Antworten", Auspraegung],
    mode_variable = "S_MODE",
    condition = EID_info[Variable == "condition_Daten", Auspraegung],
    edge_items = eval(parse( text = EID_info[Variable == "edge_items_Antwort", Auspraegung]) ),
    key_variables = key_variables,
    type = "survey"
  )
  
  time_data <-read_in_statsurv(
    file = EID_info[Variable == "Pfad_Zeit", Auspraegung],
    mode_variable = "MODE",
    condition =  EID_info[Variable == "condition_Zeit", Auspraegung],
    edge_items = eval(parse( text = EID_info[Variable == "edge_items_Zeit", Auspraegung]) ),
    key_variables = key_variables,
    INT = survey_data[, c("ID", "INTERVIEWER")],
    type = "time"
  )
  
  survey_list <- list(
    survey_data = survey_data, 
    time_data = time_data
  )
    
  
  if ( EID_info[Variable == "birth_variable", Auspraegung] != "" ) {
  
  control.dat <- create_control_dat_statsurv(
    file =  EID_info[Variable == "Pfad_Antworten", Auspraegung],
    mode_variable = "S_MODE",
    condition =  EID_info[Variable == "condition_Daten", Auspraegung],
    min_interviews = 15,
    birth_variable =  EID_info[Variable == "birth_variable", Auspraegung],
    sex_variable =  EID_info[Variable == "sex_variable", Auspraegung]
  )
  
  survey_list[["control.dat"]] <- control.dat
  }

  
return(survey_list)

}

#survey_list <- read_in_shiny("IKT19")

compute_age = function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}



# file = "~/mnt/o/STATsurv/Erhebungen/MZ/MZ68/02_Erhebung/06_Daten_Reporting/01_Daten/Surveydata.csv"
# mode_variable = "S_MODE"
# birth_variable = "SDMC_BIRTHDATE" 
# sex_variable =  "SDMC_SEX"
# condition = "C1 %in% c(1,2)"
# min_interviews = 15







create_control_dat_statsurv <- function(
  file, 
  mode_variable, 
  condition, 
  min_interviews, 
  birth_variable = "SDMC_BIRTHDATE", 
  sex_variable =  "SDMC_SEX"
) {

  # read in data 
  control.dat <- data.table(read.csv2(file,
                              header =T))
  # restrict mode  
  control.dat <- control.dat[eval(parse(text = condition)) & get(mode_variable) == "CATI", ]

  
  # rename variables
  control.dat[, ID := FID]
  control.dat[, INTERVIEWER := as.character(S_PARENTQRE_PORTALUSER)]
  control.dat[, INTERVIEWER := factor(INTERVIEWER)]
  
  # remove interviewer with too less interviews
  control.dat[, COUNT := .N, by = INTERVIEWER]
  control.dat<- control.dat[COUNT >= min_interviews, ]
  control.dat[, COUNT := NULL]
  

  # compute age 
  control.dat[, Alter := compute_age(substr(get(birth_variable), 1, 10),
                   substr(S_PARENTQRE_CLIENTTIME, 1, 10)
  )]
  control.dat[, Geschlecht := as.factor(get(sex_variable))]
  

  # restrict to control variables 
  control.dat <- control.dat[, c("ID", "INTERVIEWER", "Alter", "Geschlecht" )]
  
  }



#control.dat 
#variable = "Alter"
#binwidth = 10 
#variable = "Geschlecht"


# Funktion sollte prinzipell nicht zwischen Alter und GEschlecht sondern Numerischer und Faktor Variable unterscheiden....verfeinern. 

create_control_plot <- function(control.dat, 
                             variable) {
  control.dat <- copy(control.dat)
  
  if( variable == "Alter") {
  
  control.dat_aux = as.data.table(control.dat) %>%                                                         
    .[, list(mean_var = mean(Alter)), by=INTERVIEWER]   
  
   p <- ggplot(control.dat, aes(x = Alter ))  +
    geom_histogram(binwidth = 10, color = "grey30", fill = "white", aes(y = 10*..density..)) +  # binwidth überlgen!!
    geom_vline(xintercept = mean(control.dat[, Alter]), color = "red", linetype = "dashed") +
     geom_vline(data=control.dat_aux, aes(xintercept=mean_var), color = "blue", linetype = "dashed") +
    ggtitle(paste0("Stichprobenverteilung nach Interviewer: Alter")) +
    facet_wrap(~ INTERVIEWER)
  }
  
  if (variable == "Geschlecht") {
    shares_f <- nrow(control.dat[Geschlecht == "WEIBLICH", ])/nrow(control.dat)

   p <-  ggplot(control.dat, aes(Geschlecht)) + 
      geom_bar(color = "grey30", fill = "white", aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) + 
      geom_hline(yintercept = shares_f , color = "red", linetype = "dashed") +
      ggtitle(paste0("Stichprobenverteilung nach Interviewer: Geschlecht ")) +
        facet_wrap(~INTERVIEWER)

  }
  
  p

  }
  


#=======


# control.dat 
# variable = "Alter"
# binwidth = 10 
# variable = "Geschlecht"
# 
# control.dat <- copy(control.dat)
# 
# #control.dat_aux = as.data.table(control.dat) %>%                                                         
# #  .[, list(mean_var = mean(Geschlecht)), by=INTERVIEWER]   
# 
#  ggplot(control.dat, aes(x = Geschlecht)) +
#   geom_histogram(binwidth = binwidth, color = "grey30", fill = "white", aes(y = binwidth*..density..)) +  # binwidth überlgen!!
#   geom_vline(xintercept = mean(control.dat[, get(variable)]), color = "red", linetype = "dashed") +
#   geom_vline(data=control.dat_aux, aes(xintercept=mean_var), color = "blue", linetype = "dashed") +
#   #geom_vline(xintercept = mean(Alter), color = "red", linetype = "dashed") +
#   ggtitle(paste0("Stichprobenverteilung nach Interviewer: ", variable)) +
#   facet_wrap(~ INTERVIEWER)
# p
# 
# 
# 
# # p <- create_control_plot(control.dat, 
# #                              "Alter", 
# #                              10)
#  
# # q <- create_control_plot(control.dat, 
# #                          "Geschlecht", 1)
# #   
# # 
# # 
# # 
# # p <- ggplot(control.dat, aes(x=factor(Geschlecht)))+
# #   #geom_bar(stat="count", width=0.7, fill="steelblue")+
# #   geom_bar(stat="count", width=0.7, fill="steelblue", aes(y = (..count..)/sum(..count..))) + 
# #   #scale_y_continuous(labels=scales::percent) +
# #   ylab("relative frequencies") + 
# #  # geom_vline(xintercept = mean(control.dat[, get(variable)]), color = "red", linetype = "dashed") +
# #   #geom_vline(xintercept = mean(Alter), color = "red", linetype = "dashed") +
# #   ggtitle(paste0("Stichprobenverteilung nach Interviewer: ", variable)) +
# #   facet_wrap(~ INTERVIEWER)
# #   
# # 
# # 
# # control.dat[, Geschlecht := ifelse(Geschlecht == "MAENNLICH", as.numeric(0), as.numeric(1))]
# # 
# # p <- ggplot(control.dat, aes(x = get(variable))) +
# #   geom_histogram(binwidth = binwidth, color = "steelblue", fill = "white", aes(y = binwidth*..density..)) +  # binwidth überlgen!!
# #   geom_vline(xintercept = mean(control.dat[, get(variable)]), color = "red", linetype = "dashed") +
# #   #geom_vline(xintercept = mean(Alter), color = "red", linetype = "dashed") +
# #   ggtitle(paste0("Stichprobenverteilung nach Interviewer: ", variable)) +
# #   facet_wrap(~ INTERVIEWER)

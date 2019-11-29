#conspi_DT.m

# conspi_DT.m = conspi_DT.m 
# interviewer = "T186@statistik.gv.at"


#interviewer = "T101@statistik.gv.at"

create_text <- function(conspi_DT.m, 
                        interviewer) {
  
  # Summe Auffälligkeiten
  #=========================
  conspi_DT.m <- copy(conspi_DT.m)
  summe_auffaelligkeiten.m <- copy(conspi_DT.m)
  
  key_variables <- attr(conspi_DT.m, "key_variables")
  
  
  summe_auffaelligkeiten.m[, n_value := sum(value, na.rm = T), by = c("INTERVIEWER", "method_conspi")]
   summe_auffaelligkeiten.m[, marker := c(1L, rep(0L, .N-1L)), by=.(INTERVIEWER, method_conspi)] # das is alls noch sehr russich und muss überarberiet werden
  summe_auffaelligkeiten.m <- summe_auffaelligkeiten.m[marker == 1, ]
 
  summe_auffaelligkeiten.m[, threshold_n_value := ( mean(n_value, na.rm = T) + 1.96*sd(n_value, na.rm = T) ), by = c("method_conspi") ]  # es würd * 1.96 gehören wenn dann
  summe_auffaelligkeiten.m[, conspi_n_value := as.numeric(n_value > threshold_n_value)]
 # summe_auffälligkeiten.m[, threshold_n_value := 1.96 *sd(n_value, na.rm = T), by = c("method_conspi") ] 
  #summe_auffälligkeiten.m[, n_value.s := scale(n_value), by = c("method_conspi")]
  summe_auffaelligkeiten.m[, c("variable", "marker") := NULL ]

  
  #summe_auffaellige <- unique(summe_auffälligkeiten.m[conspi_n_value == 1, INTERVIEWER])
  

  #interviewer <- "T156@statistik.gv.at"
  

  conspi_DT.m_INT <- conspi_DT.m[INTERVIEWER == interviewer, ]
  
    # Quantilmethode
  #=================

  var_q_conspi <- conspi_DT.m_INT[method_conspi == "q_conspi" & value == 1, variable]
      
  if ( length(var_q_conspi) > 0 ) {
  
    if ( length(var_q_conspi)  == 1 ) {
    text_q_conspi <- paste(interviewer,
                           " zeigte bei der Frage ", 
                           var_q_conspi, 
                           " wiederholt extrem schnelle Zeiten, die ein vollständiges Vorlesen dieser Frage höchst unwahrscheinlich machen.", 
                           sep = ""
               
    )
    }
    
    if ( length(var_q_conspi)  > 1 ) {
      text_q_conspi <- paste(interviewer,
                             " zeigte bei ", 
                             length(var_q_conspi), 
                             " Fragen wiederholt extrem schnelle Zeiten, die ein vollständiges Vorlesen dieser Fragen höchst unwahrscheinlich machen (", 
                             paste(var_q_conspi, collapse = ", "), 
                             ").", 
                             sep = ""
                             
      )
    }
    
    
    if (summe_auffaelligkeiten.m[ INTERVIEWER == interviewer & method_conspi == "q_conspi", conspi_n_value]  )  {
      text_q_conspi_n <- paste("Kaum ein anderer Interviewer weist eine so hohe Anzahl an Fragen mit Extremzeiten auf.") } else {
        text_q_conspi_n <- ""
      }
    
    
    filter_var_q_conspi <-   intersect(var_q_conspi, key_variables)  
    
      if (  length(filter_var_q_conspi) == 1)  {
        
      if (length(var_q_conspi) == 1) {  
      text_q_filter <- "Diese Frage ist eine Filterfrage." }
        
    
      if (length(var_q_conspi) > 1) {  
      text_q_filter <- paste("Darunter war auch eine Filterfrage (",
      filter_var_q_conspi, 
       ").", 
       sep = ""
      ) }     
        
      
      } else if (length(filter_var_q_conspi) > 1) {
        text_q_filter <- paste("Darunter sind " , 
                               length(filter_var_q_conspi),
                               " Filterfragen (",
                               paste(filter_var_q_conspi, collapse = ", "), 
                               ").", 
                               sep = ""
        )
    } else {
      
      if (length(var_q_conspi) == 1) {  
        text_q_filter <-  "Diese Frage ist keine Filterfrage."}
      
      if (length(var_q_conspi) > 1) {  
      text_q_filter <-  "Darunter war keine Filterfrage."}
      
              }

    

 # }
  
 text_q <-  paste(text_q_conspi, 
        text_q_conspi_n, 
        text_q_filter 
 ) } else {
  
  text_q <- paste0(interviewer, 
                   " zeigte bei keiner Frage wiederholt extrem schnelle Zeiten")
}
  
  
  
  
  # 20%-Quantil
  #=================
  
  var_q20_conspi <- conspi_DT.m_INT[method_conspi == "q20_conspi" & value == 1, variable]
  
  if ( length(var_q20_conspi) > 0 ) {
    if ( length(var_q20_conspi)  == 1 ) {
      text_q20_conspi <- paste(interviewer,
                             " wies bei der Frage ", 
                             var_q20_conspi, 
                             " im Vergleich zu anderen Agents im Durchschnitt (20%-Quantil) deutlich schnellere Zeiten auf.", 
                             sep = ""
                             
      )
    }
    
    if ( length(var_q20_conspi)  > 1 ) {
      text_q20_conspi <- paste(interviewer,
                             " wies bei ", 
                             length(var_q20_conspi), 
                             " Fragen im Vergleich zu den anderen Agents im Durchschnitt (20%-Quantil) deutlich schnellere Zeiten auf (", 
                             paste(var_q20_conspi, collapse = ", "), 
                             ").", 
                             sep = ""
                             
      )
    }
    
    
    if (summe_auffaelligkeiten.m[ INTERVIEWER == interviewer & method_conspi == "q_conspi", conspi_n_value]  )  {
      text_q20_conspi_n <- paste(interviewer, 
                                 " hat damit bei erheblich mehr Fragen zu schnelle Durchschnittswerte als andere Agents.") } else {
        text_q20_conspi_n <- ""
      }
    
    
    filter_var_q20_conspi <-   intersect(var_q20_conspi, key_variables)  
    
    if (  length(filter_var_q20_conspi) == 1)  {
      
      if (length(var_q20_conspi) == 1) {  
        text_q20_filter <- "Es handelt sich dabei um eine Filterfrage." }
      
      
      if (length(var_q20_conspi) > 1) {  
        text_q20_filter <- paste("Darunter war auch eine Filterfrage (",
                               filter_var_q20_conspi, 
                               ").", 
                               sep = ""
        ) }     
      
      
    } else if (length(filter_var_q20_conspi) > 1) {
      text_q20_filter <- paste("Darunter waren auch " , 
                             length(filter_var_q20_conspi),
                             " Filterfragen (",
                             paste(filter_var_q20_conspi, collapse = ", "), 
                             ").", 
                             sep = ""
      )
    } else {
      
      if (length(var_q20_conspi) == 1) {  
        text_q20_filter <-  "Es handelt sich dabei um keine Filterfrage."}
      
      if (length(var_q20_conspi) > 1) {  
        text_q20_filter <-  "Darunter war keine Filterfrage."}
      
    }
    
    
    
    # }
    
    text_q20 <-  paste(text_q20_conspi, 
                     text_q20_conspi_n, 
                     text_q20_filter 
    ) } else {
      
      text_q20 <- paste0(interviewer, 
                       " war im Durchschnitt (20%-Quantil) bei keiner Frage deutlich schneller als andere Agents.")
    }
  
  
  
  

  # Abweichende Antwortmuster 
  #=====================
    
    
  var_shares_conspi <- conspi_DT.m_INT[method_conspi == "shares_conspi" & value == 1, variable]
  
  if ( summe_auffaelligkeiten.m[ INTERVIEWER == interviewer & method_conspi == "shares_conspi", conspi_n_value]  )  {
    many_different_shares <- T
    text_shares_conspi_n <- paste(interviewer, 
                               " zeigte bei ", 
                               length(var_shares_conspi ), 
                               " Fragen signifikant abweichende Antwortmuster (",
                               paste(var_shares_conspi, collapse = ", "), 
                               "). So viele Abweichungen in den Antworten hatte kaum ein anderer Agent.", 
                               sep = ""
                               
    ) } else {
      text_shares_conspi_n <- paste0(interviewer, 
                                     " zeigte im Allgmeinen keine abweichenden Antwortmuster.")
    }
    
  
  filter_var_shares_conspi <-   intersect(var_shares_conspi, key_variables)  
  
  if (length(  filter_var_shares_conspi)  > 0) {

           
    if (length( filter_var_shares_conspi) ==  1) {    
    text_shares_filter <-  paste(ifelse( exists("many_different_shares") , "Auch bei ", "Bei"),  
           " einer Filterfragen gab es abweichende Antwortmuster. Dies könnte zur Abkürzung des Fragebogens geführt haben (", 
           filter_var_shares_conspi, 
           ").", 
           sep = ""
           )
    }
           
    if (length(  filter_var_shares_conspi)  > 1) {         
           text_shares_filter <-  paste(ifelse( exists("many_different_shares") , "Auch bei ", "Bei"),  
                                        length(filter_var_shares_conspi), 
                                        " Filterfragen gab es abweichende Antwortmuster. Dies könnte zur Abkürzung des Fragebogens geführt haben (", 
                                        paste(filter_var_shares_conspi, collapse = ", "), 
                                        ").", 
                                        sep = ""
           )
    }       
           
           
           
  }  else {
    text_shares_filter <- ""
  }

  
  text_shares <-  paste(  
    text_shares_conspi_n, 
    text_shares_filter
  ) 
  
  # Gesamttext
  #============
  
  
  #text <- cat(
  #  crayon::bold("test\n"), 
  #  text_q,
  #      text_q20, 
  #      text_shares, 
  #      sep = "\n")
  
  
text <-  list(Extremzeiten = text_q, 
            Durschnittszeiten = text_q20, 
            Antwortmuster = text_shares)
  
  
  return(text)
  
  }
  
  

  
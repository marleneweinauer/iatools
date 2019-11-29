#conspi_DT.m

# conspi_DT.m = conspi_DT.m 
# interviewer = "T186@statistik.gv.at"
# method 

# jnvjksjgnsbbddbbtt:):):):)
# diese funkton kreigt dann noch einen kreativeren namen 

#plot_function <- function(conspi_DT.m, 
#                        interviewer) {
  
  # overview 
  #=========================
  
 # conspi_DT.m <- copy(conspi_DT.m)
  
  overview_plot <- function(conspi_DT.m) {
  
  conspi_DT.m[, n_value := sum(value), by = c("INTERVIEWER", "method_conspi")]
  conspi_DT.m[, marker := c(1L, rep(0L, .N-1L)), by=.(INTERVIEWER, method_conspi)] 
  overview.m <- conspi_DT.m[marker == 1, ]
  
  overview.m[, n_value.s := scale(n_value), by = c("method_conspi")]
  
  p <- ggplot(overview.m,
              aes(method_conspi, INTERVIEWER)) + 
    geom_tile(aes(fill = n_value.s),
                   colour = "black") + 
    scale_fill_gradient(low = "white",
                   high = "steelblue") +
    theme_bw()
  return(p)
  
  }
  
  
  
  # detailed plot
  #==============
  
  detailed_plot <- function(conspi_DT.m) {
       #conspi_DT.m[, conspi_variable := as.numeric(sum(value) >= 3), by = c("INTERVIEWER", "variable")]
       mimi <- copy(conspi_DT.m)
       mimi[, conspi_variable := sum(value), by = c("INTERVIEWER", "variable")]
       mimi[, INTERVIEWER := substr(INTERVIEWER, 2,4)]
       unique(mimi[conspi_variable >= 2, INTERVIEWER])
       
     w <- ggplot(mimi,
                 aes(INTERVIEWER, variable)) + geom_tile(aes(fill = conspi_variable),
                                                        colour = "black") + scale_fill_gradient(low = "white",
                                                                                                high = "steelblue") +
       theme_bw()
   return(w)
  }
  
  
  
  
  # Interviewer Plot
  #==================
  
  
  # conspi_DT.m 
  # method = "interviewer" 
  # interviewer = "T130@statistik.gv.at"
  
  interviewer_plot <- function(conspi_DT.m, 
                               method = "interviewer", 
                               interviewer ) {
  
    conspi_DT.m_INT <- conspi_DT.m[INTERVIEWER == interviewer]    
    
  q <- ggplot(conspi_DT.m_INT,
              aes(method_conspi, variable)) + geom_tile(aes(fill = value),
                                                     colour = "black") + scale_fill_gradient(low = "white",
                                                                                             high = "steelblue") +
    theme_bw()
  return(q)
  
  }
  

  
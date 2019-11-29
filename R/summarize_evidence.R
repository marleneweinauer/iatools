summarize_evidence <- function(conspi_DT.m) {
  
  conspi_DT.m <- copy(conspi_DT.m)
  conspi_DT.m[, n_value := sum(value), by = c("INTERVIEWER", "method_conspi")]
  conspi_DT.m[, marker := c(1L, rep(0L, .N-1L)), by=.(INTERVIEWER, method_conspi)] 
  overview.m <- conspi_DT.m[marker == 1, ]
  
  
  blubb <- data.table(dcast(overview.m[, c("INTERVIEWER", "method_conspi", "n_value")], 
                            formula = "INTERVIEWER ~ method_conspi",
                            value.var = "n_value"
  )
  )
  
  return(blubb)
}



# TODO: allgmein besser überlgen wie man hier die breks und clrs festlegt!!!!!!!!!!!!!!!!!!!!
display_evidence <- function(conspi_DT.m)
{
  conspi_DT.m <- copy(conspi_DT.m)
  
  blubb <- summarize_evidence(conspi_DT.m)
  
  for (el in setdiff(colnames(blubb), c("INTERVIEWER", "shares_filter_conspi"))) {
    assign( paste(el, "brks", sep = "_"), 
            quantile(blubb[, get(el)], probs = c(0, 0.4, 0.8, 1), na.rm = TRUE)
    )
    assign( paste(el, "clrs", sep =  "_"), round(seq(255, 40, length.out = 5), 0) %>%
              {paste0("rgb(255,", ., ",", ., ")")}
    )
  }
  
  if ("shares_filter_conspi" %in% colnames(blubb) )
    shares_filter_conspi_brks <- c(0, 1, 3, 5 )  # sehr random noch
  shares_filter_conspi_clrs <- round(seq(255, 40,length.out = 5), 0) %>%
    {paste0("rgb(255,", ., ",", ., ")")}
  
  
  
  
  blebb <- datatable(blubb, list(mode = "single", target = "cell"), 
                     rownames= FALSE)
  
  for (el in setdiff(colnames(blubb), "INTERVIEWER"))  {
    
    blebb <-  blebb %>% 
      formatStyle(el, 
                  backgroundColor = styleInterval(get(paste0(el, "_brks")), get(paste0(el, "_clrs")))
      )
    
  }
  
  return(blebb)
}

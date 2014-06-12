shinyTable <- function(data, stripted=F, bordered=F, hover=F, condensed=F, caption=NULL) {
  ##########################################
  # Checking the data is data frame is not #
  ##########################################
  if(!is.data.frame(data)) {
    data <- as.data.frame(data)
  }
  
  ####################################   
  # Creating the thead for the table #
  ####################################
  for(i in seq_len(dim(data)[2])) {
    if(i==1){
      thead <- tags$td(colnames(data)[1])
    } else {
      thead <- list(thead, tags$td(colnames(data)[i]))
    }
  }
  thead <- tags$thead(thead)
  
  ####################################   
  # Creating the tbody for the table #
  ####################################
  for(i in seq_len(dim(data)[1])) {
    for(j in seq_len(dim(data)[2])) {
      if(j==1){
        tbody.td <- tags$td(data[i,j])
      } else {
        tbody.td <- list(tbody.td, tags$td(data[i,j]))
      }
    }
    
    if(i==1){
      tbody <- tags$tbody(tbody.td)
    } else {
      tbody <- list(tbody, tags$tbody(tbody.td))
    }
  }
  
  ##############################
  # Applying Bootstrap classes #
  ##############################
  if(stripted) {
    class <- paste("table", "table-striped")
  } else {
    class <- "table"
  }
  
  if(bordered) {
    class <- paste(class, "table-bordered")
  }
  
  if(hover) {
    class <- paste(class, "table-hover")
  }
  
  if(condensed) {
    class <- paste(class, "table-condensed")
  }
  
  ########################
  # Creating Final Table #
  ########################
  if(is.null(caption)){
    mytable <- tags$table(class=class, list(thead, tbody))
  } else {
    mytable <- tags$table(class=class, 
                          tags$caption(caption),
                          list(thead, tbody))
  }
  
  return(mytable)
}

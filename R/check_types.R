# check colnames of input
check_colnames = function(input){
  cnames = colnames(input)
  cnames_necessary = c("id", "type", "value", "date")
  if(any(!cnames_necessary %in% cnames)){return(F)}
  else{return(T)}
}

# check type of input
check_type = function(input){
  type = class(input)
  if(type != "data.frame"){return(F)}
  else{return(T)}
}

# check if input$type is of type factor or character
check_type_of_type = function(input){
  if(is.factor(input$type)) cnames = c('id', 'date', unique(as.character(input$type)))
  if(is.character(input$type)) cnames = c('id', 'date', unique(input$type))
  if(!is.factor(input$type) && !is.character(input$type)) cnames = c('id', 'date')
  if(length(cnames) < 3) stop("The column 'type' is not of type character or factor. \n")
  return(cnames)
}


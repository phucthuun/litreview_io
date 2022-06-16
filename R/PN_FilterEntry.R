# This function filters entries

PN_FilterEntry <- function(df){
  
  cat("Example \n+ Year > 2018 & `Task type 2` == 1 \n+ Entry_ID %in% c('Richmond_2015, Task_2', 'Schlichting_2016, Task_3')")
  {filter_string <- readline("Enter your inclusion criteria >>> ")}
  
  df.filtered <- df %>%
    filter(!! rlang::parse_expr(filter_string))
  
  return(df.filtered)
  }
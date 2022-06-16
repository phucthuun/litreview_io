# This function takes the non-binary dataset and a variable of interest and produces pie chart
PN_MakePiePlotly <- function(reviewtable=reviewtable, whatvar = 'Task_type/Encoding_instruction/Design/To_be_remembered_information'){
  
  
  # Read data set
  nArticle <- reviewtable$Article_ID %>% unique() %>% length()
  nEntry <- reviewtable$Entry_ID %>% unique() %>% length()
  
  reviewtable <- reviewtable %>%
    # mutate(Entry_ID = paste(Article_ID, Task_number, sep = "_")) %>%
    select(Title, Entry_ID, whatvar)
  
  filter_string = paste0(whatvar, ">0")
  
  # Entry
  dfEntry <- reviewtable %>%
    filter(!! rlang::parse_expr(filter_string)) %>%
    group_by_at(vars(whatvar)) %>% 
    summarise(n = n(), freq = n() / nEntry) %>%
    arrange(vars(whatvar)) %>% 
    mutate(Variable = whatvar) %>%
    rename("Levels" = whatvar)
  dfEntry$Levels = factor(dfEntry$Levels, c(1:6))
  
  
  dfArticle <- reviewtable %>%
    distinct_at(vars("Title",whatvar)) %>%
    filter(!! rlang::parse_expr(filter_string)) %>%
    group_by_at(vars(whatvar)) %>%
    summarise(n = n(),freq = n() / nArticle) %>%
    arrange(vars(whatvar)) %>%
    mutate(Variable = whatvar) %>%
    rename("Levels" = whatvar)
  dfArticle$Levels = factor(dfArticle$Levels, c(1:6))
  
  # Merge 
  df <- merge(dfEntry, dfArticle, by = c("Variable","Levels"), suffix = c(".Entry",".Article"))
  
  # Make pie
  # legend text and pie title:
  if (whatvar=='Task_type'){
    pielabels =c('Direct memory','Autobiographical memory','Statistical learning',
                 'Generalization','Semantic knowledge')
    pietitle = 'Task type'} else
      
      if (whatvar=='Encoding_instruction'){
        pielabels= c('Intentional','Incidental', 'Manipulated: intention and incidental',
                     'Unspecified', 'NA, for personal events or semantic knowledge tasks')
        pietitle = 'Encoding instruction'} else
          
          if (whatvar=='Design'){
            pielabels=c('Cross-sectional','Longitudinal')
            pietitle = 'Design'
          } else
            
            if (whatvar=='To_be_remembered_information'){
              pielabels=c('Individual item','Associative co-occurence','Temporal memory',
                          'What-where-when','Story','Event')
              pietitle = 'To-be-remembered information'}
  
  p<- plot_ly(df, labels = ~Levels, values = ~freq.Entry, type = 'pie', sort=F,direction='clockwise',
              textinfo = 'none',
              insidetextfont = list(colors=projectPalette[1:nrow(df)]),
              hoverinfo = 'text',
              text = ~paste(Levels,pielabels,'\n', round(freq.Entry*100,2),'%'),
              marker = list(colors = projectPalette[1:nrow(df)],
                            line = list(color = '#FFFFFF', width = 1)),
              #The 'pull' attribute can also be used to create space between the sectors
              showlegend = FALSE)
  
  return(p)
  
  
}


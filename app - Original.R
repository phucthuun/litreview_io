rm(list=ls())

{
  library(shiny)
  library(shinyjs)
  library(DT)
  library(shinydashboard)
  library(shinyThings)
  library(shinythemes)
  library(bslib)
  library(shinycssloaders)
  
  
  library(stringr)
  library(dplyr)
  library(tidyverse)
  library(tidyr)
  library(reshape2)
  library(readxl)
  
  library(ggraph)
  library(igraph)
  library(RColorBrewer)
  library(plotly)
  library(scales)
  library(viridis)
  library(cowplot)
  
  library(giscoR)
  library(sf)
  
}

{ # retrieve configuration functions
  source("PN_int_breaks.R", local = TRUE) # get palette
  source("PN_DelayDf_Stack.R", local = TRUE) # transform delay df
  
  source("PN_GetPalette.R", local = TRUE) # get palette
  source("PN_GetConnection.R", local = TRUE) # get connections (=edges)
  source("PN_GetConnection_OverlapOrUniqueLabel.R", local = TRUE) # get connections and label them with unique article_id or "overlap"
  
  source("PN_MakePie.R", local = TRUE) # plot pie chart
  source("PN_MakePiePlotly.R", local = TRUE) # plot pie chart
  source("PN_PlotNode.R", local = TRUE) # plot nodes only (>> we have one fixed frame)
  source("PN_PlotEdge_Filter.R", local = TRUE) # plot edges and color code them by unique article_id or "overlap" label
  source("PN_PlotEdge_Group.R", local = TRUE) # plot edges and color code them by the number of entries
  
  source("PN_CountLocation.R", local = TRUE) }


{ # retrieve data
  
  # for World Map
  # Read data set
  reviewtable <- read_excel("Lit_Review_Preprocessed.xlsx")%>%
    mutate(Article_ID = str_extract_all(Entry_ID, "\\w+(?=,)"),
           Task_number = str_extract_all(Entry_ID, "(?<=,)\\w+"),.before = Entry_ID)
  
  # for Delay
  delay1 <- read_excel("Lit_Review_QC.xlsx") %>%
    select(Article_ID, Task_number,
           starts_with('Delay_'),
           starts_with('Task_type')) %>%
    mutate(Entry_ID = paste(Article_ID,Task_number,sep=', '),.before = Article_ID) %>%
    select(-c(Task_number, Article_ID)) %>%
    # remove entries without information about delay
    filter(Delay_1 > 0)
  
  # for HEB
  identifier_var <- c("Article_ID", "Task_number", "Entry_ID", "Authors", "Title", "Year")
  df <- read_excel("Lit_Review_binarized.xlsx") %>%
    mutate(Article_ID = str_split_fixed(Entry_ID, ', ', 2)[,1],
           Task_number = str_split_fixed(Entry_ID, ', ', 2)[,2], .before = Entry_ID)
  # for HEB: reverse filtering, top similar entries
  topSimilarity <- read_excel("TopSimPair.xlsx")
  
  # get hierarchy
  hierarchy <- read_excel("Hierarchy.xlsx")
  
  # get vertices
  vertices <- read_excel("Vertices.xlsx")
  vertices$group <- factor(vertices$group, levels = unique(vertices$group))
  
  # for WM
  # Get countries
  epsg_code <- 4088
  world <- gisco_get_countries() %>%
    st_transform(epsg_code)
  world <- sf::st_cast(world, 'MULTIPOLYGON')
  
  # for citation
  citation <- read_excel("citation.xlsx")
  
  
}
# ui ----
ui = navbarPage(id = 'wholePage',
                # App Title
                title=div(img(src="MPIB_Logo_EN_horizontal_RGB_White.png", height = "100px")),
                theme = shinytheme("cosmo"),
                footer = tags$div(
                  class = "footer",
                  includeHTML("end_text.html")),
                position = "fixed-top",
                header = tags$style(
                  "
                  
                  .footer {
                  width: 100%; height: 100px;
                  background-color: #ffffff;
           }
                  .navbar-right {float: right !important;}",
                  "body {padding-top: 150px; background-color: #ffffff}"),
                
                
                ## HOME ----
                tabPanel('HOME', id='home',
                         tags$head(
                           tags$link(rel = "stylesheet", 
                                     type = "text/css", 
                                     href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
                           tags$link(rel = "icon", 
                                     type = "image/png", 
                                     href = "MPIB_Logo_EN_horizontal_RGB_White.png")
                         ),
                         fluidRow(class='homebox',
                                  column(3),
                                  column(6, 
                                         p(),
                                         fluidRow(box(solidHeader = T, status = "warning", width = NULL, 
                                                      includeHTML(("home_text.html")),
                                                      # style =  "background-color:#ECFDFB",
                                                      )
                                                  )
                                  ),
                                  column(3)
                         )
                ),
                
                
                ## ABOUT ----
                tabPanel('ABOUT', id='about',
                         fluidRow(class='aboutbox',
                                  column(2),
                                  column(8, 
                                         p(),
                                         fluidRow(box(solidHeader = T, status = "warning", width = NULL, 
                                                      includeHTML(("intro_text.html"))))
                                  ),
                                  column(2)
                         )
                ),
                
                ## OVERVIEW ----
                tabPanel('OVERVIEW',
                         fluidRow(class='overview',
                                  column(2),
                                  column(8, 
                                         p(),
                                         # fluidRow(box(solidHeader = T, status = "warning", width = NULL,
                                         #              h4(strong("Publications over year", style = "color: #006c66")),
                                         #              p("A timeline plot depicting the number of publications across years.", style = "font-size: 18px;"),
                                         #              plotlyOutput("illustration_year")%>% withSpinner(type=4,color="#006c66"))),
                                         # 
                                         # fluidRow(box(solidHeader = T, status = "warning", width = NULL,
                                         #              h4(strong("Locations of data collection", style = "color: #006c66")),
                                         #              p("A heatmap of the data collection locations from the selected publications. 
                                         #         Color intensity indicates the number of entries.", style = "font-size: 18px;"),
                                         #              plotlyOutput("illustration_WM")%>% withSpinner(type=4,color="#006c66"))),
                                         
                                         fluidRow(#class='overview',
                                           column(6,
                                                  box(solidHeader = T, status = "warning", width = NULL,
                                                      h4(strong("Locations of data collection", style = "color: #006c66")),
                                                      p("A heatmap of the data collection locations from the selected publications. 
                                                        Color intensity indicates the number of entries.", style = "font-size: 18px;"),
                                                      plotlyOutput("illustration_WM")%>% withSpinner(type=4,color="#006c66"))),
                                           column(6,
                                                  box(solidHeader = T, status = "warning", width = NULL,
                                                      h4(strong("Publications over year", style = "color: #006c66")),
                                                      p("A timeline plot depicting the number of publications across years.", style = "font-size: 18px;"),
                                                      plotlyOutput("illustration_year")%>% withSpinner(type=4,color="#006c66")))
                                           
                                         ),
                                         
                                         
                                         
                                         fluidRow(box(solidHeader = T, status = "warning", width = NULL,
                                                      h4(strong("Overall literature composition", style = "color: #006c66")),
                                                      p("Pie charts depicting the relative composition of the experimental design, task type,
                                                 and to-be-remembered information method factors from memory development literature.", style = "font-size: 18px;"),
                                                      fluidRow(
                                                        column(4,
                                                               h4(strong('Design'),align = "center"),
                                                               plotlyOutput("illustration_pie.design")%>% withSpinner(type=4,color="#006c66")),
                                                        column(4,
                                                               h4(strong('Task type'),align = "center"),
                                                               plotlyOutput("illustration_pie.tasktype")%>% withSpinner(type=4,color="#006c66")),
                                                        column(4,
                                                               h4(strong('To-be-remembered information'),align = "center"),
                                                               plotlyOutput("illustration_pie.tbrinfo")%>% withSpinner(type=4,color="#006c66")))
                                         ))
                                         
                                  ),
                                  column(2)
                         )),
                
                
                ## EXPLORE ----
                tabPanel('EXPLORE',
                         fluidRow(class='explore',
                                  column(2), 
                                  column(8, 
                                         p(),
                                         
                                         dashboardPage(
                                           dashboardHeader(disable=T),
                                           dashboardSidebar(disable=T,collapsed=T,width=0.00001),
                                           dashboardBody(
                                             
                                             fluidRow(class='exploreSearch',
                                                      box(title = NULL, solidHeader = TRUE, status = 'warning', width = NULL,
                                                          
                                                          h4(strong("Search engine", style = "color: #006c66")),
                                                          ### instruction ----
                                                          fluidRow(includeHTML(("explore_instruction.html"))),
                                                          br(),
                                                          
                                                          ### search engine ====
                                                          fluidRow(
                                                            column(1),
                                                            column(width=5,align="center",
                                                                   selectizeInput("authors", label="Author",
                                                                                  choices = df$Authors %>% unlist() %>% str_extract_all("\\w+") %>% unlist() %>% unique() %>% sort(),
                                                                                  multiple = T, options = list(create = TRUE))),
                                                            
                                                            column(width=5,align="center",
                                                                   selectizeInput("title", label="Title", choices = unique(df$Title) %>% sort(), multiple = T, options = list(create = TRUE))),
                                                            column(1)
                                                            ),
                                                          
                                                          ### advanced filter ====
                                                          fluidRow(
                                                            column(3),
                                                            column(6,align="center",
                                                                   box(title = 'Advanced filter', solidHeader = T, status = "warning", width = NULL, collapsible = T, collapsed = T,
                                                                dateRangeInput("year", "Publication period",
                                                                               startview = "decade",
                                                                               start = "1970-01-01", end = NULL,
                                                                               min = "1970-01-01", max = "2030-01-01",
                                                                               format = "yyyy", separator = " - "),
                                                                selectizeInput("article_id", "Article id", choices = unique(df$Article_ID), multiple = T, options = list(create = TRUE)),
                                                                selectizeInput("article_task", "Task entry", choices = unique(df$Entry_ID), multiple = T))),
                                                            column(3)
                                                          ),
                                                          
                                                          ### data ====
                                                          fluidRow(
                                                            box(title = NULL, solidHeader = TRUE, status = 'warning', width = NULL,
                                                                p(strong("Search result", style = "color: #006c66")),
                                                                DT::dataTableOutput("table")
                                                            )
                                                          ))),
                                             
                                             
                                             ### HEB ====
                                             fluidRow(
                                               box(title = NULL, solidHeader = T, status = 'warning', width = NULL,
                                                   h4(strong("Hierarchical Edge Bundling", style = "color: #006c66")), 
                                                   fluidRow(
                                                     column(8, align='left',
                                                            radioSwitchButtons(inputId = "HEB_type",
                                                                               choices = c("Trend" = "trend", "Contrast" = "contrast", "Similarity" = "similarity"),
                                                                               selected_background = '#006c66', selected_color = "#ffffff", selected = 'trend')),
                                                     column(4, align='right',
                                                            downloadButton('download',label = 'Download data',style = "color: #000000; background-color: #ffffff; border-color: #ffffff"))),
                                                   
                                                   p("Hierarchical edge bundling plots that visualize 
                                                     the combination of methodological variables for task entries.", style = "font-size: 18px;"),
                                                   
                                                   column(12, align="center",
                                                          textOutput('HEBlegend'),
                                                          plotOutput("illustration_HEB", height = "650px", width = "700px")%>% withSpinner(type=4,color="#006c66")
                                                   ) 
                                                   
                                               )
                                             ),
                                             
                                             ### Delay ====
                                             fluidRow(
                                               box(title = NULL, solidHeader = T, status = 'warning', width = NULL, 
                                                   h4(strong("Delay", style = "color: #006c66")),
                                                   p("Delay periods ranging from 0 to 6 years across the five task types across all task entries.", style = "font-size: 18px;"),
                                                   plotlyOutput("illustration_Delay")%>% withSpinner(type=4,color="#006c66")
                                               )
                                               
                                             )
                                           )
                                         )
                                  ),
                                  
                                  column(2)
                         )
                ),
                
                # ## DATA ----
                # tabPanel('DATA'),
                
                ## style ----
                tags$head(
                  
                  tags$style(
                    # type = "text/css",
                    
                    HTML('
                    
            
            
            #about {background-color: #DEDEDE;}
            .navbar {background-color: #006c66 !important;}
            .navbar-default .navbar-brand{background-color: #006c66; color: white;}
            .navbar-default .navbar-nav > .active > a, 
            .navbar-default .navbar-nav > .active > a:focus,
            .navbar-default .navbar-nav > .active > a:hover {background-color: #338A84;}
            
            .navbar-header { width:100% }
            .navbar-nav > li > a, 
            .navbar-brand {padding-top:1px !important; padding-bottom:1px; height: 60px; width: 100%; text-align: right}
            
            .skin-blue .main-sidebar {background-color:  #ffffff;}
            .homebox{background-color: #ffffff;}
            .aboutbox{background-color: #ffffff;}
            .overview{background-color: #ffffff;}
            .explore{background-color: #ffffff;}
            
            /* body */
            .content-wrapper, .right-side {background-color: rgb(255,0,0);}
            .content-wrapper, .left-side {background-color: #ffffff;}
            .skin-blue .left-side, .skin-blue .wrapper {background-color: #ffffff;}
            
            .homebox .box.box-solid.box-warning>.box-header {color:#000000; background:#ffff;}
            .homebox .box.box-solid{background: #ffffff; border-bottom-color:#ffffff; border-left-color:#ffffff; border-right-color:#ffffff; border-top-color:#ffffff;box-shadow: none;}
             
            .aboutbox .box.box-solid.box-warning>.box-header {color:#000000; background:#ffff;}
            .aboutbox .box.box-solid{background: #ffffff; border-bottom-color:#ffffff; border-left-color:#ffffff; border-right-color:#ffffff; border-top-color:#ffffff;box-shadow: none;}
             
            .overview .box.box-solid.box-warning>.box-header {color:#000000; background:#ffff;}
            .overview .box.box-solid{background: #ffffff; border-bottom-color:#006c66; border-left-color:#006c66; border-right-color:#006c66; border-top-color:#006c66;box-shadow: none;}
            
            .explore .box.box-solid.box-warning>.box-header {color:#000000; background:#ffff;}
            .explore .box.box-solid{background: #ffffff; border-bottom-color:#006c66; border-left-color:#006c66; border-right-color:#006c66; border-top-color:#006c66;box-shadow: none;}
            
            .exploreSearch .box.box-solid{background: #ffffff !important; border-bottom-color:#ffffff !important; border-left-color:#ffffff !important; border-right-color:#ffffff !important; border-top-color:#ffffff !important;box-shadow: none;}
            
            
            /* collapse button*/
            .fa, .fas {color: black;}
            
            
                 ')))
                
                
                
)


# server ----
server <- function(input, output, session) { 
  
  # Preprocess outputs----
  # Make reactive to store filter criteria
  
  # authors input
  input.authors <- reactive({paste(input$authors, collapse = "|")})
  # title input
  input.title <- reactive({paste(input$title, collapse = "|")})
  # year input
  year_start <- reactive({format(as.Date(input$year[1]), "%Y") %>% as.numeric()})
  year_end <- reactive({format(as.Date(input$year[2]), "%Y") %>% as.numeric()})
  # Update df by authors
  reviewtable.update.authors <- reactive({
    reviewtable %>% 
      filter(Year >= year_start() & Year <= year_end(), 
             str_detect(Authors, input.authors()),
             str_detect(Title, input.title()))})
  # Update article_id input
  observe({updateSelectizeInput(session, "article_id", "Article id", choices = unique(reviewtable.update.authors()$Article_ID))})
  
  
  # article_id input
  input.article_id <- reactive({paste(input$article_id, collapse = "|")})
  # Update df by authors, year, article_id
  reviewtable.update.authors.year.article_id <- reactive({reviewtable.update.authors() %>% filter(str_detect(Article_ID, input.article_id()))})
  # Update article_task input
  observe({updateSelectizeInput(session, "article_task", "Task entry", choices = unique(reviewtable.update.authors.year.article_id()$Entry_ID))})
  
  
  # article_task input
  input.article_task <- reactive({paste(input$article_task, collapse = "|")})
  # Update df by authors, year, article_id, article_task
  reviewtable.filtered <- reactive({reviewtable.update.authors.year.article_id()%>%
      filter(str_detect(Entry_ID, input.article_task()))
  })
  
  # Make binary df
  df.filtered <- reactive({ 
    
    df %>%
      filter(str_detect(Entry_ID, reviewtable.filtered()$Entry_ID %>% paste(collapse = '|')))
    
  })
  
  # Make delay dataframe
  delay1.filtered <- reactive({ delay1 %>%
      filter(str_detect(Entry_ID, input.article_task()))
  })
  
  # Top similarity data frame
  topSimilarity.filtered <- reactive({
    
    input.entryid = reviewtable.filtered()$Entry_ID %>% paste(collapse = '|')
    
    topSimilarity%>%
      filter(str_detect(Entry1, input.entryid) | str_detect(Entry2, input.entryid)) %>%
      arrange(desc(Similarity)) %>% select(Entry1, Entry2, Similarity)
  })
  
  # citation df
  citation.filtered <- reactive({
    
    input.articleid = reviewtable.filtered()$Article_ID %>% paste(collapse = '|')
    
    citation%>%
      filter(str_detect(Article_ID, input.articleid))
  })
  
  
  ### Make connection
  # Data frame for edges (connection):
  # get connections
  connect <- reactive({PN_GetConnection(data=df.filtered(), identifier_var = identifier_var)})
  # look for overlapping edges
  connect_OverlapOrUnique <- reactive({
    
    connect_OverlapOrUnique <- PN_GetConnection_OverlapOrUniqueLabel(connect())
    connect_OverlapOrUnique$Proportion <- as.numeric(connect_OverlapOrUnique$Proportion)
    connect_OverlapOrUnique$Entry_ID <- as.factor(connect_OverlapOrUnique$Entry_ID)
    
    return(connect_OverlapOrUnique)
    
  })
  
  
  # Render filtered data table
  output$table <- DT::renderDataTable({
    
    reviewtable.filtered() %>% select(Article_ID, Authors, Title, Year, Task_number) },
    options = list(pageLength=3, scrollX='250px', autoWidth = TRUE)) 
  
  # OVERVIEW ----
  ## Article over year ----
  output$illustration_year <- renderPlotly({
    ggplotly(reviewtable %>%
               distinct(Article_ID, Year) %>%
               group_by(Year) %>%
               summarise(`No. of Published Article`=n()) %>%
               arrange(Year) %>%
               ggplot()+
               geom_line(aes(x=Year, y= `No. of Published Article`), color = '#006c66')+
               geom_point(aes(x=Year, y= `No. of Published Article`), color = '#006c66')+
               scale_y_continuous(breaks = function(x) PN_int_breaks(x, n = 10))+ scale_x_continuous(breaks = function(x) PN_int_breaks(x, n = 10))+
               theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
                     plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title = element_text(color = '#006c66'), axis.text = element_text(color = '#006c66'), axis.ticks = element_blank() 
               )
             
             
    )
    
  })
  
  
  
  
  ## WORLD MAP ----
  ### Make df for locations
  output$illustration_WM <- renderPlotly({
    
    df.locationSummary <- PN_CountLocation(data=reviewtable)
    df.location <- df.locationSummary %>%
      rename('sovereignt' = `Location`) %>%
      mutate(
        Article = case_when(n.Article > 0 & n.Article <= 10 ~ 10,
                            n.Article > 10 & n.Article <= 20 ~ 20,
                            n.Article > 20 & n.Article <= 30 ~ 30,
                            n.Article > 30 & n.Article <= 40 ~ 40,
                            n.Article > 40 ~ 100),
        text.location = paste0(sovereignt, ": ", n.Article)) %>%
      mutate(sovereignt= case_when(sovereignt == 'United States of America'~'United States',
                                   sovereignt != 'United States of America'~sovereignt)) %>%
      rename('NAME_ENGL'='sovereignt')
    
    worlddf <- left_join(world, df.location, by='NAME_ENGL')
    worlddf$Article[!is.na(worlddf$Article)] = 1
    worlddf$Article[is.na(worlddf$Article)] = 0
    worlddf$Article = as.factor(worlddf$Article)
    
    
    # Countries centroids
    symbol_pos <- st_centroid(world, of_largest_polygon = TRUE)
    symbol_pos <- merge(symbol_pos,df.location, by='NAME_ENGL') %>% arrange(n.Article)
    
    pWM <- ggplot() +
      geom_sf(data=worlddf %>% filter(Article == 1), fill = '#87E1D4', color = '#D6E6E2')+
      geom_sf(data=worlddf %>% filter(Article == 0), fill = '#D6E6E2', color = '#D6E6E2')+
      theme(panel.background = element_rect(fill = "transparent", color = 'black'), # bg of the panel
            plot.background = element_rect(fill = "transparent", color = 'black'), # bg of the plot
            axis.text=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            axis.ticks = element_blank())+
      # Labels position (centroids)
      geom_sf(data = symbol_pos, pch = 21, alpha = .6, aes(size=n.Article, fill = n.Article, color = n.Article, text = text.location))+
      scale_fill_gradientn(colours=magma(10), name="Number of Entries",
                           trans = "log", breaks = c(1,5,10,40,max(symbol_pos$n.Article)))+
      scale_color_gradientn(colours=magma(10), name="Number of Entries",
                            trans = "log", breaks = c(1,5,10,40,max(symbol_pos$n.Article)))+
      scale_size_continuous(name = "Number of Entries",  range = c(2, 18), breaks = c(1,5,10,40,max(symbol_pos$n.Article)))+
      guides(size = 'none', color = 'none', fill = 'none')  
    
    return(ggplotly(pWM, tooltip = 'text')%>%
             layout(xaxis = list(autorange = T),
                    yaxis = list(autorange = T)))
    
    
    
    
    
  })
  
  ## PIE ----
  
  output$illustration_pie.design <- renderPlotly({
    
    p <- PN_MakePiePlotly(reviewtable,'Design')
    p <- p  %>%
      add_annotations(
        y=1.05, 
        x=0.5, 
        text='',#"Design",
        showarrow=F,
        font=list(size=20)
      )
  })
  
  output$illustration_pie.tasktype <- renderPlotly({
    
    p <- PN_MakePiePlotly(reviewtable,'Task_type')
    p <- p  %>%
      add_annotations(
        y=1.05, 
        x=0.5, 
        text='',#"Task type",
        showarrow=F,
        font=list(size=20)
      )
  })
  
  output$illustration_pie.tbrinfo <- renderPlotly({
    
    p <- PN_MakePiePlotly(reviewtable,'To_be_remembered_information')
    p <- p  %>%
      add_annotations(
        y=1.05,
        x=0.5,
        text='',#"To-be-remembered \n information",
        showarrow=F,
        font=list(size=17)
      )
    
    
    
  })
  
  
  # EXPLORE ----
  
  ## HEB ----
  
  # download csv
  filename <- reactive({
    
    if (input$HEB_type == 'trend'){'data-'} else
      if (input$HEB_type == 'contrast'){'data-'} else
        if (input$HEB_type == 'similarity'){'most-similar-entries-'}
    
  })
  content <- reactive({
    
    if (input$HEB_type == 'trend'){citation.filtered()%>% pull(APAref)} else
      if (input$HEB_type == 'contrast'){connect_OverlapOrUnique()} else
        if (input$HEB_type == 'similarity'){topSimilarity.filtered() %>% head(5)}
    
  })
  output$download <-  downloadHandler(
    filename = function() {
      # paste(filename(), Sys.Date(), ".csv", sep="")
      
      if (input$HEB_type == 'trend'){paste(filename(), Sys.Date(), ".txt", sep="")} else
        if (input$HEB_type == 'contrast'){paste(filename(), Sys.Date(), ".csv", sep="")} else
          if (input$HEB_type == 'similarity'){paste(filename(), Sys.Date(), ".csv", sep="")}
      
      
    },
    content = function(file) {
      
      if (input$HEB_type == 'trend'){writeLines(content(), file)} else
        if (input$HEB_type == 'contrast'){write.csv(content(), file, row.names = F)} else
          if (input$HEB_type == 'similarity'){write.csv(content(), file, row.names = F)}
      
      
      
    }
  )
  
  
  ## Display plots
  # (static plot, the constant frame that contains only nodes)
  # Plot for hierarchical vertices: p
  p <- reactive({
    value.df <- data.frame(
      #drop identifier vars
      name = setdiff(names(df.filtered()), identifier_var),
      #nEntry=the number of entries (sum of 1s) that light up each node
      nEntry = as.numeric(colSums(df.filtered() %>% select(-identifier_var)))
    )
    
    vertices <- left_join(vertices, value.df, by = "name", all = T)
    {size.breaks = seq.int(0, max(vertices$nEntry, na.rm = T), length.out=3) %>% round()}
    
    # draw plot
    mygraph <- graph_from_data_frame(hierarchy, vertices = vertices)
    p <- PN_PlotNode(mygraph=mygraph, size.breaks = size.breaks)
    return(p)
    
  })
  
  
  output$illustration_HEB <- renderPlot({
    if (input$HEB_type == 'contrast'){
      if (nrow(df.filtered())>2) {
        showNotification("Cannot show more than two entries individually. Use the filter to reduce task entries.",type='error')
        return(p())
      }
      if (nrow(df.filtered())<=2) {
        
        value.df <- data.frame(
          #drop identifier vars
          name = setdiff(names(df.filtered()), identifier_var),
          #nEntry=the number of entries (sum of 1s) that light up each node
          nEntry = as.numeric(colSums(df.filtered() %>% select(-identifier_var)))
        )
        
        vertices <- left_join(vertices, value.df, by = "name", all = T)
        
        from <- match(connect_OverlapOrUnique()$from, vertices$name)
        to <- match(connect_OverlapOrUnique()$to, vertices$name)
        Entry_ID <- connect_OverlapOrUnique()$Entry_ID
        
        p_filtered <- PN_PlotEdge_Filter(p(), edge.from = from, edge.to = to, edge.color = Entry_ID)
        
        return(p_filtered)
        
      }
      
    }
    
    
    if (input$HEB_type == 'trend') {
      # extra: apply cut-off to determine which edges go into the plot
      connect.clean <- connect_OverlapOrUnique() %>%
        #>10% of the entries have this edge
        filter(Proportion >= 20) %>%
        #arrange in ascending order, the weakest edge will be drawn first, the most prevalent edge is drawn on top
        arrange(Proportion)
      
      from.clean <- match(connect.clean$from, vertices$name)
      to.clean <- match(connect.clean$to, vertices$name)
      my_alpha.clean <- connect.clean$Proportion %>% as.numeric()
      
      p_group <- PN_PlotEdge_Group(p(), edge.from = from.clean, edge.to = to.clean, edge.color = my_alpha.clean)
      
      return(p_group)
    }
    
    if (input$HEB_type == 'similarity') {
      
      Similarity.entries1 <- topSimilarity.filtered()$Entry1 %>% head(5) %>% paste(collapse = "|")
      Similarity.entries2 <- topSimilarity.filtered()$Entry2 %>% head(5) %>% paste(collapse = "|")
      Similarity.Entries <- paste(Similarity.entries1, Similarity.entries2, sep = "|")
      
      Similarity.df.filtered <- df %>% filter(str_detect(Entry_ID, Similarity.Entries))
      
      Similarity.value.df <- data.frame(
        #drop identifier vars
        name = setdiff(names(Similarity.df.filtered), identifier_var),
        #nEntry=the number of entries (sum of 1s) that light up each node
        nEntry = as.numeric(colSums(Similarity.df.filtered %>% select(-identifier_var)))
      )
      
      vertices <- left_join(vertices, Similarity.value.df, by = "name", all = T)
      {size.breaks = seq.int(0, max(vertices$nEntry, na.rm = T), length.out=3) %>% round()}
      
      # draw plot
      mygraph <- graph_from_data_frame(hierarchy, vertices = vertices)
      Similarity.p <- PN_PlotNode(mygraph=mygraph, size.breaks = size.breaks)
      
      Similarity.connect <- PN_GetConnection(data=Similarity.df.filtered,
                                             identifier_var = identifier_var)
      # look for overlapping edges
      Similarity.connect_OverlapOrUnique <- PN_GetConnection_OverlapOrUniqueLabel(Similarity.connect)
      Similarity.connect_OverlapOrUnique$Proportion <- as.numeric(Similarity.connect_OverlapOrUnique$Proportion)
      Similarity.connect_OverlapOrUnique$Entry_ID <- as.factor(Similarity.connect_OverlapOrUnique$Entry_ID)
      
      # extra: apply cut-off to determine which edges go into the plot
      connect.clean <- Similarity.connect_OverlapOrUnique %>%
        #>10% of the entries have this edge
        filter(Proportion >= 0) %>%
        #arrange in ascending order, the weakest edge will be drawn first, the most prevalent edge is drawn on top
        arrange(Proportion)
      
      from.clean <- match(connect.clean$from, vertices$name)
      to.clean <- match(connect.clean$to, vertices$name)
      my_alpha.clean <- connect.clean$Proportion %>% as.numeric()
      
      p_group <- PN_PlotEdge_Group(Similarity.p, edge.from = from.clean, edge.to = to.clean, edge.color = my_alpha.clean)
      
      return(p_group)
    }
    
    
  })
  
  
  ## DELAY ----
  ### Display output
  
  # Plot
  output$illustration_Delay <- renderPlotly({
    
    delay1 <- PN_DelayDf_Stack(data=delay1.filtered() %>% select(Entry_ID, starts_with('Delay_'), starts_with('Task_type')),
                               split.interval = ' - ')
    nTasktype <- delay1$Task_type %>% unique() %>% length()
    
    pDelay <- ggplot(data=delay1)+
      geom_segment(aes(x=X.a, xend=X.b, y=Y, yend=Y, group=ID_Delay, color=Task_type))+
      geom_point(aes(x=X.a, y=Y, group=ID_Delay, color=Task_type, text=text))+
      geom_point(aes(x=X.b, y=Y, group=ID_Delay, color=Task_type, text=text))+
      geom_line(aes(x=X.a, y=Y, group=Entry_ID, color=Task_type), linetype='dotted')+
      scale_x_continuous(breaks=c(0,525600,1051200,2102400,3153600), labels = c("0","1y","2y","4y","6y"), minor_breaks=seq(0,1440,by=1), limits = c(0,3153600))+
      ylim(-1, 3153600)+
      scale_color_manual(values = TaskTypelevelPalette[1:nTasktype], name = 'Task type')+
      scale_fill_manual(values = TaskTypelevelPalette[1:nTasktype], name = 'Task type')+
      labs(x = 'Delay', y = 'Task entry')+
      # guides(fill = guide_legend(override.aes = list(alpha=1,size=8)))+
      theme(
        axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line.x = element_line(colour = "black"))
    
    return(hide_guides(ggplotly(pDelay, tooltip = c('text'))))
  })
  
  
  
  
  
}


shinyApp(ui, server)

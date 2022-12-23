library(shiny)
library(shinyWidgets)
library(DT)
library(dplyr)
library(ggplot2)
library(tools)
library(visreg)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(ggiraph)
library(janitor)

imdbHeader <- dashboardHeader(title='IMDB Data Analysis')

imdbSidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      text='Main analysis', 
      tabName='home'),
    
    menuItem(
      text='Directors Analysis', 
      tabName='directorAnalysis'),
    
    menuItem(
      text='Rating Analysis', 
      tabName='ratingAnalysis'),
    
    menuItem(
      text='Gross Collection Analysis', 
      tabName='grossAnalysis')
  )
)

imdbBody <- dashboardBody(
  tabItems(
    tabItem(
      tabName='home',
      fluidPage(
        titlePanel("Choose Dataset To Analysis"),
        sidebarPanel(
          
          fileInput(
            inputId = "filedata",
            label = "Upload data. csv",
            multiple = FALSE,
            accept = c(".csv"),
            buttonLabel = "Choosing ...",
            placeholder = "No files selected yet"
          ),
          uiOutput("choose_columns")
          ,width = 3), #sidebarpanel
        
        mainPanel(
          tabsetPanel(id = "tabspanel", type = "tabs",
                      tabPanel(title = "Data Cleaned", 
                               br(),
                               DT::dataTableOutput("dataCleaned")),
                      # New tab panel for Description
                      tabPanel("Description", 
                               #label("Hello Lê Ngọc Trúc")
                               br(),
                               fileInput(
                                 inputId = "filedescription",
                                 label = "Upload file description. csv",
                                 multiple = FALSE,
                                 accept = c(".csv"),
                                 buttonLabel = "Choosing ...",
                                 placeholder = "No files selected yet"
                               ),
                               DT::dataTableOutput("Description")),
                      #Tab Plot Linear Regresstion 
                      tabPanel(title = "Plot Linear Regresstion", 
                               br(),
                               uiOutput("xvariable"),
                               uiOutput("yvariable"),
                               plotOutput(outputId = "scatterplot")),
                      tabPanel(title = "Predict Rating with Best Linear Regresstion",
                               uiOutput("newdata"),
                               br(),
                               fluidRow(
                                 column( fileInput(
                                   inputId = "filetest",
                                   label = "Upload file test csv",
                                   multiple = FALSE,
                                   accept = c(".csv"),
                                   buttonLabel = "Choosing ...",
                                   placeholder = "No files selected yet"
                                 ), width = 4),
                                 column(div(style = "padding:12px"),actionButton("predict", "Predict"), width = 4)),
                               fluidRow(
                                 column(DT::dataTableOutput('ratingPredict'), width = 12)),
                               br(),
                               verbatimTextOutput('lmSummary')
                      ))
        )
      ) #fluidpage
      
    ),
    
    tabItem(
      tabName='directorAnalysis',
      fluidPage(
        fluidRow(
          
          box(title='Top Director', width=2,
              numericInput(inputId='directorN', label='Choose Top N Directors', value=5, min=1, max=25, step=1),
              
              tableOutput(outputId='directorTable')),
          uiOutput("genreLG")
          
          
        )
      )
    ),
    
    tabItem(
      tabName='ratingAnalysis',
      fluidPage(
        fluidRow(
          box(title = 'Rating by film genre over the years', width = 12,
              plotOutput(outputId = 'ratingTimeLG'))
        )
      )
    ),
    
    tabItem(
      tabName='grossAnalysis',
      fluidPage(
        fluidRow(
          
          box(title='Top Film with the highest gross ( M$ )', width=5,
              numericInput(inputId='filmN', label='Choose Top N Film', value=5, min=1, max=25, step=1),
              
              tableOutput(outputId='filmTable')),
          box(title='Top Genre with the highest gross ( M$ )', width=5,
              numericInput(inputId='genregrossN', label='Choose Top N Film Genre', value=5, min=1, max=25, step=1),
              
              tableOutput(outputId='genregrossTable')),
            
            uiOutput("grossLG")
          
          
        )
      )
    )
  )
)

ui <- dashboardPage(header=imdbHeader, sidebar=imdbSidebar, body=imdbBody)

#server
server <- function(input, output) {
  
  dataraw <- reactive({
    req(input$filedata)
    inData <- input$filedata
    if (is.null(inData)){ return(NULL) }
    mydata <- read.csv(inData$datapath, header = TRUE, sep=",")
  })
  
  data <- reactive({
    # chuyển tập dữ liệu thô qua hàm clean_names(), gán kết quả là "idm_top1000"  
    mydata <- dataraw() %>% 
      janitor::clean_names()
    
    #Hàm distinct(). Hàm này kiểm tra tất cả các hàng và rút gọn bộ dữ liệu cho chỉ còn các hàng là duy nhất. 
    #Nghĩa là, nó loại bỏ 100% các hàng trùng lặp.
    mydata<-distinct(mydata)
    
    #Thay thế các giá trị trống thành NA
    mydata[mydata == ""]<-NA;
    #Thay thế các giá trị NA thành 0
    mydata[is.na(mydata)] <-0 
    
    
    ##Bài 4: Tạo các biến mới dựa vào các biến đã có và các điều kiện (tùy ý)
    #Đánh giá độ hay của phim
    #Từ 0 đến 39 điểm: Phim mà phần lớn cho rằng tệ
    mydata$evaluated[mydata$metascore < 40] <- 'Bad'
    #Từ 40 đến 60 điểm: Phim nằm ở mức độ trung bình. 
    mydata$evaluated[mydata$metascore < 61 & mydata$metascore >= 40] <- 'Medium'
    #Từ 61 đến 80 điểm: Phim khá hay . 
    mydata$evaluated[mydata$metascore < 81 & mydata$metascore >= 61] <- 'Good'
    #Từ 81 đến 100 điểm: Phim rất hay
    mydata$evaluated[mydata$metascore >= 81] <- 'Very Good'
    
    #Chuyển các cột giá trị thành numeric
    mydata$gross_collection = as.numeric(gsub("[$M]", "", mydata$gross_collection))
    mydata$runtime = as.numeric(gsub("[ min]", "", mydata$runtime))
    mydata$votes = as.numeric(gsub("[,]", "", mydata$votes))
    mydata
  })
  
  dataTest <- reactive({
    req(input$filetest)
    inDataTest <- input$filetest
    if (is.null(inDataTest)){ return(NULL) }
    mydata <- read.csv(inDataTest$datapath, header = TRUE, sep=",", encoding = "UTF-8")
  })
  
  output$dataCleaned  <- DT::renderDT({
    datatable(
      data(),
      caption = 'Table: Data Cleaned',
      extensions = 'Buttons',
      options = list(dom = 'Bfrtip',buttons = c('csv'),scrollX = TRUE)
    )
  })
  
  #Description
  description <- reactive({
    req(input$filedescription)
    inData2 <- input$filedescription
    if (is.null(inData2)){ return(NULL) }
    mydata <- read.csv(inData2$datapath, header = TRUE, sep=",", encoding = "UTF-8")
  })
  
  output$Description <- DT::renderDataTable({
    DT::datatable(data = description(),
                  options = list(pageLength = 10, lengthMenu = c(10, 25, 40)), 
                  rownames = FALSE)
  })
  
  
  output$xvariable <- renderUI({
    req(data())
    xa<-colnames(select_if(data(), is.numeric)) 
    pickerInput(inputId = 'xvar',
                label = 'Select x-axis variable',
                choices = c(xa[1:length(xa)]), selected=xa[1],
                options = list(`style` = "btn-info"))
    
  })
  output$yvariable <- renderUI({
    req(data())
    ya<-colnames(select_if(data(), is.numeric)) 
    pickerInput(inputId = 'yvar',
                label = 'Select y-axis variable',
                choices = c(ya[1:length(ya)]), selected=ya[2],
                options = list(`style` = "btn-info"))
    
  })
  
  
  lmModel <- reactive({
    req(data(),input$xvar,input$yvar)
    x <- as.numeric(data()[[as.name(input$xvar)]])
    y <- as.numeric(data()[[as.name(input$yvar)]])
    if (length(x) == length(y)){
      model <- lm(y ~ x, data = data(), na.action=na.exclude)
    }else model <- NULL
    return(model)
  })
  
  ggplotRegression <- function (fit) {
    require(ggplot2)
    
    ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1] )) + 
      geom_point(alpha = 0.5, size = 3) +
      stat_smooth(method = "lm") 
  }
  # Create scatterplot object the plotOutput function is expecting 
  output$scatterplot <- renderPlot({
    ggplotRegression(lmModel())
  })
  
  
  
  lmModel2 <- reactive({
    req(data())
    data_num <- data()[,c("rating","metascore","votes","gross_collection","year","runtime","primary_genre","director")]
    # Subset numeric columns with dplyr
    #define only model
    only<-lm(rating~1,data = data_num)
    #define model with all predictors
    all<-lm(rating~. , data = data_num)
    #perform forward stepwise regresstion
    forward <- step(only, direction = 'forward',scope = formula(all),trade =0)
    return(forward)
  })
  
  output$scatterplotfw <- renderPlot({
  })
  
  output$lmSummary <- renderPrint({
    req(lmModel2())
    summary(lmModel2())
  })
  
  predictRT <- eventReactive(input$predict, {
    req(dataTest())
    newdattest <- dataTest()[,c("metascore","votes","gross_collection","year","runtime","primary_genre","director")]
    p_rating <- predict(lmModel2(),
                        new = newdattest,
                        interval = "confidence",
                        level = .95
    )
    df <- data.frame(dataTest(), PREDICTED_COUNTS = p_rating)
    df
  })
  
  output$ratingPredict <- DT::renderDT({
    datatable(
      predictRT(),
      caption = 'Table: Rating Predicted Result.',
      extensions = 'Buttons',
      options = list(dom = 'Bfrtip',buttons = c('csv'),scrollX = TRUE)
    )
  })
  
  
  output$directorTable <- renderTable({
    data()%>%
      select(director) %>%
      count(director) %>%
      arrange(desc(n)) %>%
      slice_head(n=input$directorN) 
  })
  
  output$directorPlot <- renderPlotly({
    if(is.null(input$dirGenreSelect)){
      ggplot() + labs(title='Please Select a Genre') + xlim(0,10) + ylim(0, 300) + theme_bw()
    } else {
      dir <- data() %>%
        select(director) %>%
        count(director) %>%
        arrange(desc(n)) %>%
        slice_head(n=input$directorN)
      
      dirPlot <- data() %>%
        select(director, runtime, rating, primary_genre) %>%
        subset(., subset= director %in% dir$director) %>%
        subset(., subset= primary_genre %in% input$dirGenreSelect) %>%
        ggplot() + 
        geom_point(mapping=aes(runtime, rating, color=director, shape=primary_genre, size=1.5, alpha=0.75,
                               text=paste0('Director: ', director,'\nGenre: ', primary_genre, '\nRating: ', rating,
                                           '\nRuntime: ', runtime, ' min.'))) +
        xlab(label = 'Runtime') +
        ylab(label = 'Rating') +
        xlim(75, 220) +
        ylim(0, 9) +
        theme_bw() +
        theme(
          legend.position='none'
        )
      
      ggplotly(dirPlot, tooltip=c('text')) %>%
        config(displayModeBar=FALSE) %>%
        layout(
          yaxis=list(fixedrange=TRUE),
          xaxis=list(fixedrange=TRUE)
        )
    }
  })
  
  output$genreLG <- renderUI({
    box(title='', width=10, height=700,
        selectInput(
          inputId='dirGenreSelect', label='Select Genres: ',
          choices=c(str_sort(unique(data()$primary_genre))),
          multiple=TRUE, selected=c('Action', 'Adventure', 'Comedy')),
        plotlyOutput(outputId='directorPlot', height=550)
    )
  })
  
  output$ratingTimeLG <- renderPlot({
    data()[, c('rating', 'year', 'primary_genre')] %>%
      group_by(year) %>%
      ggplot(mapping = aes(x = year, y = rating, color = primary_genre)) +
      geom_line() + 
      geom_point(alpha = 0.5, size = 2) +
      xlab(label = 'Year') +
      ylab(label = 'Rating') +
      theme_bw()
  })
  output$filmTable <- renderTable({
    data()%>%
      select(ranking_of_movie,movie_name, gross_collection) %>%   #chọn các cột ranking_of_movie,movie_name, gross_collection
      mutate(
        gross_collection = as.numeric(gsub("[$M]", "", gross_collection)), #chuyển đổi gross_collection ở kiểu string sang kiểu numeric để tính toán
      ) %>% 
      arrange(desc(gross_collection)) %>% #sắp xếp giảm dần theo gross_collection
      slice_head(n=input$filmN) 
  })
  
  output$genregrossTable <- renderTable({
    data()%>%
      select(gross_collection,primary_genre) %>%
      group_by(primary_genre) %>%
      summarize(Gross_Colecction = sum(gross_collection)) %>%
      arrange(desc(Gross_Colecction)) %>%
      slice_head(n=input$genregrossN) 
  })
  
  output$grossPlot <- renderPlotly({
    if(is.null(input$dirGenreSelect)){
      ggplot() + labs(title='Please Select a Genre') + xlim(0,10) + ylim(0, 300) + theme_bw()
    } else {
      
      grPlot <- data() %>%
        select(movie_name, runtime, rating,gross_collection, primary_genre) %>%
        ggplot() + 
        geom_point(mapping=aes(runtime, rating, color=primary_genre, size=1, alpha=0.2,
                               text=paste0('Movie Name: ', movie_name,'\nGenre: ', primary_genre,'\nGross Collection: ', gross_collection, ' M$', '\nRating: ', rating,
                                           '\nRuntime: ', runtime, ' min.'))) +
        xlab(label = 'Runtime') +
        ylab(label = 'Rating') +
        xlim(75, 220) +
        ylim(0, 9) +
        theme_bw() +
        theme(
          legend.position='none'
        )
      
      ggplotly(grPlot, tooltip=c('text')) %>%
        config(displayModeBar=FALSE) %>%
        layout(
          yaxis=list(fixedrange=TRUE),
          xaxis=list(fixedrange=TRUE)
        )
    }
  })
  output$grossLG <- renderUI({
    box(title='', width=10, height=700,
        selectInput(
          inputId='dirGenreSelect', label='Select Genres: ',
          choices=c(str_sort(unique(data()$primary_genre))),
          multiple=TRUE, selected=c('Action', 'Adventure', 'Comedy')),
        plotlyOutput(outputId='grossPlot', height=550)
    )
  })
}

shinyApp(ui = ui, server = server)
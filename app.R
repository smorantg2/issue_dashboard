

#Color original (#93fff4)

# Especificamos las librerías necesarias en esta lista
packages1 = c("shiny","tidyverse","plotly","rjson","ggforce","ggiraphExtra","remotes")
packages2 = c("fontawesome")

package.check1 <- lapply(packages1, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

package.check2 <- lapply(packages2, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    remotes::install_github(paste0("rstudio/",x))
    library(x, character.only = TRUE)
  }
})

ui<-fluidPage('',
              includeCSS("www/styles.css"),
              navbarPage(div(img(src="black.png", height=40, width=140)),
                         tabPanel(h4("File import", class="h4"),
                                  wellPanel(
                                    h4("File import", icon("fas fa-folder-open")),
                                    br(),
                                    fileInput("DatosFichero", "Select data files", multiple = TRUE, accept = NULL)
                                  )),
                             
                         tabPanel(h4("Visualizations", class="h4"),
                           sidebarLayout(
                             
                             sidebarPanel(
                               h4("Control Panel", icon("fas fa-desktop"), class = "h4", align = "center"),
                               sliderInput("Slider_internal", min = 1, max = 99, value = 50, label = "% Internal Weight"),
                               sliderInput("Slider_external", min = 1, max = 99, value = 50, label = "% External Weight"),
                               uiOutput("Slider_lolipop"),
                               
                               
                               br(),
                               h4("Adjust external weights", icon("cogs"), align = "center"),
                               uiOutput("weight1"),
                               uiOutput("weight2"),
                               uiOutput("weight3"),
                               uiOutput("weight4"),
                               uiOutput("weight5"),
                               h4(textOutput("Total_pesos")),
                               br(),
                               uiOutput("actionB"),
                               br(),
                               uiOutput("text_updated")
                             ),
                             mainPanel(
                               h3("Most relevant issues", align = "center"),
                               plotOutput("lolipop"),
                               downloadButton("pngdownload", "Download", class="btn-file"),
                               uiOutput("SelectIssue", align = "center"),
                               fluidRow(
                                 column(width=4,
                                        h4("External scores", align = "center"),
                                        plotlyOutput("radar")),
                                 column(width=8, plotlyOutput("barras_topic_mapping", width = "80%"))),
                               uiOutput("SelectTopic"),
                               span(textOutput("Descripcion"), class="texto"),
                               br(),
                               br(),
                               br()
                                  
                                  
                                  )
                             )
                           )
                         )
              )

server <- function(input, output, session) { 
  
  table_data<-eventReactive(input$DatosFichero, {
    tabdata<-fromJSON(file=input$DatosFichero$datapath[input$DatosFichero$name=="tableData.json"])
    return(tabdata)
  })
  
  table_weights<-eventReactive(input$DatosFichero, {
    tabwei<-fromJSON(file=input$DatosFichero$datapath[input$DatosFichero$name=="tableWeights.json"])
    return(tabwei)
  })
  
  topic_mapping<-eventReactive(input$DatosFichero, {
    topmap<-fromJSON(file=input$DatosFichero$datapath[input$DatosFichero$name=="topic-mapping.json"])
    return(topmap)
  })
  
  DF_weights <- eventReactive(c(input$DatosFichero),{
    weights <- as.data.frame(table_weights())
    weights <- weights[,1:5]
    names(weights) <- c("Benchmark", "ObserveM", "ObserveV", "Newsflow", "Social")
    return(weights)
  })
  
  output$weight1 <- renderUI({textInput("weight1", "Benchmark", value = DF_weights()[1,"Benchmark"])})
  output$weight2 <- renderUI({textInput("weight2", "ObserveM", value = DF_weights()[1,"ObserveM"])})
  output$weight3 <- renderUI({textInput("weight3", "ObserveV", value = DF_weights()[1,"ObserveV"])})
  output$weight4 <- renderUI({textInput("weight4", "Newsflow", value = DF_weights()[1,"Newsflow"])})
  output$weight5 <- renderUI({textInput("weight5", "Social", value = DF_weights()[1,"Social"])})
  
  
  # when internal change, update external
  observeEvent(input$Slider_internal,  {
    updateSliderInput(session = session, inputId = "Slider_external", value = 100 - input$Slider_internal)
  })
  
  # when external change, update internal
  observeEvent(input$Slider_external,  {
    updateSliderInput(session = session, inputId = "Slider_internal", value = 100 - input$Slider_external)
  })
  
  


  DF_lolipop <- eventReactive(c(input$DatosFichero,input$Slider_internal, input$update_pesos), {
    
                suma <- as.numeric(input$weight1)+as.numeric(input$weight2)+as.numeric(input$weight3)+as.numeric(input$weight4)+as.numeric(input$weight5)
    
                df <- do.call(rbind.data.frame, table_data())
                colnames(df)[9:10] <- c("External","Internal")
                
                if (suma == 100){
                
                  df$External <- (df$benchmark0 * as.numeric(input$weight1) + df$observeM * as.numeric(input$weight2) + df$observeV * as.numeric(input$weight3) + df$newsflow * as.numeric(input$weight4) + df$social * as.numeric(input$weight5))/100
                }
                else if(suma != 100) {
                  df$External <- as.numeric((df$benchmark0 * as.numeric(20) + df$observeM * as.numeric(20) + df$observeV * as.numeric(20) + df$newsflow * as.numeric(20) + df$social * as.numeric(20))/100)
                  
                }
                
                df$name <- as.character(df$name)
                df$External <- df$External * (1- input$Slider_internal/100)
                df$Internal <- df$Internal * (input$Slider_internal/100)
                
                df$total <- df$External + df$Internal
                df <- df %>% arrange(total)
                df$index <-  seq(1, length.out = dim(df)[1], by = 2.5)
                df <- df %>% select(name, External, Internal, total, index)
                df <- df %>% gather("External","Internal", key = "View", value = "score") %>% arrange(total,name)
                df <- df %>% group_by(name) %>% mutate(score_frac = 2*pi*cumsum(score)/total, start = lag(score_frac, default = 0))
                df$scattersize <- sqrt(df$total)*2
                df <- df %>% arrange(desc(index))
                df$index <- df$index *2.5
                return (df)
  })
  

  
  #Vector de strings con los nombres de los Issues
  Names_Issues <- eventReactive(c(input$DatosFichero), {
    
                df_table <- do.call(rbind.data.frame, table_data())
                df_table$name <- as.character(df_table$name)
                return(df_table$name)
    
  })
  
  #Desplegable de los Issues posibles
  output$SelectIssue<-renderUI({
    selectInput("SelectIssue", label="Select Issue", Names_Issues())
  })
  
  #Text output del total de pesos de external
  output$Total_pesos <- eventReactive(c(input$weight1,input$weight2,input$weight3,input$weight4,input$weight5),{
    suma <- as.numeric(input$weight1)+as.numeric(input$weight2)+as.numeric(input$weight3)+as.numeric(input$weight4)+as.numeric(input$weight5)
    
    if(is.na(suma)) {
      paste0("Incorrect total")
    }
    else {
      paste0("Total: ", suma)
    }
  })
  
  #Update button
  output$actionB <- renderUI({
    suma <- as.numeric(input$weight1)+as.numeric(input$weight2)+as.numeric(input$weight3)+as.numeric(input$weight4)+as.numeric(input$weight5)
    
    if(is.na(suma)) {
      h4(icon("fas fa-exclamation-triangle"), "Fill all the gaps", class="aviso")
    }
    else if(suma != 100) {
      h4(icon("fas fa-exclamation-triangle"), "Total must be 100", class="aviso")
    }
    else {
      actionButton("update_pesos", "Update Weights", class="btn-file")
    }
  })
  
  #Text updated
  output$text_updated <- renderUI({
    if(input$update_pesos) {
      span(h4(icon("far fa-check-circle"), paste0("Updated"),class="actualizados"))
    }
    else {
      paste0("")
    }
  })
  
  #Dataframe de las puntuaciones de cada parte dentro de External
  DF_external <- eventReactive(c(input$DatosFichero, input$SelectIssue), {
    
                df_table <- do.call(rbind.data.frame, table_data())
                df_table$name <- as.character(df_table$name)
                data_aux <- df_table %>% filter(name == input$SelectIssue) %>%
                  select(c("benchmark0","observeM","observeV","newsflow","social"))
                return(data_aux)
    
  })
  
  
  scale <-  eventReactive(input$DatosFichero, {
    sca <- 0.5/sqrt(max(DF_lolipop()$total))
    return(sca)
  })
  
  #maxx <-  eventReactive(input$DatosFichero, {
  #  ma <- dim(DF_lolipop())[1]/2
  #  return(ma)
  #})
  
  #Devuelve un slider para NUMERO DE LOLIPOPS (ISSUES)
  output$Slider_lolipop <- renderUI({
    sliderInput("Slider_lolipop", "Number of issues to show", 
                min= 5, max = 30, value = 10, step= 1)
  })

  
  
  output$lolipop<-renderPlot({
    
   df <- DF_lolipop()[1:(input$Slider_lolipop * 2),]
    
   ggplot(df) + 
              geom_arc_bar(aes(x0 = index*2, y0 = total, r0 = sqrt(total)*scale()*8, r = sqrt(total)*scale()*10,
                               start = start, end = score_frac, fill = View), col = NA) +
              geom_segment(aes(x = index*2,xend = index*2, y = 0,yend = total-sqrt(total)*scale()*10),linetype="dotdash")+
              coord_fixed() +
              #scale_y_continuous(breaks = c(0), labels = c(""), name = "Relevancia") +
              ylab("Importance")+
              scale_x_continuous(breaks = df$index*2, labels = df$name, name = "Issue") +
              theme_minimal() +
              theme(panel.grid.minor = element_blank(),axis.text.x = element_text(hjust = 1.1, vjust=1.05, angle=45),
              text = element_text(size=15))+
              scale_fill_manual(values = c("#ABF0E9","#29B6A8"))+  
              #scale_fill_manual(values = c("#93fff4","#ff9640"))+     #Colores/ Claros c("#ABF0E9","#FDBE87")
              theme(plot.margin = unit(c(0,0,0,1), "cm"))
   
   
  })
  
  
  #Descarga de Lolipop en formato .png
  output$pngdownload <- downloadHandler(
    
    filename = function(){paste0("TOP_",input$Slider_lolipop,"_Issues.png")},
    content =  function(file) {
      
      ggsave(filename = file, width = 16, height = 9, device = "png", 
             plot = ggplot(DF_lolipop()[1:(input$Slider_lolipop * 2),]) + 
               geom_arc_bar(aes(x0 = index*2, y0 = total, r0 = sqrt(total)*scale()*8, r = sqrt(total)*scale()*10,
                                start = start, end = score_frac, fill = View), col = NA) +
               geom_segment(aes(x = index*2,xend = index*2, y = 0,yend = total-sqrt(total)*scale()*10),linetype="dotdash")+
               coord_fixed() +
               #scale_y_continuous(breaks = c(0), labels = c(""), name = "Relevancia") +
               ylab("Importance")+
               scale_x_continuous(breaks = DF_lolipop()[1:(input$Slider_lolipop * 2),]$index*2, labels = DF_lolipop()[1:(input$Slider_lolipop * 2),]$name, name = "Issue") +
               theme_minimal() +
               theme(panel.grid.minor = element_blank(),axis.text.x = element_text(hjust = 1.1, vjust=1.05, angle=45),
                     text = element_text(size=15))+
               scale_fill_manual(values = c("#ABF0E9","#29B6A8"))+
               ggtitle("Most relevant issues")+
               #scale_fill_manual(values = c("#8CECE3","#58AFA6"))+     
               theme(plot.margin = unit(c(0,0,0,1), "cm")) )
    }
    
  )
  
  
  output$radar <- renderPlotly({
    fig <- plot_ly(
      type = 'scatterpolar',
      r = as.numeric(DF_external()[1,]),
      theta = names(DF_external()),
      fill = 'toself',
      fillcolor = 'rgba(147, 255, 244, 0.6)',
      size = 2
    ) 
    fig <- fig %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,30)
          )
        ),
        showlegend = F
      )
    
    fig
  })
  
  
  
  DF_TopMap <- eventReactive(c(input$SelectIssue),{
                
    for(t in c(1:length(topic_mapping()))) {
      if(!exists("df2")) {
        df2 <- do.call(rbind.data.frame, topic_mapping()[[t]]$topics)
        df2$issue <- topic_mapping()[[t]]$name
      } else {
        df2_aux <- do.call(rbind.data.frame, topic_mapping()[[t]]$topics)
        df2_aux$issue <- topic_mapping()[[t]]$name
        df2 <- bind_rows(df2, df2_aux)

      }
    }
    
    df2 <- df2[!duplicated(df2[,1]),]
    
    return(df2)
  })
  
  
  output$barras_topic_mapping <- renderPlotly({
    
    data2_aux <- DF_TopMap() %>% filter(issue == input$SelectIssue) %>% select(c("topic_name","weight"))
    #data2_aux <- data2_aux[!duplicated(data2_aux[,1]),]
    data2_aux$topic_name <- factor(data2_aux$topic_name, levels = sort(data2_aux$topic_name, decreasing = TRUE))
    
    ggplotly(ggplot(data2_aux, aes(x=topic_name, y=weight)) +
      geom_segment( aes(xend=topic_name, yend=0),color = "gray" ) +
      geom_point( size=6, color="#ff9640") +
      coord_flip() +
      theme_minimal()+
      ggtitle(paste0(input$SelectIssue," - TOPICS"))+
      ylab("Peso")+xlab("")+
      theme(panel.grid.minor = element_blank(), text = element_text(size=10))
      
    )
    
  })
  
  #Vector de strings con los nombres de los topics
  Names_Topics <- eventReactive(c(input$DatosFichero, input$SelectIssue), {
    topics <- DF_TopMap() 
    topics <- topics %>% filter(issue == input$SelectIssue)
    return(topics$topic_name)
  })
    
  #Desplegable de los Topics posibles para ver su descripción
  output$SelectTopic<-renderUI({
    selectInput("SelectTopic", label="Select a topic to read its description", Names_Topics())
  })
  
  output$Descripcion <- eventReactive(c(input$DatosFichero, input$SelectTopic),{
    topics <- DF_TopMap() %>% filter(issue == input$SelectIssue) %>% select(topic_name, topic_desc) %>% filter(topic_name == input$SelectTopic)
    if (is.na(topics$topic_desc) == FALSE){
      paste(topics$topic_desc)
    }
    
  })
  
  
  
}

shinyApp(ui, server)

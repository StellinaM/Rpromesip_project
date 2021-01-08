#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

ui <- dashboardPage( 
    dashboardHeader(title="Project_R"),  
    dashboardSidebar(
        fileInput("selectFile", "Choose CSV File",
                  accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
        ),
        actionButton("load", "Load File")
    ),    
    dashboardBody(
        tabsetPanel(
            tabPanel("Plots",
                     sidebarPanel(
                         uiOutput("dynamicUI")
                     ),
                     mainPanel(
                         plotOutput("demoPlot")
                     )
            ),
            tabPanel("Statistical Tests",
                     sidebarPanel(
                         tableOutput("demoTable")
                     ),
                     mainPanel(
                         tableOutput("model1"),
                         tableOutput("model2")
                     )
            )
        ))
)


server <- shinyServer(function (input,output,session) {  
    v <- reactiveValues(variable = NULL, v1 = NULL, v2 = NULL, v3 = NULL)
    
    observeEvent(input$load, {
        
        if (is.null(input$selectFile)) {
            return()
        }
        else {
        v$variable<-read.table(input$selectFile$datapath,sep=",",header=T)
        d = v$variable
        
        k = 1
        sum = 0
        d_mod = data.frame(matrix(ncol=35))
        df = data.frame(matrix(ncol=35))
        
        for(i in 1:nrow(d)) {
            
            for(j in 1:length(d)) {
                if(d[i,j] == "?" || d[i,j] == " ") {
                    sum = sum + 1
                }
                else if(d[i,2] == "R") {
                    d[i,2] = 1
                    sum = sum + 0
                }
                else if(d[i,2] == "N") {
                    d[i,2] = 0
                    sum = sum + 0
                }
                else {
                    sum = sum + 0
                }
            }
            
            if(sum == 0) {
                d_mod[c(k),] = d[c(i),]
                k = k + 1
            }
            else {
                sum = 0
            }
        }

        
        for(i in 1:length(d_mod)) {
            for(j in 1:nrow(d_mod)) {
                if(i==2) {
                    df[j,2] = as.numeric(d_mod[j,2])
                }
                else if(i==35) {
                    df[j,35] = as.numeric(d_mod[j,35])
                }
                else {
                    df[j,i] = d_mod[j,i]
                }
            }
        }
        
        names = colnames(d)
        colnames(df) = c(names)
        
        attach(df)
        
        d_mod=0
        k=1
        
        for(i in 3:length(names)) {
            t = wilcox.test(df[,i]~outcome,data=df)
            
            if(t$p.value<0.05) {
                d_mod[k] = names[i]
                k = k + 1
            }
        }
        
        d_mod = select(as_data_frame(df),time,radius_mean,perimeter_mean,area_mean,radius_se,perimeter_se,area_se,radius_worst,perimeter_worst,area_worst,tumor_size,lymph_node_status)
        df_cor = cor(d_mod)
        corrplot::corrplot(df_cor, order = "hclust", tl.cex = 1, addrect = 8)
        df_cor = d_mod %>% select(-findCorrelation(df_cor, cutoff = 0.8))
        c = names(df_cor)
        df_cor = cbind(outcome = df$outcome, df_cor)
        
        model_1 = glm(outcome~time+perimeter_se+area_worst+tumor_size+lymph_node_status,data = df_cor, family = binomial)
        model_2 = glm(outcome~time+perimeter_se+area_worst+tumor_size+lymph_node_status, data = df_cor, family = binomial(link = probit))
        }
        
        v$variable = df
        v$v1 = c
        v$v2 = summary(model_1)
        v$v3 = summary(model_2)
    })
    
    output$dynamicUI <- renderUI({
        if (is.null(v$variable)) return()
        selectInput("selectColumn", "Select Column to Plot",  choices = colnames(v$variable)[-c(1,2)], 
                    selected = 1)
    })
    output$demoPlot <- renderPlot(
        if (!is.null(v$variable)) 
            boxplot(v$variable[, input$selectColumn]~v$variable$outcome,xlab="Outcome (R=1, N=0)",ylab=colnames(v$variable[input$selectColumn]))
            )
    
    output$demoTable <- renderTable(
        v$v1
    )
    
    output$model1 <- renderPrint(
        v$v2
    )
    
    output$model2 <- renderPrint(
        v$v3
    )
})


# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)
library(data.table)
library(shinythemes)
library(rJava)
library(tabulizer)
library(dplyr)
library(pdftools)
library(stringr)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Silakan unggah semua file yang dibutuhkan"),
  
  hr(),
  
  selectInput(inputId = "type",label = "Jenis File",
              choices = list("PDF" = 1,"Excel" = 2),
              selected = 1
  ),
  
  conditionalPanel(condition="input.type==1",
                   fileInput('file_pdf', 'Masukkan File PDF', accept = c('.pdf'), multiple = TRUE),
                   hr(),
                   actionButton("grt1", "Lihat file yang telah diunggah"),
                   tags$br(),
                   tags$br(),
                   verbatimTextOutput("print_action1")#,
                   #dataTableOutput("df")
  ),
  
  conditionalPanel(condition="input.type==2",
                   fileInput('file_xls', 'Masukkan File Excel', accept = c('.xls','.xlsx'), multiple = TRUE),
                   hr(),
                   actionButton("grt2", "Lihat file yang telah diunggah"),
                   tags$br(),
                   tags$br()#,
                   #verbatimTextOutput("print_action2")
  ),
  mainPanel(textOutput("count"))
)

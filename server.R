library(shiny)
library(data.table)
library(shinythemes)
library(rJava)
library(tabulizer)
library(dplyr)
library(pdftools)
library(stringr)

server <- function(input, output) {
  myfiles<-reactive({
    rbindlist(lapply(input$file_pdf$datapath, fread),
              use.names = TRUE, fill = TRUE)
  })
  
  output$count <- renderText(nrow(myfiles))
  
  lkpdf <- reactive({
    lkpdf <- input$file_pdf$datapath
    lkpdf
  })
  
  xls_name <- reactive({
    xls_name <- input$file_xls$name
    xls_name
  })
  
  
  
  # output$print_action1 <- renderText({
  #   lkpdf()
  # })
  # 
  # output$print_action2 <- renderText({
  #   xls_name()
  # })
  # 
  lkdf <- reactive({
    
    if (is.null(lkpdf())){
      return(NULL)
    }else{
      #read the transaction code
      txt.File = pdf_text(lkpdf()) %>%
        # class : char
        readr::read_lines()
      
      # select header only
      header = txt.File[4:11]
      header = str_trim(header)
      
      # define type of file
      type.1 = sum(as.numeric(grepl('LAPORAN DAFTAR BARANG MILIK NEGARA', header))) # true = 1, false = 0
      type.2 = sum(as.numeric(grepl('DAFTAR TRANSAKSI PERSEDIAAN', header)))
      type.3 = sum(as.numeric(grepl('LAPORAN POSISI BARANG MILIK NEGARA', header)))
      
      # define field Laporan base on its type of file
      if (type.1 == 1) {
        laporan = 'DAFTAR BMN'
        jns.transaksi = header[2] # jenis laporan
        rincian = header[3]
        periode = header[4]
        tahun = str_sub(header[5],-4,-1)
        satker = strsplit(sub('.*\\.', '', header[7]), " ")[[1]][1]
        kd_transaksi = substr(gsub("[[:space:]]", "", header[8]), 16, 18)
      }
      
      if (type.2 == 1) {
        laporan = 'TRANSAKSI PERSEDIAAN'
        jns.transaksi = '-' # jenis laporan
        rincian = '-'
        periode = str_sub(header[2], 37,-6)
        tahun = str_sub(header[2],-4,-1)
        satker = str_sub(header[5],-11,-6) # almost dynamic (apakah  054013400667062000KD) 000kd itu default?
        kd_transaksi = substr(gsub("[[:space:]]", "", header[6]), 16, 18)
      }
      
      if (type.3 == 1) {
        laporan = 'POSISI BMN'
        jns.transaksi = '-' # jenis laporan
        rincian = '-'
        periode = str_sub(header[2], 20,-6)
        tahun = str_sub(header[3],-4,-1)
        satker = substr(gsub("[[:space:]]", "", header[6]), 23, 28)
        kd_transaksi = "-"
      }
      
      detil = c("laporan",
                "jns.laporan",
                "rincian",
                "periode",
                "tahun",
                "satker",
                "kd_transaksi")
      value = c(laporan,
                jns.transaksi,
                rincian,
                periode,
                tahun,
                satker,
                kd_transaksi)
      
      code.File = cbind(detil, value)
      colnames(code.File) = NULL
      
      # Extract the table
      out <- extract_tables(lkpdf())
      
      # combine extracted tables into one
      final <- do.call(rbind, out)
      
      # make data frame
      final <- as.data.frame(final)
      
      #replace comma and brackets
      if(ncol(final) > 3 && ncol(final) != 5){
        final12 <- final[,c(1,2)]
        final3n <- sapply(final[,3:ncol(final)], gsub, pattern = ",", replacement= "")
        final3n <- gsub("\\(|\\)", "", final3n)
        final <- cbind(final12,final3n)
      }else{
        final <- sapply(final, gsub, pattern = ",", replacement= "")
        final <- gsub("\\(|\\)", "", final)
      }
      
      #make empty column
      tmp.col = matrix(0,7,(ncol(final)-2))
      code.File = cbind(code.File, tmp.col)
      lk_df <- rbind(code.File,final)
    }
  })
  
  output$df <- renderDataTable({
    
    if(!is.null(lkpdf())){
      # Create 0-row data frame which will be used to store data
      dat <- data.frame(x = numeric(0), y = numeric(0))
      
      withProgress(message = 'Memuat Tabel', value = 0, {
        # Number of times we'll go through the loop
        n <- 10
        
        for(i in 1:n){
          # Each time through the loop, add another row of data. This is
          # a stand-in for a long-running computation.
          dat <- rbind(dat, lkdf())
          
          # Increment the progress bar, and update the detail text.
          incProgress(1/n, detail = paste("Bagian", i))
          
          # Pause for 0.1 seconds to simulate a long computation.
          Sys.sleep(0.1)
        }
        dat
      })
    }
  })
  
  pdftocsv <- observeEvent(input$grt1, ignoreInit = TRUE, {
    
    satker <- 101
    #define path and name for file csv
    filename <- paste('http://10.2.3.42/xampp/htdocs/telaah/web/uploads',satker,'/',input$file_pdf$name, '.csv')
    filename <- gsub(" ", "", filename)
    filename <- gsub(".pdf", "", filename)
    
    write.csv(lkdf(), filename)
    showModal(modalDialog("Data Berhasil Disimpan. Silakan ulangi langkah input untuk memasukkan file lainnya"))
  })
  
  xlstocsv <- observeEvent(input$grt2, ignoreInit = TRUE, {
    
    xls_path <- input$file_xls$datapath
    
    df <- read_excel(xls_path)
    
    #change value na with 0
    df[is.na(df)] <- 0
    
    satker <- 101
    #define path and name for file csv
    filename <- paste('C:/xampp/htdocs/telaah/R/',satker,'/',input$file_xls$name, '.csv')
    filename <- gsub(" ", "", filename)
    filename <- gsub(".xlsx", "", filename)
    
    write.csv(df, filename)
    showModal(modalDialog("Data Berhasil Disimpan. Silakan ulangi langkah input untuk memasukkan file lainnya"))
  })
}

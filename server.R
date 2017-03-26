library(shiny)

library(stringi)
library(stringr)
library(data.table)
library(qdap)



shinyServer(function(input, output) {
  
    output$nextWord <- renderTable({
    input$submitButton
    #isolate(input$wordsInput)
    
    
    
    w<-read.table(file="w")
    z<-read.table(file="z")
    y<-read.table(file="y")
    x<-read.table(file="x")
    
    
    
    w[,5]<-as.character(w[,5])
    z[,5]<-as.character(z[,5])
    y[,5]<-as.character(y[,5])
    
    
    input_full<-input$wordsInput
    input_full<-tolower(input_full)
    input_full<-str_trim(input_full,side="both")
    a<-wc(input_full)
    
    if(a>=3) {
      input_full<-word(input_full,start=-3,end=-1)
      input_2<-word(input_full,start=2,end=3)
      input_1<-word(input_full,start=3,end=3)
      example4gram<-w[grep(paste("^",input_full, "$", sep=""), w[,4]),]
      example3gram<-z[grep(paste("^",input_2, "$", sep=""), z[,4]),]
      example2gram<-y[grep(paste("^",input_1, "$", sep=""), y[,4]),]
      count3gram<-z[grep(paste("^",input_full," ", "$", sep=""), z[,1]),][1,2]
      count2gram<-y[grep(paste("^",input_2," ", "$", sep=""), y[,1]),][1,2]
      count1gram<-x[grep(paste("^",input_1," ", "$", sep=""), x[,1]),][1,2]
    }
    
   
    if(a==2) {
      input_2<-input_full
      input_1<-word(input_full,start=2,end=2)
      example4gram<-data.frame(ngrams=character(),freq=integer(),prop=numeric(),col1=character(),
                               next_word=character())
      example3gram<-z[grep(paste("^",input_2, "$", sep=""), z[,4]),]
      example2gram<-y[grep(paste("^",input_1, "$", sep=""), y[,4]),]
      count2gram<-y[grep(paste("^",input_2," ", "$", sep=""), y[,1]),][1,2]
      count1gram<-x[grep(paste("^",input_1," ", "$", sep=""), x[,1]),][1,2]  
    }
    
    if(a==1) {
      input_1<-input_full
      example4gram<-data.frame(ngrams=character(),freq=integer(),prop=numeric(),col1=character(),
                               next_word=character())
      example3gram<-data.frame(ngrams=character(),freq=integer(),prop=numeric(),col1=character(),
                               next_word=character())
      example2gram<-y[grep(paste("^",input_1, "$", sep=""), y[,4]),]
      count1gram<-x[grep(paste("^",input_1," ", "$", sep=""), x[,1]),][1,2]
      
    }
    
    
    #input_full<-word(input_full,start=-3,end=-1)
    #input_2<-word(input_full,start=2,end=3)
    #input_1<-word(input_full,start=3,end=3)
    
    #example4gram<-w[grep(paste("^",input_full, "$", sep=""), w[,4]),]
    #example3gram<-z[grep(paste("^",input_2, "$", sep=""), z[,4]),]
    #example2gram<-y[grep(paste("^",input_1, "$", sep=""), y[,4]),]
    #count3gram<-z[grep(paste("^",input_full," ", "$", sep=""), z[,1]),][1,2]
    #count2gram<-y[grep(paste("^",input_2," ", "$", sep=""), y[,1]),][1,2]
    #count1gram<-x[grep(paste("^",input_1," ", "$", sep=""), x[,1]),][1,2]
    
    example4gram$scores = rep(0, nrow(example4gram))
    example3gram$scores = rep(0, nrow(example3gram))
    example2gram$scores = rep(0, nrow(example2gram))
    example4gram_new<-data.frame(next_word=character(),scores=numeric())
    example3gram_new<-data.frame(next_word=character(),scores=numeric(),
                                 stringsAsFactors=FALSE)
    example2gram_new<-data.frame(next_word=character(),scores=numeric(),
                                 stringsAsFactors=FALSE)
    
    
    if(nrow(example4gram)>0) {
      for (i in 1:nrow(example4gram)) {
        example4gram[i,6]<-example4gram[i,2]/count3gram
        #pred4word[i]<-example4gram[i,5]
        i<-i+1
      }
    }
    example4gram <- example4gram[order(-example4gram$scores),] 
    example4gram_new<-example4gram[,5:6]
    
    
    m<-1
    if(nrow(example3gram)>0) {
      for (j in 1:nrow(example3gram)) {
        if(example3gram[j,5] %in% unique(example4gram[,5]) == FALSE) {
          #example3gram_new[m,1]<-example3gram[j,4]
          example3gram_new[m,1]<-example3gram[j,5]
          example3gram_new[m,2]<-0.4*example3gram[j,2]/count2gram
          m<-m+1
        }
        #pred3word[j]<-example3gram[j,5]
        j<-j+1
      }
    }
    
    example3gram_new <- example3gram_new[order(-example3gram_new$scores),]
    
    
    n<-1
    if (nrow(example2gram)>0) {
      for (k in 1:nrow(example2gram)) {
        if(example2gram[k,5] %in% unique(c(example4gram[,5],example3gram[,5])) == FALSE) {
          #example2gram_new[n,1]<-example2gram[k,4]
          example2gram_new[n,1]<-example2gram[k,5]
          example2gram_new[n,2]<-0.4*0.4*example2gram[k,2]/count1gram
          n<-n+1
          }
        k<-k+1
      }
    }
    
    example2gram_new <- example2gram_new[order(-example2gram_new$scores),]
    
    prediction<-rbind(example4gram_new,example3gram_new,
                      example2gram_new)
    prediction<-prediction[order(-prediction$scores),]
    prediction<-prediction[1:5,]
    
    if (is.na(prediction[1,2])) {
      prediction<-x[1:5,-2]
      
    }
    prediction<-as.data.table(prediction)
    results<-prediction
  })
}
)



  
  
   


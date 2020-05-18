library(shinythemes)

#setwd("C:\\Users\\ASUS\\Desktop\\R files\\Predictive-model-for-English-text")
source("Frequencies.R")



library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage("Predictive text", theme = shinytheme("flatly"),
                 tabPanel("Text Generator",
                          h3("Generate English text using your initial input"),
                          textInput("txt2","Enter initial Text",width="80%",placeholder="Type something in English"),
                          numericInput("nb","Number of words to generate", value = 50),
                          actionButton("gen", "Generate"),
                          br(),
                          h3(textOutput("gt")),
                          h4(textOutput("par1"))
                          
                 ),
                 tabPanel("Auto-complete",
                          h3("Initial English text"),
                          textInput("txt1","Enter Text",width="80%",placeholder=". . ."),
                          br(),
                          h3("Predicted next word"),
                          h4(textOutput("p1")),
                          h4(textOutput("p2")),
                          h4(textOutput("p3"))
                 ),
                 
                 tabPanel("Most used words",
                          h3("Find the rank and the frequency of a word"),
                          textInput("txt3","Enter a word",placeholder = "e.g. table,phone..."),
                          br(),
                          h4(textOutput("ans1")),
                          br(),
                          br(),
                          h3("Find the word with a given rank"),
                          numericInput("rnk","Enter a rank",value=69),
                          br(),
                          h4(textOutput("ans2")),
                          br(),
                          br(),
                          h3("Compare two words"),
                          div(
                              style = "display: flex; flex-wrap: wrap;",
                              div(
                                  style = "flex: 1;",
                                  textInput("wrd1","Word 1", placeholder = "word 1")
                              ),
                              div(
                                  style = "flex: 1;",
                                  textInput("wrd2", "Word 2",placeholder = "word 2")
                              )
                              
                          ),
                          actionButton("com","Compare"),
                          h4(textOutput("com1")),
                          h4(textOutput("com2")),br(),br(),br(),br(),br()
                          
                          ),
                 
                 tabPanel("Most used nGrams",
                          img(src="Words_freq.png", height="70%", width="70%", align="center"),br(),br(),
                          img(src="2Gram_freq.jpg", height="70%", width="70%", align="center"),br(),br(),
                          img(src="3Gram_freq.jpg", height="70%", width="70%", align="center"),br(),br(),
                          img(src="4Gram_freq.jpg", height="70%", width="70%", align="center"),br(),br(),br(),
                          
                          
                          
                 )
                 )


# Define server logic required to draw a histogram
server <- function(input, output,session) {
    
    preds<-reactive({
        x<-input$txt1
        
        ans<-nextt(x)
        
        ans
    })
    
    
    output$p1<-renderText({
        if(!is.na(preds()[1,2]))
        paste(preds()[1,1]," - ",format(preds()[1,2]*100,digits=4),"%")
    })
    
    output$p2<-renderText({
        if(!is.na(preds()[2,2]))
            
        paste(preds()[2,1]," - ",format(preds()[2,2]*100,digits=4),"%")
    })
    output$p3<-renderText({
        if(!is.na(preds()[3,2]))
            
        paste(preds()[3,1]," - ",format(preds()[3,2]*100,digits=4),"%")
    })
    
     
        
    
    textvals <- reactiveVal("")
    active <- reactiveVal(FALSE)
    
    ans<-reactiveVal("")
    counter<-reactiveVal(0)
    ret<-reactiveVal("")
   
    output$par1 <- renderText({
        textvals()
    })
    
    output$gt<-renderText({
        aa<-counter()
        if(aa==0)
            "Generated Text"
        else
        {
            paste("Generated Text : ", counter(),"/",input$nb)
        }
            
    })
    
    
    
    observe({
        invalidateLater(200, session)
        isolate({               

            if (active()) {
                    
                if(ans()=="")
                    ans(paste0(ans(),input$txt2))
                ret(nextt(ans()))
                ans(paste( ans(), sample(ret()[,1],1,prob=ret()[,2]) ))
                textvals(ans() )
                counter(counter()+1)
                if (counter()==as.numeric(input$nb)) {
                    active(FALSE)
                }
               
            }
        }) 
        
    })
    
    observeEvent(input$gen, {
        ans("")
        textvals("")
        ret("")
        counter(0)
        active(TRUE)
    })
    
    
    
    
    
output$ans1<-renderText({
   rw<-freq1[freq1$words==input$txt3,][1,]
   r<-formatC(rw$rank,big.mark = ",")
   frq<-paste(format(rw$p,digits=4,scientific = FALSE),"%")
   
   th<-"th"
   if(r=="1") th<-"st"
   if(r=="2") th<-"nd"
   
   if(input$txt3=="")
       "¯\\_(•_•)_/¯"
   else if(!is.na(rw$rank))
    paste("The word ",input$txt3," is the ",r,th," most used word in English with a freqency of ",frq)
   else if(input$txt3!="")
    "This word doesn't exist lol ¯\\_(•_•)_/¯"
   
   })

output$ans2<-renderText({
    
    if(is.na(input$rnk) ) 
        "¯\\_(•_•)_/¯"
     else if(input$rnk<=0 )
        "tet9ou7ech? ¯\\_(•_•)_/¯"
    else if(input$rnk>476667 )
        "ya bro win mechi ¯\\_(•_•)_/¯"
    
    
        
    else{
    
    rw<-freq1[freq1$rank==input$rnk,][1,]
    
    th<-"th"
    if(input$rnk==1) th<-"st"
    if(input$rnk==2) th<-"nd"
    else
        paste("The ",input$rnk,th," most used word English word is : ",rw$words)
    
    }
    })

  
observeEvent(input$com, {
    
    rw1<-freq1[freq1$words==input$wrd1,][1,]
    rw2<-freq1[freq1$words==input$wrd2,][1,]
    
    if(!is.na(rw1) & !is.na(rw2)){
    output$com1<-renderText({
        paste("word 1 : ",rw1$words,"is ranked ",rw1$rank)
    })
    
    output$com2<-renderText({
        paste("word 2 : ",rw2$words,"is ranked ",rw2$rank)
    })
    }
    
    
    
})

    

}

# Run the application 
shinyApp(ui = ui, server = server)

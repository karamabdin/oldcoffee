
#old Coffee machine

library(shiny)

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      
  h4("What do you prefer with your coffee?"),
      numericInput("milk1", "Milk:", 0, min = 0, max = 5),
      
      numericInput("sugar1", "Sugar:", 0, min = 0, max = 5),
      
   #   numericInput("water1", "Water:", 1, min = 1, max = 1),
  #    numericInput("Repair_parts", "Repair_parts:", 20, min = 0, max = 30),
     
      plotOutput("plot")
      
    ),
    mainPanel(
      #  verbatimTextOutput("text1"),
      numericInput("coffee", "how many coffee you can do..", 10, min = 0, max = 10),
      br(),
      
      
      actionButton("press",h3("Press!and take your coffee..")),
      br(),
      br(),
      verbatimTextOutput("text2", placeholder = TRUE),
      br(),
      br(),
      br(),
      br(),
      br(),
    
      textInput("inText",  p(code("To do list ..."))),
      actionButton("refill","refill the caffee"), 
      br(),
      numericInput("milk", "Milk:", 20, min = 0, max = 20),
      actionButton("refill2","refill the Milk"),
      
      
      numericInput("sugar", "Sugar:", 20, min = 0, max = 20),
      actionButton("refill3","refill the sugar"),
      
      numericInput("water", "Water:", 20, min = 0, max = 20),
      actionButton("refill4","refill the water"),
      br(),
      br(),
      br(),
      br(),
      br(),
      textInput("inText2", p(code("Ripair list ..."))),
      
      numericInput("Repair_parts", "Repair_parts:", 20, min = 0, max = 30),
      actionButton("refill5","refill Repair_parts"),
      numericInput("macshine_age", "macshine_age:", 0, min = 0, max = 60)
      
    )
  )
)

server <- function(input, output, session) {
  

  observeEvent(input$press,{
  
    if(input$macshine_age < 30){
      if(input$Repair_parts>0){
        if(input$water>0){
          if(input$sugar>0){
            if(input$milk>0){
              if(input$coffee>0){
                
                updateNumericInput(session, "coffee", value = input$coffee - 1)
                output$coffeevalue <- renderText({ input$coffee })
                
                updateNumericInput(session, "milk", value = input$milk - input$milk1  )
                output$milkvalue <- renderText({input$milk})
                
                updateNumericInput(session, "sugar", value = input$sugar - input$sugar1)
                output$sugervalue <- renderText({input$suger})
                
                updateNumericInput(session, "water", value = input$water - 1)
                output$watervalue <- renderText({input$water})
                
                updateNumericInput(session, "Repair_parts", value = input$Repair_parts - 1)
                output$repair_partsvalue <- renderText({input$Repair_parts})
                
                updateNumericInput(session, "macshine_age", value = input$macshine_age + 1)
                output$macshine_agevalue <- renderText({input$macshine_age})
                
                output$text2 <- renderText("please take your Coffee...")
              #  input$text3 <- renderPrint(print("please take your Coffee...")) 
               # tt <- matrix(c(input$coffee,input$milk,input$sugar,input$water))
             #   updateTextInput(session, "inText", value = "please take your Coffee..." )
                qq <- matrix(nrow = 4,ncol = 2)
                qq[1,2] <- input$coffee - 1
                qq[2,2] <- input$milk - input$milk1
                qq[3,2] <- input$sugar - input$sugar1
                qq[4,2] <- input$water - 1
                qq[1,1] <- 20
                qq[2,1] <- 20
                qq[3,1] <- 20
                qq[4,1] <- 20
                rownames(qq)<- c("Coffee", "milk", "sugar", "water")
                foo1 <- function(){
                  
                  output$plot <-  renderPlot(
                    barplot(qq[,2], 
                            main= "main",
                            ylab="Residual ",
                            xlab="Materials",
                            ylim=c(0,20))
                  )
                }
                 foo1()
                #####################################
                # macshine <- data.frame(macshine_age,Repair_parts,water, sugar, milk, coffee)
                #output$text2 <- renderText(paste("coffee=",coffee,"macshine_age=",macshine_age, "teak your coffee... "))
                #    output$table2 <- DT::renderDataTable({
                #     DT::datatable(macshine)   })
              }else({ output$text2 <- renderText("please check the to do List ")
                updateTextInput(session, "inText", value = "The machine needs Coffee" )
                }
               
              )
            
            }else({ output$text2 <- renderText("please check the to do List ")
            updateTextInput(session, "inText", value = "The machine needs milk" )
            })
          }else({ output$text2 <- renderText("please check the to do List ")
          updateTextInput(session, "inText", value = "The machine needs sugar" )
          }
          )
        }else({ output$text2 <- renderText("please check the to do List ")
        updateTextInput(session, "inText", value = "The machine needs water" )
        }
        )
      }else ({ output$text2 <- renderText("please check the repair List ")
      updateTextInput(session, "inText2", value = "The machine needs repair" )
      })
    }else({ output$text2 <- renderText("please buy a new macshinee ")
    updateTextInput(session, "inText", value = "please buy a new macshinee :) " )
    updateTextInput(session, "inText2", value = "please buy a new macshinee " )
    }
    )
  })

  observeEvent(input$refill,{
    updateNumericInput(session, "coffee", value = 10)
    updateTextInput(session, "inText", value = "doing" )
    
    
    qq <- matrix(nrow = 4,ncol = 2)
    qq[1,2] <- 10
    qq[2,2] <- input$milk
    qq[3,2] <- input$sugar 
    qq[4,2] <- input$water 
    qq[1,1] <- 20
    qq[2,1] <- 20
    qq[3,1] <- 20
    qq[4,1] <- 20
    rownames(qq)<- c("Coffee", "milk", "sugar", "water")
    
    output$plot <-  renderPlot(
      barplot(qq[,2], 
              main= "main",
              ylab="Residual ",
              xlab="Materials",
              ylim=c(0,20))
    )
    
    
  })
  observeEvent(input$refill2,{
    updateNumericInput(session, "milk", value = 20)
    updateTextInput(session, "inText", value = "doing" )
    
    qq <- matrix(nrow = 4,ncol = 2)
    qq[1,2] <- input$coffee 
    qq[2,2] <- 20
    qq[3,2] <- input$sugar 
    qq[4,2] <- input$water 
    qq[1,1] <- 20
    qq[2,1] <- 20
    qq[3,1] <- 20
    qq[4,1] <- 20
    rownames(qq)<- c("Coffee", "milk", "sugar", "water")
 
      output$plot <-  renderPlot(
        barplot(qq[,2], 
                main= "main",
                ylab="Residual ",
                xlab="Materials",
                ylim=c(0,20))
      )
    
  })
  observeEvent(input$refill3,{
    updateNumericInput(session, "sugar", value = 20)
    updateTextInput(session, "inText", value = "doing" )
    
    
    qq <- matrix(nrow = 4,ncol = 2)
    qq[1,2] <- input$coffee 
    qq[2,2] <- input$milk
    qq[3,2] <- 20
    qq[4,2] <- input$water 
    qq[1,1] <- 20
    qq[2,1] <- 20
    qq[3,1] <- 20
    qq[4,1] <- 20
    rownames(qq)<- c("Coffee", "milk", "sugar", "water")
    
    output$plot <-  renderPlot(
      barplot(qq[,2], 
              main= "main",
              ylab="Residual ",
              xlab="Materials",
              ylim=c(0,20))
    )
    
  })
  observeEvent(input$refill4,{
    updateNumericInput(session, "water", value = 20)
    updateTextInput(session, "inText", value = "doing" )
    
    
    qq <- matrix(nrow = 4,ncol = 2)
    qq[1,2] <- input$coffee 
    qq[2,2] <- input$milk
    qq[3,2] <- input$sugar 
    qq[4,2] <- 20 
    qq[1,1] <- 20
    qq[2,1] <- 20
    qq[3,1] <- 20
    qq[4,1] <- 20
    rownames(qq)<- c("Coffee", "milk", "sugar", "water")
    
    output$plot <-  renderPlot(
      barplot(qq[,2], 
              main= "main",
              ylab="Residual ",
              xlab="Materials",
              ylim=c(0,20))
    )
    
  })
  observeEvent(input$refill5,{
    updateNumericInput(session, "Repair_parts", value = 30)
    updateTextInput(session, "inText2", value = "doing" )
  })

}

shinyApp(ui, server)
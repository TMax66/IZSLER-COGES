
  ## Create the dataset
 
  repmat <- read_excel("repsim.xlsx")
  replab <- read_excel("repsim.xlsx", 
                       sheet = "replab")
  
  
  df <-repmat %>% 
    left_join(replab, by = "Reparto") %>% 
    mutate(hperc = rep(0, dim(.)[1])) %>% 
    filter(Reparto == "SO") %>% 
    pivot_wider( names_from = "Laboratorio", values_from = hperc, values_fill = 0) %>% 
    mutate(tot = rep(0, dim(.)[1]))
  
  
  
  server <- shinyServer(function(input, output, session) {
    
    # Initiate your table
    previous <- reactive({df})
    
    MyChanges <- reactive({
      if(is.null(input$hotable1)){return(previous())}
      else if(!identical(previous(),input$hotable1)){
        # hot.to.df function will convert your updated table into the dataframe
        mytable <- as.data.frame(hot_to_r(input$hotable1))
        # here the second column is a function of the first and it will be multipled by 100 given the values in the first column
        mytable <- mytable[1:dim(df)[1],]
        
        # Add some test cases
        # mytable[,1][is.na(mytable[,1])] <- 1
        # mytable[,2][is.na(mytable[,2])] <- 1
        # mytable[,3] <- mytable[,1]*mytable[,2]
        mytable
      }
    })
    output$hotable1 <- renderRHandsontable({rhandsontable(MyChanges())})
  })
  
 
 
    


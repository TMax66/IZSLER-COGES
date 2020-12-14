server<-function(input, output) {
  
  
    

  df <- reactive(
    
    data.frame(
      RT = input$rt,
      FTE = input$fte,
      rid = input$pc/100,
      vRT = input$Vrt/100,
      vFTE = input$Vfte/100
    ) %>% 
      mutate(FTEp = FTE-(FTE*rid),
        
             RFTEt = RT/FTE, 
             
             RFTEprog = RT/(FTE-FTEp), 
             
             VARrfte = 100*((RFTEprog-RFTEt)/RFTEt), 
             
             
             VarRT = RT+(RT*vRT),
             
             VarFT = FTEp+(FTEp*vFTE),
             
             RFTEr = VarRT/VarFT
             
             ) %>% 
      select(RT, FTE, FTEp, RFTEt, RFTEprog, VARrfte)
    
  
    
  )

output$tb <- renderTable(df())
  
  
# t <- reactive(
#     data.frame(
#     "rfter" = input$rt/input$fted, 
#     "rftep" = input$rt/(input$fted-(input$fted*rid()))
#   ) %>% 
#     mutate(variaz = 100*((rftep-rfter)/rfter))
#   )
#     
#   output$tb <- renderTable(t())
#    
  

  
 
  
}






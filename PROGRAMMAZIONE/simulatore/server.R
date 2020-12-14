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
             
             RFTEprog = RT/(FTEp), 
             
             VARrfte = 100*((RFTEprog-RFTEt)/RFTEt), 
             
             VarRT = RT+(RT*vRT),
             
             VarFT = FTEp+(FTEp*vFTE),
             
             RFTEr = VarRT/VarFT, 
             
             VARRFTEr = 100*((RFTEr-RFTEt)/RFTEt), 
             VARRFTEr2 = 100*((RFTEr-RFTEprog)/RFTEprog),

             )  
  )
output$tb <- renderTable(df() %>% 
                           
                           select(RT, FTE, FTEp, RFTEt, RFTEprog, VARrfte)
                         
                         )
output$tb2 <- renderTable(df() %>% 
                           
                           select(VarRT, VarFT, RFTEr, VARRFTEr, VARRFTEr2)
                         
)

  
 
  
}






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
             
             VARRFTEr = ifelse(rid == 0, "", 100*((RFTEr-RFTEt)/RFTEt)), 
             
             TN = 100, 
             
             RisN = ifelse(rid==0, "",
               
               (VARRFTEr*100)/VARrfte)

             )  
  )

output$tb <- renderTable(df() %>% 
                           
                           select("Ricavo Totale Previsto" = RT, "FTE disponibili" = FTE, 
                                  
                                "FTE programmato" = FTEp, "Ricavo per FTE teorico" =RFTEt, 
                                
                                "Ricavo per FTE programmato" =RFTEprog, Target=VARrfte, "Target Normalizzato" = TN)
                         
                         )
# output$tb2 <- renderTable(df() %>% 
#                            
#                            select("Ricavo Totale" = VarRT, "FTE" =VarFT, "Ricavo per FTE" = RFTEr, 
#                                   Risultato=VARRFTEr, "Risultato Normalizzato" = RisN)
#                          
# )

  
 
  
}






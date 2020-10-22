ui<-navbarPage("IZSLER-Carichi di lavoro e attività 2019",
    theme = shinytheme("cerulean"),
           
    tabPanel("Dipartimento",
             sidebarLayout(
               sidebarPanel(
                 # selectInput("indicatore", "Indicatore", 
                 #             choices = c("FTE previsto" = "FTEpr", "FTE reale" = "FTEr", "% di ore lavorate" = "Perchwd",
                 #                          "Tempo medio per esame (minuti)" = "tempo medio esame", 
                 #                         "ore previste da contratto" = "hprev",
                 #                         "ore effettivamente erogate" = "hworked", 
                 #                         "N.esami (x1000)" = "esami", "Ricavi (x1000)" = "ricavi"))
                ), 
               mainPanel(
                 fluidRow(
                   wellPanel(
                     tableOutput("tdip")
                   )
                 ),
                 fluidRow(
                   wellPanel(
                       selectInput("indicatore", "Indicatore", 
                                   choices = c("FTE previsto" = "FTEpr", "FTE reale" = "FTEr", "% di ore lavorate" = "Perchwd",
                                               "Tempo medio per esame (minuti)" = "tempo medio esame", 
                                               "ore previste da contratto" = "hprev",
                                               "ore effettivamente erogate" = "hworked", 
                                               "N.esami (x1000)" = "esami", "Ricavi (x1000)" = "ricavi")),
                       
                   plotOutput("diplot")
                   )
                 ), 
                 fluidRow(
                   wellPanel(
                   plotOutput("dipheat")
                   )
                 )
                 
               )
             )),

           tabPanel("Dipartimento/Reparti",
                    sidebarLayout(
                    
                      sidebarPanel(
                        selectInput("indicatore2", "Indicatore", 
                                    choices = c("FTE previsto" = "FTEpr", "FTE reale" = "FTEr", "% di ore lavorate" = "Perchwd",
                                                "Tempo medio per esame (minuti)" = "tempo medio esame", 
                                                "ore previste da contratto" = "hprev",
                                                "ore effettivamente erogate" = "hworked", 
                                                "N.esami (x1000)" = "esami", "Ricavi (x1000)" = "ricavi")), 
                        hr(),
                        selectInput("dip", "Dipartimento", choices = unique(factor(dati$Dipartimento)))
                      ), 

                    mainPanel(
                      
                      fluidRow(
                        wellPanel(
                          tableOutput("tdiprep")
                        )
                      ),
                      fluidRow(
                        wellPanel(
                          plotOutput("drplot")
                        )
                      ),
                      fluidRow(
                        wellPanel(
                          plotOutput("drheat")
                        )
                      )
                      
                    )
                      )
                    ),
         tabPanel("Dipartimento/Laboratori", 

                  sidebarLayout(
                    
                    sidebarPanel(
                      selectInput("indicatore3", "Indicatore", 
                                  choices = c("FTE previsto" = "FTEpr", "FTE reale" = "FTEr", "% di ore lavorate" = "Perchwd",
                                              "Tempo medio per esame (minuti)" = "tempo medio esame", 
                                              "ore previste da contratto" = "hprev",
                                              "ore effettivamente erogate" = "hworked", 
                                              "N.esami (x1000)" = "esami", "Ricavi (x1000)" = "ricavi")), 
                      hr(),
                      selectInput("dip2", "Dipartimento", choices = unique(factor(dati$Dipartimento)))
                    ), 
                    mainPanel(
                      fluidRow(
                        wellPanel(
                          tableOutput("tdiplab")
                        )
                      ),
                      
                      fluidRow(
                        wellPanel(
                          plotOutput("dlplot")
                        )
                      ),
                      fluidRow(
                        wellPanel(
                          plotOutput("dlheat")
                        )
                      )
                      
                    )
                    
                      )
               ), 
        tabPanel("Dati", 
             fluidRow(
               DTOutput("dataset")
             )
             ),
        tabPanel("Pivot table",
             fluidRow(
               rpivotTableOutput("test")
             )
    )
    
    
)

    
    
           # 
           # tabPanel("ATTIVITA'", 
           #          
           #          fluidRow(
           #            plotOutput("time2")
           #          )
           # 
           #          ), 
           # tabPanel("CLIENTI", 
           #          sidebarLayout(
           #            sidebarPanel(
           #              selectInput("rep", "Seleziona Reparto", 
           #                  c( "Tutti", unique(as.character(dati$reparto))) ),
           #              sliderInput("mes", "top clienti", 
           #                          min=1, max= 50, value=10)),
           #         
           #          mainPanel(
           #            DT::dataTableOutput("clienti"),
           #            hr(),
           #            br()
           #            #plotOutput("timecl")
           #            )
           #          
           #          )), 
           # tabPanel("TABELLA PIVOT", 
           #            fluidPage(
           #              fluidRow(
           #                downloadButton("download_pivot", label = "Excel")), 
           #                fluidRow(
           #                column(6,div(style="height:10px"),rpivotTableOutput("pivot")
           #                      ))
           #                
           #              ))
           # ,
           # 
           #        
           #          
           # 
           # tabPanel("DATI", 
           #         
           #            fluidRow(
           #              DT::dataTableOutput("tab")
           #            )
           #          )
           # )

    ui <- dashboardPage(
    dashboardHeader(title = "IZSLER- Attività Personale"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Attività", tabName = "att", icon = icon("dashboard")),
            menuItem("Ricavi", tabName = "ric", icon = icon("th")),
            menuItem("Personale", tabName="pers", icon = icon("edit"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName="att", 
                fluidRow( 
                    
                    box(width=12, solidHeader = TRUE,
                        fluidRow(
                            column(4,
                                   valueBoxOutput("attv", width = NULL)),
                             column(4,
                                   valueBoxOutput("attuff", width = NULL)),
                             column(4,
                                    valueBoxOutput("attnuff", width = NULL))#,
                            # column(3,
                            #        valueBoxOutput("heif", width = NULL))
                        ))
                    ),
                fluidRow(
                    
                    box(width=12,solidHeader = TRUE,height=800,
                        fluidRow(
                            column(3,
                                   selectInput("attività", "Seleziona tipologia attività",
                                    c("Totale esami"="all", 
                                      "Esami da attività ufficiale"="Ufficiale", 
                                      "Esami da attività non ufficiale"= "Non Ufficiale")
                                               )
                                   ),
                            br(),
                            br(),
                            column(9,
                                   plotOutput("nesami", height = 650))
                        ))
                    
                )

            ),
            tabItem(
                tabName = "ric",
                fluidRow(
                    box(width=12, solidHeader = TRUE,
                        fluidRow(
                            column(3,
                                   valueBoxOutput("rict", width = NULL)),
                            column(3,
                                   valueBoxOutput("ricuff", width = NULL)),
                            column(2,
                                   valueBoxOutput("ricnuff", width = NULL)),
                            column(2,
                                   valueBoxOutput("vprod",width = NULL)),
                            column(2,
                                   valueBoxOutput("attint",width = NULL))

                        )
                    
                    
                    
                    
                )
                ),
                fluidRow(
                    box(width = 12, solidHeader = TRUE,height=800,
                        fluidRow(
                            column(3,
                                   selectInput("attività", "Seleziona tipologia attività",
                                               c("Totale Ricavi"="all", 
                                                 "Ricavi da attività ufficiale"="Ufficiale", 
                                                 "Ricavi  da attività non ufficiale"= "Non Ufficiale",
                                                 "Ricavi da Vendita Prodotti"="Vendita Prodotti",
                                                 "Ricavi da Attività interna"="Attività Interna")
                                   )
                            ),
                            br(),
                            br(),
                            column(9,
                                   plotOutput("Ricavi", height = 650))
                            
                        )
                        )
                )

                ),
                
                tabItem(
                    tabName = "Personale",
                    fluidRow(
                    ))
            )
        )
        
        
        
    )
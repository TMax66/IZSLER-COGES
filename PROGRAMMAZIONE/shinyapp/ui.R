ui <- dashboardPage(
  dashboardHeader(title = "IZSLER-Indicatori di Performances", titleWidth = 400),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("IZSLER", tabName = "izsler", icon = icon("globe")),
      menuItem("Dipartimento Sicurezza Alimentare", tabName = "dsalim", icon = icon("cog")),
      menuItem("Dipartimento Tutela e Salute Animale", tabName = "dsa", icon = icon("cog")),
      menuItem("Area Territoriale Lombardia", tabName = "lomb", icon = icon("cog")),
      menuItem("Area Territoriale Emilia Romagna", tabName = "emil", icon = icon("cog"))
      # menuItem("Note", tabName = "help", icon = icon("cog"))
    )
  ),
  dashboardBody(
  tabItems(
#####IZSLER#####
    tabItem( tabName = "izsler", 
    fluidRow(
    valueBoxOutput("esami"),
    valueBoxOutput("ra"),
    valueBoxOutput("vp"),
    valueBoxOutput("ai"),
    valueBoxOutput("rt"),
    valueBoxOutput("rfte"),
    
    div(id='clickdiv0',
        valueBoxOutput("IF")),
    bsModal("P", "Pubblicazioni IF", "clickdiv0",tableOutput("articoli"), size = "large"),
    
    div(id='clickdiv1',
        valueBoxOutput("Int")),
    bsModal("CI", "Partecipazione a convegni internazionali", "clickdiv1", tableOutput("convegni"), size = "large"),

    div(id='clickdiv2',
      valueBoxOutput("Naz")),
    bsModal("CN", "Partecipazione a convegni nazionali", "clickdiv2", tableOutput("nazionali"), size = "large"),
  
    ), 
    
    br(),
    
    fluidRow( 
    tableOutput("t")),
    
    br(),
    fluidRow(
    column(1, 
    radioButtons("ind", "", 
                c("IP" = "IP","Dipartimento" = "Dipartimento" ))),
    column(11, 
             div(id = 'clickdiv00',
                 plotOutput("tbd")),
                 bsModal("TW", "",  'clickdiv00', tableOutput("tbw")))
     
    )
  ), 
####Dipartimento Sicurezza Alimentare####
  tabItem(tabName = "dsalim", 
      fluidRow(
        valueBoxOutput("esami2"),
        valueBoxOutput("ra2"),
        valueBoxOutput("vp2"),
        valueBoxOutput("ai2"),
        valueBoxOutput("rt2"),
        valueBoxOutput("rfte2"),
      
      div(id='clickdiv3',
          valueBoxOutput("IF2")),
      bsModal("P2", "Pubblicazioni IF", "clickdiv3",tableOutput("articoli2"), size = "large"),
      
      div(id='clickdiv4',
          valueBoxOutput("Int2")),
      bsModal("CI2", "Partecipazione a convegni internazionali", "clickdiv4", tableOutput("convegni2"), size = "large"),
      
      div(id='clickdiv5',
          valueBoxOutput("Naz2")),
      bsModal("CN2", "Partecipazione a convegni nazionali", "clickdiv5", tableOutput("nazionali2"), size = "large"),
      
      
      
      ),
      
      br(),
      
      fluidRow( 
        tableOutput("t2")),
      
      br(),
      fluidRow(
        column(1, 
               radioButtons("ind2", "", 
                            c( "IP" = "IP","Reparto" = "Reparto"))),
        
        column(11, 
               div(id = 'clickdiv01',
                   plotOutput("tbd2")),
               bsModal("TW2", "",  'clickdiv01', tableOutput("tbw2")))
      )
), 

####Dipartimento Tutela Salute Animale#####
  tabItem(tabName = "dsa", 
          fluidRow(
            valueBoxOutput("esami3"),
            valueBoxOutput("ra3"),
            valueBoxOutput("vp3"),
            valueBoxOutput("ai3"),
            valueBoxOutput("rt3"),
            valueBoxOutput("rfte3"),
            
            div(id='clickdiv6',
                valueBoxOutput("IF3")),
            bsModal("P3", "Pubblicazioni IF", "clickdiv6",tableOutput("articoli3"), size = "large"),
            
            div(id='clickdiv7',
                valueBoxOutput("Int3")),
            bsModal("CI3", "Partecipazione a convegni internazionali", "clickdiv7", tableOutput("convegni3"), size = "large"),
            
            div(id='clickdiv8',
                valueBoxOutput("Naz3")),
            bsModal("CN3", "Partecipazione a convegni nazionali", "clickdiv8", tableOutput("nazionali3"), size = "large"),
          ),
          
          br(),
          
          fluidRow( 
            tableOutput("t3")),
          
          br(),
          fluidRow(
            column(1, 
                   radioButtons("ind3", "", 
                                c( "IP" = "IP","Reparto" = "Reparto"))),
            
            column(11, 
                   div(id = 'clickdiv02',
                       plotOutput("tbd3")),
                   bsModal("TW3", "",  'clickdiv02', tableOutput("tbw3")))
            
            
          )
  ), 
          
###Area Territoriale Lombardia####
  tabItem(tabName = "lomb", 
                  fluidRow(
                    valueBoxOutput("esami4"),
                    valueBoxOutput("ra4"),
                    valueBoxOutput("vp4"),
                    valueBoxOutput("ai4"),
                    valueBoxOutput("rt4"),
                    valueBoxOutput("rfte4"),
                    
                    div(id='clickdiv9',
                        valueBoxOutput("IF4")),
                    bsModal("P4", "Pubblicazioni IF", "clickdiv9",tableOutput("articoli4"), size = "large"),
                    
                    div(id='clickdiv10',
                        valueBoxOutput("Int4")),
                    bsModal("CI4", "Partecipazione a convegni internazionali", "clickdiv10", tableOutput("convegni4"), size = "large"),
                    
                    div(id='clickdiv11',
                        valueBoxOutput("Naz4")),
                    bsModal("CN4", "Partecipazione a convegni nazionali", "clickdiv11", tableOutput("nazionali4"), size = "large"),

                  ),
          br(),
          
          fluidRow( 
            tableOutput("t4")), 
          
          br(),
          fluidRow(
            column(1, 
                   radioButtons("ind4", "", 
                                c( "IP" = "IP","Reparto" = "Reparto"))),
            
            column(11, 
                   div(id = 'clickdiv03',
                       plotOutput("tbd4")),
                   bsModal("TW4", "",  'clickdiv03', tableOutput("tbw4")))
            
            
            
          )

          ), 
###Area Territoriale Emilia Romagna#####
  tabItem(tabName = "emil", 
          fluidRow(
            valueBoxOutput("esami5"),
            valueBoxOutput("ra5"),
            valueBoxOutput("vp5"),
            valueBoxOutput("ai5"),
            valueBoxOutput("rt5"),
            valueBoxOutput("rfte5"),
            
            div(id='clickdiv12',
                valueBoxOutput("IF5")),
            bsModal("P5", "Pubblicazioni IF", "clickdiv12",tableOutput("articoli5"), size = "large"),
            
            div(id='clickdiv13',
                valueBoxOutput("Int5")),
            bsModal("CI5", "Partecipazione a convegni internazionali", "clickdiv13", tableOutput("convegni5"), size = "large"),
            
            div(id='clickdiv14',
                valueBoxOutput("Naz5")),
            bsModal("CN5", "Partecipazione a convegni nazionali", "clickdiv14", tableOutput("nazionali5"), size = "large"),
            
          ),
          br(),
          
          fluidRow( 
            tableOutput("t5")), 
          
          br(),
          fluidRow(
            column(1, 
                   radioButtons("ind5", "", 
                                c( "IP" = "IP","Reparto" = "Reparto"))),
            column(11, 
                   div(id = 'clickdiv04',
                       plotOutput("tbd5")), 
                  bsModal("TW5", "",'clickdiv04',tableOutput("tbw5") )
          )
          )
  
) 
# , 

###Note####_____________________________________________________________________
# tabItem(tabName = "help", 
#         includeHTML("intro.html")
#         )


)))


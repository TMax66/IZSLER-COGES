ui <- dashboardPage(
  dashboardHeader(title = "IZSLER-KPI", titleWidth = 300),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("IZSLER", tabName = "izsler", icon = icon("globe")),
      menuItem("Dipartimento Sicurezza Alimentare", tabName = "dsalim", icon = icon("cog")),
      menuItem("Dipartimento Tutela e Salute Animale", tabName = "dsa", icon = icon("cog")),
      menuItem("Area Territoriale Lombardia", tabName = "lomb", icon = icon("cog")),
      menuItem("Area Territoriale Emilia Romagna", tabName = "emil", icon = icon("cog"))
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
                c("Dipartimento" = "Dipartimento", "KPI" = "KPI"))),
      column(11, 
      plotOutput("tbd"))
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
                            c("Reparto" = "Reparto", "KPI" = "KPI"))),
        column(11, 
               plotOutput("tbd2"))
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
                                c("Reparto" = "Reparto", "KPI" = "KPI"))),
            column(11, 
                   plotOutput("tbd3"))
          )
  ), 
          
          
          
          
          
          
          
          
###Area Territoriale Lombardia####
  tabItem(tabName = "lomb"), 
###Area Territoriale Emilia Romagna#####
  tabItem(tabName = "emil")
  
)))


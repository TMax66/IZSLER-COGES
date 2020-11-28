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
    
  valueBoxOutput("Int"), 
    
    # div(id='clickdiv1',
    #     valueBoxOutput("Int")),
    # bsModal("cint", "Partecipazione a convegni internazionali", "clickdiv1", tableOutput("articoli"), size = "large"),

    valueBoxOutput("Naz")
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
        valueBoxOutput("rfte2")),
      
      
      br(),
      
      fluidRow( 
        tableOutput("t2")),

      
      
      
      
      
), 





  tabItem(tabName = "dsa"), 
  tabItem(tabName = "lomb"), 
  tabItem(tabName = "emil")
  
)))


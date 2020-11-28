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
    
    #valueBoxOutput("IF"), 
    
    div(id='clickdiv0',
        valueBoxOutput("IF")),
    bsModal("P", "Paper", "clickdiv0",tableOutput("articoli"), size = "large"),
    
    valueBoxOutput("Int"), 
    valueBoxOutput("Naz")
    ), 
    br(),
    fluidRow( 
    tableOutput("t")
    ),
    br(),
    fluidRow(
      column(1, 
    radioButtons("ind", "", 
                c("Dipartimento" = "Dipartimento", "KPI" = "KPI"))),
      column(11, 
      plotOutput("tbd"))
    )
  ), 
  
  tabItem(tabName = "dsalim"), 
  tabItem(tabName = "dsa"), 
  tabItem(tabName = "lomb"), 
  tabItem(tabName = "emil")
  
  
  
)))


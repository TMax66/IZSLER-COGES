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
    valueBoxOutput("IF"), 
    valueBoxOutput("Int"), 
    valueBoxOutput("Naz")
    ), 
    br(),
    fluidRow( 
    tableOutput("t")
    ),
    br(),
    fluidRow(
      plotOutput("tbd")
    )
  ), 
  
  tabItem(tabName = "dsalim"), 
  tabItem(tabName = "dsa"), 
  tabItem(tabName = "lomb"), 
  tabItem(tabName = "emil")
  
  
  
)))


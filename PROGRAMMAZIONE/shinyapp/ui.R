ui <- dashboardPage(
  dashboardHeader(titleWidth = 300),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("IZSLER", tabName = "izsler", icon = icon("globe")),
      menuItem("Dipartimento Sicurezza Alimentare", tabName = "dsa", icon = icon("cog")),
      menuItem("Dipartimento Tutela e Salute Animale", tabName = "dsa", icon = icon("cog")),
      menuItem("Area Territoriale Lombardia", tabName = "dsa", icon = icon("cog")),
      menuItem("Area Territoriale Emilia Romagna", tabName = "dsa", icon = icon("cog"))
    )
  ),
  dashboardBody(
    fluidRow(
    valueBoxOutput("esami"),
    valueBoxOutput("ra"),
    valueBoxOutput("vp"),
    valueBoxOutput("ai"),
    valueBoxOutput("rt"),
    valueBoxOutput("rfte"),
    valueBoxOutput("IF")
    ), 
    hr(),
    br(), 
    
    fluidRow( 
    tableOutput("t")
    )
  )
)


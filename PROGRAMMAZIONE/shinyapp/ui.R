ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Dipartimento Sicurezza Alimentare", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    fluidRow(
    valueBoxOutput("esami"),
    valueBoxOutput("ra"),
    valueBoxOutput("vp"),
    valueBoxOutput("ai"),
    valueBoxOutput("rt"),
    valueBoxOutput("rfte")
    ), 
    hr(),
    br(), 
    tableOutput("t")
  )
)


data_nettoye <- read.csv("donnees_nettoyees.csv", sep=",", dec=".")


library(shiny)
library(shinydashboard)
library(tibble)
library(scales)
library(DT)

install.packages("rsconnect")

ui <- dashboardPage(
  dashboardHeader(title = "Analyse Nutri-Score"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Présentation des variables", 
               tabName = "presentation"),
      
      menuItem("Analyse par groupe/catégories", 
               tabName = "comparaison"),
      
      menuItem("ACP", 
               tabName = "acp")
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # Onglet PRESENTATION
      tabItem(tabName = "presentation",
              fluidRow(
                valueBox(
                  value = "Analyse des données Open Food Facts",
                  subtitle = "Les variables",
                  width = 8
                ),
                box(title = "Distribution des groupes alimentaires", width = 6,
                    plotOutput("dist_group")),
                box(title = "Distribution des nutriments", width = 6,
                    plotOutput("dist_nutriment"))
              ),
              fluidRow(
                box(
                  selectInput("choix_dist",
                              "Variable à visualiser :",
                              list("Lipides" = 1,
                                   "Acides gras saturés" = 2,
                                   "Sucres" = 3,
                                   "Sel" = 4,
                                   "Protéines" = 5)
                  ),
                  width = 6
                ),
                box(
                  sliderInput("nb_classe",
                              label = "Nombre de classes pour les histogrammes :",
                              min = 5,
                              max = 50,
                              value = 30),
                  width = 6
                )
              )
              
      ),
      
      # Onglet ANALYSE PAR GROUPE ET CATEGORIES
      tabItem(tabName = "comparaison",
              fluidRow(
                box(title = "Boîte à moustache par groupe alimentaire", width = 6,
                    plotOutput("boxplot_group")),
                box(title = "Boîte à moustache par Nutri-score", width = 6,
                    plotOutput("boxplot_nutriscore"))
              ),
              fluidRow(
                box(
                  selectInput("choix_var",
                              "Variable à visualiser :",
                              list("Lipides" = 1,
                                   "Acides gras saturés" = 2,
                                   "Sucres" = 3,
                                   "Sel" = 4,
                                   "Protéines" = 5)
                  ),
                  width = 12
                )
              )
      ),
      
      # Onglet ACP
      tabItem(tabName = "acp",
              fluidRow(
                box(title = "ACP - Variables", width = 6,
                    plotOutput("pca_var")),
                box(title = "ACP - Produits", width = 6,
                    plotOutput("pca_ind"))
              )
      )
    )
  )
)


server <- function(input, output) {
  
  
  dist_var <- reactive({
    if (input$choix_dist == 1) return("fat_100g")
    if (input$choix_dist == 2) return("saturated.fat_100g")
    if (input$choix_dist == 3) return("sugars_100g")
    if (input$choix_dist == 4) return("salt_100g")
    if (input$choix_dist == 5) return("proteins_100g")
  })
  
  
  var_choisie <- reactive({
    if (input$choix_var == 1) return("fat_100g")
    if (input$choix_var == 2) return("saturated.fat_100g")
    if (input$choix_var == 3) return("sugars_100g")
    if (input$choix_var == 4) return("salt_100g")
    if (input$choix_var == 5) return("proteins_100g")
  })
  
  output$dist_group <- renderPlot({
    ggplot(data_nettoye, aes(x = pnns_groups_1)) +
      geom_bar(fill = "steelblue") +
      coord_flip() +
      labs(x = "Groupes alimentaires", y = "Nombre de produits")
  })
  
  output$dist_nutriment <- renderPlot({
    ggplot(data_nettoye, aes_string(x = dist_var())) +
      geom_histogram(bins = input$nb_classe, fill = "orange", color = "black") +
      labs(x = dist_var(), y = "Fréquence")
  })
  
  output$boxplot_group <- renderPlot({
    ggplot(data_nettoye, aes_string(x = "pnns_groups_1", y = var_choisie())) +
      geom_boxplot(fill = "skyblue") +
      coord_flip() +
      labs(x = "Groupe alimentaire", y = var_choisie())
  })
  
  output$boxplot_nutriscore <- renderPlot({
    ggplot(data_nettoye, aes_string(x = "nutriscore_cat", y = var_choisie())) +
      geom_boxplot(fill = "tomato") +
      labs(x = "Nutri-score", y = var_choisie())
  })
  
  output$pca_var <- renderPlot({
    quanti_data <- data_nettoye %>% select(fat_100g:proteins_100g)
    acp <- PCA(quanti_data, scale.unit = TRUE, ncp = 5, graph = FALSE)
    fviz_pca_var(acp, col.var = "contrib",
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
  })
  
  output$pca_ind <- renderPlot({
    quanti_data <- data_nettoye %>% select(fat_100g:proteins_100g)
    acp <- PCA(quanti_data, scale.unit = TRUE, ncp = 5, graph = FALSE)
    fviz_pca_ind(acp, 
                 habillage = as.factor(data_nettoye$pnns_groups_1),
                 palette = "jco", 
                 addEllipses = TRUE, 
                 label = "none")
  })
}

shinyApp(ui = ui, server = server)

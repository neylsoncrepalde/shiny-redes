#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(igraph)
library(sand)


# Define UI for application that draws a histogram
ui <- navbarPage("Análise de Redes Sociais - GIARS", theme = "slate_bootstrap.css",
                 
                 tabPanel("Apresentação", 
                          sidebarLayout(
                            sidebarPanel(style = "background-color: #BA1723;",
                                         
                              img(src="http://www.giars.ufmg.br/images/logo.png", height=107, width=225)
                            ),
                            mainPanel(
                              h1("Aplicação de Análise de Redes Sociais - GIARS"),
                              
                              p("Esta é uma aplicação de testes de Análise de Redes Sociais desenvolvido 
                                por Neylson Crepalde e pelo GIARS (Grupo Interdisciplinar de Pesquisa em Análise
                                de Redes Sociais). Seu objetivo é facilitar o aprendizado e fomentar o uso 
                                de ferramentas de ARS no Brasil."),
                              br(),
                              p("Esta aplicação está em desenvolvimento!")
                              )
                          )),             
                 
                 # Application title
                 #titlePanel("Análise de Redes Sociais"),
                 
                 tabPanel("Rede de Lazega", 
                          # Sidebar with a slider input for number of bins 
                          sidebarLayout(
                            sidebarPanel(style = "background-color: #BA1723;",
                                         
                              img(src="http://www.giars.ufmg.br/images/logo.png", height=107, width=225),
                              helpText("Trabalhando com a rede de advogados de E. Lazega."),
                              textInput("text", label = "Defina o título da rede", value = "Advogados - Componente Principal"),
                              selectInput("algoritmo", label = "Defina o algoritmo de visualização",
                                          choices = c("Fruchterman-Reingold","Kamada-Kawai",
                                                      "Escalonamento Multidimensional","Circular"),
                                          selected = "Fruchterman-Reingold"),
                              
                              selectInput("metrica", label = "Métrica de Rede",
                                          choices = c("Nenhum","Centralidade de Grau",
                                                      "Centralidade de Intermediação",
                                                      "Centralidade de Proximidade","Constraint"),
                                          selected = "Nenhum"),
                              
                              
                              submitButton(text = "Atualizar")
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                              plotOutput("net")
                            )
                          )
                 ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  dataInput = reactive({
    lazega = upgrade_graph(lazega)
    lazega = delete_vertices(lazega, c(8,23))
  })
  
  output$net <- renderPlot({
    
    algo = switch(input$algoritmo,
                  "Fruchterman-Reingold" = layout_with_fr,
                  "Kamada-Kawai" = layout_with_kk,
                  "Escalonamento Multidimensional" = layout_with_mds,
                  "Circular" = layout_in_circle)
    
    escore = switch(input$metrica,
                    "Nenhum" = 15,
                    "Centralidade de Grau" = degree(dataInput()),
                    "Centralidade de Intermediação" = betweenness(dataInput())/4,
                    "Centralidade de Proximidade" = closeness(dataInput())*700,
                    "Constraint" = constraint(dataInput())*20
    )
    
    
    plot.igraph(dataInput(), layout=algo, vertex.size=escore, 
                #vertex.color='SkyBlue2',
                main = input$text)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


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
                 navbarMenu("Redes - Exemplos",
                   
                 tabPanel("Rede de Advogados", 
                          # Sidebar with a slider input for number of bins 
                          sidebarLayout(
                            sidebarPanel(style = "background-color: #BA1723;",
                                         tags$head(tags$style("#net{height:80vh !important;}")),
                                         
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
                 ),
                 
                 tabPanel("Rede de Blogs", 
                          # Sidebar with a slider input for number of bins 
                          sidebarLayout(
                            sidebarPanel(style = "background-color: #BA1723;",
                                         tags$head(tags$style("#net2{height:80vh !important;}")),
                                         
                                         img(src="http://www.giars.ufmg.br/images/logo.png", height=107, width=225),
                                         helpText("Trabalhando com a subrede de blogs de política Franceses"),
                                         textInput("text2", label = "Defina o título da rede", value = "Blogs de Política Franceses"),
                                         selectInput("algoritmo2", label = "Defina o algoritmo de visualização",
                                                     choices = c("Fruchterman-Reingold","Kamada-Kawai",
                                                                 "Escalonamento Multidimensional","Circular"),
                                                     selected = "Fruchterman-Reingold"),
                                         
                                         selectInput("metrica2", label = "Métrica de Rede",
                                                     choices = c("Nenhum","Centralidade de Grau",
                                                                 "Centralidade de Intermediação",
                                                                 "Centralidade de Proximidade","Constraint"),
                                                     selected = "Nenhum"),
                                         
                                         
                                         submitButton(text = "Atualizar")
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                              plotOutput("net2")
                            )
                          )
                 )
          )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Rede de Advogados Lazega
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
    
    
    plot.igraph(dataInput(), layout=algo, vertex.size=escore, vertex.label.cex=1,
                #vertex.color='SkyBlue2',
                main = input$text)
  })
  
  # Rede de blogs
  dataInput2 = reactive({
    blogs = upgrade_graph(fblog)
  })
  
  output$net2 <- renderPlot({
    
    algo2 = switch(input$algoritmo2,
                  "Fruchterman-Reingold" = layout_with_fr,
                  "Kamada-Kawai" = layout_with_kk,
                  "Escalonamento Multidimensional" = layout_with_mds,
                  "Circular" = layout_in_circle)
    
    escore2 = switch(input$metrica2,
                    "Nenhum" = 5,
                    "Centralidade de Grau" = degree(dataInput2())/2,
                    "Centralidade de Intermediação" = betweenness(dataInput2())/100,
                    "Centralidade de Proximidade" = closeness(dataInput2())*2000,
                    "Constraint" = constraint(dataInput2())*40
    )
    
    plot.igraph(dataInput2(), layout=algo2, vertex.size=escore2, vertex.label=NA,
                #vertex.color='SkyBlue2',
                main = input$text2)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


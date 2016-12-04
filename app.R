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
ui <- fluidPage(
  
  
   # Application title
   titlePanel("Análise de Redes Sociais"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         helpText("Trabalhando com a rede de advogados de E. Lazega."),
         textInput("text", label = "Defina o título da rede", value = "Advogados (Lazega)"),
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
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  dataInput = reactive({
    lazega = upgrade_graph(lazega)
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
                    "Centralidade de Intermediação" = betweenness(dataInput()),
                    "Centralidade de Proximidade" = closeness(dataInput()),
                    "Constraint" = constraint(dataInput()))
    
    
      plot.igraph(dataInput(), layout=algo, vertex.size=escore, 
                  #vertex.color='SkyBlue2',
           main = input$text)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

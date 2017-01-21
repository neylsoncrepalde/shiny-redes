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
library(ggplot2)
library(descr)
library(magrittr)

# Define UI for application that draws a histogram
ui <- navbarPage("Análise de Redes Sociais - GIARS", theme = "slate_bootstrap.css",
                 
                 tabPanel("Apresentação", 
                          sidebarLayout(
                            sidebarPanel(style = "background-color: #BA1723;",
                                         
                                         img(src="http://www.giars.ufmg.br/images/logo.png", height=107, width=225),
                                         
                                         p("Esta é uma aplicação de testes de Análise de Redes Sociais desenvolvida 
                                         por", tags$b(a("Neylson Crepalde", href="https://www.facebook.com/neylson.crepalde")), 
                                          "e pelo", tags$b("GIARS"), "(Grupo Interdisciplinar de Pesquisa em Análise
                                         de Redes Sociais). Ela foi desenvolvida com", a("Shiny", href="https://shiny.rstudio.com/"),
                                         "e seu objetivo é facilitar o aprendizado e fomentar o uso de ferramentas de ARS no Brasil."),
                                         br(),
                                         p("Visite o site do GIARS!"),
                                         tags$b(a("www.giars.ufmg.br", href="http://www.giars.ufmg.br")),
                                         
                                         br(),
                                         br(),
                                         p("Dúvidas? Entre em contato conosco pelo nosso e-mail ou página do Facebook!"),
                                         a(href="mailto:giarsufmg@gmail.com", img(src="mail-6-64x64.png", width=35, height=35)),
                                         a(href="https://www.facebook.com/giarsufmg", img(src="facebook-3-64x64.png", width=35, height=35))
                            ),
                            mainPanel(
                              h1("Aplicação de Análise de Redes Sociais - GIARS"),
                              p("Esta aplicação possui dois bancos de dados relacionais (duas redes) embutidos no pacote", 
                                tags$b("sand"),"do", tags$b("R.")),
                              br(),
                              
                              h2("Rede de advogados de Lazega"),
                              p("A rede de advogados foi extraída de um estudo sobre redes de parcerias corporativas entre advogados.
                                Esse estudo aconteceu numa firma de advocacia em New England, EUA e foi conduzido pelo sociólogo
                                francês", a("Emmanuel Lazega", href="http://elazega.fr/"), ". Ela inclui medidas de 71 advogados 
                                dessa firma, sua rede de laços fortes com colegas de trabalho, redes de aconselhamento, de amizade
                                e redes de controle indireto. Aqui temos acesso apenas a um", tags$em("subset"), "de 34 parceiros dessa firma."),
                              p("Os", tags$b("atributos"),"da rede disponíveis aqui são", tags$b("gênero"), "codificado como 
                                1=HOMEM e 2=MULHER e", tags$b("escritório"),"codificado como 1=BOSTON, 2=HARTFORD e 3=PROVIDENCE (Fonte:
                                documentação do pacote sand)."),
                              h4("Referência:"),
                              p("E. Lazega, The Collegial Phenomenon: The Social Mechanisms of Cooperation Among Peers in a 
                                Corporate Law Partnership. Oxford University Press, Oxford (2001)."),
                              
                              br(),
                              h2("Rede de blogs de política franceses"),
                              p("Este banco de dados consiste de uma subrede de blogs de política franceses extraídos de uma coleta",
                                em("cross-section"),"de mais de 1100 desses blogs em um dia de outubro de 2006 e classificados pelo 
                                projeto Observatório Presidencial quanto à afiliação política. Aqui, temos uma amostra de 192 vértices
                                e 1431 laços. O laço é não-direcionado e consiste da existência de um link para outro blog. Para mais
                                informações, consulte", a("http://observatoire-presidentielle.fr/", href="http://observatoire-presidentielle.fr/"),
                                "(Fonte: documentação do pacote sand).")
                              
                              
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
                                                    
                                                    selectInput("metrica", label = "Métricas de redes",
                                                                choices = c("Nenhum","Centralidade de Grau",
                                                                            "Centralidade de Intermediação",
                                                                            "Centralidade de Proximidade","Constraint"),
                                                                selected = "Nenhum"),
                                                    
                                                    selectInput("atributos", label = "Atributos qualitativos",
                                                                choices = c("Nenhum","Gênero","Escritório")),
                                                    
                                                    submitButton(text = "Atualizar")
                                       ),
                                       
                                       # Show a plot of the generated distribution
                                       mainPanel(
                                         tabsetPanel(
                                           tabPanel("Grafo",
                                                    plotOutput("net")
                                           ),
                                           tabPanel("Distribuição da métrica",
                                                    plotOutput("met_plot")
                                           ),
                                           tabPanel("Análise dos atributos",
                                                    verbatimTextOutput("freq1")
                                           )
                                         )
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
                                                    
                                                    checkboxInput("atributos2",
                                                                  label="Atributo qualitativo - Partido Político",
                                                                  value = FALSE
                                                    ),
                                                    
                                                    submitButton(text = "Atualizar")
                                       ),
                                       
                                       # Show a plot of the generated distribution
                                       mainPanel(
                                         tabsetPanel(
                                           tabPanel("Grafo",
                                                    plotOutput("net2")
                                           ),
                                           tabPanel("Distribuição da métrica",
                                                    plotOutput("met_plot2")
                                           ),
                                           tabPanel("Análise dos atributos",
                                                    verbatimTextOutput("freq2")
                                           )
                                         )
                                       )
                                     )
                            )
                 ),
                 
                 navbarMenu('Analise a sua rede',
                            tabPanel('Instruções',
                                     sidebarLayout(
                                       sidebarPanel(
                                         style = "background-color: #BA1723;",
                                         tags$head(tags$style("#net2{height:80vh !important;}")),
                                         
                                         img(src="http://www.giars.ufmg.br/images/logo.png", height=107, width=225),
                                         h1('Instruções')
                                         ),
                                       mainPanel(
                                         h2('Analise a sua própria rede no shiny app do GIARS:'),
                                         br(),
                                         
                                         p('Passo 1 - ')
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
    
    atrib = switch(input$atributos,
                   "Nenhum" = as.factor(1),
                   "Gênero" = as.factor(V(dataInput())$Gender),
                   "Escritório" = as.factor(V(dataInput())$Office)
    )
    
    plot.igraph(dataInput(), layout=algo, vertex.size=escore, vertex.label.cex=1,
                vertex.color=atrib, main = input$text)
  })
  
  # Programar o plot das métricas
  output$met_plot = renderPlot({
    escore_met = switch(input$metrica,
                        "Nenhum" = NULL,
                        "Centralidade de Grau" = degree(dataInput()),
                        "Centralidade de Intermediação" = betweenness(dataInput()),
                        "Centralidade de Proximidade" = closeness(dataInput()),
                        "Constraint" = constraint(dataInput())
    )
    
    ggplot(NULL, aes(escore_met))+geom_histogram(col="white", bins=10)+
      labs(x="",y="",title="Distribuição da métrica selecionada")+
      theme_gray(base_size = 12)
    
  })
  
  output$freq1 = renderPrint({
    atrib.tab = switch(input$atributos,
                       "Nenhum" = NULL,
                       "Gênero" = V(dataInput())$Gender,
                       "Escritório" = V(dataInput())$Office
    )
    
    if (is.null(atrib.tab)){cat("Nenhum atributo selecionado.")}
    else{freq(atrib.tab, plot=F)}
    
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
    
    if(input$atributos2 == FALSE){
      plot.igraph(dataInput2(), layout=algo2, vertex.size=escore2, vertex.label=NA,
                  #vertex.color='SkyBlue2',
                  main = input$text2)
    }
    else{
      plot.igraph(dataInput2(), layout=algo2, vertex.size=escore2, vertex.label=NA,
                  vertex.color=as.factor(V(dataInput2())$PolParty),
                  main = input$text2)
    }
    
  })
  
  output$met_plot2 = renderPlot({
    escore_met2 = switch(input$metrica2,
                         "Nenhum" = NULL,
                         "Centralidade de Grau" = degree(dataInput2()),
                         "Centralidade de Intermediação" = betweenness(dataInput2()),
                         "Centralidade de Proximidade" = closeness(dataInput2()),
                         "Constraint" = constraint(dataInput2())
    )
    
    ggplot(NULL, aes(escore_met2))+geom_histogram(col="white", bins=20)+
      labs(x="",y="",title="Distribuição da métrica selecionada")+
      theme_gray(base_size = 12)
    
  })
  
  output$freq2 = renderPrint({
    if(input$atributos2 == FALSE){cat("Nenhum atributo selecionado.")}
    else{freq(V(dataInput2())$PolParty, plot=F)}
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


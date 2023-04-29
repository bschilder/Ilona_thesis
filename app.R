require(shiny)
require(visNetwork)
source(here::here("code","prep_graph.R"))
source(here::here("code","visnetwork.R"))
g <- prep_graph() 
nodes <- as.data.frame(g)
  
server <- function(input, output) {
    output$network_proxy_nodes <- visNetwork::renderVisNetwork({ 
        vn <- visnetwork(g = g,  
                         randomSeed = 2019,
                         physics = TRUE, 
                         main = NULL,
                         submain = NULL, 
                         save_path = NULL,
                         show_plot = FALSE,
                         navigationButtons = TRUE, 
                         select_dropdown = TRUE, 
                         open = FALSE)    
        vn$plot
    }) 
    
}

ui <-  div(style=paste("margin: -2% -2% 0% -2%",
                       sep=";"),
           fluidPage(  
        visNetworkOutput("network_proxy_nodes",
                         width = "100%", 
                         height = "105vh")
    ) 
)

shinyApp(ui = ui, server = server)

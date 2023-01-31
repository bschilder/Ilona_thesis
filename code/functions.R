
prep_strings <- function(x,
                         width = 50){
    paste(stringr::str_wrap(gsub("\n"," ",x), width = width),collapse="\n")
}

prep_graph <- function(file=here::here("data","contributors.xlsx")){ 
    
    dt <- (xlsx::read.xlsx(file, sheetIndex = 1) |>
            data.table::data.table()
    )[(!is.na(from) & !is.na(to)),]
    dt[,from:=lapply(from, prep_strings)][,to:=lapply(to,prep_strings)]
    vertices <- data.table::data.table(node=unique(c(dt$from,dt$to)))
    
    g <- igraph::graph_from_data_frame(d = dt, vertices = vertices)
    igraph::V(g)$connections <- igraph::degree(g,mode = "all")
    color_var <- "connections"
    #### Assign node colors ####
    ncolors <- length(unique(igraph::vertex_attr(g,color_var)))
    my_palette <- pals::tol(ncolors+1)
    igraph::V(g)$color <- my_palette[
        cut(igraph::vertex_attr(g,color_var) ,ncolors)
    ]
    # plot(g)
    return(g)
}

plot_graph <- function(g,
                       save_path=here::here("figures",
                                            "contributors_network.html"),
                       layout = "layout_with_fr",
                       randomSeed = 2023){
    
    #### Create plot ####
    visnet <- visNetwork::visIgraph(g, 
                                    layout = layout, 
                                    randomSeed = randomSeed) |>
        visNetwork::visNodes(shape = "elipses", 
                             color = list(border = "rgba(255,255,255,1)"),
                             font = list(color="white", 
                                         strokeWidth=5,
                                         strokeColor="rgba(0,0,0,0.5)"), 
                             shadow = list(enabled=TRUE),
                             scaling=list(max=200,
                                          min=100,
                                          label=list(min=50,
                                                     maxVisible=2000,
                                                     max=100)
                             )
        ) |>
        visNetwork::visEdges(arrows = "",
                             color = list(color="rgb(55,123,181, 0.7)",
                                          opacity=.5),
                             smooth = list(enabled=TRUE,
                                           type="cubicBezier",
                                           roundness=.5),
                             width = 10) |>
        visNetwork::visExport(type = "pdf", 
                              name = gsub("\\.html","",basename(save_path)))
    
    #### Save plot ####
    dir.create(dirname(save_path),showWarnings = FALSE, recursive = TRUE)
    visNetwork::visSave(visnet, 
                        file = save_path, 
                        selfcontained = TRUE,
                        background = "black")
    return(visnet)
}

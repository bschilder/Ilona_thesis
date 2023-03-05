
prep_strings <- function(x,
                         width = 50){
    stringr::str_trim(x) |>
        gsub(pattern = "\n",replacement = " ")
}

prep_graph <- function(file=here::here("data","contributors.xlsx")){ 
    
    #### Edges ####
    edges_dt <- (xlsx::read.xlsx(file, sheetName = "edges") |>
            data.table::data.table()
    )[(!is.na(from) & !is.na(to)),]
    edges_dt[,from:=lapply(from, prep_strings)][,to:=lapply(to,prep_strings)]
    #### Nodes ####
    nodes_dt <- xlsx::read.xlsx(file, sheetName = "nodes") |>
            data.table::data.table()
    nodes_dt <- nodes_dt[,key:=lapply(key, prep_strings)] 
    nodes_dt[,name:=(
        ifelse(is.na(role),
               paste0(entity),
               paste0(paste0(entity),
                      " (",role,")")) |>
            stringr::str_wrap(width = 80) |>
            gsub(pattern="[(]",replacement="\n(") |>
            gsub(pattern="[,]",replacement=",\n") |>
            gsub(pattern="\n\n",replacement="\n") 
    ) 
    ]
    nodes_dt$key <- unlist(nodes_dt$key)
    nodes_dt_agg <- nodes_dt[,list(entity=toString(unique(entity)),
                                   role=toString(unique(role)),
                                   name=toString(unique(name)),
                                   cluster=list(unique(cluster)),
                                   gradient=list(unique(gradient)),
                                   show=list(unique(show))
                                ),
                          by="key"]   
    nodes_dt_agg[,title:=paste0(
        "<strong>entity</strong>: ",entity,"<br>",
        ifelse(role=="NA","",paste0("<strong>role</strong>: ",role,"<br>")),
        "<strong>cluster</strong>: ",cluster,"<br>"
    )]
    # setdiff(c(edges_dt$from,edges_dt$to),
    #         nodes_dt$old_name)
    # setdiff(nodes_dt$old_name,
    #         c(edges_dt$from,edges_dt$to))
    g <- igraph::graph_from_data_frame(d = edges_dt, 
                                       vertices = nodes_dt_agg) 
    #### Compute the number of connections per node ####
    igraph::V(g)$connections <- igraph::degree(g,mode = "all")
    igraph::V(g)$value <- igraph::V(g)$connections
    color_var <- "cluster"
    #### Assign node colors ####
    ncolors <- length(unique(igraph::vertex_attr(g,color_var)))
    my_palette <- ggplot2::alpha(pals::ocean.phase(ncolors+1),alpha = .75)
    igraph::V(g)$color <- my_palette[
        cut(sapply(igraph::vertex_attr(g,color_var),function(x){x[[1]]}) ,ncolors)
    ]
    # plot(g)
    return(g)
}

plot_graph <- function(g,
                       save_path=here::here("figures",
                                            "contributors_network.html"),
                       layout = "layout_with_kk",
                       solver = "forceAtlas2Based",
                       physics = FALSE,
                       forceAtlas2Based = list(
                           avoidOverlap=.75,
                           gravitationalConstant=-100),
                       show_plot = TRUE,
                       main = "Collaboration network",
                       submain = NULL,
                       background = "black",
                       randomSeed = 2023){
    library(dplyr)
    # templateR:::args2vars(plot_graph)
    #### Create plot ####
    vn <-  
        visNetwork::toVisNetworkData(g) %>%
        {
            do.call(visNetwork::visNetwork,
                    c(., list(main = main,
                              submain = submain,
                              background = background)
                    )
            )
        } |>
        visNetwork::visIgraphLayout(layout = "layout_nicely",
                                    type = "full",
                                    physics = physics,
                                    randomSeed = randomSeed) |>
        visNetwork::visPhysics(solver=solver,
                               forceAtlas2Based=forceAtlas2Based,
                               enabled = physics) |>
        visNetwork::visNodes(#shape = "circle",     
                             font = list(color="white", 
                                         strokeWidth=5,
                                         strokeColor="rgba(0,0,0,0.5)"),  
                             shadow = list(enabled=TRUE,
                                           size = 10), 
                             borderWidth=3,
                             borderWidthSelected=6,
                             color = list(border = "rgba(255,255,255,.5)",
                                          hover = list(background="rgba(0,0,0,.5)"),
                                          highlight = list(background="#00FFFFCF",
                                                           border="#00FFFFCF")
                             ),
                             # scaling=list(max=200,
                             #              # min=100,
                             #              label=list(min=50,
                             #                         maxVisible=2000,
                             #                         max=100
                             #                         )
                             # )
        ) |>
        visNetwork::visEdges(arrows = "",
                             # color = list(color="rgb(55,123,181, 0.7)",
                             #              opacity=.5),
                             smooth = list(enabled=TRUE,
                                           type="cubicBezier",
                                           roundness=.5),
                             dashes=TRUE,
                             width = 3) |>
        # visNetwork::visExport(type = "pdf", 
        #                       name = gsub("\\.html","",basename(save_path))) |>
        visNetwork::visInteraction(hover = TRUE) |>
        visNetwork::visOptions(height = 800,
                               width=1300,
                               selectedBy = "cluster",
                               highlightNearest = list(enabled=TRUE,
                                                       degree=1))
    #### Show plot ####
    if(show_plot) methods::show(vn) 
    #### Save plot ####
    if(!is.null(save_path)){
        dir.create(dirname(save_path),showWarnings = FALSE, recursive = TRUE)
        message("Saving graph --> ",save_path)
        visNetwork::visSave(vn, 
                            file = save_path, 
                            selfcontained = TRUE,
                            background = background)
    } 
    return(vn)
}


prep_strings <- function(x,
                         width = 50){
    stringr::str_trim(x) |>
        gsub(pattern = "\n",replacement = " ")
}
check_diff <- function(edges_dt,
                       nodes_dt){
    setdiff(c(edges_dt$from,edges_dt$to),
            nodes_dt$old_name)
    setdiff(nodes_dt$old_name,
            c(edges_dt$from,edges_dt$to))
}

prep_graph <- function(file=here::here("data","contributors.xlsx")){ 
    
    color_var <- "cluster"
    #### Edges ####
    edges_dt <- (xlsx::read.xlsx(file, sheetName = "edges") |>
            data.table::data.table()
    )[(!is.na(from) & !is.na(to)),]
    edges_dt[,from:=sapply(from, prep_strings)][,to:=sapply(to,prep_strings)] 
    #### Nodes ####
    nodes_dt <- xlsx::read.xlsx(file, sheetName = "nodes") |>
            data.table::data.table()
    nodes_dt <- nodes_dt[,key:=lapply(key, prep_strings)] 
    nodes_dt[,name:=(
        ifelse(is.na(role),
               paste0(entity),
               paste0(paste0(entity),
                      " (",role,")")) |>
            stringr::str_wrap(width = 50) |>
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
                                   cluster_str=paste(unique(cluster),collapse =";"),
                                   gradient=unique(na.omit(gradient)),
                                   show=unique(show)
                                ),
                          by="key"][,show:=ifelse(is.na(show),TRUE,show)] 
    nodes_dt_agg$i <- seq_len(nrow(nodes_dt_agg))
    nodes_dt_agg <- nodes_dt_agg |>  
        dplyr::group_by(cluster_str) |> 
        dplyr::mutate(gradient_i=ifelse(!is.na(gradient) & gradient, 
                                        (dplyr::row_number()/dplyr::n())+.1,
                                        .75)) |>
        data.table::data.table()
    # #### Assign node colors #### 
    clusters_unique <- unique(unlist(nodes_dt_agg[[color_var]]))
    ncolors <- length(clusters_unique)
    my_palette <- stats::setNames(pals::ocean.phase(ncolors+1)[-1],
                                  as.character(clusters_unique))
    nodes_dt_agg[,color:=sapply(cluster, function(x){
        my_palette[as.character(x[[1]])]
    })][,color:=ggplot2::alpha(color,gradient_i)]
    nodes_dt_agg[,border:=sapply(cluster, function(x){
        idx <- if(length(x)>1) 2 else 1
        ggplot2::alpha(my_palette[as.character(x[[idx]])], 1)
    })]
    #### Set shape ####
    nodes_dt_agg[,shape:=ifelse(show,"dot","text")]
    #### Add hover info ####
    nodes_dt_agg[,title:=paste0(
        "<strong>entity</strong>: ",entity,"<br>",
        ifelse(role=="NA","",paste0("<strong>role</strong>: ",role,"<br>")),
        "<strong>cluster</strong>: ",cluster,"<br>",
        "<strong>color</strong>: ",color,"<br>",
        "<strong>border</strong>: ",border,"<br>"
    )] 
    #### Sort by cluster ####
    nodes_dt_agg <- suppressWarnings(
        nodes_dt_agg[order(as.numeric(nodes_dt_agg$cluster_str)),]
    )
    
    #### Add all possible connections within clusters ##### 
    extra_edges <- lapply(unique(nodes_dt_agg$cluster),
                          function(clust){
        nodes <- unique(nodes_dt_agg[cluster==clust,]$key)
        cj <- data.table::CJ(from=nodes,to=nodes,unique = TRUE)     
        cj[from!=to][,hidden:=TRUE]
    }) |> data.table::rbindlist()
    edges_dt <- data.table::rbindlist(list(edges_dt[,hidden:=FALSE],
                                           extra_edges), fill = TRUE)
    
    #### Convert to graph ####
    g <- igraph::graph_from_data_frame(d = edges_dt, 
                                       vertices = nodes_dt_agg) 
    g <- tidygraph::as_tbl_graph(g) 
    #### Compute the number of connections per node ####
    igraph::V(g)$connections <- igraph::degree(g,mode = "all")
    igraph::V(g)$value <- igraph::V(g)$connections
    color_var <- "cluster" 
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
                           avoidOverlap=1,
                           gravitationalConstant=-500),
                       show_plot = TRUE,
                       main = "Collaboration network",
                       submain = NULL,
                       background = "black",
                       randomSeed = 2023){
    # devoptera::args2vars(plot_graph, packages="dplyr")
    #### Create plot ####
    set.seed(randomSeed)
    vn <-  
        visNetwork::toVisNetworkData(g) %>%
        {
            do.call(visNetwork::visNetwork,
                    c(., list(main = main,
                              submain = submain,
                              height = 14043/4,
                              width = 9933/4,
                              background = background)
                    )
            )
        } |>
        visNetwork::visIgraphLayout(layout = "layout_with_graphopt",
                                    type = "full",
                                    charge = .008,
                                    # mass = 100,
                                    # spring.length = 50,
                                    spring.constant = 0.1,
                                    physics = physics,
                                    randomSeed = randomSeed) |>
        # visNetwork::visLayout(improvedLayout = TRUE,
        #                       clusterThreshold=10000) |>
        visNetwork::visPhysics(solver=solver,
                               forceAtlas2Based=forceAtlas2Based,
                               enabled = TRUE) |>
        visNetwork::visNodes(font = list(color="white", 
                                         strokeWidth=5,
                                         strokeColor="rgba(0,0,0,0.5)"),  
                             shadow = list(enabled=TRUE,
                                           size = 20, 
                                           color="rgba(255,255,255,0.5)"), 
                             borderWidth=3,
                             borderWidthSelected=6,
                             color = list(border="background",
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
                             # shadow = list(enabled=TRUE,
                             #               size = 20, 
                             #               color="rgba(255,255,255,0.5)"), 
                             smooth = list(enabled=TRUE,
                                           type="cubicBezier",
                                           roundness=.5),
                             dashes=TRUE,
                             width = 3) |>
        # visNetwork::visExport(type = "pdf", 
        #                       name = gsub("\\.html","",basename(save_path))) |>
        visNetwork::visInteraction(hover = TRUE) |>
        visNetwork::visOptions(
                               selectedBy = list(variable="cluster_str",
                                                 main="Cluster",
                                                 sort=FALSE),
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

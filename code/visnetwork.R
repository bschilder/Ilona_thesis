visnetwork <- function(g,
                       save_path=here::here("figures",
                                            "contributors_network.html"),
                       layout = "layout_with_kk",
                       solver = "forceAtlas2Based",
                       physics = FALSE,
                       forceAtlas2Based = list(
                           damping=3.75,# higher damping = less spinning
                           avoidOverlap=2,
                           gravitationalConstant= -175
                           # springLength=50
                           ),
                       groups = NULL,
                       show_plot = TRUE,
                       main = "Scroll to zoom. Double-click background to resize.",
                       submain = "Select cluster:",
                       background = "rgb(8,36,81)",
                       open=FALSE,
                       stabilization = TRUE,
                       select_dropdown = TRUE,
                       navigationButtons = TRUE,
                       width = "100%", 
                       height = "90vh",
                       x_start=c(0,1000),
                       y_start=c(100,-100),
                       stroke_alpha = .5,
                       randomSeed = 2023){
    # devoptera::args2vars(plot_graph, packages="dplyr")
    #### Create plot ####
    set.seed(randomSeed)
    # igraph::V(g)$y <- ifelse(igraph::V(g)$name=="Deep Structure",)
    start <- data.table::as.data.table(g)[,list(x=ifelse(group=="Deep Structure" |entity=="Deep Structure",
                                                         x_start[1],x_start[2]),
                                                y=ifelse(group=="Deep Structure" |entity=="Deep Structure",
                                                         y_start[1],y_start[2]))] |> as.matrix()
    
    vn <-  
        visNetwork::toVisNetworkData(g) %>%
        {
            do.call(visNetwork::visNetwork,
                    c(., list(main = list(text=main,
                                          style="color:white"),
                              submain = list(text=submain,
                                             style="color:white"),
                              background = background,
                              width = width,
                              height = height
                              )
                    )
            )
        } |>
        # visNetwork::visIgraph(igraph = g, type="full") |> 
        visNetwork::visIgraphLayout(layout = "layout_with_graphopt",
                                    # layout = "layout_with_kk",
                                    type = "full",
                                    start = start,
                                    # niter = 50,
                                    # charge = .025,
                                    # mass = 100,
                                    # spring.length = 50,
                                    # spring.constant = 1,
                                    physics = physics,
                                    randomSeed = randomSeed) |>
        # visNetwork::visLayout(randomSeed = randomSeed) |>
        visNetwork::visPhysics(solver=solver,
                               # timestep = .25,
                               # maxVelocity = 100, 
                               forceAtlas2Based=forceAtlas2Based,
                               stabilization = list(enabled=stabilization,
                                                    fit=TRUE),
                               enabled = physics) |>
        visNetwork::visNodes(font = list(color="white", 
                                         strokeWidth=5,
                                         strokeColor=paste0(
                                             "rgba(0,0,0,",stroke_alpha,")"
                                         )),  
                             shadow = list(enabled=TRUE,
                                           size = 5, 
                                           color="rgba(255,255,255,0.5)"), 
                             borderWidth=3,
                             borderWidthSelected=6,
                             color = list(hover=list(background="black"),
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
        visNetwork::visEdges(arrows = NULL,
                             # shadow = list(enabled=TRUE,
                             #               size = 20, 
                             #               color="rgba(255,255,255,0.5)"), 
                             smooth = list(enabled=TRUE,
                                           type="cubicBezier",
                                           roundness=.5),
                             dashes = TRUE,
                             width = 3) |>
        # visNetwork::visExport(type = "pdf",
        #                       name = gsub("\\.html","",basename(save_path))) |>
        # visNetwork::visGroups(groupname = "Deep Structure", color = "yellow", shape = "triangle") |>
        visNetwork::visInteraction(hover = TRUE,
                                   navigationButtons = navigationButtons,
                                   tooltipDelay = 100) |>
        # visNetwork::visLegend() |>
        visNetwork::visOptions(
            # width = "300%",
            # height = "100vh",
            selectedBy = if(isTRUE(select_dropdown)){
                list(variable="cluster_str",
                     main="--",
                     style="
                     position: fixed;
                     top: 10px;
                     left: 10px;
                     z-index:10;",
                     sort=FALSE)
                } else {NULL},  
            autoResize = TRUE, 
            highlightNearest = list(enabled=TRUE,
                                    degree=1))  
    vn <-  vn |> visNetwork::visEvents(type = "on", 
                                       doubleClick = "function(){ this.fit()}")        
    if(!is.null(groups)){
        vn <- vn |> visNetwork::visClusteringByGroup(groups = as.factor(groups),
                                                     shape = "diamond",
                                                     scale_size = TRUE,
                                                     color = ggplot2::alpha("white",.75),
                                                     force = TRUE,
                                                     label = "+ Double-click to expand +\n")
    } 
    #### Show plot ####
    if(show_plot) methods::show(vn) 
    #### Save plot ####
    if(!is.null(save_path)){
        dir.create(dirname(save_path),showWarnings = FALSE, recursive = TRUE)
        message("Saving graph --> ",save_path)
        visNetwork::visSave(vn, 
                            file = save_path, 
                            selfcontained = TRUE,
                            background = if(is.null(background)) "white" else background
                            )
    } 
    if(isTRUE(open)) utils::browseURL(save_path)
    return(list(plot=vn,
                path=save_path))
}


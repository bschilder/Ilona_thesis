prep_graph <- function(file=here::here("data","contributors.xlsx")){ 
    
    color_var <- "subcluster_str"
    #### Edges ####
    edges_dt <- (xlsx::read.xlsx(file, sheetName = "edges") |>
                     data.table::data.table()
    )[(!is.na(from) & !is.na(to)),]
    edges_dt[,from:=sapply(from, prep_strings)][,to:=sapply(to,prep_strings)] 
    edges_dt[is.na(hidden)]$hidden <- FALSE
    #### Nodes ####
    nodes_dt <- xlsx::read.xlsx(file, sheetName = "nodes") |>
        data.table::data.table()
    nodes_dt <- nodes_dt[,key:=lapply(key, prep_strings)] 
    nodes_dt[,name:=(
        # entity |>
        ifelse(is.na(role),
               entity,
               paste0(entity," (",role,")") ) |>
            stringr::str_wrap(width = 35) |>
            gsub(pattern="[(]",replacement="\n(") |>
            # gsub(pattern="[,]",replacement=",\n") |>
            gsub(pattern="\n\n",replacement="\n") 
    ) 
    ]
    nodes_dt[,subcluster_name:=data.table::fcoalesce(subcluster_name,
                                                     as.character(subcluster))]
    nodes_dt$key <- unlist(nodes_dt$key)
    nodes_dt_agg <- nodes_dt[,list(entity=toString(unique(entity)),
                                   role=toString(unique(role)),
                                   name=toString(unique(na.omit(name))),
                                   # cluster=list(unique(cluster)),
                                   cluster_str=paste(unique(na.omit(cluster_name)),
                                                     collapse =";"),
                                   # subcluster=unique(na.omit(subcluster)),
                                   subcluster_str=paste(unique(subcluster_name),
                                                        collapse =";"),
                                   gradient=unique(na.omit(gradient)),
                                   hidden=unique(na.omit(hidden)),
                                   mult=ifelse(is.na(mult),1,mult)
    ),
    by="key"] |> unique()
    # nodes_dt_agg[duplicated(nodes_dt_agg$key)]$name
    nodes_dt_agg[,hidden:=ifelse(is.na(hidden),FALSE,hidden)] 
    nodes_dt_agg$i <- seq_len(nrow(nodes_dt_agg))
    min_alpha <- .35
    nodes_dt_agg <- nodes_dt_agg |>  
        dplyr::group_by(cluster_str) |> 
        dplyr::mutate(gradient_i=ifelse(!is.na(gradient) & gradient, 
                                        (dplyr::row_number()/dplyr::n())+min_alpha,
                                        .75)) |>
        data.table::data.table()
    
    
    # check_diff(edges_dt,nodes_dt_agg)
    #### Assign node colors ####
    clusters_unique <- unique(unlist(nodes_dt_agg[[color_var]]))
    ncolors <- length(clusters_unique)
    my_palette <- stats::setNames(pals::ocean.phase(ncolors+1)[-1],
                                  as.character(clusters_unique))
    nodes_dt_agg[,color:=sapply(get(color_var), function(x){
        my_palette[as.character(x[[1]])]
    })][,color:=ggplot2::alpha(color,gradient_i)]
    nodes_dt_agg[,border:=sapply(get(color_var), function(x){
        idx <- if(length(x)>1) 2 else 1
        ggplot2::alpha(my_palette[as.character(x[[idx]])], 1)
    })]
    #### Set shape ####
    nodes_dt_agg[,shape:=ifelse(hidden,"text","dot")]
    #### Add hover info ####
    nodes_dt_agg[,title:=paste0(
        "<strong>entity</strong>: ",entity,"<br>",
        ifelse(role=="NA","",paste0("<strong>role</strong>: ",paste(stringr::str_wrap(role,width = 30),sep = "<br>"),"<br>")),
        "<strong>Cluster</strong>: ",cluster_str,"<br>",
        ifelse(subcluster_str=="NA","",paste0("<strong>Subcluster</strong>: ",subcluster_str))
        # "<strong>color</strong>: ",color,"<br>",
        # "<strong>border</strong>: ",border,"<br>"
    )] 
    #### Sort by cluster ####
    nodes_dt_agg <- suppressWarnings(
        nodes_dt_agg[order(as.numeric(nodes_dt_agg$cluster_str)),]
    ) 
    #### Add all possible connections within clusters ##### 
    extra_edges1 <- lapply(unique(nodes_dt_agg$cluster_str),
                           function(clust){
                               nodes <- unique(nodes_dt_agg[cluster_str==clust,]$key)
                               cj <- data.table::CJ(from=nodes,to=nodes,unique = TRUE)     
                               cj[from!=to][,hidden:=TRUE]
                           }) |> data.table::rbindlist()
    extra_edges2 <- lapply(unique(nodes_dt_agg$subcluster_str),
                           function(clust){
                               if(clust==10) return(NULL)
                               nodes <- unique(nodes_dt_agg[subcluster_str==clust,]$key)
                               cj <- data.table::CJ(from=nodes,to=nodes,unique = TRUE)     
                               cj[from!=to][,hidden:=TRUE] 
                           }) |> data.table::rbindlist()
    edges_dt_merge <- data.table::rbindlist(list(edges_dt,
                                                 # extra_edges1,
                                                 extra_edges2), 
                                            fill = TRUE)
    # edges_dt <- edges_dt[!duplicated(paste(edges_dt$from,edges_dt$to))]
    #### Convert to graph ####
    check_diff(edges_dt,nodes_dt_agg)
    g <- igraph::graph_from_data_frame(d = edges_dt_merge, 
                                       vertices = nodes_dt_agg) #|>
    g <- tidygraph::as_tbl_graph(g)
    #### Compute the number of connections per node ####
    igraph::V(g)$connections <- igraph::degree(g,mode = "all")
    #### Add size ####
    igraph::V(g)$value <- igraph::V(g)$connections*igraph::V(g)$mult 
    #### Add shape ####
    igraph::V(g)$shape <- ifelse(igraph::V(g)$cluster_str=="Projects","hexagon","dot")
    #### Add group ####
    igraph::V(g)$group <- igraph::V(g)$cluster_str
    # View(tidyr::as_tibble(g))
    # plot(g)
    return(g)
}


prep_strings <- function(x,
                         width = 50){
    stringr::str_trim(x) |>
        gsub(pattern = "\n",replacement = ",<br> ") |>
        gsub(pattern = " +",replacement = " ")
}
check_diff <- function(edges_dt,
                       nodes_dt){
    # message("Checking: from --> key")
    # setdiff(edges_dt$from,
    #         nodes_dt$key) |> print()
    # message("Checking: to --> key")
    # setdiff(edges_dt$to,
    #         nodes_dt$key)|> print()
    # message("Checking: key --> from")
    # setdiff(nodes_dt$key,
    #         edges_dt$from)|> print()
    # message("Checking: key --> to")
    # setdiff(nodes_dt$key,
    #         edges_dt$to)|> print()
    message("Checking: from/to --> key")
    setdiff(unique(c(edges_dt$from,edges_dt$to)),
            nodes_dt$key) |> print()
    message("Checking: key --> from/to")
    setdiff(nodes_dt$key,
            unique(c(edges_dt$from,edges_dt$to))) |> print()
    
}
# check_diff(edges_dt,nodes_dt)

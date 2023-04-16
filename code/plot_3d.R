plot_3d <- function(g,
                    layout_func = igraph::layout.fruchterman.reingold,
                    node_color_var = "subcluster_str",
                    edge_color_var = "zend",
                    text_color_var = node_color_var,
                    node_symbol_var = "shape",
                    node_palette = pals::ocean.phase,
                    edge_palette = pals::ocean.phase,
                    node_opacity = .75,
                    edge_opacity = .5,
                    kde_palette = pals::ocean.deep,
                    add_kde = TRUE,
                    extend_kde = 1.5,
                    bg_color = kde_palette(100)[1],
                    add_labels = TRUE,
                    keep_grid = FALSE,
                    aspectmode = 'cube',
                    hover_width=80,
                    label_width=80,
                    seed = 2023,
                    showlegend = TRUE,
                    show_plot = TRUE,
                    save_path = tempfile(fileext = "network_3d.html"),
                    verbose = TRUE){
    
    # devoptera::args2vars(plot_3d, path="code/",
    #                      pattern="plot_3d",
    #                      reassign = TRUE)
    d <- HPOExplorer:::igraph_to_plotly(g)
    vdf <- d$vertices
    vdf[,label:=gsub("\n","<br>",name)]
    edf <- d$edges
    # HPOExplorer::network_3d()
    # remove(d)
    #### Create paths and nodes ####
    fig <-
        plotly::plot_ly() |>
        plotly::add_paths(data = edf,
                          x = ~xend,
                          y = ~yend,
                          z = ~zend,
                          xend = ~xend,
                          yend = ~yend,
                          # zend = ~zend,
                          # color = ~get(edge_color_var),
                          colors = rev(edge_palette(50)),
                          line = list(shape = "spline"),
                          type = "scatter3d",
                          name = "edge",
                          opacity = edge_opacity,
                          mode = "lines",
                          hoverinfo = "none",
                          showlegend = FALSE,
                          inherit = FALSE) |>
        plotly::add_markers(data = vdf,
                            x = ~x,
                            y = ~y,
                            z = ~z,
                            symbol = ~stringr::str_wrap(
                                get(node_symbol_var),
                                width = label_width
                            ),
                            # size = ~ontLvl,
                            color = ~ get(node_color_var),
                            colors = (
                                node_palette(length(
                                    unique(vdf[[node_color_var]])
                                ))
                            ),
                            marker = list(
                                line = list(
                                    color = bg_color
                                )
                            ),
                            hovertext = ~ gsub(
                                "<strong>|</strong>","",
                                title),
                            opacity = node_opacity,
                            showlegend = showlegend,
                            type = "scatter3d",
                            mode = "markers")
    #### Add KDE ####
    if(isTRUE(add_kde)){
        kd <- HPOExplorer::kde_surface(xyz = vdf,
                                       extend_kde = extend_kde)
        fig <- fig |>
            plotly::add_surface(data = kd,
                                x = ~x,
                                y = ~y,
                                z = ~z,
                                opacity = 1,
                                hoverinfo = "none",
                                colorscale = list(
                                    seq(0,1,length.out=6),
                                    kde_palette(n = 6)
                                ),
                                showlegend = FALSE,
                                inherit = FALSE)
    }
    #### Add text ####
    if(isTRUE(add_labels)){
        fig <- fig |>
            plotly::add_text(data = vdf,
                             x = ~x,
                             y = ~y,
                             z = ~z,
                             text = ~label,
                             color = ~ get(text_color_var),
                             colors = (
                                 node_palette(length(
                                     unique(vdf[[text_color_var]])
                                 ))
                             ),
                             # textfont = list(color="rbga(255,255,255,.8"),
                             hoverinfo = "none",
                             inherit = FALSE,
                             showlegend = FALSE)
    }
    #### Add layout ####
    scene <- lapply(stats::setNames(c("xaxis","yaxis","zaxis"),
                                    c("xaxis","yaxis","zaxis")),
                    function(x){list(showgrid = keep_grid,
                                     showline = keep_grid,
                                     showspikes = keep_grid,
                                     zeroline = keep_grid,
                                     title = list(
                                         text=if(isTRUE(keep_grid)){
                                             x
                                         } else {""}
                                     ),
                                     showscale=FALSE,
                                     showticklabels = keep_grid)}
    )
    scene$aspectmode <- aspectmode
    fig <- fig |>
        plotly::layout(
            hoverlabel = list(align = "left"),
            plot_bgcolor = bg_color,
            paper_bgcolor = bg_color,
            scene = scene,
            uniformtext=list(maxsize=8, mode='hide'),
            showlegend = FALSE
        ) |>
        plotly::hide_colorbar()
        # plotly::colorbar(title=edge_color_var)
    # file <- file.path("~/Downloads","hpo_plotly.png")
    # plotly::export(p = fig,
    #                file = file,
    #                selenium = RSelenium::rsDriver(browser = "chrome"))
    
    # reticulate::py_install(packages = "kaleido",)
    # plotly::save_image(p = fig,file = file, width = 10, height =10)
    if(isTRUE(show_plot)) methods::show(fig)
    if(!is.null(save_path)) {
        message("Saving interactive plot -->",save_path)
        dir.create(dirname(save_path),showWarnings = FALSE, recursive = TRUE)
        htmlwidgets::saveWidget(widget = fig,
                                file = save_path,
                                selfcontained = TRUE)
    }
}
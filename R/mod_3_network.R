# ============================================================
# mod_3_network.R — Module 3: Network-Based Mining Workbench
# ============================================================

mod_3_sidebar <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "param-section",
      h5(icon("project-diagram"), "Network Construction"),
      selectInput(ns("m3_edge_weight"), "Edge Weight Type",
        choices = c("PMI","Frequency","Hybrid"), selected = "PMI"),
      uiOutput(ns("weight_explain")),
      conditionalPanel(
        condition = paste0("input['", ns("m3_edge_weight"), "'] == 'Hybrid'"),
        sliderInput(ns("m3_hybrid_alpha"), "Alpha (PMI weight)", min = 0, max = 1, value = 0.5, step = 0.05),
        p(class = "text-muted", style = "font-size:11px;", "Beta (Frequency weight) is derived automatically: β = 1 − α.")
      ),
      sliderInput(ns("m3_min_freq"), "Min Co-occurrence", min = 1, max = 20, value = 3)
    ),
    div(class = "param-section",
      h5(icon("sitemap"), "Community Detection"),
      p(class = "text-muted", style = "font-size:11px;", "Louvain algorithm: fast, widely used community detection."),
      uiOutput(ns("data_status"))
    ),
    div(class = "param-section",
      h5(icon("play-circle"), "Pipeline Run"),
      sliderInput(ns("m3_kcore_quantile"), "Core Shrink Quantile (strength pruning threshold)", min = 0.30, max = 0.90, value = 0.70, step = 0.01),
      p(class = "text-muted", style = "font-size:11px;", "Higher = more aggressive pruning = smaller core formulas.")
    ),
    div(class = "param-section",
      h5(icon("book"), "Herb Property Dictionary"),
      uiOutput(ns("dict_status")),
      fileInput(ns("custom_attr"), "Upload Custom Property Dict (.xlsx)", accept = ".xlsx")
    ),
    div(class = "sidebar-action-bar",
      parameter_guide("m3"),
      actionButton(ns("run_m3"), "Run Full Pipeline", icon = icon("play"), class = "btn-success btn-block"),
      br(),
      actionButton(ns("reset_params"), "Reset to Global Defaults", icon = icon("undo"), class = "btn-sm btn-block")
    )
  )
}

mod_3_network_ui <- function(id) {
  ns <- NS(id)
  tagList(
    workflow_guide("m3"),
    fluidRow(
      box(title = "Network-Based Mining Workbench", status = "primary", solidHeader = TRUE, width = 12,
        p("Co-occurrence network → Louvain community detection → k-core core formula extraction.
           PMI corrects frequency bias — two herbs score high only if they co-occur significantly more than expected by chance.")
      )
    ),
    fluidRow(
      box(title = "Herb Property Dictionary", status = "info", solidHeader = TRUE, width = 12,
        DTOutput(ns("dt_herb_attr"))
      )
    ),
    br(),
    fluidRow(
      box(title = "Results", status = "success", solidHeader = TRUE, width = 12,
        verbatimTextOutput(ns("net_summary")),
        br(),
        h5("All Core Herbal Combinations"),
        DTOutput(ns("dt_core")),
        br(),
        downloadButton(ns("dl_core"), "Download Core Herbal Combinations (CSV)", class = "btn-sm"),
        hr(),
        h5("Per-Community Results"),
        fluidRow(
          column(3,
            selectInput(ns("result_comm"), "Select Community", choices = "(run pipeline first)"),
            br(),
            h5(textOutput(ns("comm_herb_title"))),
            DTOutput(ns("dt_comm_herbs")),
            br(), hr(),
            h5("Download Results"),
            p(style="font-size:11px; color:#6c757d;", "Select communities and content:"),
            selectInput(ns("dl_communities"), "Communities", choices = character(0), multiple = TRUE),
            checkboxInput(ns("dl_net"), "Network Graph (PNG)", TRUE),
            checkboxInput(ns("dl_radar"), "Radar Charts (PNG)", TRUE),
            checkboxInput(ns("dl_table"), "Herb Table (CSV)", TRUE),
            br(),
            downloadButton(ns("dl_zip"), "Download Selected (ZIP)", class = "btn-sm btn-block")
          ),
          column(9,
            h5("Core Herbal Combination"),
            textOutput(ns("core_formula_text")),
            br(),
            h5("Community Network"),
            plotlyOutput(ns("comm_subgraph"), height = "500px"),
            br(),
            h5("Radar Charts"),
            fluidRow(
              column(4, plotlyOutput(ns("radar_sx"), height = "280px")),
              column(4, plotlyOutput(ns("radar_ww"), height = "280px")),
              column(4, plotlyOutput(ns("radar_gj"), height = "280px"))
            )
          )
        )
      )
    )
  )
}

mod_3_network_server <- function(input, output, session, shared_data) {
  datatable <- function(data, ...) localized_datatable(data, ..., lang = runtime_lang(shared_data))

  pipe_res <- reactiveVal(NULL)

  .first_valid <- function(r, s) {
    choices <- paste0("C", r$ids)
    if (length(s) == 1 && nzchar(s) && s %in% choices) s else choices[1]
  }

  output$data_status <- renderUI({
    if (is.null(shared_data$clean))
      div(class = "alert alert-warning", "No cleaned data. Run M0 first.")
    else
      div(class = "alert alert-success", runtime_tr("Ready: %d prescriptions.", runtime_lang(shared_data), nrow(shared_data$clean)))
  })

  output$weight_explain <- renderUI({
    txt <- switch(input$m3_edge_weight,
      "PMI" = "Corrects frequency bias. Recommended.",
      "Frequency" = "Raw co-occurrence count.",
      "Hybrid" = "Blend PMI and frequency."
    )
    div(class = "alert alert-info", style = "font-size:12px; padding:8px; margin-top:5px;", txt)
  })

  observeEvent(input$m3_hybrid_alpha, {
    # β is derived, preventing reciprocal observer loops and preserving α + β = 1.
  })

  observeEvent(input$reset_params, {
    p <- shared_data$active_params
    updateSelectInput(session, "m3_edge_weight", selected = p$m3_edge_weight)
    updateSliderInput(session, "m3_hybrid_alpha", value = p$m3_hybrid_alpha)
    updateSliderInput(session, "m3_min_freq", value = p$m3_min_freq)
    updateSliderInput(session, "m3_kcore_quantile", value = p$m3_kcore_quantile)
    showNotification("Parameters reset", type = "message")
  })

  observeEvent(input$run_m3, {
    req(shared_data$clean, shared_data$clean_col)
    set.seed(shared_data$active_params$random_seed)

    data <- shared_data$clean; col <- shared_data$clean_col
    wt <- input$m3_edge_weight; mf <- input$m3_min_freq; kcq <- input$m3_kcore_quantile
    alpha <- if (is.null(input$m3_hybrid_alpha)) 0.5 else input$m3_hybrid_alpha
    beta <- round(1 - alpha, 2)

    # Step 1
    net <- tryCatch({
      if (wt == "Hybrid") build_network_pmi_freq(data, col, mf, alpha, beta, positive_pmi_only = FALSE)
      else if (wt == "Frequency") { n <- build_cooccurrence_network(data, col, mf, positive_pmi_only = FALSE); E(n$graph)$weight <- n$edge_data$freq; n }
      else build_cooccurrence_network(data, col, mf, positive_pmi_only = TRUE)
    }, error = function(e) e)
    if (inherits(net, "error")) { pipe_res(list(error = net$message)); return() }
    g <- net$graph

    # Step 2
    set.seed(shared_data$active_params$random_seed)
    comm <- tryCatch(cluster_louvain(g, weights = E(g)$weight), error = function(e) e)
    if (inherits(comm, "error")) { pipe_res(list(error = comm$message)); return() }
    memb <- data.frame(Herb = names(membership(comm)), Community = membership(comm), stringsAsFactors = FALSE)
    ids <- sort(unique(memb$Community))

    # Step 3
    core <- tryCatch(extract_core_kcore_freq(g, comm, kcq, shared_data$active_params$m3_min_nodes), error = function(e) e)
    if (inherits(core, "error")) { pipe_res(list(error = core$message)); return() }

    # E: warn if any communities were skipped as too small
    if (!is.null(core$small_communities) && length(core$small_communities) > 0) {
      showNotification(sprintf("Skipped %d small community(s) (<5 herbs each) in core formula extraction: C%s",
                               length(core$small_communities),
                               paste(core$small_communities, collapse=", C")),
                       type = "warning", duration = 15)
    }

    # Step 4
    radar_list <- list()
    if (!is.null(shared_data$herb_attr) && nrow(shared_data$herb_attr) > 0) {
      for (cid in ids) {
        herbs_c <- memb$Herb[memb$Community == cid]
        dist <- compute_property_count(herbs_c, shared_data$herb_attr)
        nn <- length(herbs_c)
        radar_list[[paste0("C", cid)]] <- list(
          sx = prepare_single_radar(dist$四性, RADAR_LEVELS$sx, nn),
          ww = prepare_single_radar(dist$五味, RADAR_LEVELS$ww, nn),
          gj = prepare_single_radar(dist$归经, RADAR_LEVELS$gj, nn)
        )
      }
    }

    ns_text <- sprintf("Nodes: %d  |  Edges: %d  |  Density: %.4f  |  Avg Degree: %.1f",
                       vcount(g), ecount(g), edge_density(g), mean(degree(g)))

    # Pre-compute concentric layout data for all communities
    cg <- lapply(ids, function(cid) {
      nc <- memb$Herb[memb$Community==cid]
      if (length(nc) < 2) return(NULL)
      gs <- induced_subgraph(g, nc)
      if (vcount(gs) > 30) { dg <- degree(gs); gs <- induced_subgraph(gs, names(sort(dg, decreasing=TRUE)[1:30])) }
      # Use strength from all_strength_rank (k-core result) for consistency with the table
      sr <- core$all_strength_rank
      sv <- setNames(sr$Strength[sr$Community == cid], sr$Herb[sr$Community == cid])
      sv <- sv[V(gs)$name]
      qq <- quantile(sv, probs=c(0.33,0.66), na.rm=TRUE)
      ly <- ifelse(sv>=qq[2],1L,ifelse(sv>=qq[1],2L,3L)); names(ly) <- V(gs)$name
      xx <- numeric(vcount(gs)); yy <- numeric(vcount(gs)); names(xx) <- names(yy) <- V(gs)$name
      for(l in sort(unique(ly))) {
        ii <- names(ly)[ly==l]; if(length(ii)==0) next
        sv2 <- sv[ii]; ii <- ii[order(sv2, decreasing=TRUE)]
        th <- seq(0,2*pi,length.out=length(ii)+1)[-(length(ii)+1)]
        rl <- ifelse(l==1,60,ifelse(l==2,160,280))
        xx[ii] <- rl*cos(th); yy[ii] <- rl*sin(th)
      }
      mx <- max(sv); mn <- min(sv)
      sn <- if(mx>mn) (sv-mn)/(mx-mn) else rep(0.5, vcount(gs))
      bg <- sapply(sn,function(v){
        vs <- 1 - v
        r1<-as.integer(230 + vs*25); g1<-as.integer(57 + vs*186); b1<-as.integer(70 + vs*135)
        sprintf("#%02X%02X%02X",r1,g1,b1)
      })
      names(bg) <- V(gs)$name
      list(
        nd=data.frame(id=V(gs)$name,label=V(gs)$name,x=xx[V(gs)$name],y=yy[V(gs)$name],
          value=sv,bg=bg,stringsAsFactors=FALSE),
        ed=as_data_frame(gs,what="edges")
      )
    })
    names(cg) <- paste0("C",ids)

    res <- list(
      net_stats = ns_text,
      n_comm = length(ids),
      modularity = modularity(comm),
      core = core,
      memb = memb,
      ids = ids,
      cg = cg,
      radar = radar_list,
      edge_data = net$edge_data,
      edge_weight_mode = wt,
      edge_weight_column = if (wt == "Frequency") "freq" else if (wt == "Hybrid") "weight" else "PMI"
    )

    rm(g); gc()
    choices <- paste0("C", ids)
    updateSelectInput(session, "result_comm", choices = choices, selected = choices[1])
    pipe_res(res)
    shared_data$module_results$m3 <- res
    updateSelectInput(session, "dl_communities", choices = choices, selected = choices[1])
    showNotification(paste("Done!", length(ids), "communities."), type = "message")
  })

  # OUTPUTS
  output$net_summary <- renderText({
    lang <- runtime_lang(shared_data)
    r <- pipe_res()
    if (is.null(r)) return(runtime_tr("Click Run Full Pipeline to start.", lang))
    if (!is.null(r$error)) return(paste(runtime_tr("Error:", lang), r$error))
    if (is.null(r$ids) || length(r$ids)==0) return(runtime_tr("No communities found.", lang))
    stats <- r$net_stats
    if (identical(lang, "zh")) {
      stats <- gsub("Nodes:", "节点：", stats, fixed = TRUE)
      stats <- gsub("Edges:", "边：", stats, fixed = TRUE)
      stats <- gsub("Density:", "密度：", stats, fixed = TRUE)
      stats <- gsub("Avg Degree:", "平均度：", stats, fixed = TRUE)
    }
    paste0(stats, "\n", runtime_tr("Communities: ", lang), r$n_comm, "  | ", runtime_tr("Modularity: ", lang), round(r$modularity, 3))
  })

  output$dt_core <- renderDT({
    lang <- runtime_lang(shared_data)
    r <- pipe_res()
    if (is.null(r) || !is.null(r$error) || is.null(r$core)) return(datatable(localized_placeholder("No data", lang), rownames=FALSE))
    d <- r$core$core_formula_table
    if (is.null(d)) return(datatable(localized_placeholder("No core formulas", lang), rownames=FALSE))
    d$Community <- paste0("C", d$Community)
    if ("Note" %in% names(d) && identical(lang, "zh"))
      d$Note <- sub("^extracted: ([0-9]+) herbs after ([0-9]+) iterations$", "经 \\2 次迭代提取 \\1 味药物", d$Note)
    datatable(d, options=list(pageLength=15), rownames=FALSE)
  })

  output$core_formula_text <- renderText({
    lang <- runtime_lang(shared_data)
    r <- pipe_res()
    if (is.null(r) || !is.null(r$error) || is.null(r$ids) || length(r$ids)==0) return("")
    s <- .first_valid(r, input$result_comm)
    cid <- as.integer(gsub("C","",s))
    cf <- r$core$core_formula_table
    if (is.null(cf)) return("")
    row <- cf[cf$Community==cid,]
    herbs <- sort(r$memb$Herb[r$memb$Community==cid])
    paste0(s, " - ", length(herbs), runtime_tr(" herbs", lang), "  |  ", runtime_tr("Core Formula: ", lang), if(nrow(row)>0) row$Core_Formula[1] else "")
  })

  output$dt_comm_herbs <- renderDT({
    lang <- runtime_lang(shared_data)
    r <- pipe_res()
    if (is.null(r) || !is.null(r$error) || is.null(r$core) || is.null(r$core$all_strength_rank))
      return(datatable(data.frame(stats::setNames(list(runtime_tr("No data", lang), 0), c(runtime_tr("Herb", lang), runtime_tr("Strength", lang))), check.names = FALSE), rownames=FALSE))
    s <- .first_valid(r, input$result_comm)
    cid <- as.integer(gsub("C","",s))
    sr <- r$core$all_strength_rank
    # Use the SAME strength values as the concentric layout (k-core extraction result)
    df <- sr[sr$Community==cid, c("Herb","Strength")]
    df <- df[order(-df$Strength),]; rownames(df) <- NULL
    datatable(df, options=list(pageLength=15,dom="tp"), rownames=FALSE) %>% formatRound("Strength",4)
  })

  output$comm_herb_title <- renderText({
    lang <- runtime_lang(shared_data)
    r <- pipe_res()
    if (is.null(r) || !is.null(r$error) || is.null(r$ids) || length(r$ids)==0) return(runtime_tr("Community Herb List", lang))
    s <- .first_valid(r, input$result_comm)
    cid <- as.integer(gsub("C","",s))
    paste0(s, " - ", sum(r$memb$Community==cid), runtime_tr(" herbs", lang))
  })

  output$comm_subgraph <- renderPlotly({
    lang <- runtime_lang(shared_data)
    r <- pipe_res()
    if (is.null(r) || !is.null(r$error) || is.null(r$cg) || length(r$cg)==0)
      return(plotly_empty() %>% layout(paper_bgcolor="#fff",plot_bgcolor="#fff"))
    s <- .first_valid(r, input$result_comm)
    sg <- r$cg[[s]]
    if (is.null(sg) || is.null(sg$nd) || nrow(sg$nd)==0)
      return(plotly_empty() %>% layout(paper_bgcolor="#fff",plot_bgcolor="#fff"))

    nd <- sg$nd; ed <- sg$ed

    # Edge segments with NAs
    ex <- c(); ey <- c()
    for (i in seq_len(nrow(ed))) {
      f <- match(ed$from[i], nd$id); t <- match(ed$to[i], nd$id)
      if (!is.na(f) && !is.na(t)) {
        ex <- c(ex, nd$x[f], nd$x[t], NA)
        ey <- c(ey, nd$y[f], nd$y[t], NA)
      }
    }

    plot_ly() %>%
      add_trace(x=ex, y=ey, type="scatter", mode="lines",
                line=list(color="rgba(180,180,180,0.20)", width=1),
                hoverinfo="none", showlegend=FALSE) %>%
      add_trace(x=nd$x, y=nd$y, type="scatter", mode="markers+text",
                text=nd$label, textposition="middle center",
                textfont=list(size=11, color="#111111",
                              family="sans-serif"),
                marker=list(
                  size=15 + nd$value/max(nd$value)*22,
                  color=nd$bg,
                  line=list(color="#ccc", width=0.6)
                ),
                hoverinfo="text",
                hovertext=paste0(nd$label,"<br>", runtime_tr("Strength: ", lang),round(nd$value,4)),
                showlegend=FALSE) %>%
      layout(
        xaxis=list(visible=FALSE, range=c(-350,350), fixedrange=FALSE, scaleanchor="y", scaleratio=1),
        yaxis=list(visible=FALSE, range=c(-350,350), fixedrange=FALSE),
        paper_bgcolor="#fff", plot_bgcolor="#fff",
        margin=list(t=5,b=5,l=5,r=5),
        hovermode="closest",
        dragmode="pan"
      ) %>%
      config(displayModeBar=TRUE,
             modeBarButtonsToRemove=list("sendDataToCloud","select2d","lasso2d",
               "autoScale2d","hoverClosestCartesian","hoverCompareCartesian"))
  })

  output$radar_sx <- renderPlotly({
    r <- pipe_res()
    if (is.null(r) || is.null(r$radar) || length(r$radar)==0) return(plotly_empty() %>% layout(paper_bgcolor="#fff",plot_bgcolor="#fff"))
    s <- input$result_comm; choices <- names(r$radar)
    if (!(length(s)==1 && s %in% choices)) s <- choices[1]
    if (!(s %in% choices)) return(plotly_empty() %>% layout(paper_bgcolor="#fff",plot_bgcolor="#fff"))
    build_radar_plotly(r$radar[[s]]$sx, paste(s, "-", runtime_tr("Medicinal properties", runtime_lang(shared_data))), "#3776D6")
  })
  output$radar_ww <- renderPlotly({
    r <- pipe_res()
    if (is.null(r) || is.null(r$radar) || length(r$radar)==0) return(plotly_empty() %>% layout(paper_bgcolor="#fff",plot_bgcolor="#fff"))
    s <- input$result_comm; choices <- names(r$radar)
    if (!(length(s)==1 && s %in% choices)) s <- choices[1]
    if (!(s %in% choices)) return(plotly_empty() %>% layout(paper_bgcolor="#fff",plot_bgcolor="#fff"))
    build_radar_plotly(r$radar[[s]]$ww, paste(s, "-", runtime_tr("Medicinal flavors", runtime_lang(shared_data))), "#4DAF4A")
  })
  output$radar_gj <- renderPlotly({
    r <- pipe_res()
    if (is.null(r) || is.null(r$radar) || length(r$radar)==0) return(plotly_empty() %>% layout(paper_bgcolor="#fff",plot_bgcolor="#fff"))
    s <- input$result_comm; choices <- names(r$radar)
    if (!(length(s)==1 && s %in% choices)) s <- choices[1]
    if (!(s %in% choices)) return(plotly_empty() %>% layout(paper_bgcolor="#fff",plot_bgcolor="#fff"))
    build_radar_plotly(r$radar[[s]]$gj, paste(s, "-", runtime_tr("Meridian tropism", runtime_lang(shared_data))), "#E41A1C")
  })

  output$dl_core <- downloadHandler(
    filename = function() localized_filename("core_formulas", "csv", runtime_lang(shared_data)),
    content = function(file) {
      r <- pipe_res(); req(r, !is.null(r$core))
      d <- r$core$core_formula_table; d$Community <- paste0("C", d$Community)
      write.csv(localized_columns(d, runtime_lang(shared_data)), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  output$dl_zip <- downloadHandler(
    filename = function() localized_filename("tcm_communities", "zip", runtime_lang(shared_data)),
    content = function(file) {
      r <- pipe_res()
      if (is.null(r) || is.null(r$cg)) {
        showNotification("No results to download. Run pipeline first.", type = "message")
        return()
      }
      sel <- input$dl_communities
      if (is.null(sel) || length(sel) == 0) {
        showNotification("Select at least one community.", type = "message")
        return()
      }
      tmpdir <- tempfile(); dir.create(tmpdir)
      for (s in sel) {
        cid <- as.integer(gsub("C", "", s))
        comm_dir <- file.path(tmpdir, s); dir.create(comm_dir)

        if (isTRUE(input$dl_table)) {
          sr <- r$core$all_strength_rank
          df <- sr[sr$Community == cid, c("Herb", "Strength")]
          df <- df[order(-df$Strength), ]; rownames(df) <- NULL
          lang <- runtime_lang(shared_data)
          write.csv(localized_columns(df, lang),
                    file.path(comm_dir, paste0(s, "_", runtime_tr("herb_table", lang), ".csv")),
                    row.names = FALSE, fileEncoding = "UTF-8")
        }

        if (isTRUE(input$dl_net)) {
          sg <- r$cg[[s]]
          if (!is.null(sg) && !is.null(sg$nd) && nrow(sg$nd) > 0) {
            nd <- sg$nd; ed <- sg$ed
            es <- data.frame(x=NA_real_, y=NA_real_, xend=NA_real_, yend=NA_real_)
            for (i in seq_len(nrow(ed))) {
              f <- match(ed$from[i], nd$id); t <- match(ed$to[i], nd$id)
              if (!is.na(f) && !is.na(t))
                es <- rbind(es, data.frame(x=nd$x[f],y=nd$y[f],xend=nd$x[t],yend=nd$y[t]))
            }
            p <- ggplot() +
              geom_segment(data=es, aes(x=x,y=y,xend=xend,yend=yend),
                           color="grey85", linewidth=0.3, na.rm=TRUE) +
              geom_point(data=nd, aes(x=x,y=y,size=value,fill=I(bg)),
                         shape=21, color="grey70", stroke=0.3) +
              geom_text(data=nd, aes(x=x,y=y,label=label),
                        size=2.8, color="#111111", fontface="bold") +
              scale_size_continuous(range=c(5,18), guide="none") +
              coord_fixed(xlim=c(-330,330),ylim=c(-330,330)) + theme_void()
            ggsave(file.path(comm_dir, paste0(s, "_network.png")), p,
                   width=10, height=10, dpi=200, bg="white")
          }
        }

        if (isTRUE(input$dl_radar) && !is.null(r$radar) && s %in% names(r$radar)) {
          rd <- r$radar[[s]]
          png(file.path(comm_dir, paste0(s, "_radar.png")), width=1800, height=600, res=200)
          par(mfrow=c(1,3), mar=c(3,2,3,2))
          plot_radar_single(rd$sx, paste(s, "-", runtime_tr("Medicinal properties", lang)))
          plot_radar_single(rd$ww, paste(s, "-", runtime_tr("Medicinal flavors", lang)))
          plot_radar_single(rd$gj, paste(s, "-", runtime_tr("Meridian tropism", lang)))
          par(mfrow=c(1,1)); dev.off()
        }
      }
      owd <- setwd(tmpdir)
      utils::zip(file, list.files(tmpdir, recursive = TRUE))
      setwd(owd)
      unlink(tmpdir, recursive = TRUE)
    }
  )

  output$dict_status <- renderUI({
    d <- shared_data$herb_attr
    if (!is.null(d)) span(class = "text-success", runtime_tr("Loaded: %d herbs", runtime_lang(shared_data), nrow(d)))
    else span(class = "text-warning", "Not loaded.")
  })
  observeEvent(input$custom_attr, {
    req(input$custom_attr)
    shared_data$herb_attr <- tryCatch(read_excel(input$custom_attr$datapath), error = function(e) NULL)
  })
  output$dt_herb_attr <- renderDT({
    lang <- runtime_lang(shared_data)
    d <- shared_data$herb_attr
    if (is.null(d)) return(datatable(localized_placeholder("No property dict", lang, "Tip"), rownames = FALSE))
    datatable(d, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
}

build_radar_plotly <- function(vec, title_str, col) {
  df <- data.frame(axis = names(vec), value = as.numeric(vec))
  plot_ly() %>%
    add_trace(data = df, type = "scatterpolar", mode = "lines+markers",
              r = ~value, theta = ~axis, fill = "toself",
              fillcolor = paste0("rgba(", paste(col2rgb(col), collapse = ","), ",0.15)"),
              line = list(color = col, width = 2), marker = list(color = col, size = 6), name = title_str) %>%
    layout(polar = list(radialaxis = list(range = c(0, max(0.01, max(df$value) * 1.1)),
                        tickformat = ".0%", nticks = 4, tickangle = 0, tickfont = list(size = 9), showline = TRUE),
                        angularaxis = list(showline = TRUE)),
           title = title_str, showlegend = FALSE, margin = list(t = 40, b = 20, l = 40, r = 40),
           dragmode = FALSE, hovermode = "closest") %>%
    config(displayModeBar = FALSE, scrollZoom = FALSE, editable = FALSE)
}

# ============================================================
# app.R — Core TCM Prescription Mining Platform
# Layout: Top Navbar -> Secondary Tabs -> Left Params / Right Results
# ============================================================

# ===== Load foundations =====
source("global.R")

# ===== Load utility functions =====
util_files <- c(
  "utils_helpers.R", "utils_data_cleaning.R", "utils_network.R",
  "utils_community.R", "utils_core_extraction.R", "utils_validation.R",
  "utils_radar.R", "utils_concentric.R", "utils_zhengsu.R",
  "utils_network_enhance.R", "utils_i18n.R"
)
for (f in util_files) {
  path <- file.path("R", f)
  if (file.exists(path)) source(path)
  else message("Missing util: ", path)
}
check_i18n_keys()

# ===== Load module files =====
mod_files <- list.files("R/", pattern = "^mod_", full.names = TRUE)
for (f in mod_files) source(f)

# ============================================================
# UI
# ============================================================
ui <- dashboardPage(
  skin = "blue",

  # ---- Header with Top Navigation ----
  dashboardHeader(
    title = tags$span("Core TCM", style = "font-weight:bold;"),
    titleWidth = 380,

    # Custom top navigation bar
    tags$li(class = "topnav-container dropdown",
      uiOutput("topnav_content")
    ),

    tags$li(class = "header-language dropdown",
      div(class = "header-language-control",
        selectInput("app_lang", tags$span(class = "sr-only", "Language / 语言"),
                    choices = c("中文" = "zh", "English" = "en"),
                    selected = "zh", width = "110px", selectize = FALSE)
      )
    ),

    # Global Settings gear icon
    tags$li(uiOutput("global_settings_button"), class = "header-settings dropdown")
  ),

  # ---- Sidebar: Dynamic Parameter Panel ----
  dashboardSidebar(
    width = 380,
    uiOutput("sidebar_content")
  ),

  # ---- Body: Secondary Tabs + Results ----
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css?v=4"),
      i18n_client_script(),
      tags$style(HTML("
        /* ---- Button colors: ALL #79CDCD ---- */
        .skin-blue .main-sidebar .btn.btn-primary,
        .skin-blue .main-sidebar .btn-primary.btn-block,
        .skin-blue .main-sidebar a.btn-primary,
        .skin-blue .main-sidebar .btn-primary,
        .skin-blue .sidebar-action-bar .btn-primary,
        .skin-blue .main-sidebar .btn.btn-success,
        .skin-blue .main-sidebar .btn-success.btn-block,
        .skin-blue .main-sidebar a.btn-success,
        .skin-blue .main-sidebar .btn-success,
        .skin-blue .sidebar-action-bar .btn-success,
        .skin-blue .main-sidebar .btn-sm.btn-block,
        .skin-blue .main-sidebar a.btn-sm,
        .skin-blue .main-sidebar .btn-sm,
        .skin-blue .sidebar-action-bar .btn-sm {
          background-color: #79CDCD !important;
          border-color: #79CDCD !important;
          color: #2c3e50 !important;
        }
        .skin-blue .main-sidebar .btn-primary:hover,
        .skin-blue .main-sidebar .btn-success:hover,
        .skin-blue .main-sidebar .btn-sm:hover {
          background-color: #69BDBD !important;
          border-color: #69BDBD !important;
          color: #2c3e50 !important;
        }

        /* ---- Action bar: JS fixes it to sidebar bottom ---- */
        .sidebar-action-bar {
          box-sizing: border-box;
          background: #ffffff;
          padding: 14px 18px 16px 18px;
          border-top: 2px solid #dee2e6;
          box-shadow: 0 -4px 12px rgba(0,0,0,0.12);
          text-align: center;
        }

        /* ---- Run buttons: full-width ---- */
        .skin-blue .main-sidebar .sidebar-action-bar .btn-block {
          display: block !important;
          width: 100% !important;
          float: none !important;
          margin-left: 0 !important;
          margin-right: 0 !important;
        }

        /* ---- Reset buttons: centered, auto-width ---- */
        .skin-blue .main-sidebar .sidebar-action-bar .btn-sm {
          display: inline-block !important;
          width: auto !important;
          min-width: 120px !important;
          float: none !important;
        }
        .skin-blue .main-sidebar .sidebar-action-bar .btn-sm.btn-block {
          display: table !important;
          width: auto !important;
          min-width: 120px !important;
          margin-left: auto !important;
          margin-right: auto !important;
          float: none !important;
        }

        /* ---- Alert boxes: #EE6363 left border, light pink bg ---- */
        .skin-blue .main-sidebar .alert {
          border: 1px solid #e9ecef !important;
          border-left: 3px solid #EE6363 !important;
          background-color: #fef0f0 !important;
          color: #2c3e50 !important;
        }
        .content-wrapper .alert,
        .right-side .alert {
          border: 1px solid #e9ecef !important;
          border-left: 3px solid #EE6363 !important;
          background-color: #fef0f0 !important;
          color: #2c3e50 !important;
        }

        /* ---- Result boxes: all #E0EEEE ---- */
        .box.box-solid.box-primary,
        .box.box-solid.box-success,
        .box.box-solid.box-info,
        .box.box-solid.box-warning {
          border-top-color: #E0EEEE !important;
        }
        .box.box-solid.box-primary > .box-header,
        .box.box-solid.box-success > .box-header,
        .box.box-solid.box-info > .box-header,
        .box.box-solid.box-warning > .box-header,
        .box.box-primary > .box-header,
        .box.box-success > .box-header,
        .box.box-info > .box-header,
        .box.box-warning > .box-header {
          background-color: #E0EEEE !important;
          color: #2c3e50 !important;
        }
        .box.box-primary,
        .box.box-success,
        .box.box-info,
        .box.box-warning {
          border-top: 3px solid #E0EEEE !important;
        }

        /* ---- Tab box nav-tabs: #E0EEEE ---- */
        .nav-tabs-custom .nav-tabs,
        .nav-tabs-custom .nav.nav-tabs,
        .box .nav-tabs,
        .tab-content .nav-tabs {
          background-color: #E0EEEE !important;
        }
        .nav-tabs-custom .nav-tabs li.active > a,
        .nav-tabs-custom .nav.nav-tabs li.active > a,
        .box .nav-tabs li.active > a {
          background-color: #ffffff !important;
          border-bottom-color: #ffffff !important;
        }
        .nav-tabs-custom .nav-tabs > li > a,
        .box .nav-tabs > li > a {
          color: #2c3e50 !important;
        }

        /* ---- Slider progress bar: #B0E0E6 ---- */
        .skin-blue .main-sidebar .irs-bar,
        .skin-blue .main-sidebar .irs-bar-edge,
        .skin-blue .main-sidebar .irs-single,
        .skin-blue .main-sidebar .irs-from,
        .skin-blue .main-sidebar .irs-to {
          background: #B0E0E6 !important;
          border-color: #B0E0E6 !important;
        }
        .skin-blue .main-sidebar .irs-slider {
          border-color: #B0E0E6 !important;
        }
        .main-sidebar .irs-bar,
        .main-sidebar .irs-bar-edge,
        .main-sidebar .irs-single,
        .main-sidebar .irs-from,
        .main-sidebar .irs-to {
          background: #B0E0E6 !important;
          border-color: #B0E0E6 !important;
        }
        .main-sidebar .irs-slider {
          border-color: #B0E0E6 !important;
        }
        /* Right-side content slider: #B0E0E6 */
        .content-wrapper .irs-bar,
        .content-wrapper .irs-bar-edge,
        .content-wrapper .irs-single,
        .content-wrapper .irs-from,
        .content-wrapper .irs-to,
        .right-side .irs-bar,
        .right-side .irs-bar-edge,
        .right-side .irs-single,
        .right-side .irs-from,
        .right-side .irs-to {
          background: #B0E0E6 !important;
          border-color: #B0E0E6 !important;
        }
        .content-wrapper .irs-slider,
        .right-side .irs-slider {
          border-color: #B0E0E6 !important;
        }

        /* ---- Data Management: valueBox + download colors ---- */
        .small-box.bg-blue { background-color: #EEA2AD !important; }
        .small-box.bg-green { background-color: #7CCD7C !important; }
        #m0-dl_clean { background-color: #EEA2AD !important; border-color: #EEA2AD !important; color: #2c3e50 !important; }
        #m0-dl_xlsx  { background-color: #7CCD7C !important; border-color: #7CCD7C !important; color: #2c3e50 !important; }

        /* ---- Right-side download buttons: #79CDCD (m0 excluded by ID priority) ---- */
        .content-wrapper .btn.btn-default,
        .right-side .btn.btn-default,
        .content-wrapper .btn-sm,
        .right-side .btn-sm {
          background-color: #79CDCD !important;
          border-color: #79CDCD !important;
          color: #2c3e50 !important;
        }
        .content-wrapper .btn.btn-default:hover,
        .right-side .btn.btn-default:hover,
        .content-wrapper .btn-sm:hover,
        .right-side .btn-sm:hover {
          background-color: #69BDBD !important;
          border-color: #69BDBD !important;
          color: #2c3e50 !important;
        }
      ")),

      # Top nav click handler
      tags$script(HTML("
        $(document).on('click', '.topnav-item', function(e) {
          e.preventDefault();
          $('.topnav-item').removeClass('active');
          $(this).addClass('active');
          Shiny.setInputValue('active_topnav', $(this).attr('data-tn'));
        });

        $(document).on('click', '.subtab-item', function(e) {
          e.preventDefault();
          var $this = $(this);
          var section = $this.closest('.subtab-bar').attr('data-section');
          $this.siblings().removeClass('active');
          $this.addClass('active');
          Shiny.setInputValue(section, $this.attr('data-st'));
        });
      "))
    ),

    # Fix sidebar positioning
    tags$script(HTML("
      var SIDEBAR_W = '380px';

      function fixAllLayout() {
        var headerH = Math.ceil($('.main-header').outerHeight()) || 50;
        // Logo — floats in fixed header, no separate fixed positioning needed
        $('.main-header .logo').css({
          float: 'left', display: 'block',
          width: SIDEBAR_W, minWidth: SIDEBAR_W, maxWidth: SIDEBAR_W,
          boxSizing: 'border-box',
          height: '50px', lineHeight: '50px',
          backgroundColor: '#ffffff', color: '#2c3e50',
          padding: '0', margin: '0',
          border: 'none', borderBottom: '1px solid #dee2e6',
          borderRight: '1px solid #dee2e6',
          textAlign: 'center'
        });
        // Sidebar — exact 380px, white bg, NO gap-causing properties
        $('.main-sidebar').css({
          position: 'fixed', top: headerH + 'px', left: '0',
          width: SIDEBAR_W, minWidth: SIDEBAR_W, maxWidth: SIDEBAR_W,
          boxSizing: 'border-box', overflow: 'auto', zIndex: 810,
          height: 'calc(100vh - ' + headerH + 'px)',
          backgroundColor: '#ffffff',
          padding: '0', margin: '0',
          border: 'none', borderRight: '1px solid #dee2e6',
          paddingBottom: '250px'
        });
        $('.main-sidebar .sidebar').css({
          paddingBottom: '250px', backgroundColor: '#ffffff'
        });
        // Content — flush against sidebar (left = 380px)
        // If subtab bar exists and is fixed, push content down by its height
        var $visibleSubtabs = $('.subtab-bar:visible');
        var hasSubtabs = $visibleSubtabs.length > 0;
        if (hasSubtabs) {
          $('.subtab-bar').css({
            position: 'fixed', top: headerH + 'px', left: SIDEBAR_W, right: '0',
            zIndex: 790, margin: '0',
            paddingLeft: '0'
          });
        }
        $('.content-wrapper, .right-side').css({
          margin: '0', marginLeft: SIDEBAR_W,
          padding: '0', border: 'none', overflow: 'visible',
          paddingTop: hasSubtabs ? Math.ceil($visibleSubtabs.first().outerHeight()) + 'px' : '0'
        });
        $('.content, section.content').css({
          padding: '0 15px 15px 0', margin: '0', border: 'none'
        });
        $('.wrapper').css({
          height: 'auto', overflow: 'visible',
          paddingTop: headerH + 'px', margin: '0'
        });
        $('.content-header, section.content-header').hide();
        // Zero the entire topnav chain: ul.navbar-nav → li → nav → first a
        $('.main-header .navbar .nav, .main-header .navbar .navbar-nav').css({margin: '0', padding: '0'});
        $('.topnav-container').css({padding: '0', margin: '0'});
        $('.topnav').css({padding: '0', margin: '0'});
        $('.main-header .navbar').css({
          marginLeft: SIDEBAR_W, backgroundColor: '#fff',
          height: '50px', minHeight: '50px'
        });
        // Action bar
        var $bar = $('.sidebar-action-bar').last();
        if ($bar.length) {
          $bar.css({
            position: 'fixed', bottom: '0', left: '0',
            width: SIDEBAR_W, zIndex: 900,
            backgroundColor: '#ffffff',
            borderTop: '2px solid #dee2e6',
            boxShadow: '0 -4px 12px rgba(0,0,0,0.12)',
            padding: '14px 18px 16px 18px',
            textAlign: 'center', boxSizing: 'border-box'
          });
          // Dynamic padding to prevent parameter occlusion
          var barH = $bar.outerHeight(true) + 20;
          $('.main-sidebar').css({paddingBottom: barH + 'px'});
          $('.main-sidebar .sidebar').css({paddingBottom: barH + 'px'});
        }
      }

      $(document).ready(function() {
        $('.wrapper').css({height: 'auto', overflow: 'visible'});
        $('body').css({overflow: 'auto', height: 'auto', backgroundColor: '#ffffff'});
        $('html').css({overflow: 'auto', height: 'auto', backgroundColor: '#ffffff'});
        fixAllLayout();
        // English labels and narrow viewports can increase header height.
        // Keep fixed panels below the actual header, including after renderUI.
        var layoutQueued = false;
        function scheduleLayout() {
          if (layoutQueued) return;
          layoutQueued = true;
          requestAnimationFrame(function() {
            layoutQueued = false;
            fixAllLayout();
          });
        }
        $(window).on('resize.coretcm', scheduleLayout);
        $(document).on('shiny:value shiny:visualchange', scheduleLayout);
        if (window.ResizeObserver) {
          var headerObserver = new ResizeObserver(scheduleLayout);
          headerObserver.observe(document.querySelector('.main-header'));
        }

        // Re-fix on sidebar content change (renderUI switches between modules)
        var sidebarEl = document.querySelector('.main-sidebar .sidebar');
        if (sidebarEl) {
          var observer = new MutationObserver(function() {
            setTimeout(fixAllLayout, 200);
          });
          observer.observe(sidebarEl, {childList: true, subtree: true});
        }
      });
    ")),

    # Global Settings Modal
    tags$div(class = "modal fade", id = "globalSettingsModal",
      tags$div(class = "modal-dialog modal-lg",
          tags$div(class = "modal-content",
            uiOutput("global_settings_header"),
            tags$div(class = "modal-body",
              uiOutput("global_settings_body")
            ),
            tags$div(class = "modal-footer",
              uiOutput("global_settings_footer")
            )
          )
      )
    ),

    # ========================================================
    # BODY CONTENT
    # ========================================================

    # ---- Data Cleaning ----
    conditionalPanel(
      condition = "input.active_topnav == 'data_cleaning' || input.active_topnav == null",
      tags$div(class = "subtab-bar", `data-section` = "subtab_dc",
        uiOutput("subtab_dc_content")
      ),
      conditionalPanel(
        condition = "input.subtab_dc == 'dc_data' || input.subtab_dc == null",
        mod_0_data_ui("m0")
      ),
      conditionalPanel(
        condition = "input.subtab_dc == 'dc_clinical'",
        mod_1_clinical_ui("m1")
      )
    ),

    # ---- Core Herbs ----
    conditionalPanel(
      condition = "input.active_topnav == 'core_herbs'",
      tags$div(class = "subtab-bar", `data-section` = "subtab_ch",
        uiOutput("subtab_ch_content")
      ),
      conditionalPanel(
        condition = "input.subtab_ch == 'ch_desc' || input.subtab_ch == null",
        mod_2_descriptive_ui("m2")
      ),
      conditionalPanel(
        condition = "input.subtab_ch == 'ch_net'",
        mod_3_network_ui("m3")
      ),
      conditionalPanel(
        condition = "input.subtab_ch == 'ch_rules'",
        mod_2_rules_ui("m2")
      ),
      conditionalPanel(
        condition = "input.subtab_ch == 'ch_cluster'",
        mod_2_cluster_ui("m2")
      ),
      conditionalPanel(
        condition = "input.subtab_ch == 'ch_compare'",
        mod_6_comparison_ui("m6")
      )
    ),

    # ---- Validation ----
    conditionalPanel(
      condition = "input.active_topnav == 'validation'",
      mod_4_validation_ui("m4")
    ),

    # ---- Formula-Syndrome Association ----
    conditionalPanel(
      condition = "input.active_topnav == 'fsa'",
      tags$div(class = "subtab-bar", `data-section` = "subtab_fsa",
        uiOutput("subtab_fsa_content")
      ),
      conditionalPanel(
        condition = "input.subtab_fsa == 'fsa_feat' || input.subtab_fsa == null",
        mod_5_feat_ui("m5")
      ),
      conditionalPanel(
        condition = "input.subtab_fsa == 'fsa_rec'",
        mod_5_rec_ui("m5")
      )
    )
  )
)

# ============================================================
# Server
# ============================================================
server <- function(input, output, session) {

  shared_data <- reactiveValues(
    raw            = NULL,
    clean          = NULL,
    clean_col      = NULL,
    raw_formula_col = NULL,
    dose_preserved_col = NULL,
    parsing_warnings = NULL,
    dict           = NULL,
    herb_attr      = NULL,
    active_params  = DEFAULT_PARAMS,
    module_results = list(),
    lang           = "zh"
  )

  observeEvent(input$app_lang, {
    shared_data$lang <- input$app_lang
  }, ignoreInit = FALSE)

  lang <- reactive({
    if (is.null(shared_data$lang)) "zh" else shared_data$lang
  })

  nav_item_class <- function(base, value, selected, default) {
    if (is.null(selected)) selected <- default
    paste(base, if (identical(value, selected)) "active" else "")
  }

  output$topnav_content <- renderUI({
    l <- lang()
    tags$nav(class = "topnav",
      tags$a(href = "#", class = nav_item_class("topnav-item", "data_cleaning", input$active_topnav, "data_cleaning"), `data-tn` = "data_cleaning",
             icon("broom"), paste0(" ", tr("nav_data_cleaning", l))),
      tags$a(href = "#", class = nav_item_class("topnav-item", "core_herbs", input$active_topnav, "data_cleaning"), `data-tn` = "core_herbs",
             icon("leaf"), paste0(" ", tr("nav_core_herbs", l))),
      tags$a(href = "#", class = nav_item_class("topnav-item", "validation", input$active_topnav, "data_cleaning"), `data-tn` = "validation",
             icon("check-double"), paste0(" ", tr("nav_validation", l))),
      tags$a(href = "#", class = nav_item_class("topnav-item", "fsa", input$active_topnav, "data_cleaning"), `data-tn` = "fsa",
             icon("map"), paste0(" ", tr("nav_fsa", l)))
    )
  })

  output$global_settings_button <- renderUI({
    a(
      icon("cog"),
      href = "#", `data-toggle` = "modal", `data-target` = "#globalSettingsModal",
      class = "dropdown", style = "color:#495057; padding:15px 18px; font-size:16px;",
      title = tr("global_settings", lang())
    )
  })

  output$subtab_dc_content <- renderUI({
    l <- lang()
    tagList(
      tags$a(href = "#", class = nav_item_class("subtab-item", "dc_data", input$subtab_dc, "dc_data"), `data-st` = "dc_data",
             icon("database"), paste0(" ", tr("sub_data_management", l))),
      tags$a(href = "#", class = nav_item_class("subtab-item", "dc_clinical", input$subtab_dc, "dc_data"), `data-st` = "dc_clinical",
             icon("stethoscope"), paste0(" ", tr("sub_clinical_features", l)))
    )
  })

  output$subtab_ch_content <- renderUI({
    l <- lang()
    tagList(
      tags$a(href = "#", class = nav_item_class("subtab-item", "ch_desc", input$subtab_ch, "ch_desc"), `data-st` = "ch_desc",
             icon("chart-bar"), paste0(" ", tr("sub_descriptive", l))),
      tags$a(href = "#", class = nav_item_class("subtab-item", "ch_rules", input$subtab_ch, "ch_desc"), `data-st` = "ch_rules",
             icon("link"), paste0(" ", tr("sub_rules", l))),
      tags$a(href = "#", class = nav_item_class("subtab-item", "ch_cluster", input$subtab_ch, "ch_desc"), `data-st` = "ch_cluster",
             icon("th-large"), paste0(" ", tr("sub_cluster", l))),
      tags$a(href = "#", class = nav_item_class("subtab-item", "ch_net", input$subtab_ch, "ch_desc"), `data-st` = "ch_net",
             icon("project-diagram"), paste0(" ", tr("sub_network", l))),
      tags$a(href = "#", class = nav_item_class("subtab-item", "ch_compare", input$subtab_ch, "ch_desc"), `data-st` = "ch_compare",
             icon("balance-scale"), paste0(" ", tr("sub_compare", l)))
    )
  })

  output$subtab_fsa_content <- renderUI({
    l <- lang()
    tagList(
      tags$a(href = "#", class = nav_item_class("subtab-item", "fsa_feat", input$subtab_fsa, "fsa_feat"), `data-st` = "fsa_feat",
             icon("chart-bar"), paste0(" ", tr("sub_feature_association", l))),
      tags$a(href = "#", class = nav_item_class("subtab-item", "fsa_rec", input$subtab_fsa, "fsa_feat"), `data-st` = "fsa_rec",
             icon("flask"), paste0(" ", tr("sub_prescription_recommendation", l)))
    )
  })

  output$global_settings_header <- renderUI({
    tags$div(class = "modal-header",
      tags$h4(class = "modal-title", icon("cog"), tr("global_settings", lang())),
      tags$button(type = "button", class = "close", `data-dismiss` = "modal", tags$span("x"))
    )
  })

  output$global_settings_body <- renderUI({
    mod_global_settings_modal("global_settings", lang(), isolate(shared_data$active_params))
  })

  output$global_settings_footer <- renderUI({
    tagList(
      downloadButton("global_settings-export_json", tr("export_json", lang()), class = "btn-info"),
      actionButton("global_settings-apply_global", "Save as Global Defaults", icon = icon("check"), class = "btn-success")
    )
  })

  # ---- Dynamic Sidebar Content ----
  output$sidebar_content <- renderUI({
    tn <- input$active_topnav
    if (is.null(tn)) tn <- "data_cleaning"

    if (tn == "data_cleaning") {
      st <- input$subtab_dc
      if (is.null(st) || st == "dc_data") {
        mod_0_sidebar("m0")
      } else if (st == "dc_clinical") {
        mod_1_sidebar("m1")
      }
    } else if (tn == "core_herbs") {
      st <- input$subtab_ch
      if (is.null(st) || st == "ch_desc") {
        mod_2_descriptive_sidebar("m2")
      } else if (st == "ch_net") {
        mod_3_sidebar("m3")
      } else if (st == "ch_rules") {
        mod_2_rules_sidebar("m2")
      } else if (st == "ch_cluster") {
        mod_2_cluster_sidebar("m2")
      } else if (st == "ch_compare") {
        mod_6_sidebar("m6")
      }
    } else if (tn == "validation") {
      mod_4_sidebar("m4")
    } else if (tn == "fsa") {
      st <- input$subtab_fsa
      if (is.null(st) || st == "fsa_feat") {
        mod_5_sidebar("m5")
      } else if (st == "fsa_rec") {
        mod_5_rec_sidebar("m5")
      }
    } else {
      p("Select a module to see parameters.")
    }
  })

  # ---- Module Servers (same namespaces as before) ----
  callModule(mod_0_data_server,          "m0", shared_data)
  callModule(mod_1_clinical_server,      "m1", shared_data)
  callModule(mod_2_traditional_server,   "m2", shared_data)
  callModule(mod_3_network_server,       "m3", shared_data)
  callModule(mod_4_validation_server,    "m4", shared_data)
  callModule(mod_5_mapping_server,       "m5", shared_data)
  callModule(mod_6_comparison_server,    "m6", shared_data)
  callModule(mod_global_settings_server, "global_settings", shared_data)
}

shinyApp(ui, server)

#' load_parmesan UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom dplyr %>% 
mod_load_parmesan_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("varViewOut")),
    uiOutput(ns("desagregacionOut")),
    uiOutput(ns("aggOut")),
    uiOutput(ns("NumericFilters")),
    uiOutput(ns("filterOptions")),
    uiOutput(ns("NumericRange")),
    uiOutput(ns("controls"))
  )
}

#' load_parmesan Server Functions
#'
#' @noRd 
mod_load_parmesan_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    

    var_opts <- reactive({
      req(r$active_viz)
      if (is.null(r$vars_f)) return()
      catVars <- r$vars_f$vars
      #print(catVars)
      if (r$active_viz %in% c("map", "map_bubbles")) {
        ch <-  catVars[grepl("alcaldia", tolower(catVars))]
      } else if (r$active_viz %in% c("line", "area")) {
        ch <- c("Histórico CDMX", catVars)
      } else {
        ch <- catVars
      }
      ch
    })

    varDef <- reactive({
      req(var_opts())
      var_opts()[1]
    })
    
    
    output$varViewOut <- renderUI({
      req(var_opts())
      shiny::selectizeInput(ns("varViewId"),
                            "Selección de variable a visualizar",
                            choices = var_opts(),
                            selected = varDef())
    })

    desVarOpts <- reactive({
      req(r$dic_f)
      req(r$varViewId)
      req(r$active_viz)
      if (is.null(r$vars_f)) return()
      catVars <- r$vars_f$vars
      print(catVars)
      varPsel <- data.frame(id = c("ninguna", catVars),
                            label = c("Ninguna", catVars))
      varPsel <- varPsel %>% dplyr::filter(id != r$varViewId)

      if (req(r$active_viz) %in% c("map", "map_bubbles"))  {
         setNames(c("ninguna", r$dic_f$label[grepl("colonia", tolower(r$dic_f$label))]),
                  c("Ninguna", r$dic_f$label[grepl("colonia", tolower(r$dic_f$label))]))
      } else {
        setNames(varPsel$id, varPsel$label)
      }
    })
    
    
    output$desagregacionOut <- renderUI({
      req(r$active_viz)
      if (r$active_viz %in% "line") return()
      req(desVarOpts())
     shiny::radioButtons(ns("desagregacionId"),
                          "Desagregar por:",
                          choices = desVarOpts(),
                          selected = "ninguna")
    })


    
    output$aggOut <- renderUI({
      req(r$active_viz)
      #catVars <- r$vars_f$vars
      numVars <- r$allNums
      
      if (r$active_viz %in% "map_bubbles") return()
      if (is.null(numVars)) {
        ch <- setNames(c("count", "pctg"), c("Conteo", "Porcentaje"))
      } else {
        ch <- setNames(c("count", "sum", "mean", "pctg"), c("Conteo", "Suma", "Promedio", "Porcentaje"))
      }
      
      shiny::radioButtons(ns("aggId"), 
                          "Tipo de unidad",
                          choices = ch,
                          selected = "count"
                          )
    })
    
    observe({
      r$varViewId <- input[["varViewId"]]
      r$desagregacionId <- input[["desagregacionId"]]
      r$aggId <- input[["aggId"]]
    })

    # # Filtros q afectan la base -----------------------------------------------
    
    output$filterOptions <- renderUI({
      req(r$vars_f)
      req(r$allCats)
      df <- r$vars_f
      if (nrow(df) > 0) {
        purrr::map(1:nrow(df), function(i) {
          shiny::selectizeInput(inputId = ns(df$id[i]), 
                                label = df$vars[i], 
                                choices = r$allCats[[df$vars[i]]],
                                selected = "Todas", multiple = TRUE, 
                                options = list(plugins = list(
                                  "remove_button",
                                  "drag_drop"))
          )
        })
      } else {
        return()
      }
      
    })
    
    output$NumericFilters <- renderUI({
      req(r$allNums)
      req(r$aggId)
      if (r$aggId == "count") return()
      shiny::selectizeInput(inputId = ns("numericSelected"), 
                            label = "Variable numerica", 
                            choices = r$allNums,
                            selected = r$allNums[1], multiple = TRUE, 
                            options = list(
                              maxItems = 2,
                              plugins = list(
                                "remove_button",
                                "drag_drop"))
      )
    })
    
    output$NumericRange <- renderUI({
      req(r$allNums)
      req(r$numRange)
      if (length(r$allNums) > 0) {
        purrr::map(r$allNums, function(i) {
          #print(i)
          rangeDef <- r$numRange %>% dplyr::filter(id %in% i)
          #print(rangeDef)
          shiny::sliderInput(inputId = ns(paste0(rangeDef$id, "range")),
                             label = rangeDef$id,
                             min = rangeDef$min, 
                             max = rangeDef$max,
                             value = c(rangeDef$min, rangeDef$max)
          )
        })
      } else {
        return()
      }
    })
    
    
    
    observe({
      req(r$allNums)
      r$varNum <- input$numericSelected
      extra_nums <- paste0(r$allNums, "range")
      for(nums_input in extra_nums){
        get_nums_input <- input[[nums_input]]
        r[[nums_input]] <- get_nums_input
      }
    })
    
    observe({
      req(r$vars_f)
      df <- r$vars_f
      extra_inputs <- df$id
      for(extra_input in extra_inputs){
        get_extraInput <- input[[extra_input]]
        r[[extra_input]] <- get_extraInput
      }
    })
    
    # ###########################################################################    
    # 
    observe({
      tryCatch({
        if (is.null(r$labelChange)) return()
        req(r$vars_f)
        varS <- r$vars_f
        
        purrr::map(1:nrow(varS), function(i) {
          varToSel <- isolate({input[[varS$id[i]]]})
          if (length(varToSel) > 1) {
            if ("Todas" %in% varToSel) {
              if (varToSel[[1]] == "Todas") {
                varToSel <- setdiff(varToSel, "Todas")
              } else {
                varToSel <- "Todas"
              } 
            }
          }
          print(varToSel)
          print(input[[varS$id[i]]])
          updateSelectizeInput(session,
                               inputId = varS$id[i],
                               choices = isolate({setNames(paste0(gsub("\\s*\\([^\\)]+\\)","", r$labelChange[[varS$vars[i]]])),
                                                           r$labelChange[[varS$vars[i]]])}),
                               selected = varToSel
          )
        })
      },
      error = function(cond) {
        return()
      })
    })





    plotSel <- reactive({
      req(r$active_viz)
      r$active_viz
    })



    varTwoSel <- reactive({
      req(r$desagregacionId)
      r$desagregacionId != "ninguna"
    })

    stackLabel <- reactive({
      HTML("<span style='margin-left:5px; margin-top: -6px;'>Apilar barras </span>")
    })

    axisLabel <- reactive({
      HTML("<span style='margin-left:5px; margin-top: -6px;'> Invertir selección de Variables </span>")
    })

    sortLabel <- reactive({
      HTML("<span style='margin-left:5px; margin-top: -6px;'> Ordenar </span>")
    })


    colors_default <- reactive({
      req(r$active_viz)
      if (r$active_viz %in% c("map")) {
        list(
          palette_a = c("#1B5C51", "#4E786F", "#66887F", "#7E9992", "#96ACA5", "#AFBFBB", "#C8D4D1", "#E2EBE9"),
          palette_b = c("#B48E5D", "#C3A57D", "#CBB18E", "#D3BDA0", "#DCCAB2", "#E4D6C5", "#EDE3D7", "#F6F1EB"),
          palette_c = c("#0E709E", "#568BB2", "#709ABC", "#88A9C7", "#9FBAD2", "#B5CADD", "#CBDBE8", "#E0EDF3"),
          palette_d = c("#253786", "#52599C", "#696DA9", "#8182B6", "#999AC4", "#B1B1D2", "#CACADE", "#E1E2EB"),
          palette_e = c("#9E2348", "#B15267", "#BB6979", "#C6818D", "#D19AA3", "#DCB3B9", "#E8CCD1", "#F4E5E9"),
          palette_f = c("#B33718", "#C45633", "#CC6644", "#D47657", "#DD876B", "#E69880", "#EFAA96", "#F8BBAD")
        )

      # } else if (r$active_viz == "map_bubbles") {
      #   list(
      #     palette_a = c("#3E9FCC"),
      #     palette_b = c("#93D0F1"),
      #     palette_c = c("#19719F")
      #   )
      } else if (r$active_viz %in% c("treemap")) {
        list(
          palette_a = c("#1B5C51", "#4E786F", "#66887F", "#7E9992", "#96ACA5", "#AFBFBB", "#C8D4D1", "#E2EBE9"),
          palette_b = c("#B48E5D", "#C3A57D", "#CBB18E", "#D3BDA0", "#DCCAB2", "#E4D6C5", "#EDE3D7", "#F6F1EB"),
          palette_c = c("#0E709E", "#568BB2", "#709ABC", "#88A9C7", "#9FBAD2", "#B5CADD", "#CBDBE8", "#E0EDF3"),
          palette_d = c("#253786", "#52599C", "#696DA9", "#8182B6", "#999AC4", "#B1B1D2", "#CACADE", "#E1E2EB"),
          palette_e = c("#9E2348", "#B15267", "#BB6979", "#C6818D", "#D19AA3", "#DCB3B9", "#E8CCD1", "#F4E5E9"),
          palette_f = c("#B33718", "#C45633", "#CC6644", "#D47657", "#DD876B", "#E69880", "#EFAA96", "#F8BBAD")

        )
      } else if (r$active_viz == "bar"){
        req(r$desagregacionId)
        if (r$desagregacionId != "ninguna") {
          list(
            palette_a = c("#3E9FCC", "#8A6BAC", "#EA5254", "#F18951", "#FCC448", "#71B365"),
            palette_b = c("#93D0F1", "#D8CEE4", "#EB9594", "#F9BE9B", "#FFE095", "#CBE3C6"),
            palette_c = c("#19719F", "#5D3A84", "#D02622", "#D16020", "#CF981B", "#438536")
          )
        } else {
          list(
            palette_a = c("#1B5C51"),
            palette_b = c("#B48E5D"),
            palette_c = c("#0E709E"),
            palette_d = c("#253786"),
            palette_e = c("#9E2348"),
            palette_f = c("#B33718")
          )
        }
      } else if (r$active_viz %in% c("line", "area")) {
        req(r$varViewId)
        if(r$varViewId == "Histórico CDMX") {
          list(
            palette_a = c("#1B5C51"),
            palette_b = c("#B48E5D"),
            palette_c = c("#0E709E"),
            palette_d = c("#253786"),
            palette_e = c("#9E2348"),
            palette_f = c("#B33718")
          )
        } else {
          list(
            palette_a = c("#3E9FCC", "#8A6BAC", "#EA5254", "#F18951", "#FCC448", "#71B365"),
            palette_b = c("#93D0F1", "#D8CEE4", "#EB9594", "#F9BE9B", "#FFE095", "#CBE3C6"),
            palette_c = c("#19719F", "#5D3A84", "#D02622", "#D16020", "#CF981B", "#438536")
          )
        }
      } else {
        return()
      }

    })

    colors_show <- reactive({
      if (is.null(colors_default())) return()
      cd <- colors_default()
      lc <- purrr::map(names(cd), function(palette) {
        # palette <- "palette_a"
        colors <- cd[[palette]]
        as.character( div(
          purrr::map(colors, function(color) {
            div(style=paste0("width: 20px; height: 20px; display: inline-block; background-color:", color, ";"))
          })
        ))
      })
      names(lc) <- names(cd)
      lc
    })

    agg_palette <- reactive({
      if (is.null(r$active_viz)) return()
      if (is.null(colors_show())) return()
      colors_show()
    })


    # fec_opts <- reactive({
    #   setNames(c("FechaInicioR", "Año_hecho"),
    #            c("Fecha en que se hizo la denuncia",
    #              "Fecha en que se cometió el delito"))
    # })
    # 
    # fec_select <- reactive({
    #   #req(fec_opts())
    #   #fec_opts()[1]
    #   "FechaInicioR"
    # })

    # maxIn <- reactive({
    #   req(r$d_sel)
    #   df <- r$d_sel
    #   max(lubridate::dmy(df$FechaInicio), na.rm = TRUE)
    # })
    # 
    # minIn <- reactive({
    #   req(r$d_sel)
    #   df <- r$d_sel
    #   min(lubridate::dmy(df$FechaInicio), na.rm = TRUE)
    # })


    # anioHolder <- reactive({
    #   req(maxIn())
    #   req(minIn())
    #   #c(minIn(), maxIn())
    #   paste0(format(minIn(), format="%Y-%m"), " al ", format(maxIn(), format="%Y-%m"))
    # })


    # Initialize parmesan
    path <- app_sys("app/app_config/parmesan")
    parmesan <- parmesan::parmesan_load(path)
    parmesan_input <- parmesan::parmesan_watch(input, parmesan)

    parmesan::output_parmesan("controls",
                              parmesan = parmesan,
                              #r = r,
                              input = input,
                              output = output,
                              session = session,
                              env = environment())
    # # ======================================================================================================================
    # Pass all inputs from parmesan to other parts of the app as reactiveValues
    parmesan_inputs <- purrr::map(parmesan, function(.x) { purrr::map_chr(.x$inputs, "id")}) %>% unlist(use.names = FALSE)

    observe({
      for(parmesan_input in parmesan_inputs){

        get_input <- input[[parmesan_input]]
        #if(!is.null(get_input)){
        r[[parmesan_input]] <- isolate(get_input)
        #}
      }
    })

# 
#     li <- reactive({
#       df <- parmesan:::index_inputs(session = session, input = input, parmesan = parmesan, numberLabel = TRUE,
#                                     disincludeInputs = c("varViewId", "desagregacionId", "aggId", "anioId", "colorsId",
#                                                          "fechasId", "stackedId", "sortBar", "axisId")) %>% plyr::compact()
#       df
#     })
# 
#     id_parmesan <- reactive({
#       req(parmesan)
#       parmesan::parmesan_input_ids(parmesan = parmesan)
#     })


    observe({
      #r$parmesan_input <- parmesan_input()
      #r$info_inputs <- li()
      #r$info_ids <- id_parmesan()
      #r$info_parmesan <- parmesan
      r$colorsPlot <- colors_default()
    })
    
    
    
  })
}

## To be copied in the UI
# mod_load_parmesan_ui("load_parmesan_ui_1")

## To be copied in the server
# mod_load_parmesan_server("load_parmesan_ui_1")

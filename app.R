# ============================================================
#  Vizualizador de Permisos de Circulación
# Autor: Daniel Jiménez
# 
# ============================================================

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(scales)
library(shinyWidgets)
library(readxl)

# ------------------------------------------------------------
# 1. CARGA DE DATOS
# ------------------------------------------------------------
datos <- read_excel("BBDD_PERMISOS_CIRCULACION_2016_2024.xlsx")
datos$Anio <- as.integer(datos$Anio)

cols_combustible <- c("Bencina", "Diésel", "Gas", "Eléctrico", "Otro")
cols_catalizador <- c("Catalítico", "NoCatalítico")

PALETA <- c("#1B4F72","#2E86C1","#28B463","#F39C12","#E74C3C",
            "#8E44AD","#17A589","#D35400","#AED6F1","#717D7E")

# Layout base para todos los gráficos plotly
base_layout <- list(
  font       = list(family = "Arial, sans-serif", size = 12),
  paper_bgcolor = "rgba(0,0,0,0)",
  plot_bgcolor  = "rgba(0,0,0,0)",
  margin     = list(l = 60, r = 20, t = 30, b = 60),
  legend     = list(
    orientation = "h",        # leyenda horizontal
    x = 0, y = -0.25,         # debajo del gráfico
    xanchor = "left",
    font = list(size = 11)
  ),
  hoverlabel = list(bgcolor = "#1B2631", font = list(color = "white", size = 12))
)

# ------------------------------------------------------------
# 2. UI
# ------------------------------------------------------------
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = span(icon("car"), " Permisos de Circulación"),
    titleWidth = 290
  ),
  
  dashboardSidebar(
    width = 290,
    sidebarMenu(
      menuItem("Resumen General",   tabName = "resumen",     icon = icon("chart-pie")),
      menuItem("Tendencia Anual",   tabName = "tendencia",   icon = icon("chart-line")),
      menuItem("Por Región/Comuna", tabName = "geo",         icon = icon("map-marker-alt")),
      menuItem("Combustibles",      tabName = "combustible", icon = icon("gas-pump")),
      menuItem("Tipo de Vehículo",  tabName = "vehiculo",    icon = icon("truck")),
      menuItem("Porcentajes",       tabName = "porcentajes", icon = icon("percent")),
      menuItem("Datos Crudos",      tabName = "tabla",       icon = icon("table"))
    ),
    hr(),
    h5("  Filtros", style = "color:#aaa; padding-left:15px;"),
    
    sliderInput("filtro_anio", "Años de registro:",
                min   = min(datos$Anio), max = max(datos$Anio),
                value = c(min(datos$Anio), max(datos$Anio)),
                sep = "", step = 1),
    
    pickerInput("filtro_region", "Región:",
                choices  = c("Todas", sort(unique(datos$`Glosa Región`))),
                selected = "Todas",
                options  = list(`live-search` = TRUE)),
    
    pickerInput("filtro_destino", "Destino:",
                choices  = c("Todos", sort(unique(datos$Destino))),
                selected = "Todos",
                options  = list(`live-search` = TRUE)),
    
    pickerInput("filtro_tipo", "Tipo Vehículo:",
                choices  = c("Todos", sort(unique(datos$TipoVehiculo))),
                selected = "Todos",
                options  = list(`live-search` = TRUE)),
    
    br(),
    actionButton("btn_reset", "Limpiar filtros",
                 icon = icon("undo"), width = "90%",
                 style = "margin-left:5%; background:#2E86C1; color:white;")
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background: #f4f6f9; }
      .box { border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,.08); }
      h3.box-title { font-weight: 600; }

      /* ── Footer ── */
      .footer-bar {
        background: #1B2631;
        color: #aab7c4;
        padding: 14px 30px;
        font-size: 13px;
        display: flex;
        align-items: center;
        justify-content: space-between;
        flex-wrap: wrap;
        gap: 8px;
        margin-top: 20px;
        border-top: 3px solid #2E86C1;
      }
      .footer-bar .footer-autor { color: #ffffff; font-weight: 600; font-size: 14px; }
      .footer-bar .footer-sep   { color: #2E86C1; margin: 0 8px; }
      .footer-bar a             { color: #2E86C1; text-decoration: none; }
      .footer-bar a:hover       { text-decoration: underline; }
    "))),
    
    tabItems(
      
      # ── RESUMEN ──────────────────────────────────────────────
      tabItem("resumen",
              fluidRow(
                valueBoxOutput("vbox_total",    width = 3),
                valueBoxOutput("vbox_regiones", width = 3),
                valueBoxOutput("vbox_comunas",  width = 3),
                valueBoxOutput("vbox_anios",    width = 3)
              ),
              fluidRow(
                box(title = "Vehículos por Destino", width = 6, status = "primary",
                    plotlyOutput("plot_destino", height = 360)),
                box(title = "Vehículos por Tipo", width = 6, status = "primary",
                    plotlyOutput("plot_tipo", height = 360))
              ),
              fluidRow(
                box(title = "Combustible por Región", width = 12, status = "info",
                    plotlyOutput("plot_comb_region", height = 380))
              )
      ),
      
      # ── TENDENCIA ANUAL ──────────────────────────────────────
      tabItem("tendencia",
              fluidRow(
                box(title = "Total Vehículos por Año", width = 12, status = "primary",
                    plotlyOutput("plot_tendencia", height = 400))
              ),
              fluidRow(
                box(title = "Evolución por Tipo de Vehículo", width = 6, status = "info",
                    plotlyOutput("plot_tend_tipo", height = 400)),
                box(title = "Evolución por Destino", width = 6, status = "info",
                    plotlyOutput("plot_tend_destino", height = 400))
              )
      ),
      
      # ── GEO ──────────────────────────────────────────────────
      tabItem("geo",
              fluidRow(
                box(title = "Regiones por Total Vehículos", width = 6, status = "primary",
                    plotlyOutput("plot_region", height = 450)),
                box(title = "Top 15 Comunas", width = 6, status = "primary",
                    plotlyOutput("plot_comuna", height = 450))
              )
      ),
      
      # ── COMBUSTIBLES ─────────────────────────────────────────
      tabItem("combustible",
              fluidRow(
                box(title = "Total por Combustible", width = 6, status = "warning",
                    plotlyOutput("plot_combustible", height = 360)),
                box(title = "Catalítico vs No Catalítico", width = 6, status = "warning",
                    plotlyOutput("plot_catalitico", height = 360))
              ),
              fluidRow(
                box(title = "Evolución por Tipo de Combustible y Año", width = 12, status = "success",
                    plotlyOutput("plot_comb_evol", height = 400))
              )
      ),
      
      # ── TIPO VEHÍCULO ─────────────────────────────────────────
      tabItem("vehiculo",
              fluidRow(
                box(title = "Combustible por Tipo de Vehículo", width = 12, status = "primary",
                    plotlyOutput("plot_tipo_comb", height = 450))
              ),
              fluidRow(
                box(title = "Heatmap: Tipo × Año", width = 12, status = "info",
                    plotlyOutput("plot_heatmap", height = 380))
              )
      ),
      
      # ── PORCENTAJES ───────────────────────────────────────────
      tabItem("porcentajes",
              fluidRow(
                box(width = 12, status = "primary", title = "Modo de visualización",
                    radioButtons("pct_metrica", label = NULL,
                                 choices  = c("Frecuencia absoluta" = "abs",
                                              "Porcentaje (%)"      = "pct"),
                                 selected = "pct", inline = TRUE)
                )
              ),
              fluidRow(
                box(title = "Combustible por Año", width = 6, status = "warning",
                    plotlyOutput("plot_pct_comb_anio", height = 420)),
                box(title = "Tipo de Vehículo por Año", width = 6, status = "warning",
                    plotlyOutput("plot_pct_tipo_anio", height = 420))
              ),
              fluidRow(
                box(title = "Destino por Año", width = 6, status = "info",
                    plotlyOutput("plot_pct_destino_anio", height = 420)),
                box(title = "Distribución por Región", width = 6, status = "info",
                    plotlyOutput("plot_pct_region", height = 420))
              ),
              fluidRow(
                box(title = "Tabla resumen: % Combustible por Año", width = 12, status = "primary",
                    DTOutput("tabla_pct"))
              )
      ),
      
      # ── TABLA ─────────────────────────────────────────────────
      tabItem("tabla",
              fluidRow(
                box(title = "Datos filtrados", width = 12, status = "primary",
                    downloadButton("btn_download", "Descargar CSV",
                                   style = "margin-bottom:10px;"),
                    DTOutput("tabla_datos"))
              )
      )
    ),
    
    # ── FOOTER ────────────────────────────────────────────────
    tags$div(class = "footer-bar",
             tags$div(
               tags$span(class = "footer-autor", icon("user"), " Daniel Jiménez Salazar"),
               tags$span(class = "footer-sep", "|"),
               tags$span(icon("building"), " Estudiante de Sociología, Universidad Católica del Maule"),
               tags$span(class = "footer-sep", "|"),
               tags$span(icon("envelope"), " daniel.jimenez@alumnos.ucm.cl")
             ),
             tags$div(
               tags$span(icon("database"), " Fuente: Instituto Nacional de Estadísticas de Chile (INE), Permisos de circulación.
https://www.ine.gob.cl/estadisticas-por-tema/servicios/permisos-de-circulacion"),
               tags$span(class = "footer-sep", "|"),
               tags$span(icon("calendar-alt"),
                         paste0(" Actualizado: ", format(Sys.Date(), "%B %Y")))
             )
    )
  )
)

# ------------------------------------------------------------
# 3. SERVER
# ------------------------------------------------------------
server <- function(input, output, session) {
  
  # ── Datos filtrados ───────────────────────────────────────
  df <- reactive({
    d <- datos %>%
      filter(Anio >= input$filtro_anio[1],
             Anio <= input$filtro_anio[2])
    if (input$filtro_region  != "Todas") d <- d %>% filter(`Glosa Región` == input$filtro_region)
    if (input$filtro_destino != "Todos") d <- d %>% filter(Destino        == input$filtro_destino)
    if (input$filtro_tipo    != "Todos") d <- d %>% filter(TipoVehiculo   == input$filtro_tipo)
    d
  })
  
  # ── Helper: total vehículos agrupado ─────────────────────
  tot_grupo <- function(d, ...) {
    d %>%
      group_by(...) %>%
      summarise(
        total = sum(rowSums(across(all_of(cols_combustible)), na.rm = TRUE)),
        .groups = "drop"
      )
  }
  
  # ── Helper: aplica base_layout a un plot_ly ───────────────
  apply_layout <- function(p, xlab = "", ylab = "", dtick_x = NULL, yfmt = NULL,
                           barmode = NULL, legend_below = TRUE) {
    xaxis <- list(title = list(text = xlab, font = list(size = 12)),
                  tickfont = list(size = 11))
    if (!is.null(dtick_x)) xaxis$dtick <- dtick_x
    
    yaxis <- list(title = list(text = ylab, font = list(size = 12)),
                  tickfont = list(size = 11))
    if (!is.null(yfmt)) yaxis$tickformat <- yfmt
    
    legend_cfg <- if (legend_below) {
      list(orientation = "h", x = 0, y = -0.28, xanchor = "left",
           font = list(size = 11))
    } else {
      list(font = list(size = 11))
    }
    
    args <- list(
      p,
      xaxis        = xaxis,
      yaxis        = yaxis,
      legend       = legend_cfg,
      font         = list(family = "Arial, sans-serif", size = 12),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      margin       = list(l = 70, r = 20, t = 30, b = 80),
      hoverlabel   = list(bgcolor = "#1B2631", font = list(color = "white", size = 12))
    )
    if (!is.null(barmode)) args$barmode <- barmode
    
    do.call(layout, args)
  }
  
  # ── Reset filtros ─────────────────────────────────────────
  observeEvent(input$btn_reset, {
    updateSliderInput(session, "filtro_anio",
                      value = c(min(datos$Anio), max(datos$Anio)))
    updatePickerInput(session, "filtro_region",  selected = "Todas")
    updatePickerInput(session, "filtro_destino", selected = "Todos")
    updatePickerInput(session, "filtro_tipo",    selected = "Todos")
  })
  
  # ── VALUE BOXES ───────────────────────────────────────────
  output$vbox_total <- renderValueBox(
    valueBox(
      format(sum(rowSums(df() %>% select(all_of(cols_combustible)), na.rm = TRUE)),
             big.mark = "."),
      "Total Vehículos", icon = icon("car"), color = "blue"))
  
  output$vbox_regiones <- renderValueBox(
    valueBox(n_distinct(df()$`Glosa Región`),
             "Regiones", icon = icon("map"), color = "green"))
  
  output$vbox_comunas <- renderValueBox(
    valueBox(n_distinct(df()$`Glosa Comuna`),
             "Comunas", icon = icon("city"), color = "orange"))
  
  output$vbox_anios <- renderValueBox(
    valueBox(n_distinct(df()$Anio),
             "Años de registro", icon = icon("calendar"), color = "purple"))
  
  # ── RESUMEN ───────────────────────────────────────────────
  output$plot_destino <- renderPlotly({
    d <- tot_grupo(df(), Destino) %>% arrange(desc(total))
    plot_ly(d, labels = ~Destino, values = ~total, type = "pie",
            marker   = list(colors = PALETA),
            textinfo = "label+percent",
            textfont = list(size = 12),
            insidetextorientation = "radial") %>%
      layout(
        showlegend   = TRUE,
        legend       = list(orientation = "h", x = 0, y = -0.15,
                            xanchor = "left", font = list(size = 11)),
        margin       = list(l = 10, r = 10, t = 30, b = 60),
        paper_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  output$plot_tipo <- renderPlotly({
    d <- tot_grupo(df(), TipoVehiculo) %>% arrange(total)
    plot_ly(d,
            x      = ~total,
            y      = ~reorder(TipoVehiculo, total),
            type   = "bar",
            orientation = "h",
            marker = list(color = PALETA[2],
                          line  = list(color = "white", width = 0.8)),
            text   = ~format(total, big.mark = "."),
            textposition = "outside",
            textfont = list(size = 11)) %>%
      apply_layout(xlab = "Vehículos", ylab = "")
  })
  
  output$plot_comb_region <- renderPlotly({
    d <- df() %>%
      group_by(`Glosa Región`) %>%
      summarise(across(all_of(cols_combustible), sum), .groups = "drop") %>%
      pivot_longer(-`Glosa Región`, names_to = "Combustible", values_to = "n") %>%
      group_by(`Glosa Región`) %>%
      mutate(pct = n / sum(n))
    
    plot_ly(d, x = ~`Glosa Región`, y = ~pct, color = ~Combustible,
            type = "bar", colors = PALETA[1:5],
            hovertemplate = "%{fullData.name}: %{y:.1%}<extra></extra>") %>%
      apply_layout(xlab = "Región", ylab = "Proporción",
                   yfmt = ".0%", barmode = "stack")
  })
  
  # ── TENDENCIA ANUAL ───────────────────────────────────────
  output$plot_tendencia <- renderPlotly({
    d <- tot_grupo(df(), Anio)
    plot_ly(d, x = ~Anio, y = ~total, type = "scatter", mode = "lines+markers",
            line     = list(color = PALETA[1], width = 3),
            marker   = list(size = 9, color = PALETA[2],
                            line = list(color = "white", width = 2)),
            text     = ~format(total, big.mark = "."),
            hovertemplate = "Año %{x}: %{text} vehículos<extra></extra>") %>%
      apply_layout(xlab = "Año", ylab = "Total Vehículos",
                   dtick_x = 1, legend_below = FALSE)
  })
  
  output$plot_tend_tipo <- renderPlotly({
    d <- tot_grupo(df(), Anio, TipoVehiculo)
    plot_ly(d, x = ~Anio, y = ~total, color = ~TipoVehiculo,
            type = "scatter", mode = "lines+markers",
            colors = PALETA,
            marker = list(size = 7),
            hovertemplate = "%{fullData.name} — Año %{x}: %{y:,}<extra></extra>") %>%
      apply_layout(xlab = "Año", ylab = "Total Vehículos", dtick_x = 1)
  })
  
  output$plot_tend_destino <- renderPlotly({
    d <- tot_grupo(df(), Anio, Destino)
    plot_ly(d, x = ~Anio, y = ~total, color = ~Destino,
            type = "scatter", mode = "lines+markers",
            colors = PALETA,
            marker = list(size = 7),
            hovertemplate = "%{fullData.name} — Año %{x}: %{y:,}<extra></extra>") %>%
      apply_layout(xlab = "Año", ylab = "Total Vehículos", dtick_x = 1)
  })
  
  # ── GEO ──────────────────────────────────────────────────
  output$plot_region <- renderPlotly({
    d <- tot_grupo(df(), `Glosa Región`) %>% arrange(total)
    plot_ly(d,
            x      = ~total,
            y      = ~reorder(`Glosa Región`, total),
            type   = "bar", orientation = "h",
            marker = list(color = PALETA[1],
                          line  = list(color = "white", width = 0.8)),
            text   = ~format(total, big.mark = "."),
            textposition = "outside",
            hovertemplate = "%{y}: %{x:,}<extra></extra>") %>%
      apply_layout(xlab = "Vehículos", ylab = "", legend_below = FALSE)
  })
  
  output$plot_comuna <- renderPlotly({
    d <- tot_grupo(df(), `Glosa Comuna`) %>% arrange(total) %>% tail(15)
    plot_ly(d,
            x      = ~total,
            y      = ~reorder(`Glosa Comuna`, total),
            type   = "bar", orientation = "h",
            marker = list(color = PALETA[2],
                          line  = list(color = "white", width = 0.8)),
            text   = ~format(total, big.mark = "."),
            textposition = "outside",
            hovertemplate = "%{y}: %{x:,}<extra></extra>") %>%
      apply_layout(xlab = "Vehículos", ylab = "", legend_below = FALSE)
  })
  
  # ── COMBUSTIBLES ─────────────────────────────────────────
  output$plot_combustible <- renderPlotly({
    d <- df() %>%
      summarise(across(all_of(cols_combustible), sum)) %>%
      pivot_longer(everything(), names_to = "Combustible", values_to = "Total") %>%
      arrange(desc(Total))
    
    plot_ly(d,
            x      = ~reorder(Combustible, -Total),
            y      = ~Total,
            type   = "bar",
            marker = list(color = PALETA[1:5],
                          line  = list(color = "white", width = 0.8)),
            text   = ~format(Total, big.mark = "."),
            textposition = "outside",
            hovertemplate = "%{x}: %{y:,}<extra></extra>") %>%
      apply_layout(xlab = "", ylab = "Total Vehículos", legend_below = FALSE)
  })
  
  output$plot_catalitico <- renderPlotly({
    d <- df() %>%
      summarise(across(all_of(cols_catalizador), sum)) %>%
      pivot_longer(everything(), names_to = "Tipo", values_to = "Total")
    
    plot_ly(d, labels = ~Tipo, values = ~Total, type = "pie",
            marker   = list(colors = c(PALETA[3], PALETA[5])),
            textinfo = "label+percent+value",
            textfont = list(size = 12),
            insidetextorientation = "radial") %>%
      layout(
        showlegend    = TRUE,
        legend        = list(orientation = "h", x = 0.1, y = -0.1,
                             font = list(size = 12)),
        margin        = list(l = 10, r = 10, t = 30, b = 60),
        paper_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  output$plot_comb_evol <- renderPlotly({
    d <- df() %>%
      group_by(Anio) %>%
      summarise(across(all_of(cols_combustible), sum), .groups = "drop")
    
    plot_ly(d, x = ~Anio) %>%
      add_lines(y = ~Bencina,   name = "Bencina",
                line = list(color = PALETA[1], width = 2.5),
                marker = list(size = 6)) %>%
      add_lines(y = ~`Diésel`,  name = "Diésel",
                line = list(color = PALETA[5], width = 2.5),
                marker = list(size = 6)) %>%
      add_lines(y = ~Eléctrico, name = "Eléctrico",
                line = list(color = PALETA[3], width = 2.5),
                marker = list(size = 6)) %>%
      add_lines(y = ~Gas,       name = "Gas",
                line = list(color = PALETA[4], width = 2.5),
                marker = list(size = 6)) %>%
      add_lines(y = ~Otro,      name = "Otro",
                line = list(color = PALETA[10], width = 2.5),
                marker = list(size = 6)) %>%
      apply_layout(xlab = "Año", ylab = "Vehículos", dtick_x = 1)
  })
  
  # ── TIPO VEHÍCULO ─────────────────────────────────────────
  output$plot_tipo_comb <- renderPlotly({
    d <- df() %>%
      group_by(TipoVehiculo) %>%
      summarise(across(all_of(cols_combustible), sum), .groups = "drop") %>%
      pivot_longer(-TipoVehiculo, names_to = "Combustible", values_to = "n")
    
    plot_ly(d, x = ~TipoVehiculo, y = ~n, color = ~Combustible,
            type = "bar", colors = PALETA[1:5],
            hovertemplate = "%{fullData.name}: %{y:,}<extra></extra>") %>%
      apply_layout(xlab = "", ylab = "Total Vehículos", barmode = "stack")
  })
  
  output$plot_heatmap <- renderPlotly({
    d <- tot_grupo(df(), Anio, TipoVehiculo) %>%
      pivot_wider(names_from = TipoVehiculo, values_from = total, values_fill = 0)
    mat <- as.matrix(d[, -1])
    rownames(mat) <- d$Anio
    
    plot_ly(z = mat, x = colnames(mat), y = as.character(rownames(mat)),
            type = "heatmap", colorscale = "Blues",
            hovertemplate = "Tipo: %{x}<br>Año: %{y}<br>Vehículos: %{z:,}<extra></extra>") %>%
      layout(
        xaxis  = list(title = "Tipo de Vehículo", tickfont = list(size = 11)),
        yaxis  = list(title = "Año", tickfont = list(size = 11), dtick = 1),
        margin = list(l = 60, r = 20, t = 30, b = 60),
        paper_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  # ── PORCENTAJES ───────────────────────────────────────────
  output$plot_pct_comb_anio <- renderPlotly({
    d <- df() %>%
      group_by(Anio) %>%
      summarise(across(all_of(cols_combustible), sum), .groups = "drop") %>%
      pivot_longer(-Anio, names_to = "Combustible", values_to = "total")
    
    if (input$pct_metrica == "pct") {
      d <- d %>% group_by(Anio) %>% mutate(valor = total / sum(total))
      ylab <- "Porcentaje"; yfmt <- ".1%"
    } else {
      d <- d %>% mutate(valor = total)
      ylab <- "Vehículos"; yfmt <- ","
    }
    
    plot_ly(d, x = ~Anio, y = ~valor, color = ~Combustible,
            type = "bar", colors = PALETA[1:5],
            hovertemplate = "%{fullData.name} %{x}: %{y}<extra></extra>") %>%
      apply_layout(xlab = "Año", ylab = ylab,
                   dtick_x = 1, yfmt = yfmt, barmode = "stack")
  })
  
  output$plot_pct_tipo_anio <- renderPlotly({
    d <- tot_grupo(df(), Anio, TipoVehiculo)
    
    if (input$pct_metrica == "pct") {
      d <- d %>% group_by(Anio) %>% mutate(valor = total / sum(total))
      ylab <- "Porcentaje"; yfmt <- ".1%"
    } else {
      d <- d %>% mutate(valor = total)
      ylab <- "Vehículos"; yfmt <- ","
    }
    
    plot_ly(d, x = ~Anio, y = ~valor, color = ~TipoVehiculo,
            type = "bar", colors = PALETA,
            hovertemplate = "%{fullData.name} %{x}: %{y}<extra></extra>") %>%
      apply_layout(xlab = "Año", ylab = ylab,
                   dtick_x = 1, yfmt = yfmt, barmode = "stack")
  })
  
  output$plot_pct_destino_anio <- renderPlotly({
    d <- tot_grupo(df(), Anio, Destino)
    
    if (input$pct_metrica == "pct") {
      d <- d %>% group_by(Anio) %>% mutate(valor = total / sum(total))
      ylab <- "Porcentaje"; yfmt <- ".1%"
    } else {
      d <- d %>% mutate(valor = total)
      ylab <- "Vehículos"; yfmt <- ","
    }
    
    plot_ly(d, x = ~Anio, y = ~valor, color = ~Destino,
            type = "bar", colors = PALETA,
            hovertemplate = "%{fullData.name} %{x}: %{y}<extra></extra>") %>%
      apply_layout(xlab = "Año", ylab = ylab,
                   dtick_x = 1, yfmt = yfmt, barmode = "stack")
  })
  
  output$plot_pct_region <- renderPlotly({
    d <- tot_grupo(df(), `Glosa Región`) %>%
      mutate(pct = total / sum(total)) %>%
      arrange(total)
    
    if (input$pct_metrica == "pct") {
      d <- d %>% mutate(valor = pct)
      xlab <- "Porcentaje"; xfmt <- ".1%"
    } else {
      d <- d %>% mutate(valor = total)
      xlab <- "Vehículos"; xfmt <- ","
    }
    
    plot_ly(d,
            x      = ~valor,
            y      = ~reorder(`Glosa Región`, valor),
            type   = "bar", orientation = "h",
            marker = list(color = PALETA[2],
                          line  = list(color = "white", width = 0.8)),
            hovertemplate = "%{y}: %{x}<extra></extra>") %>%
      apply_layout(xlab = xlab, ylab = "", yfmt = xfmt, legend_below = FALSE)
  })
  
  # Tabla resumen porcentajes
  output$tabla_pct <- renderDT({
    d <- df() %>%
      group_by(Anio) %>%
      summarise(
        Total = sum(rowSums(across(all_of(cols_combustible)), na.rm = TRUE)),
        across(all_of(cols_combustible), sum),
        .groups = "drop"
      ) %>%
      mutate(across(all_of(cols_combustible),
                    ~ paste0(round(. / Total * 100, 1), "%"),
                    .names = "pct_{.col}")) %>%
      select(Anio, Total, starts_with("pct_"))
    
    names(d) <- gsub("pct_", "% ", names(d))
    
    datatable(d,
              options  = list(pageLength = 15, scrollX = TRUE,
                              language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")),
              rownames = FALSE)
  })
  
  # ── TABLA CRUDA ───────────────────────────────────────────
  output$tabla_datos <- renderDT({
    datatable(df(),
              options  = list(pageLength = 15, scrollX = TRUE,
                              language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")),
              rownames = FALSE,
              filter   = "top")
  })
  
  output$btn_download <- downloadHandler(
    filename = function() paste0("permisos_actualizado_al", Sys.Date(), ".csv"),
    content  = function(file) write_csv(df(), file)
  )
}

# ------------------------------------------------------------
# 4. EJECUTAR
# ------------------------------------------------------------
shinyApp(ui, server)



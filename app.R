# ============================================================
# APP.R — Aplicación Shiny principal
# ============================================================
library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(rnaturalearth)
library(zip)
library(bsicons)
library(ncdf4)
library(curl)
library(lubridate)
library(dplyr)
library(ggplot2)
library(metR)
library(MBA)
library(shinycssloaders)
library(colourpicker)
library(plotly)

options(timeout = 120)

source("estilo.R")
source("graficas.R")

# ============================================================
# DATOS Y CUADRÍCULA
# ============================================================
world <- ne_countries(scale = "medium", returnclass = "sf")
south_america <- world[world$continent == "South America", ]
anio_actual <- as.integer(format(Sys.Date(), "%Y"))

parsear_boya <- function(id) {
  match_lat <- regexec("^([0-9.]+)([ns])", id)
  partes <- regmatches(id, match_lat)[[1]]
  if (length(partes) < 3) return(NULL)
  lat_num <- as.numeric(partes[2]); lat_dir <- partes[3]
  lat <- if (lat_dir == "n") lat_num else -lat_num
  match_lon <- regexec("([0-9.]+)([ew])$", id)
  partes_lon <- regmatches(id, match_lon)[[1]]
  if (length(partes_lon) < 3) return(NULL)
  lon_num <- as.numeric(partes_lon[2]); lon_dir <- partes_lon[3]
  lon <- if (lon_dir == "e") lon_num else -lon_num
  lon_original <- lon
  if (lon > 0 && lon %in% c(165, 167, 168, 170)) lon <- lon - 360
  data.frame(lat = lat, lng = lon, lng_real = lon, lng_original = lon_original,
             id = id, stringsAsFactors = FALSE)
}

boyas_filtradas <- list(
  dyn = c("0.7n110w","0n110w","0n125w","0n140w","0n155w","0n165e","0n170e","0n170w","0n180w","0n95w",
          "10n95w","2n110w","2n125w","2n140w","2n155w","2n165e","2n170w","2n180w","2n95w","2s110w",
          "2s125w","2s140w","2s155w","2s165e","2s170w","2s180w","2s95w","3.5n95w","5n110w","5n125w",
          "5n140w","5n155w","5n165e","5n170w","5n180w","5n95w","5s110w","5s125w","5s140w","5s155w",
          "5s165e","5s170w","5s180w","5s95w","7n132w","7n140w","7n147w","8n110w","8n125w","8n155w",
          "8n165e","8n167e","8n168e","8n170w","8n180w","8n95w","8s110w","8s125w","8s155w","8s165e",
          "8s170w","8s180w","8s95w","9n140w"),
  iso = c("0.7n110w","0.7s110w","0n108w","0n110.5w","0n110w","0n125w","0n140w","0n152w","0n155w",
          "0n165e","0n170e","0n170w","0n180w","0n85w","0n95w","10n95w","1n153w","1s153w","2n110w",
          "2n125w","2n140w","2n155w","2n165e","2n170w","2n180w","2n95w","2s110w","2s125w","2s140w",
          "2s155w","2s165e","2s170w","2s180w","2s95w","3.5n95w","5n110w","5n125w","5n140w","5n155w",
          "5n165e","5n170w","5n180w","5n95w","5s110w","5s125w","5s140w","5s155w","5s165e","5s170w",
          "5s180w","5s95w","7n132w","7n140w","7n147w","7n150w","8n110w","8n125w","8n150w","8n155w",
          "8n165e","8n167e","8n168e","8n170w","8n180w","8n95w","8s110w","8s125w","8s155w","8s165e",
          "8s170w","8s180w","8s95w","9n140w"),
  sst = c("0.7n110w","0.7s110w","0n108w","0n110.5w","0n110w","0n125w","0n140w","0n152w","0n155w",
          "0n165e","0n170e","0n170w","0n180w","0n85w","0n95w","10n95w","1n153w","1s153w","2n110w",
          "2n125w","2n140w","2n155w","2n165e","2n170w","2n180w","2n95w","2s110w","2s125w","2s140w",
          "2s155w","2s165e","2s170w","2s180w","2s95w","3.5n95w","5n110w","5n125w","5n140w","5n155w",
          "5n165e","5n170w","5n180w","5n95w","5s110w","5s125w","5s140w","5s155w","5s165e","5s170w",
          "5s180w","5s95w","6n150w","7n132w","7n140w","7n147w","7n150w","8n110w","8n125w","8n150w",
          "8n155w","8n165e","8n167e","8n168e","8n170w","8n180w","8n95w","8s110w","8s125w","8s155w",
          "8s165e","8s170w","8s180w","8s95w","9n140w"),
  t = c("0.7n110w","0.7s110w","0n108w","0n110.5w","0n110w","0n125w","0n140w","0n152w","0n155w",
        "0n165e","0n170e","0n170w","0n180w","0n85w","0n95w","10n95w","1n153w","1s153w","2n110w",
        "2n125w","2n140w","2n155w","2n165e","2n170w","2n180w","2n95w","2s110w","2s125w","2s140w",
        "2s155w","2s165e","2s170w","2s180w","2s95w","3.5n95w","5n110w","5n125w","5n140w","5n155w",
        "5n165e","5n170w","5n180w","5n95w","5s110w","5s125w","5s140w","5s155w","5s165e","5s170w",
        "5s180w","5s95w","6n150w","7n132w","7n140w","7n147w","7n150w","8n110w","8n125w","8n150w",
        "8n155w","8n165e","8n167e","8n168e","8n170w","8n180w","8n95w","8s110w","8s125w","8s155w",
        "8s165e","8s170w","8s180w","8s95w","9n140w"),
  w = c("0.7n110w","0.7s110w","0n108w","0n110.5w","0n110w","0n125w","0n140w","0n152w","0n155w",
        "0n165e","0n170e","0n170w","0n176w","0n180w","0n85w","0n95w","10n95w","1n153w","1s153w",
        "1s167e","2n110w","2n125w","2n140w","2n155w","2n157w","2n165e","2n170w","2n180w","2n95w",
        "2s110w","2s125w","2s140w","2s155w","2s165e","2s170w","2s180w","2s95w","3.5n95w","5n110w",
        "5n125w","5n140w","5n155w","5n165e","5n170w","5n180w","5n95w","5s110w","5s125w","5s140w",
        "5s155w","5s165e","5s170w","5s180w","5s95w","6n150w","7n132w","7n140w","7n147w","7n150w",
        "8n110w","8n125w","8n150w","8n155w","8n165e","8n167e","8n168e","8n170w","8n180w","8n95w",
        "8s110w","8s125w","8s155w","8s165e","8s170w","8s180w","8s95w","9n140w")
)

crear_df_desde_lista <- function(lista_ids) {
  df_list <- lapply(lista_ids, parsear_boya)
  df <- do.call(rbind, df_list)
  if (is.null(df) || nrow(df) == 0) return(data.frame())
  df$lat <- as.numeric(df$lat); df$lng <- as.numeric(df$lng)
  df$lng_real <- as.numeric(df$lng_real); df$lng_original <- as.numeric(df$lng_original)
  df$tipo <- ifelse(df$lat == 0, "Boyas Ecuatoriales", "Boyas Perfiladoras")
  df$region <- "Pacífico Abierto"
  df$region[df$lat >= -10 & df$lat <= 0 & df$lng_real >= -90 & df$lng_real <= -80] <- "Niño 1+2"
  df$region[df$lat >= -5 & df$lat <= 5 & df$lng_real >= -150 & df$lng_real <= -90] <- "Niño 3"
  df$region[df$lat >= -5 & df$lat <= 5 & (df$lng_real >= 160 | df$lng_real <= -150)] <- "Niño 4"
  df$region[df$lat >= -5 & df$lat <= 5 & df$lng_real >= -170 & df$lng_real <= -120] <- "Niño 3.4"
  df$color <- ifelse(df$lat == 0, "#DC2626", "#0369A1")
  df$label <- paste0("Boya TAO: ", abs(df$lat), "°", ifelse(df$lat >= 0, "N", "S"),
                     " | ", abs(df$lng_original), "°", ifelse(df$lng_original >= 0, "E", "W"))
  df
}

boyas_dyn <- crear_df_desde_lista(boyas_filtradas$dyn)
boyas_iso <- crear_df_desde_lista(boyas_filtradas$iso)
boyas_sst <- crear_df_desde_lista(boyas_filtradas$sst)
boyas_t   <- crear_df_desde_lista(boyas_filtradas$t)
boyas_w   <- crear_df_desde_lista(boyas_filtradas$w)

boyas_por_variable <- list(dyn = boyas_dyn, iso = boyas_iso, sst = boyas_sst,
                           t = boyas_t, w = boyas_w)

mapa_variables_nc <- list(sst = "T_25", dyn = "DYN_13", iso = "ISO_6", t = NULL, w = "WU_422")

set.seed(123)
south_america$color_normal <- rep(colores_pasteles, length.out = nrow(south_america))

# ============================================================
# HELPER: generar cada nav_panel de variable
# ============================================================
crear_tab_variable <- function(var_id, titulo) {
  df <- boyas_por_variable[[var_id]]
  choices <- if (!is.null(df) && nrow(df) > 0) setNames(df$id, df$label)
  else c("Sin boyas disponibles" = "")
  defs <- defaults_var[[var_id]]
  
  extra_panel_t <- if (var_id == "t") {
    accordion_panel("Climatología y profundidad",
                    icon = bsicons::bs_icon("sliders"),
                    dateInput(paste0("clim_ini_", var_id), "Inicio climatología:",
                              value = "1991-01-01", format = "yyyy-mm-dd"),
                    dateInput(paste0("clim_fin_", var_id), "Fin climatología:",
                              value = "2020-12-31", format = "yyyy-mm-dd"),
                    numericInput(paste0("prof_max_", var_id), "Profundidad máxima (m):",
                                 value = 500, min = 50, max = 1000, step = 50),
                    div(style = "margin-top:8px; padding:8px 10px; border-radius:6px; background:#FEF3C7; border:1px solid #FCD34D; color:#78350F; font-size:0.78rem; line-height:1.35;",
                        bsicons::bs_icon("exclamation-triangle-fill"),
                        HTML(" Se requieren <strong>al menos 25 años</strong> dentro del rango. Si 1991 no tiene datos, se usará el año más cercano."))
    )
  } else NULL
  
  panel_colores <- accordion_panel("Colores", icon = bsicons::bs_icon("palette-fill"),
                                   div(style = "color:#64748B; font-size:0.78rem; margin-bottom:10px;",
                                       "Personaliza los 5 colores principales del gráfico."),
                                   crear_controles_color(var_id))
  
  panel_tema <- if (var_id %in% c("sst", "dyn", "iso", "w")) {
    accordion_panel("Apariencia", icon = bsicons::bs_icon("brush-fill"),
                    crear_control_tema(var_id))
  } else NULL
  
  if (var_id == "t") {
    sp <- function(id) withSpinner(plotOutput(id, height = "450px"), type = 8,
                                   color = "#0284C7", size = 1.2, color.background = "#FFFFFF")
    card_hp1 <- card(class = "shadow-sm border-0", style = "margin-bottom: 15px;",
                     card_header(class = "d-flex justify-content-between align-items-center",
                                 span(bsicons::bs_icon("graph-up"), " Serie Histórica P1"),
                                 boton_descarga("dl_hist_p1_t")),
                     card_body(class = "p-2", sp("hist_p1_t")))
    card_hp2 <- card(class = "shadow-sm border-0", style = "margin-bottom: 15px;",
                     card_header(class = "d-flex justify-content-between align-items-center",
                                 span(bsicons::bs_icon("graph-up"), " Serie Histórica P2"),
                                 boton_descarga("dl_hist_p2_t")),
                     card_body(class = "p-2", sp("hist_p2_t")))
    card_ap1 <- card(class = "shadow-sm border-0", style = "margin-bottom: 15px;",
                     card_header(class = "d-flex justify-content-between align-items-center",
                                 span(bsicons::bs_icon("activity"), " Anomalías P1"),
                                 boton_descarga("dl_anom_p1_t")),
                     card_body(class = "p-2", sp("anom_p1_t")))
    card_ap2 <- card(class = "shadow-sm border-0",
                     card_header(class = "d-flex justify-content-between align-items-center",
                                 span(bsicons::bs_icon("activity"), " Anomalías P2"),
                                 boton_descarga("dl_anom_p2_t")),
                     card_body(class = "p-2", sp("anom_p2_t")))
    contenido_graficas <- div(card_hp1, card_hp2, card_ap1, card_ap2)
  } else if (var_id == "w") {
    altura_w   <- "820px"
    alto_card  <- "900px"
    alto_fila  <- "900px"
    
    sp_w <- function(id) withSpinner(
      plotlyOutput(id, height = altura_w, width = "100%"),
      type = 8, color = "#0284C7", size = 1.2, color.background = "#FFFFFF")
    
    card_hp1 <- card(class = "shadow-sm border-0",
                     style = paste0("height: ", alto_card, ";"),
                     card_header(class = "d-flex justify-content-between align-items-center",
                                 span(bsicons::bs_icon("graph-up"), " Serie Histórica P1"),
                                 boton_descarga("dl_hist_p1_w")),
                     card_body(class = "p-2", style = "height: calc(100% - 55px);",
                               sp_w("hist_p1_w")))
    card_hp2 <- card(class = "shadow-sm border-0",
                     style = paste0("height: ", alto_card, ";"),
                     card_header(class = "d-flex justify-content-between align-items-center",
                                 span(bsicons::bs_icon("graph-up"), " Serie Histórica P2"),
                                 boton_descarga("dl_hist_p2_w")),
                     card_body(class = "p-2", style = "height: calc(100% - 55px);",
                               sp_w("hist_p2_w")))
    card_ap1 <- card(class = "shadow-sm border-0",
                     style = paste0("height: ", alto_card, ";"),
                     card_header(class = "d-flex justify-content-between align-items-center",
                                 span(bsicons::bs_icon("activity"), " Anomalías P1"),
                                 boton_descarga("dl_anom_p1_w")),
                     card_body(class = "p-2", style = "height: calc(100% - 55px);",
                               sp_w("anom_p1_w")))
    card_ap2 <- card(class = "shadow-sm border-0",
                     style = paste0("height: ", alto_card, ";"),
                     card_header(class = "d-flex justify-content-between align-items-center",
                                 span(bsicons::bs_icon("activity"), " Anomalías P2"),
                                 boton_descarga("dl_anom_p2_w")),
                     card_body(class = "p-2", style = "height: calc(100% - 55px);",
                               sp_w("anom_p2_w")))
    
    contenido_graficas <- layout_columns(
      col_widths  = c(6, 6),
      row_heights = c(alto_fila, alto_fila),
      card_hp1, card_hp2,
      card_ap1, card_ap2
    )
  } else {
    altura_grafica <- "560px"
    spinner_hist <- withSpinner(
      plotlyOutput(paste0("hist_", var_id), height = altura_grafica, width = "100%"),
      type = 8, color = "#0284C7", size = 1.2, color.background = "#FFFFFF")
    spinner_anom <- withSpinner(
      plotlyOutput(paste0("anom_", var_id), height = altura_grafica, width = "100%"),
      type = 8, color = "#0284C7", size = 1.2, color.background = "#FFFFFF")
    
    card_hist <- card(class = "shadow-sm border-0", style = "margin-bottom: 15px;",
                      card_header(class = "d-flex justify-content-between align-items-center",
                                  span(bsicons::bs_icon("graph-up"), " Serie Histórica"),
                                  boton_descarga(paste0("dl_hist_", var_id))),
                      card_body(class = "p-2", spinner_hist))
    card_anom <- card(class = "shadow-sm border-0",
                      card_header(class = "d-flex justify-content-between align-items-center",
                                  span(bsicons::bs_icon("activity"), " Anomalías"),
                                  boton_descarga(paste0("dl_anom_", var_id))),
                      card_body(class = "p-2", spinner_anom))
    
    contenido_graficas <- div(card_hist, card_anom)
  }
  
  nav_panel(titulo,
            layout_sidebar(
              sidebar = sidebar(width = 340,
                                div(style = "padding:6px 4px 15px 4px;",
                                    div(style = "color:#075985; font-size:0.78rem; font-weight:700; letter-spacing:1px; text-transform:uppercase; margin-bottom:3px;", "CONFIGURACIÓN"),
                                    div(style = "color:#17324D; font-size:1.25rem; font-weight:750;", titulo),
                                    div(style = "color:#64748B; font-size:0.82rem; margin-top:3px;",
                                        paste0(nrow(df), " boyas disponibles"))
                                ),
                                accordion(multiple = TRUE, open = c("Boya", "Períodos"),
                                          accordion_panel("Boya", icon = bsicons::bs_icon("geo-alt-fill"),
                                                          selectInput(paste0("boya_", var_id), "Seleccionar boya:", choices = choices)
                                          ),
                                          accordion_panel("Períodos", icon = bsicons::bs_icon("calendar-range"),
                                                          div(style = "display:flex; gap:10px;",
                                                              numericInput(paste0("anio_p1_", var_id), "Año período 1:",
                                                                           value = 1997, min = 1979, max = 2030, step = 1),
                                                              numericInput(paste0("anio_p2_", var_id), "Año período 2:",
                                                                           value = anio_actual - 1, min = 1979, max = 2030, step = 1)
                                                          ),
                                                          div(style = "margin-top:10px; padding:10px 12px; border-radius:8px; background:#E0F2FE; border:1px solid #BAE6FD; color:#075985; font-size:0.82rem; line-height:1.4;",
                                                              bsicons::bs_icon("info-circle-fill"),
                                                              HTML(" Cada período gráfica <strong>24 meses</strong>. Los años se sincronizan automáticamente."))
                                          ),
                                          extra_panel_t,
                                          panel_colores,
                                          panel_tema,
                                          accordion_panel("Títulos y etiquetas", icon = bsicons::bs_icon("type"),
                                                          textAreaInput(paste0("titulo_hist_", var_id), "Título histórico (opcional):",
                                                                        value = defs$titulo_hist, rows = 2),
                                                          textAreaInput(paste0("titulo_anom_", var_id), "Título anomalías (opcional):",
                                                                        value = defs$titulo_anom, rows = 2),
                                                          textInput(paste0("ylab_hist_", var_id), "Etiqueta eje Y (histórico):",
                                                                    value = defs$ylab_hist),
                                                          textInput(paste0("ylab_anom_", var_id), "Etiqueta eje Y (anomalías):",
                                                                    value = defs$ylab_anom)
                                          )
                                )
              ),
              contenido_graficas
            )
  )
}

# ============================================================
# UI
# ============================================================
ui <- page_navbar(
  title = tagList(bsicons::bs_icon("activity", size = "1.1em"), span("Monitoreo ENSO")),
  id = "nav_monitoreo",
  theme = tema_bslib,
  
  header = tagList(
    tags$head(tags$style(css_monitoreo)),
    tags$div(style = "position:absolute; top:13px; right:18px; z-index:1050;",
             input_dark_mode(id = "modo_oscuro", mode = "light"))
  ),
  
  nav_panel("Ubicación",
            layout_sidebar(
              sidebar = sidebar(width = 340,
                                div(style = "padding:6px 4px 15px 4px;",
                                    div(style = "color:#075985; font-size:0.78rem; font-weight:700; letter-spacing:1px; text-transform:uppercase; margin-bottom:3px;", "MONITOREO"),
                                    div(style = "color:#17324D; font-size:1.25rem; font-weight:750;", "TAO/TRITON"),
                                    div(style = "color:#64748B; font-size:0.82rem; margin-top:3px;", "Pacífico ecuatorial")
                                ),
                                accordion(multiple = TRUE, open = c("Filtros Geográficos", "Descarga NOAA"),
                                          accordion_panel("Filtros Geográficos", icon = bsicons::bs_icon("geo-alt-fill"),
                                                          selectInput("filtro_region", "Filtrar por Región:",
                                                                      choices = c("Todas","Niño 1+2","Niño 3","Niño 4","Niño 3.4")),
                                                          selectInput("filtro_meridiano", "Filtrar por Longitud:", choices = c("Todas"))
                                          ),
                                          accordion_panel("Descarga NOAA", icon = bsicons::bs_icon("cloud-arrow-down-fill"),
                                                          selectInput("var_descarga", "Variable a descargar:",
                                                                      choices = c("Altura Dinámica"="dyn","Isoterma 20°C"="iso",
                                                                                  "Temperatura Superficial"="sst","Temperatura Subsuperficial"="t",
                                                                                  "Vientos"="w")),
                                                          tags$label(class = "form-label", `for` = "modo_descarga", "Alcance de descarga:"),
                                                          radioButtons("modo_descarga", label = NULL,
                                                                       choices = c("Boya seleccionada (Ninguna)" = "seleccionada",
                                                                                   "Todas las boyas filtradas" = "filtradas"),
                                                                       selected = "filtradas"),
                                                          uiOutput("aviso_filtro_descarga"),
                                                          tags$div(style = "margin-top:14px; padding-top:12px; border-top:1px solid #D8E8F0;",
                                                                   uiOutput("btn_descargar_ui"))
                                          )
                                )
              ),
              card(full_screen = TRUE, class = "shadow-sm border-0",
                   card_header(class = "d-flex justify-content-between align-items-center",
                               span(bsicons::bs_icon("map-fill"), " Distribución Espacial de la Red TAO/TRITON")),
                   card_body(class = "p-0", leafletOutput("map", height = "100%"))
              )
            )
  ),
  
  crear_tab_variable("sst", "Temperatura Superficial"),
  crear_tab_variable("dyn", "Altura Dinámica"),
  crear_tab_variable("iso", "Isoterma de 20°C"),
  crear_tab_variable("t",   "Temperatura Subsuperficial"),
  crear_tab_variable("w",   "Vientos")
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  cache_dir <- file.path(tempdir(), "tao_cache")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  cache_nc <- new.env()
  
  boyas_var <- reactive({ req(input$var_descarga); boyas_por_variable[[input$var_descarga]] })
  boya_activa <- reactiveVal(NULL)
  
  observeEvent(c(input$var_descarga, input$filtro_region, input$filtro_meridiano), {
    boya_activa(NULL)
  }, ignoreInit = TRUE)
  
  observeEvent(c(input$var_descarga, input$filtro_region), {
    req(input$var_descarga)
    df <- boyas_var()
    if (nrow(df) == 0) {
      updateSelectInput(session, "filtro_meridiano",
                        choices = c("Sin boyas disponibles" = "Todas"), selected = "Todas"); return()
    }
    if (input$filtro_region == "Todas") lons <- sort(unique(df$lng), decreasing = TRUE)
    else lons <- sort(unique(df$lng[df$region == input$filtro_region]), decreasing = TRUE)
    if (length(lons) > 0) {
      lons_real <- ifelse(lons < -180, lons + 360, lons)
      etiq <- paste0(abs(lons_real), "°", ifelse(lons_real >= 0, "E", "W"))
      opciones <- c("Todas" = "Todas", setNames(as.character(lons), etiq))
    } else opciones <- c("Sin longitudes en esta región" = "Todas")
    updateSelectInput(session, "filtro_meridiano", choices = opciones, selected = "Todas")
  })
  
  observeEvent(boya_activa(), {
    b <- boya_activa()
    if (is.null(b) || nrow(b) == 0) {
      updateRadioButtons(session, "modo_descarga",
                         choiceNames = c("Seleccione una boya","Todas las boyas filtradas"),
                         choiceValues = c("seleccionada","filtradas"), selected = "filtradas")
    } else {
      lat_txt <- paste0(abs(b$lat), "°", ifelse(b$lat >= 0, "N","S"))
      lng_txt <- paste0(abs(b$lng_original), "°", ifelse(b$lng_original >= 0, "E","W"))
      updateRadioButtons(session, "modo_descarga",
                         choiceNames = c(paste("Boya:", lat_txt, lng_txt), "Todas las boyas filtradas"),
                         choiceValues = c("seleccionada","filtradas"), selected = "seleccionada")
    }
  }, ignoreNULL = FALSE)
  
  boyas_filtradas <- reactive({
    df <- boyas_var()
    if (nrow(df) == 0) return(df)
    if (input$filtro_region != "Todas") df <- df[df$region == input$filtro_region, ]
    if (!is.null(input$filtro_meridiano) && input$filtro_meridiano != "Todas") {
      df <- df[df$lng == as.numeric(input$filtro_meridiano), ]
    }
    df
  })
  
  output$aviso_filtro_descarga <- renderUI({
    req(input$modo_descarga)
    b <- boya_activa()
    if (input$modo_descarga == "filtradas" && input$filtro_region == "Todas" && input$filtro_meridiano == "Todas") {
      div(id = "aviso_filtro_descarga", bsicons::bs_icon("exclamation-triangle-fill"),
          HTML(" Para descargar varias boyas, <strong>filtre primero por Región o Longitud.</strong>"))
    } else if (input$modo_descarga == "seleccionada" && (is.null(b) || nrow(b) == 0)) {
      div(id = "aviso_filtro_descarga", bsicons::bs_icon("info-circle-fill"),
          HTML(" <strong>Seleccione una boya</strong> en el mapa haciendo clic sobre ella."))
    }
  })
  
  output$btn_descargar_ui <- renderUI({
    b <- boya_activa()
    cond_todas <- (!is.null(input$modo_descarga) && input$modo_descarga == "filtradas" &&
                     input$filtro_region == "Todas" && input$filtro_meridiano == "Todas")
    cond_boya <- (!is.null(input$modo_descarga) && input$modo_descarga == "seleccionada" &&
                    (is.null(b) || nrow(b) == 0))
    if (cond_todas || cond_boya) {
      tags$button("Descargar NetCDF", class = "btn btn-primary w-100 shadow-sm disabled",
                  style = "cursor: not-allowed; opacity: 0.55;", disabled = "disabled", type = "button")
    } else downloadButton("btn_descargar", "Descargar NetCDF", class = "btn-primary w-100 shadow-sm")
  })
  
  output$map <- renderLeaflet({
    leaflet() |>
      addMapPane("pane_boyas", zIndex = 410) |>
      addMapPane("pane_seleccionada", zIndex = 420) |>
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri Topo") |>
      addProviderTiles(providers$Esri.OceanBasemap, group = "Esri Océano") |>
      setView(lng = -140, lat = 0, zoom = 3) |>
      addPolylines(lng = c(-210, -60), lat = c(0, 0), color = "#0F172A", weight = 1.3,
                   dashArray = "5, 5", group = "Línea Ecuatorial (0°)") |>
      addPolygons(data = south_america, fillColor = ~color_normal, fillOpacity = 0.7,
                  color = "black", weight = 1.3, label = ~name,
                  highlightOptions = highlightOptions(color = "black", fillColor = "white",
                                                      fillOpacity = 0.25, weight = 3.5, bringToFront = TRUE),
                  group = "Sudamérica") |>
      addRectangles(lng1=-90, lat1=-10, lng2=-80, lat2=0, color="#DC2626", weight=2, fillOpacity=0.15, label="Niño 1+2", group="Niño 1+2") |>
      addRectangles(lng1=-150, lat1=-5, lng2=-90, lat2=5, color="#F97316", weight=2, fillOpacity=0.12, label="Niño 3", group="Niño 3") |>
      addRectangles(lng1=-200, lat1=-5, lng2=-150, lat2=5, color="#16A34A", weight=2, fillOpacity=0.12, label="Niño 4", group="Niño 4") |>
      addRectangles(lng1=-170, lat1=-5, lng2=-120, lat2=5, color="#9333EA", weight=2, dashArray="5, 5", fillOpacity=0.10, label="Niño 3.4", group="Niño 3.4") |>
      addScaleBar(position = "bottomleft") |>
      addMiniMap(tiles = providers$Esri.WorldTopoMap, toggleDisplay = TRUE, position = "bottomright") |>
      addLayersControl(baseGroups = c("Esri Topo","Esri Océano"),
                       overlayGroups = c("Boyas TAO/TRITON","Línea Ecuatorial (0°)","Sudamérica",
                                         "Niño 1+2","Niño 3","Niño 4","Niño 3.4"),
                       options = layersControlOptions(collapsed = TRUE))
  })
  
  observe({
    df <- boyas_filtradas()
    proxy <- leafletProxy("map")
    proxy %>% clearGroup("Boyas TAO/TRITON")
    if (nrow(df) > 0) {
      proxy %>% addCircleMarkers(
        data = df, lng = ~lng, lat = ~lat, layerId = ~id,
        radius = 6, fillColor = ~color, color = "#FFFFFF", weight = 2, fillOpacity = 0.95,
        label = ~label, options = pathOptions(pane = "pane_boyas"),
        popup = ~paste0(
          "<div style='min-width:200px; padding:5px;'>",
          "<div style='color:#075985; font-size:15px; font-weight:700; margin-bottom:7px;'>", label, "</div>",
          "<b>Tipo:</b> ", tipo, "<br><b>Región:</b> ", region, "<br>",
          "<b>Latitud:</b> ", lat, "°<br>",
          "<b>Longitud:</b> ", abs(lng_original), "°", ifelse(lng_original >= 0, "E","W"), "<br>",
          "<button onclick='Shiny.setInputValue(\"ver_serie\", {id: \"", id, "\"}, {priority: \"event\"})' ",
          "class='btn btn-primary btn-sm' style='margin-top:8px; background:#0284C7; color:white; border:none; padding:4px 10px; border-radius:5px;'>",
          "📊 Ver serie temporal</button></div>"),
        group = "Boyas TAO/TRITON")
    }
  })
  
  # ---------- Modal ----------
  definir_breaks_x <- function(fechas) {
    rango_dias <- as.numeric(diff(range(fechas, na.rm = TRUE)), units = "days")
    if (rango_dias < 30) list(breaks="1 day", labels="%d-%b")
    else if (rango_dias < 365) list(breaks="1 month", labels="%b %Y")
    else if (rango_dias < 365*3) list(breaks="3 months", labels="%b %Y")
    else if (rango_dias < 365*5) list(breaks="6 months", labels="%b %Y")
    else list(breaks="1 year", labels="%Y")
  }
  
  extraer_serie_temporal <- function(var_data, ntime) {
    if (is.vector(var_data)) {
      if (length(var_data) != ntime) {
        if (length(var_data) > ntime) var_data <- var_data[1:ntime]
        else var_data <- c(var_data, rep(NA, ntime - length(var_data)))
      }
      return(var_data)
    }
    dims <- dim(var_data)
    if (is.null(dims)) {
      var_data <- as.vector(var_data)
      if (length(var_data) > ntime) var_data <- var_data[1:ntime]
      else if (length(var_data) < ntime) var_data <- c(var_data, rep(NA, ntime - length(var_data)))
      return(var_data)
    }
    idx_tiempo <- which(dims == ntime); if (length(idx_tiempo) == 0) idx_tiempo <- length(dims)
    idx <- rep(1, length(dims)); idx[idx_tiempo] <- 1:ntime
    var_data_ext <- do.call(`[`, c(list(var_data), as.list(idx)))
    if (!is.null(dim(var_data_ext)) && length(dim(var_data_ext)) > 1) var_data_ext <- as.vector(var_data_ext)
    if (!is.vector(var_data_ext)) var_data_ext <- as.vector(var_data_ext)
    if (length(var_data_ext) > ntime) var_data_ext <- var_data_ext[1:ntime]
    else if (length(var_data_ext) < ntime) var_data_ext <- c(var_data_ext, rep(NA, ntime - length(var_data_ext)))
    var_data_ext
  }
  
  obtener_datos <- function(nc, var_name, prof_idx = NULL) {
    tryCatch({
      time <- ncvar_get(nc, "time")
      time_units <- ncatt_get(nc, "time", "units")$value
      ref <- strsplit(time_units, " ")[[1]][3]
      ref_date <- as.Date(ref, format = "%Y-%m-%d")
      time_dates <- ref_date + time
      ntime <- length(time)
      var_data <- ncvar_get(nc, var_name)
      if (is.array(var_data) && length(dim(var_data)) == 1) var_data <- as.vector(var_data)
      if (is.vector(var_data)) {
        if (length(var_data) != ntime) {
          if (length(var_data) > ntime) var_data <- var_data[1:ntime]
          else var_data <- c(var_data, rep(NA, ntime - length(var_data)))
        }
        return(data.frame(Fecha = time_dates, Valor = var_data))
      }
      if (is.matrix(var_data)) {
        if (nrow(var_data) == ntime) {
          if (!is.null(prof_idx) && prof_idx <= ncol(var_data)) var_data <- var_data[, prof_idx]
          else var_data <- var_data[, 1]
        } else if (ncol(var_data) == ntime) {
          if (!is.null(prof_idx) && prof_idx <= nrow(var_data)) var_data <- var_data[prof_idx, ]
          else var_data <- var_data[1, ]
        } else var_data <- var_data[, 1]
        var_data <- as.vector(var_data)
        if (length(var_data) != ntime) {
          if (length(var_data) > ntime) var_data <- var_data[1:ntime]
          else var_data <- c(var_data, rep(NA, ntime - length(var_data)))
        }
        return(data.frame(Fecha = time_dates, Valor = var_data))
      }
      if (length(dim(var_data)) > 2) {
        var_data <- extraer_serie_temporal(var_data, ntime)
        return(data.frame(Fecha = time_dates, Valor = var_data))
      }
      NULL
    }, error = function(e) NULL)
  }
  
  render_grafico_base <- function(df_ts) {
    breaks_info <- definir_breaks_x(df_ts$Fecha)
    output$grafico_serie <- renderPlot({
      par(mar = c(4, 3, 0.5, 1))
      plot(df_ts$Fecha, df_ts$Valor, type = "l", col = "#0284C7", lwd = 1,
           xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n",
           ylim = range(df_ts$Valor, na.rm = TRUE))
      points(df_ts$Fecha, df_ts$Valor, col = "#0284C7", pch = 16, cex = 0.1)
      if (!is.null(breaks_info)) {
        fechas_seq <- seq(from = min(df_ts$Fecha, na.rm = TRUE),
                          to = max(df_ts$Fecha, na.rm = TRUE),
                          by = breaks_info$breaks)
        axis(1, at = fechas_seq, labels = format(fechas_seq, breaks_info$labels),
             cex.axis = 0.7, las = 2)
      } else axis(1, cex.axis = 0.7, las = 2)
      axis(2, cex.axis = 0.7); box()
    }, height = 200, width = 350, res = 72)
  }
  
  observeEvent(input$ver_serie, {
    req(input$ver_serie, input$var_descarga)
    id_boya <- input$ver_serie$id
    var_seleccionada <- input$var_descarga
    df_var <- boyas_var()
    boya <- df_var[df_var$id == id_boya, ]
    if (nrow(boya) == 0) { showNotification("No se encontró la boya seleccionada.", type="error"); return() }
    lon_orig <- boya$lng_original[1]
    lon_str <- if (lon_orig < 0) paste0(abs(lon_orig), "w") else paste0(lon_orig, "e")
    lat_abs <- abs(boya$lat[1]); lat_dir <- ifelse(boya$lat[1] >= 0, "n", "s")
    nombre_archivo <- paste0(var_seleccionada, lat_abs, lat_dir, lon_str, "_dy.cdf")
    url <- paste0("https://www.pmel.noaa.gov/tao/taoweb/disdel_data/cdf/sites/daily/", nombre_archivo)
    ruta_local <- file.path(cache_dir, nombre_archivo)
    
    if (!is.null(cache_nc[[nombre_archivo]])) nc <- cache_nc[[nombre_archivo]]
    else if (file.exists(ruta_local)) {
      nc <- tryCatch(nc_open(ruta_local), error = function(e) NULL)
      if (!is.null(nc)) cache_nc[[nombre_archivo]] <- nc
    } else {
      mensaje_html <- HTML("Conectando con el servidor...<br>⏱️ Tiempo transcurrido: <b><span id='dl_timer'>0.0</span> s</b>
        <script>var dl_sec=0; var dl_int=setInterval(function(){dl_sec+=0.1; var el=document.getElementById('dl_timer');
        if(el){el.innerText=dl_sec.toFixed(1);} else {clearInterval(dl_int);}},100);</script>")
      id_notif <- showNotification(mensaje_html, duration = NULL, type = "message")
      on.exit(removeNotification(id_notif), add = TRUE)
      tryCatch(curl::curl_download(url, destfile = ruta_local, quiet = TRUE),
               error = function(e) { showNotification("Error al descargar el archivo.", type="error"); return() })
      nc <- tryCatch(nc_open(ruta_local), error = function(e) NULL)
      if (is.null(nc)) return()
      cache_nc[[nombre_archivo]] <- nc
    }
    
    vars <- names(nc$var)
    variable_a_graficar <- NULL
    profundidades_info <- NULL
    
    if (var_seleccionada == "t") {
      vars_t <- grep("^T_", vars, value = TRUE)
      if (length(vars_t) == 0) { showNotification("Sin variables T_* en el archivo.", type="error"); return() }
      var_principal <- vars_t[1]; variable_a_graficar <- var_principal
      var_dims <- nc$var[[var_principal]]$dim
      depth_dim <- NULL
      for (d in var_dims) if (d$name == "depth") { depth_dim <- d; break }
      if (!is.null(depth_dim)) {
        depth_vals <- ncvar_get(nc, "depth")
        if (length(depth_vals) > 1) profundidades_info <- list(
          vals = depth_vals, names = paste0(depth_vals, " m"), indices = seq_along(depth_vals))
      }
    } else {
      var_nc <- mapa_variables_nc[[var_seleccionada]]
      if (is.null(var_nc)) { showNotification("Sin mapeo para esta variable.", type="error"); return() }
      if (!(var_nc %in% vars)) {
        showNotification(paste0("Variable '", var_nc, "' no existe."), type="error", duration=10); return()
      }
      variable_a_graficar <- var_nc
    }
    
    if (!is.null(profundidades_info)) {
      output$selector_profundidad <- renderUI({
        selectInput("profundidad_seleccionada", paste("Profundidad:", id_boya),
                    choices = setNames(profundidades_info$indices, profundidades_info$names), selected = 1)
      })
      observeEvent(input$profundidad_seleccionada, {
        req(input$profundidad_seleccionada)
        df_ts <- obtener_datos(nc, variable_a_graficar, prof_idx = as.numeric(input$profundidad_seleccionada))
        if (!is.null(df_ts) && nrow(df_ts) > 0) render_grafico_base(df_ts)
      }, ignoreNULL = FALSE)
      showModal(modalDialog(
        tags$style(".modal-header{justify-content:center !important; border-bottom:none !important; padding-top:15px !important; padding-bottom:0 !important;} .modal-body{padding-top:0 !important; padding-bottom:5px !important;}"),
        uiOutput("selector_profundidad"),
        div(style="width:100%; text-align:center; margin-top:-5px;", plotOutput("grafico_serie", height="170px")),
        footer = modalButton("Cerrar"), size = "m", easyClose = TRUE))
      isolate({
        df_ts_inicial <- obtener_datos(nc, variable_a_graficar, prof_idx = 1)
        if (!is.null(df_ts_inicial) && nrow(df_ts_inicial) > 0) render_grafico_base(df_ts_inicial)
      })
    } else {
      df_ts <- obtener_datos(nc, variable_a_graficar)
      if (is.null(df_ts) || nrow(df_ts) == 0) { showNotification("Sin datos válidos.", type="error"); return() }
      showModal(modalDialog(
        tags$style(".modal-header{justify-content:center !important; border-bottom:none !important; padding-top:15px !important; padding-bottom:0 !important;} .modal-body{padding-top:0 !important; padding-bottom:5px !important;}"),
        title = tags$div(style="width:100%; text-align:center; font-size:14px; font-weight:bold; margin-bottom:0;",
                         paste("Serie temporal: ", id_boya)),
        div(style="width:100%; text-align:center; margin-top:0;", plotOutput("grafico_serie", height="180px")),
        footer = modalButton("Cerrar"), size = "m", easyClose = TRUE))
      render_grafico_base(df_ts)
    }
  })
  
  observe({
    b <- boya_activa()
    if (is.null(b) || nrow(b) == 0) leafletProxy("map") |> clearGroup("Boya Seleccionada")
    else leafletProxy("map") |> clearGroup("Boya Seleccionada") |>
      addCircleMarkers(data = b, lng = ~lng, lat = ~lat, layerId = paste0("sel_", b$id),
                       radius = 8, fillColor = "#FFD700", color = "#000000", weight = 2,
                       fillOpacity = 1, options = pathOptions(pane = "pane_seleccionada"),
                       group = "Boya Seleccionada")
  })
  
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (grepl("^sel_", click$id)) return()
    df_var <- boyas_var()
    sel <- df_var[df_var$id == click$id, ]
    if (nrow(sel) > 0) boya_activa(sel)
  })
  
  output$btn_descargar <- downloadHandler(
    filename = function() {
      datos_objetivo <- if (input$modo_descarga == "seleccionada") boya_activa() else boyas_filtradas()
      if (nrow(datos_objetivo) == 1) {
        lat <- datos_objetivo$lat[1]; lng_orig <- datos_objetivo$lng_original[1]
        lon_str <- if (lng_orig < 0) paste0(abs(lng_orig), "w") else paste0(lng_orig, "e")
        return(paste0(input$var_descarga, abs(lat), ifelse(lat >= 0, "n", "s"), lon_str, "_dy.cdf"))
      } else return(paste0("Datos_TAO_", input$var_descarga, "_", Sys.Date(), ".zip"))
    },
    content = function(file) {
      datos_objetivo <- if (input$modo_descarga == "seleccionada") boya_activa() else boyas_filtradas()
      if (nrow(datos_objetivo) == 0) return(req(FALSE))
      if (input$modo_descarga == "filtradas" && input$filtro_region == "Todas" && input$filtro_meridiano == "Todas") req(FALSE)
      mensaje_html <- HTML("Contactando servidores NOAA...<br>⏱️ Tiempo: <b><span id='dl_timer_zip'>0.0</span> s</b>
        <script>var dl_sec_zip=0; var dl_int_zip=setInterval(function(){dl_sec_zip+=0.1; var el=document.getElementById('dl_timer_zip');
        if(el){el.innerText=dl_sec_zip.toFixed(1);} else {clearInterval(dl_int_zip);}},100);</script>")
      id_notif <- showNotification(mensaje_html, duration = NULL, type = "message")
      on.exit(removeNotification(id_notif))
      nombres_archivos <- c()
      for (i in 1:nrow(datos_objetivo)) {
        lat <- datos_objetivo$lat[i]; lng_orig <- datos_objetivo$lng_original[i]
        lon_str <- if (lng_orig < 0) paste0(abs(lng_orig), "w") else paste0(lng_orig, "e")
        nombres_archivos <- c(nombres_archivos,
                              paste0(input$var_descarga, abs(lat), ifelse(lat >= 0, "n", "s"), lon_str, "_dy.cdf"))
      }
      urls <- paste0("https://www.pmel.noaa.gov/tao/taoweb/disdel_data/cdf/sites/daily/", nombres_archivos)
      temp_dir <- tempdir(); rutas_locales <- file.path(temp_dir, nombres_archivos)
      tryCatch({
        res_descargas <- curl::multi_download(urls, destfiles = rutas_locales)
        rutas_exitosas <- rutas_locales[res_descargas$status_code %in% c(200, 226)]
      }, error = function(e) rutas_exitosas <- c())
      if (length(rutas_exitosas) == 0) {
        showNotification("Ningún archivo encontrado en NOAA.", type = "error")
        ruta_vacia <- file.path(temp_dir, "sin_datos.txt")
        writeLines("No se encontraron archivos .cdf.", ruta_vacia)
        file.copy(ruta_vacia, file)
      } else if (length(rutas_exitosas) == 1) file.copy(rutas_exitosas[1], file)
      else zip::zipr(zipfile = file, files = rutas_exitosas)
    }
  )
  
  # ============================================================
  # GRÁFICAS POR PESTAÑA
  # ============================================================
  obtener_nc_boya <- function(var_id, boya) {
    lat <- boya$lat[1]; lng_orig <- boya$lng_original[1]
    lon_str <- if (lng_orig < 0) paste0(abs(lng_orig), "w") else paste0(lng_orig, "e")
    nombre_archivo <- paste0(var_id, abs(lat), ifelse(lat >= 0, "n", "s"), lon_str, "_dy.cdf")
    if (!is.null(cache_nc[[nombre_archivo]])) return(cache_nc[[nombre_archivo]])
    ruta_local <- file.path(cache_dir, nombre_archivo)
    url <- paste0("https://www.pmel.noaa.gov/tao/taoweb/disdel_data/cdf/sites/daily/", nombre_archivo)
    if (!file.exists(ruta_local)) {
      ok <- tryCatch({ curl::curl_download(url, destfile = ruta_local, quiet = TRUE); TRUE },
                     error = function(e) FALSE)
      if (!ok) return(NULL)
    }
    nc <- tryCatch(nc_open(ruta_local), error = function(e) NULL)
    if (is.null(nc)) return(NULL)
    cache_nc[[nombre_archivo]] <- nc
    nc
  }
  

  a_plotly <- function(p) {
    gg <- ggplotly(p, tooltip = "text")
    for (i in seq_along(gg$x$data)) {
      tr <- gg$x$data[[i]]
      m  <- tr$mode
      if (is.character(m) && length(m) == 1 && grepl("text", m)) {
        tr$mode <- switch(m,
                          "text"               = "lines",
                          "lines+text"         = "lines",
                          "text+lines"         = "lines",
                          "markers+text"       = "markers",
                          "text+markers"       = "markers",
                          "lines+markers+text" = "lines+markers",
                          m
        )
        gg$x$data[[i]] <- tr
      }
    }
    gg %>% layout(autosize = TRUE) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE, responsive = TRUE)
  }
  
  variables_config <- list(
    sst = list(graficar = graficar_sst, ylab_hist = "[°C]",             ylab_anom = "Anomalías [°C]"),
    dyn = list(graficar = graficar_dyn, ylab_hist = "Altura Dinámica [cm]", ylab_anom = "Anomalías [cm]"),
    iso = list(graficar = graficar_iso, ylab_hist = "Profundidad [m]",  ylab_anom = "Profundidad [m]"),
    w   = list(graficar = graficar_w,   ylab_hist = "Velocidad [m/s]",  ylab_anom = "Anomalía [m/s]")
  )
  
  for (v in names(variables_config)) {
    local({
      var_id <- v
      cfg <- variables_config[[v]]
      id_boya_in  <- paste0("boya_", var_id)
      id_p1_in    <- paste0("anio_p1_", var_id)
      id_p2_in    <- paste0("anio_p2_", var_id)
      id_hist_out <- paste0("hist_", var_id)
      id_anom_out <- paste0("anom_", var_id)
      id_th_in    <- paste0("titulo_hist_", var_id)
      id_ta_in    <- paste0("titulo_anom_", var_id)
      id_ylh_in   <- paste0("ylab_hist_", var_id)
      id_yla_in   <- paste0("ylab_anom_", var_id)
      
      boya_sel <- reactive({
        df <- boyas_por_variable[[var_id]]
        req(input[[id_boya_in]])
        if (is.null(df) || nrow(df) == 0) return(df[0, , drop = FALSE])
        df[df$id == input[[id_boya_in]], , drop = FALSE]
      })
      
      df_merge_react <- reactive({
        boya <- boya_sel()
        if (is.null(boya) || nrow(boya) == 0) return(NULL)
        nc <- obtener_nc_boya(var_id, boya)
        if (is.null(nc)) return(NULL)
        var_nc <- mapa_variables_nc[[var_id]]
        if (is.null(var_nc)) {
          vars <- names(nc$var)
          vars_t <- grep("^T_[0-9]+", vars, value = TRUE)
          if (length(vars_t) == 0) return(NULL)
          var_nc <- vars_t[1]
        }
        tryCatch(procesar_nc(nc, var_nc), error = function(e) NULL)
      })
      
      anio_p1_deb <- debounce(reactive({ input[[id_p1_in]] }), 600)
      anio_p2_deb <- debounce(reactive({ input[[id_p2_in]] }), 600)
      th_deb  <- debounce(reactive({ input[[id_th_in]]  }), 400)
      ta_deb  <- debounce(reactive({ input[[id_ta_in]]  }), 400)
      ylh_deb <- debounce(reactive({ input[[id_ylh_in]] }), 400)
      yla_deb <- debounce(reactive({ input[[id_yla_in]] }), 400)
      
      colores_react <- reactive({
        c(input[[paste0("col1_", var_id)]],
          input[[paste0("col2_", var_id)]],
          input[[paste0("col3_", var_id)]],
          input[[paste0("col4_", var_id)]],
          input[[paste0("col5_", var_id)]])
      })
      tema_react <- reactive({
        if (var_id %in% c("sst", "dyn", "iso", "w")) {
          input[[paste0("tema_", var_id)]] %||% (if (var_id == "w") "bw" else "minimal")
        } else "minimal"
      })
      
      grafico_react <- reactive({
        dm <- df_merge_react()
        es_w <- (var_id == "w")
        
        if (is.null(dm) || nrow(dm) == 0) {
          p <- ggplot_aviso("Datos insuficientes",
                            "Sin datos disponibles (descargando o boya sin datos)")
          if (es_w) return(list(hist_p1 = p, hist_p2 = p, anom_p1 = p, anom_p2 = p))
          return(list(hist = p, anom = p))
        }
        anios_disp <- unique(dm$anio)
        anios_clim <- anios_disp[anios_disp >= 1991 & anios_disp <= 2020]
        if (length(anios_clim) < 25) {
          p <- ggplot_aviso("Datos insuficientes",
                            paste0("Se requieren al menos 25 años para la climatología.\n",
                                   "Años disponibles en [1991-2020]: ", length(anios_clim)))
          if (es_w) return(list(hist_p1 = p, hist_p2 = p, anom_p1 = p, anom_p2 = p))
          return(list(hist = p, anom = p))
        }
        
        y1 <- anio_p1_deb(); y2 <- anio_p2_deb()
        req(y1, y2)
        th  <- th_deb(); ta  <- ta_deb()
        ylh <- ylh_deb(); yla <- yla_deb()
        
        clim_range <- attr(dm, "clim_range")
        if (!is.null(clim_range) && length(clim_range) == 2) {
          pat <- "\\[\\d{4}-\\d{4}\\]"
          repl <- paste0("[", clim_range[1], "-", clim_range[2], "]")
          if (!is.null(th) && grepl(pat, th)) th <- gsub(pat, repl, th)
          if (!is.null(ta) && grepl(pat, ta)) ta <- gsub(pat, repl, ta)
        }
        
        tryCatch({
          args_list <- list(
            dm, c(y1, y1 + 1), c(y2, y2 + 1),
            if (is.null(th)  || th  == "") paste("Serie Histórica -", boya_sel()$label[1]) else th,
            if (is.null(ta)  || ta  == "") paste("Anomalías -",        boya_sel()$label[1]) else ta,
            ylab_hist = if (is.null(ylh) || ylh == "") cfg$ylab_hist else ylh,
            ylab_anom = if (is.null(yla) || yla == "") cfg$ylab_anom else yla,
            colores   = colores_react()
          )
          if (var_id %in% c("sst", "dyn", "iso", "w")) args_list$tema <- tema_react()
          do.call(cfg$graficar, args_list)
        }, error = function(e) {
          p <- ggplot_aviso("Error al generar gráfico", e$message)
          if (es_w) return(list(hist_p1 = p, hist_p2 = p, anom_p1 = p, anom_p2 = p))
          list(hist = p, anom = p)
        })
      })
      
      if (var_id == "w") {
        output[[paste0("hist_p1_", var_id)]] <- renderPlotly({ a_plotly(grafico_react()$hist_p1) })
        output[[paste0("hist_p2_", var_id)]] <- renderPlotly({ a_plotly(grafico_react()$hist_p2) })
        output[[paste0("anom_p1_", var_id)]] <- renderPlotly({ a_plotly(grafico_react()$anom_p1) })
        output[[paste0("anom_p2_", var_id)]] <- renderPlotly({ a_plotly(grafico_react()$anom_p2) })
        
        ancho_dl_w <- 7; alto_dl_w <- 9
        output[[paste0("dl_hist_p1_", var_id)]] <- downloadHandler(
          filename = function() paste0("hist_p1_", var_id, "_", Sys.Date(), ".png"),
          content = function(file) ggsave(file, plot = grafico_react()$hist_p1,
                                          width = ancho_dl_w, height = alto_dl_w,
                                          dpi = 900, bg = "white", units = "in", limitsize = FALSE)
        )
        output[[paste0("dl_hist_p2_", var_id)]] <- downloadHandler(
          filename = function() paste0("hist_p2_", var_id, "_", Sys.Date(), ".png"),
          content = function(file) ggsave(file, plot = grafico_react()$hist_p2,
                                          width = ancho_dl_w, height = alto_dl_w,
                                          dpi = 900, bg = "white", units = "in", limitsize = FALSE)
        )
        output[[paste0("dl_anom_p1_", var_id)]] <- downloadHandler(
          filename = function() paste0("anom_p1_", var_id, "_", Sys.Date(), ".png"),
          content = function(file) ggsave(file, plot = grafico_react()$anom_p1,
                                          width = ancho_dl_w, height = alto_dl_w,
                                          dpi = 900, bg = "white", units = "in", limitsize = FALSE)
        )
        output[[paste0("dl_anom_p2_", var_id)]] <- downloadHandler(
          filename = function() paste0("anom_p2_", var_id, "_", Sys.Date(), ".png"),
          content = function(file) ggsave(file, plot = grafico_react()$anom_p2,
                                          width = ancho_dl_w, height = alto_dl_w,
                                          dpi = 900, bg = "white", units = "in", limitsize = FALSE)
        )
      } else {
        output[[id_hist_out]] <- renderPlotly({ a_plotly(grafico_react()$hist) })
        output[[id_anom_out]] <- renderPlotly({ a_plotly(grafico_react()$anom) })
        
        ancho_dl <- 14; alto_dl <- 7
        output[[paste0("dl_hist_", var_id)]] <- downloadHandler(
          filename = function() paste0("hist_", var_id, "_", Sys.Date(), ".png"),
          content = function(file) ggsave(file, plot = grafico_react()$hist,
                                          width = ancho_dl, height = alto_dl,
                                          dpi = 900, bg = "white", units = "in", limitsize = FALSE)
        )
        output[[paste0("dl_anom_", var_id)]] <- downloadHandler(
          filename = function() paste0("anom_", var_id, "_", Sys.Date(), ".png"),
          content = function(file) ggsave(file, plot = grafico_react()$anom,
                                          width = ancho_dl, height = alto_dl,
                                          dpi = 900, bg = "white", units = "in", limitsize = FALSE)
        )
      }
    })
  }
  
  # ============================================================
  # Bloque específico para Temperatura Subsuperficial  (NO interactiva)
  # ============================================================
  local({
    id_boya_in <- "boya_t"
    boya_sel_t <- reactive({
      df <- boyas_por_variable[["t"]]
      req(input[[id_boya_in]])
      if (is.null(df) || nrow(df) == 0) return(df[0, , drop = FALSE])
      df[df$id == input[[id_boya_in]], , drop = FALSE]
    })
    
    df_perfil_react <- reactive({
      boya <- boya_sel_t()
      if (is.null(boya) || nrow(boya) == 0) return(NULL)
      nc <- obtener_nc_boya("t", boya)
      if (is.null(nc)) return(NULL)
      vars <- names(nc$var)
      vars_t <- grep("^T_[0-9]+", vars, value = TRUE)
      if (length(vars_t) == 0) return(NULL)
      tryCatch(procesar_nc_perfil(nc, vars_t[1]), error = function(e) NULL)
    })
    
    anio_p1_deb  <- debounce(reactive({ input$anio_p1_t }),  700)
    anio_p2_deb  <- debounce(reactive({ input$anio_p2_t }),  700)
    clim_ini_deb <- debounce(reactive({ input$clim_ini_t }), 700)
    clim_fin_deb <- debounce(reactive({ input$clim_fin_t }), 700)
    prof_max_deb <- debounce(reactive({ input$prof_max_t }), 700)
    titulo_hist_deb <- debounce(reactive({ input$titulo_hist_t }), 400)
    titulo_anom_deb <- debounce(reactive({ input$titulo_anom_t }), 400)
    ylab_hist_deb   <- debounce(reactive({ input$ylab_hist_t   }), 400)
    ylab_anom_deb   <- debounce(reactive({ input$ylab_anom_t   }), 400)
    
    colores_react_t <- reactive({
      c(input[["col1_t"]], input[["col2_t"]], input[["col3_t"]],
        input[["col4_t"]], input[["col5_t"]])
    })
    
    datos_contorno_react <- reactive({
      df <- df_perfil_react()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      y1 <- anio_p1_deb(); y2 <- anio_p2_deb()
      req(y1, y2)
      clim_ini <- clim_ini_deb(); clim_fin <- clim_fin_deb(); prof_max <- prof_max_deb()
      if (is.null(clim_ini)) clim_ini <- "1991-01-01"
      if (is.null(clim_fin)) clim_fin <- "2020-12-31"
      if (is.null(prof_max)) prof_max <- 500
      tryCatch(
        calcular_datos_contorno_t(df, c(y1, y1 + 1), c(y2, y2 + 1),
                                  inicio = clim_ini, fin = clim_fin, prof_max = prof_max),
        error = function(e) list(status = "insuficiente", mensaje = paste("Error:", e$message))
      )
    })
    
    plots_contorno_react <- reactive({
      datos <- datos_contorno_react()
      if (is.null(datos)) {
        p_aviso <- ggplot_aviso("Datos insuficientes",
                                "Sin datos disponibles (descargando o boya sin datos)")
        return(list(hist_p1 = p_aviso, hist_p2 = p_aviso, anom_p1 = p_aviso, anom_p2 = p_aviso))
      }
      th  <- titulo_hist_deb(); ta  <- titulo_anom_deb()
      ylh <- ylab_hist_deb();   yla <- ylab_anom_deb()
      label_boya <- if (nrow(boya_sel_t()) > 0) boya_sel_t()$label[1] else ""
      tryCatch({
        construir_plots_contorno_t(
          datos,
          if (is.null(th) || th == "") paste("Temperatura Subsuperficial -", label_boya) else th,
          if (is.null(ta) || ta == "") paste("Anomalías -", label_boya) else ta,
          ylab_hist = if (is.null(ylh) || ylh == "") "Profundidad [m]" else ylh,
          ylab_anom = if (is.null(yla) || yla == "") "Profundidad [m]" else yla,
          colores   = colores_react_t()
        )
      }, error = function(e) {
        p_err <- ggplot_aviso("Error al generar gráfico", e$message)
        list(hist_p1 = p_err, hist_p2 = p_err, anom_p1 = p_err, anom_p2 = p_err)
      })
    })
    
    output$hist_p1_t <- renderPlot({ plots_contorno_react()$hist_p1 })
    output$hist_p2_t <- renderPlot({ plots_contorno_react()$hist_p2 })
    output$anom_p1_t <- renderPlot({ plots_contorno_react()$anom_p1 })
    output$anom_p2_t <- renderPlot({ plots_contorno_react()$anom_p2 })
    
    output$dl_hist_p1_t <- downloadHandler(
      filename = function() paste0("hist_p1_t_", Sys.Date(), ".png"),
      content = function(file) {
        ggsave(file, plot = plots_contorno_react()$hist_p1,
               width = 14, height = 6, dpi = 900, bg = "white", units = "in", limitsize = FALSE)
      }
    )
    output$dl_hist_p2_t <- downloadHandler(
      filename = function() paste0("hist_p2_t_", Sys.Date(), ".png"),
      content = function(file) {
        ggsave(file, plot = plots_contorno_react()$hist_p2,
               width = 14, height = 6, dpi = 900, bg = "white", units = "in", limitsize = FALSE)
      }
    )
    output$dl_anom_p1_t <- downloadHandler(
      filename = function() paste0("anom_p1_t_", Sys.Date(), ".png"),
      content = function(file) {
        ggsave(file, plot = plots_contorno_react()$anom_p1,
               width = 14, height = 6, dpi = 900, bg = "white", units = "in", limitsize = FALSE)
      }
    )
    output$dl_anom_p2_t <- downloadHandler(
      filename = function() paste0("anom_p2_t_", Sys.Date(), ".png"),
      content = function(file) {
        ggsave(file, plot = plots_contorno_react()$anom_p2,
               width = 14, height = 6, dpi = 900, bg = "white", units = "in", limitsize = FALSE)
      }
    )
  })
  
  `%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a
}

shinyApp(ui, server)
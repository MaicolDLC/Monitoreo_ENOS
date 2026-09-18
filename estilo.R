# ============================================================
# ESTILO.R — Paleta, tema bslib, CSS y helpers de UI
# ============================================================

colores_pasteles <- c(
  "#FDE68A","#BBF7D0","#BFDBFE","#E9D5FF","#FECACA","#D9F99D","#A7F3D0","#BAE6FD",
  "#DDD6FE","#FBCFE8","#FEF08A","#86EFAC","#93C5FD","#C4B5FD","#FCA5A5","#D4D4D8",
  "#FDE047","#6EE7B7","#7DD3FC","#A78BFA"
)

# --- Tema bslib ---------------------------------------------------------
tema_bslib <- bs_theme(
  version = 5, primary = "#075985", secondary = "#0EA5E9",
  bg = "#F1F7FB", fg = "#17324D",
  base_font = font_google("Inter"), heading_font = font_google("Inter"),
  "border-radius" = "0.7rem", "navbar-bg" = "#063B5C", "navbar-color" = "#FFFFFF"
)

# --- CSS personalizado --------------------------------------------------
css_monitoreo <- HTML("
  :root { --fondo: #F1F7FB; --texto: #17324D; }
  body { background: var(--fondo) !important; color: var(--texto); font-family: 'Inter', sans-serif; }
  .navbar { background: linear-gradient(90deg, #063B5C 0%, #075985 55%, #0369A1 100%) !important; min-height: 64px; box-shadow: 0 3px 12px rgba(3, 105, 161, 0.25); border-bottom: 3px solid #38BDF8; }
  .navbar-brand { color: #FFFFFF !important; font-weight: 700; font-size: 1.2rem; }
  .navbar-brand svg { margin-right: 7px; vertical-align: -2px; }
  .navbar-nav .nav-link { color: rgba(255,255,255,0.78) !important; font-weight: 500; padding: 0 16px !important; transition: all 0.2s ease; }
  .navbar-nav .nav-link:hover, .navbar-nav .nav-link.active { color: #FFFFFF !important; background: rgba(255,255,255,0.15); border-radius: 8px; font-weight: 650; }
  .sidebar { background: linear-gradient(180deg, #EAF6FC 0%, #F5FAFD 100%) !important; border-right: 1px solid #C7DDEA !important; box-shadow: 3px 0 12px rgba(7, 89, 133, 0.08); }
  .accordion-item { border: 1px solid #BFD7E5 !important; border-radius: 10px !important; margin-bottom: 12px; background: #FFFFFF; box-shadow: 0 2px 8px rgba(7, 89, 133, 0.08); }
  .accordion-button { background: linear-gradient(90deg, #DFF3FC, #EFF9FD) !important; color: #075985 !important; font-weight: 650; padding: 13px 15px !important; border: none !important; }
  .accordion-button:not(.collapsed) { background: linear-gradient(90deg, #BAE6FD, #E0F2FE) !important; color: #064E6E !important; box-shadow: inset 4px 0 0 #0284C7; }
  .accordion-body { background: #FFFFFF; padding: 17px 16px 18px 16px; }
  .form-label { color: #164E63 !important; font-weight: 600; font-size: 0.88rem; margin-bottom: 6px; }
  .form-select, .form-control { border: 1px solid #B8D4E3 !important; border-radius: 8px !important; background-color: #F8FCFE !important; color: #17324D !important; min-height: 40px; }
  .form-select:hover, .form-control:hover { border-color: #38BDF8 !important; }
  .form-select:focus, .form-control:focus { border-color: #0284C7 !important; box-shadow: 0 0 0 3px rgba(14,165,233,0.15) !important; }
  textarea.form-control { min-height: 60px !important; }
  #modo_descarga .form-check { margin: 8px 0 !important; padding: 0 !important; border: none !important; background: transparent !important; }
  #modo_descarga .form-check-input { position: absolute; opacity: 0; pointer-events: none; }
  #modo_descarga .form-check-label { display: block; width: 100%; padding: 13px 13px 13px 43px; border: 1px solid #C7DDEA; border-radius: 10px; background: #F8FCFE; color: #36556B; font-size: 0.86rem; font-weight: 550; cursor: pointer; position: relative; transition: all 0.2s ease; }
  #modo_descarga .form-check-label::before { content: ''; position: absolute; left: 15px; top: 50%; transform: translateY(-50%); width: 16px; height: 16px; border: 2px solid #7AAEC5; border-radius: 50%; background: #FFFFFF; }
  #modo_descarga .form-check-input:checked + .form-check-label { border-color: #0284C7; background: linear-gradient(135deg, #E0F2FE, #F0F9FF); color: #075985; font-weight: 650; box-shadow: 0 3px 9px rgba(2,132,199,0.15); transform: translateY(-1px); }
  #modo_descarga .form-check-input:checked + .form-check-label::before { border: 5px solid #0284C7; }
  #modo_descarga .form-check-label:hover { border-color: #7DD3FC; background: #F0F9FF; transform: translateY(-1px); }
  #aviso_filtro_descarga { margin-top: 10px; padding: 10px 12px; border-radius: 8px; background: #FFF7ED; border: 1px solid #FED7AA; color: #9A3412; font-size: 0.80rem; line-height: 1.4; }
  .btn-primary { background: linear-gradient(135deg, #0369A1, #0284C7) !important; border: none !important; border-radius: 9px !important; font-weight: 650 !important; padding: 10px 15px !important; box-shadow: 0 4px 10px rgba(2,132,199,0.25); transition: all 0.2s ease; }
  .btn-primary:hover { background: linear-gradient(135deg, #075985, #0369A1) !important; transform: translateY(-1px); box-shadow: 0 6px 14px rgba(2,132,199,0.32); }
  .card { border: 1px solid #C7DDEA !important; border-radius: 12px !important; background: #FFFFFF !important; box-shadow: 0 4px 16px rgba(7,89,133,0.10) !important; overflow: hidden; }
  .card-header { background: linear-gradient(90deg, #E0F2FE 0%, #F0F9FF 100%) !important; color: #075985 !important; font-weight: 700 !important; padding: 13px 17px !important; border-bottom: 1px solid #BAE6FD !important; }
  .leaflet-container { background: #DCEEF7 !important; font-family: 'Inter', sans-serif; }
  .bslib-mode-switch { border: 1px solid #7DD3FC !important; background: #FFFFFF !important; color: #075985 !important; box-shadow: 0 3px 8px rgba(0,0,0,0.15) !important; border-radius: 8px !important; }
  [data-bs-theme='dark'] body { background: #071E2C !important; }
  [data-bs-theme='dark'] .sidebar { background: linear-gradient(180deg, #08283A, #0B3449) !important; border-right: 1px solid #15506C !important; }
  [data-bs-theme='dark'] .accordion-item { background: #0C3043 !important; border-color: #1D5870 !important; }
  [data-bs-theme='dark'] .accordion-button { background: #0D3B52 !important; color: #BAE6FD !important; }
  [data-bs-theme='dark'] .accordion-button:not(.collapsed) { background: #075985 !important; color: #FFFFFF !important; }
  [data-bs-theme='dark'] .accordion-body { background: #0C3043 !important; }
  [data-bs-theme='dark'] .form-label { color: #BAE6FD !important; }
  [data-bs-theme='dark'] .form-select, [data-bs-theme='dark'] .form-control { background: #092738 !important; color: #E0F2FE !important; border-color: #28657F !important; }
  [data-bs-theme='dark'] .form-check-label { color: #CBD5E1 !important; }
  [data-bs-theme='dark'] #modo_descarga .form-check-label { background: #092738; border-color: #28657F; color: #CBD5E1; }
  [data-bs-theme='dark'] #modo_descarga .form-check-input:checked + .form-check-label { background: #0D4058; border-color: #38BDF8; color: #BAE6FD; }
  [data-bs-theme='dark'] .card { background: #0B2B3D !important; border-color: #15506C !important; }
  [data-bs-theme='dark'] .card-header { background: linear-gradient(90deg, #0B405B, #0D3447) !important; color: #BAE6FD !important; border-color: #15506C !important; }
  [data-bs-theme='dark'] .bslib-mode-switch { border: 1.5px solid #64b5f6 !important; background-color: #0b1d3a !important; color: #ffeb3b !important; }
  [data-bs-theme='dark'] div[style*='padding:6px 4px 15px 4px;'] > div:nth-child(1) { color: #BAE6FD !important; }
  [data-bs-theme='dark'] div[style*='padding:6px 4px 15px 4px;'] > div:nth-child(2) { color: #E0F2FE !important; }
  [data-bs-theme='dark'] div[style*='padding:6px 4px 15px 4px;'] > div:nth-child(3) { color: #94A3B8 !important; }
  .shiny-spinner-output-container { background: #FFFFFF !important; }
  .color-grid { display: grid; grid-template-columns: 1fr; gap: 8px; }
  .color-grid .form-group { margin-bottom: 0 !important; }
  .color-grid .form-group label { font-size: 0.78rem !important; margin-bottom: 2px !important; }
  .btn-dl-header { background: #FFFFFF !important; color: #075985 !important; border: 1px solid #BAE6FD !important; border-radius: 8px !important; padding: 4px 9px !important; font-size: 0.82rem; box-shadow: 0 2px 5px rgba(7,89,133,0.10); transition: all 0.15s ease; }
  .btn-dl-header:hover { background: #0284C7 !important; color: #FFFFFF !important; border-color: #0284C7 !important; transform: translateY(-1px); box-shadow: 0 4px 10px rgba(2,132,199,0.28); }
")

# ============================================================
# HELPERS: tema ggplot, defaults de colores, controles de UI
# ============================================================

obtener_tema <- function(nombre = "minimal") {
  switch(nombre,
         "minimal" = ggplot2::theme_minimal(),
         "bw"      = ggplot2::theme_bw(),
         "classic" = ggplot2::theme_classic(),
         "light"   = ggplot2::theme_light(),
         "gray"    = ggplot2::theme_gray(),
         ggplot2::theme_minimal()
  )
}

etiquetas_color <- function(var_id) {
  switch(var_id,
         "sst" = c("Prom. histórico", "Línea serie P1", "Línea serie P2", "Línea cero", "Separador año"),
         "dyn" = c("Línea serie P1", "Línea serie P2", "Prom. histórico", "Línea cero", "Separador año"),
         "iso" = c("Línea serie P1", "Línea serie P2", "Prom. histórico", "Línea cero", "Separador año"),
         "w"   = c("Barras del año", "Línea prom. histórico", "Anomalía positiva", "Anomalía negativa", "Separador año"),
         "t"   = c("Contorno sólido Temp.", "Contorno discontinuo Temp.", "Contorno sólido Anom.",
                   "Contorno discontinuo Anom.", "Etiqueta de contorno"),
         c("Color 1", "Color 2", "Color 3", "Color 4", "Color 5")
  )
}

colores_default <- function(var_id) {
  switch(var_id,
         "sst" = c("#0000CD", "#D32F2F", "black",  "red", "red"),
         "dyn" = c("#D32F2F", "black",  "#6495ED","red", "red"),
         "iso" = c("#D32F2F", "black",  "#B0B0B0","red", "red"),
         "w"   = c("#D32F2F", "blue",   "#D32F2F","#4B92DB", "red"),
         "t"   = c("black",   "grey40", "black",  "grey40", "grey30"),
         c("#D32F2F", "black", "blue", "green", "red")
  )
}

crear_controles_color <- function(var_id) {
  labs <- etiquetas_color(var_id)
  defs <- colores_default(var_id)
  div(class = "color-grid",
      colourpicker::colourInput(paste0("col1_", var_id), labs[1], value = defs[1]),
      colourpicker::colourInput(paste0("col2_", var_id), labs[2], value = defs[2]),
      colourpicker::colourInput(paste0("col3_", var_id), labs[3], value = defs[3]),
      colourpicker::colourInput(paste0("col4_", var_id), labs[4], value = defs[4]),
      colourpicker::colourInput(paste0("col5_", var_id), labs[5], value = defs[5])
  )
}

crear_control_tema <- function(var_id) {
  default_tema <- if (identical(var_id, "w")) "bw" else "minimal"
  selectInput(paste0("tema_", var_id), "Tema de la gráfica:",
              choices = c("Minimal"                = "minimal",
                          "Blanco y negro"         = "bw",
                          "Clásico"                = "classic",
                          "Claro"                  = "light",
                          "Gris (defecto ggplot)"  = "gray"),
              selected = default_tema)
}

boton_descarga <- function(id) {
  downloadButton(id, NULL,
                 icon = shiny::icon("download"),
                 class = "btn-dl-header",
                 title = "Descargar PNG alta resolución")
}
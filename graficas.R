# ============================================================
# GRAFICAS.R — Procesamiento NetCDF y funciones de gráficas
# ------------------------------------------------------------
# CORRECCIÓN: en graficar_sst/dyn/iso se eliminaron los overrides
# de panel.background y panel.grid.major dentro del theme() final,
# para que el objeto theme devuelto por obtener_tema(tema) se
# refleje VISIBLEMENTE en AMBAS gráficas (histórica y anomalías).
# Se conserva panel.border (define el borde de la zona de trazado)
# y panel.grid.minor = element_blank() por limpieza visual.
# graficar_w NO acepta tema (barra sin selector de apariencia).
# ============================================================

# ============================================================
# HELPER: aviso "Datos insuficientes"
# ============================================================
ggplot_aviso <- function(titulo = "Datos insuficientes",
                         subtitulo = "No hay datos disponibles para los años seleccionados") {
  ggplot() +
    theme_void() +
    annotate("text", x = 0, y = 0.55, label = titulo,
             col = "#64748B", size = 6, fontface = "bold") +
    annotate("text", x = 0, y = 0.42, label = subtitulo,
             col = "#94A3B8", size = 3.8, lineheight = 1.2) +
    coord_cartesian(xlim = c(-1, 1), ylim = c(0, 1)) +
    theme(plot.margin = margin(30, 30, 30, 30))
}

# ============================================================
# FUNCIONES DE PROCESAMIENTO NetCDF
# ============================================================
procesar_nc <- function(nc, var_name, inicio = "1991-01-01", fin = "2020-12-31") {
  datos    <- ncvar_get(nc, var_name)
  time_val <- ncvar_get(nc, "time")
  tunits   <- ncatt_get(nc, "time", "units")$value
  fechas   <- as.POSIXct(time_val * 86400, origin = sub(".*since ", "", tunits), tz = "UTC")
  ntime    <- length(fechas)
  
  if (!is.null(dim(datos)) && length(dim(datos)) > 1) {
    dims <- dim(datos)
    idx_tiempo <- which(dims == ntime)
    if (length(idx_tiempo) >= 1) {
      idx <- rep(1L, length(dims))
      idx[idx_tiempo[1]] <- seq_len(ntime)
      datos <- do.call(`[`, c(list(datos), as.list(idx)))
    } else datos <- as.vector(datos)
  }
  datos <- as.numeric(datos)
  if (length(datos) != ntime) {
    if (length(datos) > ntime) datos <- datos[1:ntime]
    else datos <- c(datos, rep(NA_real_, ntime - length(datos)))
  }
  
  df <- data.frame(fechas = fechas, datos = datos)
  df$doy <- yday(df$fechas)
  df$anio <- as.integer(year(df$fechas))
  
  anio_ini_sol <- as.integer(format(as.Date(inicio), "%Y"))
  anio_fin_sol <- as.integer(format(as.Date(fin),   "%Y"))
  anios_data   <- sort(unique(df$anio))
  anios_validos <- anios_data[anios_data >= anio_ini_sol & anios_data <= anio_fin_sol]
  
  if (length(anios_validos) > 0) {
    anio_ini_efec <- min(anios_validos)
    anio_fin_efec <- max(anios_validos)
  } else {
    anio_ini_efec <- anio_ini_sol
    anio_fin_efec <- anio_fin_sol
  }
  
  df_sub <- df[df$anio >= anio_ini_efec & df$anio <= anio_fin_efec, ]
  
  if (nrow(df_sub) == 0) {
    df_merge <- df; df_merge$clim_dia <- NA_real_
  } else {
    df_clim <- df_sub %>%
      dplyr::group_by(doy) %>%
      dplyr::summarise(clim_dia = round(mean(datos, na.rm = TRUE), 2), .groups = "drop")
    df_merge <- dplyr::left_join(df, df_clim, by = "doy")
  }
  
  df_merge$anomalia <- round(df_merge$datos - df_merge$clim_dia, 2)
  df_merge <- df_merge[order(df_merge$fechas), ]
  rownames(df_merge) <- NULL
  attr(df_merge, "clim_range") <- c(anio_ini_efec, anio_fin_efec)
  df_merge
}

procesar_nc_perfil <- function(nc, var_name) {
  datos    <- ncvar_get(nc, var_name)
  time_val <- ncvar_get(nc, "time")
  tunits   <- ncatt_get(nc, "time", "units")$value
  fechas   <- as.POSIXct(time_val * 86400, origin = sub(".*since ", "", tunits), tz = "UTC")
  ntime    <- length(fechas)
  
  depth <- tryCatch(ncvar_get(nc, "depth"), error = function(e) NULL)
  if (is.null(depth) || length(depth) == 0) return(NULL)
  
  dims <- dim(datos)
  if (is.null(dims)) return(NULL)
  
  if (length(dims) == 1) {
    if (dims[1] != ntime) return(NULL)
    datos <- matrix(datos, nrow = 1); depth <- depth[1]
  } else if (length(dims) == 2) {
    if (dims[1] == length(depth) && dims[2] == ntime) { }
    else if (dims[1] == ntime && dims[2] == length(depth)) datos <- t(datos)
    else return(NULL)
  } else return(NULL)
  
  indices <- which(!is.na(datos), arr.ind = TRUE)
  if (nrow(indices) == 0) return(NULL)
  
  df <- data.frame(Profundidad = depth[indices[, 1]],
                   Fecha = fechas[indices[, 2]],
                   TEMP = datos[indices], stringsAsFactors = FALSE)
  df <- df %>%
    dplyr::mutate(
      Fecha       = as.Date(Fecha),
      AÑO         = as.integer(lubridate::year(Fecha)),
      DOY         = as.integer(lubridate::yday(Fecha)),
      Profundidad = as.numeric(Profundidad),
      TEMP        = as.numeric(TEMP)
    )
  df
}

# ============================================================
# HELPERS PARA SERIES TEMPORALES
# ============================================================
df_vacio_24m <- function() {
  data.frame(fechas = as.POSIXct(character(0)), datos = numeric(0), doy = integer(0),
             anio = integer(0), clim_dia = numeric(0), anomalia = numeric(0), dia_rel = numeric(0),
             Serie = character(0), stringsAsFactors = FALSE)
}

preparar_24m <- function(df_merge, anios_p1, anios_p2) {
  df_p1 <- df_merge %>%
    dplyr::filter(anio %in% anios_p1) %>%
    dplyr::mutate(
      dia_rel = as.numeric(difftime(fechas, as.Date(paste0(anios_p1[1], "-01-01")), units = "days")) + 1,
      Serie = paste0("Años: ", anios_p1[1], "-", anios_p1[2]))
  df_p2 <- df_merge %>%
    dplyr::filter(anio %in% anios_p2) %>%
    dplyr::mutate(
      dia_rel = as.numeric(difftime(fechas, as.Date(paste0(anios_p2[1], "-01-01")), units = "days")) + 1,
      Serie = paste0("Años: ", anios_p2[1], "-", anios_p2[2]))
  if (nrow(df_p1) == 0 && nrow(df_p2) == 0) return(df_vacio_24m())
  if (nrow(df_p1) == 0) df_p1 <- df_vacio_24m()
  if (nrow(df_p2) == 0) df_p2 <- df_vacio_24m()
  dplyr::bind_rows(df_p1, df_p2)
}

crear_eje_x <- function() {
  fechas_base <- seq(as.Date("2021-01-01"), as.Date("2022-12-01"), by = "month")
  dias_eje    <- as.numeric(difftime(fechas_base, as.Date("2021-01-01"), units = "days")) + 1
  meses       <- rep(c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"), 2)
  list(dias = dias_eje, meses = meses)
}

clim_continua <- function(df_merge) {
  df_clim <- df_merge[!duplicated(df_merge$doy), c("doy", "clim_dia")]
  df_clim <- df_clim[order(df_clim$doy), ]
  vals <- df_clim$clim_dia
  if (length(vals) == 0) return(rep(NA_real_, 731))
  if (length(vals) >= 365) vals <- vals[1:365] else vals <- rep(vals, length.out = 365)
  rep(vals, 2)[1:731]
}

# ============================================================
# GRÁFICAS DE SERIES TEMPORALES POR VARIABLE
# ============================================================
graficar_dyn <- function(df_merge, anios_p1, anios_p2, titulo_hist, titulo_anom,
                         ylab_hist = "Altura Dinámica [cm]", ylab_anom = "Anomalías [cm]",
                         colores = NULL, tema = "minimal") {
  if (is.null(colores) || length(colores) < 5) colores <- c("#D32F2F","black","#6495ED","red","red")
  c_p1 <- colores[1]; c_p2 <- colores[2]; c_hist <- colores[3]
  c_cero <- colores[4]; c_sep <- colores[5]
  
  eje <- crear_eje_x(); df_plot <- preparar_24m(df_merge, anios_p1, anios_p2)
  clim_ext <- clim_continua(df_merge)
  df_clim24 <- data.frame(dia_rel = 1:731, clim_dia = clim_ext)
  minv <- min(c(df_plot$datos, df_clim24$clim_dia), na.rm = TRUE)
  maxv <- max(c(df_plot$datos, df_clim24$clim_dia), na.rm = TRUE)
  if (!is.finite(minv)) { minv <- -5; maxv <- 5 }
  ymin <- floor(minv/5)*5; ymax <- ceiling(maxv/5)*5
  colores1 <- setNames(c(c_p1, c_p2, c_hist),
                       c(paste0("Años: ",anios_p1[1],"-",anios_p1[2]),
                         paste0("Años: ",anios_p2[1],"-",anios_p2[2]), "Prom. Histórico"))
  p_hist <- ggplot() +
    geom_segment(data=df_clim24, aes(x=dia_rel,xend=dia_rel,y=ymin,yend=clim_dia,color="Prom. Histórico"),
                 linewidth=0.65, alpha=0.8) +
    geom_vline(xintercept=366, color=c_sep, linetype="dotted", linewidth=0.75, alpha=0.7) +
    geom_line(data=df_plot, aes(x=dia_rel,y=datos,color=Serie), linewidth=0.85) +
    scale_color_manual(name=NULL, values=colores1) +
    scale_x_continuous(breaks=eje$dias, labels=eje$meses, limits=c(1,731), expand=c(0.01,0)) +
    scale_y_continuous(breaks=seq(ymin,ymax,by=5), limits=c(ymin,ymax)) +
    labs(title=titulo_hist, x=NULL, y=ylab_hist) +
    obtener_tema(tema) + coord_cartesian(clip="off") +
    theme(panel.border = element_rect(color="black", fill=NA, linewidth=0.6),
          panel.grid.minor = element_blank(),
          plot.title=element_text(size=13,face="bold",margin=margin(b=-15)),
          axis.text.x=element_text(size=10),
          legend.position="top", legend.justification="right", legend.direction="horizontal",
          legend.key=element_blank(), legend.background=element_blank(),
          plot.margin=margin(t=15,r=10,b=10,l=10))
  minv2 <- min(df_plot$anomalia, na.rm=TRUE); maxv2 <- max(df_plot$anomalia, na.rm=TRUE)
  if (!is.finite(minv2)) { minv2 <- -5; maxv2 <- 5 }
  ymin2 <- floor(minv2/5)*5; ymax2 <- ceiling(maxv2/5)*5
  colores2 <- setNames(c(c_p1, c_p2),
                       c(paste0("Años: ",anios_p1[1],"-",anios_p1[2]),
                         paste0("Años: ",anios_p2[1],"-",anios_p2[2])))
  p_anom <- ggplot(df_plot, aes(x=dia_rel,y=anomalia,color=Serie)) +
    geom_hline(yintercept=0, color=c_cero, linetype="dashed", linewidth=0.75) +
    geom_line(linewidth=0.85) + scale_color_manual(name=NULL, values=colores2) +
    scale_x_continuous(breaks=eje$dias, labels=eje$meses, limits=c(1,731), expand=c(0.01,0)) +
    scale_y_continuous(breaks=seq(ymin2,ymax2,by=5), limits=c(ymin2,ymax2)) +
    labs(title=titulo_anom, x=NULL, y=ylab_anom) +
    obtener_tema(tema) + coord_cartesian(clip="off") +
    theme(panel.border = element_rect(color="black", fill=NA, linewidth=0.6),
          panel.grid.minor = element_blank(),
          plot.title=element_text(size=13,face="bold",margin=margin(b=-15)),
          axis.text.x=element_text(angle=0,size=10),
          legend.position="top", legend.justification="right", legend.direction="horizontal",
          legend.key=element_blank(), legend.background=element_blank(),
          plot.margin=margin(t=15,r=15,b=10,l=10))
  list(hist=p_hist, anom=p_anom)
}

graficar_iso <- function(df_merge, anios_p1, anios_p2, titulo_hist, titulo_anom,
                         ylab_hist = "Profundidad [m]", ylab_anom = "Profundidad [m]",
                         colores = NULL, tema = "minimal") {
  if (is.null(colores) || length(colores) < 5) colores <- c("#D32F2F","black","#B0B0B0","red","red")
  c_p1 <- colores[1]; c_p2 <- colores[2]; c_hist <- colores[3]
  c_cero <- colores[4]; c_sep <- colores[5]
  
  eje <- crear_eje_x(); df_plot <- preparar_24m(df_merge, anios_p1, anios_p2)
  clim_ext <- clim_continua(df_merge)
  df_clim24 <- data.frame(dia_rel = 1:731, clim_dia = clim_ext)
  minv <- min(c(df_plot$datos, df_clim24$clim_dia), na.rm=TRUE)
  maxv <- max(c(df_plot$datos, df_clim24$clim_dia), na.rm=TRUE)
  if (!is.finite(minv)) { minv <- 0; maxv <- 200 }
  ymin <- floor(minv/10)*10; ymax <- ceiling(maxv/10)*10
  colores1 <- setNames(c(c_p1, c_p2, c_hist),
                       c(paste0("Años: ",anios_p1[1],"-",anios_p1[2]),
                         paste0("Años: ",anios_p2[1],"-",anios_p2[2]), "Prom. Histórico"))
  p_hist <- ggplot() +
    geom_segment(data=df_clim24, aes(x=dia_rel,xend=dia_rel,y=ymin,yend=clim_dia,color="Prom. Histórico"),
                 linewidth=0.65, alpha=0.8) +
    geom_vline(xintercept=366, color=c_sep, linetype="dotted", linewidth=0.85, alpha=0.7) +
    geom_line(data=df_plot, aes(x=dia_rel,y=datos,color=Serie), linewidth=0.75) +
    scale_color_manual(name=NULL, values=colores1) +
    scale_x_continuous(breaks=eje$dias, labels=eje$meses, limits=c(1,731), expand=c(0.01,0)) +
    scale_y_reverse(breaks=seq(ymin,ymax,by=10), limits=c(ymax,ymin)) +
    labs(title=titulo_hist, x=NULL, y=ylab_hist) +
    obtener_tema(tema) + coord_cartesian(clip="off") +
    theme(panel.border = element_rect(color="black", fill=NA, linewidth=0.6),
          panel.grid.minor = element_blank(),
          plot.title=element_text(size=13,face="bold",margin=margin(b=-15)),
          axis.text.x=element_text(size=10),
          legend.position="top", legend.justification="right", legend.direction="horizontal",
          legend.key=element_blank(), legend.background=element_blank(),
          plot.margin=margin(t=15,r=10,b=10,l=10))
  minv2 <- min(df_plot$anomalia, na.rm=TRUE); maxv2 <- max(df_plot$anomalia, na.rm=TRUE)
  if (!is.finite(minv2)) { minv2 <- -10; maxv2 <- 10 }
  ymin2 <- floor(minv2/10)*10; ymax2 <- ceiling(maxv2/10)*10
  colores2 <- setNames(c(c_p1, c_p2),
                       c(paste0("Años: ",anios_p1[1],"-",anios_p1[2]),
                         paste0("Años: ",anios_p2[1],"-",anios_p2[2])))
  p_anom <- ggplot(df_plot, aes(x=dia_rel,y=anomalia,color=Serie)) +
    geom_hline(yintercept=0, color=c_cero, linetype="dashed", linewidth=0.5) +
    geom_line(linewidth=0.75) + scale_color_manual(name=NULL, values=colores2) +
    scale_x_continuous(breaks=eje$dias, labels=eje$meses, limits=c(1,731), expand=c(0.01,0)) +
    scale_y_reverse(breaks=seq(ymin2,ymax2,by=10), limits=c(ymax2,ymin2)) +
    labs(title=titulo_anom, x=NULL, y=ylab_anom) +
    obtener_tema(tema) + coord_cartesian(clip="off") +
    theme(panel.border = element_rect(color="black", fill=NA, linewidth=0.6),
          panel.grid.minor = element_blank(),
          plot.title=element_text(size=13,face="bold",margin=margin(b=-15)),
          axis.text.x=element_text(angle=0,size=10),
          legend.position="top", legend.justification="right", legend.direction="horizontal",
          legend.key=element_blank(), legend.background=element_blank(),
          plot.margin=margin(t=15,r=15,b=10,l=10))
  list(hist=p_hist, anom=p_anom)
}

graficar_sst <- function(df_merge, anios_p1, anios_p2, titulo_hist, titulo_anom,
                         ylab_hist = "[°C]", ylab_anom = "Anomalías [°C]",
                         colores = NULL, tema = "minimal") {
  if (is.null(colores) || length(colores) < 5) colores <- c("#0000CD","#D32F2F","black","red","red")
  c_hist <- colores[1]; c_p1 <- colores[2]; c_p2 <- colores[3]
  c_cero <- colores[4]; c_sep <- colores[5]
  
  eje <- crear_eje_x(); df_plot <- preparar_24m(df_merge, anios_p1, anios_p2)
  clim_ext <- clim_continua(df_merge)
  df_clim24 <- data.frame(dia_rel = 1:731, clim_dia = clim_ext)
  minv <- min(c(df_plot$datos, df_clim24$clim_dia), na.rm=TRUE)
  maxv <- max(c(df_plot$datos, df_clim24$clim_dia), na.rm=TRUE)
  if (!is.finite(minv)) { minv <- 20; maxv <- 30 }
  ymin <- floor(minv/2)*2; ymax <- ceiling(maxv/2)*2
  colores1 <- setNames(c(c_hist, c_p1, c_p2),
                       c("Prom. Histórico",
                         paste0("Años: ",anios_p1[1],"-",anios_p1[2]),
                         paste0("Años: ",anios_p2[1],"-",anios_p2[2])))
  p_hist <- ggplot() +
    geom_vline(xintercept=366, color=c_sep, linetype="dotted", linewidth=0.5, alpha=0.5) +
    geom_line(data=df_clim24, aes(x=dia_rel,y=clim_dia,color="Prom. Histórico"), linewidth=1) +
    geom_line(data=df_plot, aes(x=dia_rel,y=datos,color=Serie), linewidth=0.75) +
    scale_color_manual(name=NULL, values=colores1) +
    scale_x_continuous(breaks=eje$dias, labels=eje$meses, limits=c(1,731), expand=c(0.01,0)) +
    scale_y_continuous(breaks=seq(ymin,ymax,by=2), limits=c(ymin,ymax)) +
    labs(title=titulo_hist, x=NULL, y=ylab_hist) +
    obtener_tema(tema) + coord_cartesian(clip="off") +
    theme(panel.border = element_rect(color="black", fill=NA, linewidth=0.6),
          panel.grid.minor = element_blank(),
          plot.title=element_text(size=13,face="bold",margin=margin(b=-15)),
          axis.text.x=element_text(size=10),
          legend.position="top", legend.justification="right", legend.direction="horizontal",
          legend.key=element_blank(), legend.background=element_blank(),
          plot.margin=margin(t=15,r=10,b=10,l=10))
  minv2 <- min(df_plot$anomalia, na.rm=TRUE); maxv2 <- max(df_plot$anomalia, na.rm=TRUE)
  if (!is.finite(minv2)) { minv2 <- -2; maxv2 <- 2 }
  ymin2 <- floor(minv2/2)*2; ymax2 <- ceiling(maxv2/2)*2
  colores2 <- setNames(c(c_p1, c_p2),
                       c(paste0("Años: ",anios_p1[1],"-",anios_p1[2]),
                         paste0("Años: ",anios_p2[1],"-",anios_p2[2])))
  p_anom <- ggplot(df_plot, aes(x=dia_rel,y=anomalia,color=Serie)) +
    geom_hline(yintercept=0, color=c_cero, linetype="dashed", linewidth=0.5) +
    geom_vline(xintercept=366, color=c_sep, linetype="dotted", linewidth=0.5, alpha=0.5) +
    geom_line(linewidth=0.75) + scale_color_manual(name=NULL, values=colores2) +
    scale_x_continuous(breaks=eje$dias, labels=eje$meses, limits=c(1,731), expand=c(0.01,0)) +
    scale_y_continuous(breaks=seq(ymin2,ymax2,by=2), limits=c(ymin2,ymax2)) +
    labs(title=titulo_anom, x=NULL, y=ylab_anom) +
    obtener_tema(tema) + coord_cartesian(clip="off") +
    theme(panel.border = element_rect(color="black", fill=NA, linewidth=0.6),
          panel.grid.minor = element_blank(),
          plot.title=element_text(size=13,face="bold",margin=margin(b=-15)),
          axis.text.x=element_text(angle=0,size=10),
          legend.position="top", legend.justification="right", legend.direction="horizontal",
          legend.key=element_blank(), legend.background=element_blank(),
          plot.margin=margin(t=15,r=15,b=10,l=10))
  list(hist=p_hist, anom=p_anom)
}

# ------------------------------------------------------------
# graficar_w: NO recibe parámetro `tema`. Es la gráfica de BARRAS
# y siempre usa theme_bw(). El selector de apariencia no se
# muestra en el panel Vientos (ver app.R: panel_tema = NULL).
# ------------------------------------------------------------
graficar_w <- function(df_merge, anios_p1, anios_p2, titulo_hist, titulo_anom,
                       ylab_hist = "Velocidad [m/s]", ylab_anom = "Anomalía [m/s]",
                       colores = NULL) {
  if (is.null(colores) || length(colores) < 5) colores <- c("#D32F2F","blue","#D32F2F","#4B92DB","red")
  c_hist_bar <- colores[1]; c_hist_line <- colores[2]
  c_anom_pos <- colores[3]; c_anom_neg  <- colores[4]
  c_sep      <- colores[5]
  
  eje <- crear_eje_x()
  
  df_p1 <- df_merge %>% dplyr::filter(anio %in% anios_p1) %>%
    dplyr::mutate(dia_rel = as.numeric(difftime(fechas, as.Date(paste0(anios_p1[1],"-01-01")), units="days"))+1,
                  Periodo = paste0("Año: ",anios_p1[1],"-",anios_p1[2]))
  df_p2 <- df_merge %>% dplyr::filter(anio %in% anios_p2) %>%
    dplyr::mutate(dia_rel = as.numeric(difftime(fechas, as.Date(paste0(anios_p2[1],"-01-01")), units="days"))+1,
                  Periodo = paste0("Año: ",anios_p2[1],"-",anios_p2[2]))
  df_plot <- dplyr::bind_rows(df_p1, df_p2)
  
  if (nrow(df_plot) == 0) {
    p_aviso <- ggplot_aviso(
      "Datos insuficientes",
      paste0("No hay registros para los años seleccionados\n",
             "(período 1: ", anios_p1[1], "-", anios_p1[2],
             "  |  período 2: ", anios_p2[1], "-", anios_p2[2], ")"))
    return(list(hist = p_aviso, anom = p_aviso))
  }
  
  df_plot <- df_plot %>%
    dplyr::mutate(Color_Anom = ifelse(anomalia >= 0, "Positiva", "Negativa"))
  
  max_abs_h <- max(abs(c(df_plot$datos, df_plot$clim_dia)), na.rm=TRUE)
  if (!is.finite(max_abs_h) || max_abs_h == 0) max_abs_h <- 10
  xmax_h <- ceiling(max_abs_h)
  colores_w_hist <- setNames(c(c_hist_bar, c_hist_line), c("Datos del Año","Prom. Histórico"))
  p_hist <- ggplot(df_plot) +
    geom_vline(xintercept=0, color="black", linewidth=0.3) +
    geom_segment(aes(y=dia_rel,yend=dia_rel,x=0,xend=datos,color="Datos del Año"), linewidth=0.47) +
    geom_path(aes(y=dia_rel,x=clim_dia,color="Prom. Histórico"), linewidth=0.5) +
    geom_hline(yintercept=366, color=c_sep, linetype="dashed", linewidth=0.5, alpha=0.7) +
    scale_color_manual(name=NULL, values=colores_w_hist) +
    scale_x_continuous(limits=c(-xmax_h,xmax_h), breaks=seq(-xmax_h,xmax_h,by=2)) +
    scale_y_reverse(breaks=eje$dias, labels=eje$meses, expand=c(0.01,0)) +
    facet_wrap(~ Periodo, ncol=2) +
    labs(title=titulo_hist, x=ylab_hist, y=NULL) + theme_bw() +
    theme(plot.title=element_text(hjust=0.5,size=15,face="bold",margin=margin(b=15)),
          strip.background=element_rect(fill="white",color=NA),
          strip.text=element_text(size=13),
          panel.grid.major=element_line(color="#E0E0E0",linewidth=0.4),
          panel.grid.minor=element_blank(),
          axis.text.y=element_text(size=11), axis.text.x=element_text(size=11),
          legend.position="bottom", legend.justification="right", legend.key=element_blank())
  
  max_abs_a <- max(abs(df_plot$anomalia), na.rm=TRUE)
  if (!is.finite(max_abs_a) || max_abs_a == 0) max_abs_a <- 5
  xmax_a <- ceiling(max_abs_a); if (xmax_a %% 2 != 0) xmax_a <- xmax_a + 1
  colores_w_anom <- setNames(c(c_anom_pos, c_anom_neg), c("Positiva","Negativa"))
  p_anom <- ggplot(df_plot) +
    geom_vline(xintercept=0, color="black", linewidth=0.3) +
    geom_segment(aes(y=dia_rel,yend=dia_rel,x=0,xend=anomalia,color=Color_Anom), linewidth=0.47) +
    geom_hline(yintercept=366, color=c_sep, linetype="dashed", linewidth=0.5, alpha=0.7) +
    scale_color_manual(values=colores_w_anom) +
    scale_x_continuous(limits=c(-xmax_a,xmax_a), breaks=seq(-xmax_a,xmax_a,by=2)) +
    scale_y_reverse(breaks=eje$dias, labels=eje$meses, expand=c(0.01,0)) +
    facet_wrap(~ Periodo, ncol=2) +
    labs(title=titulo_anom, x=ylab_anom, y=NULL) + theme_bw() +
    theme(plot.title=element_text(hjust=0.5,size=15,face="bold",margin=margin(b=15)),
          strip.background=element_rect(fill="white",color=NA),
          strip.text=element_text(size=13),
          panel.grid.major=element_line(color="#E0E0E0",linewidth=0.4),
          panel.grid.minor=element_blank(),
          axis.text.y=element_text(size=11), axis.text.x=element_text(size=11),
          legend.position="none")
  
  list(hist=p_hist, anom=p_anom)
}

# ============================================================
# Gráficas de contorno para Temperatura Subsuperficial
# ============================================================
formar_titulo_con_clim <- function(titulo, ini, fin) {
  if (is.null(titulo) || titulo == "") return(NULL)
  if (grepl("\\[\\d{4}-\\d{4}\\]", titulo)) {
    return(gsub("\\[\\d{4}-\\d{4}\\]", paste0("[", ini, "-", fin, "]"), titulo))
  }
  paste0(titulo, "\nClimatología: ", ini, "-", fin)
}

calcular_datos_contorno_t <- function(df_perfil, anios_p1, anios_p2,
                                      inicio = "1991-01-01", fin = "2020-12-31",
                                      prof_max = 300) {
  anio_ini_sol <- as.integer(format(as.Date(inicio), "%Y"))
  anio_fin_sol <- as.integer(format(as.Date(fin),   "%Y"))
  
  anios_disp <- unique(df_perfil$AÑO)
  anios_validos <- sort(anios_disp[anios_disp >= anio_ini_sol & anios_disp <= anio_fin_sol])
  
  if (length(anios_validos) < 25) {
    return(list(status = "insuficiente",
                mensaje = paste0("Se requieren al menos 25 años de datos para la climatología.\n",
                                 "Años disponibles en el rango [", anio_ini_sol, "-", anio_fin_sol, "]: ",
                                 length(anios_validos))))
  }
  
  anio_ini_efec <- min(anios_validos)
  anio_fin_efec <- max(anios_validos)
  
  climatologia <- df_perfil %>%
    dplyr::filter(AÑO >= anio_ini_efec, AÑO <= anio_fin_efec) %>%
    dplyr::group_by(DOY, Profundidad) %>%
    dplyr::summarise(TEMP_clim = mean(TEMP, na.rm = TRUE), .groups = "drop")
  
  if (nrow(climatologia) == 0) {
    return(list(status = "insuficiente",
                mensaje = "No hay climatología disponible en el rango elegido"))
  }
  
  df_full <- df_perfil %>%
    dplyr::left_join(climatologia, by = c("DOY", "Profundidad")) %>%
    dplyr::mutate(ANOM = TEMP - TEMP_clim)
  
  preparar_periodo <- function(años) {
    df_p <- df_full %>% dplyr::filter(AÑO %in% años, Profundidad <= prof_max)
    if (nrow(df_p) == 0) return(NULL)
    
    fecha_min <- min(df_p$Fecha, na.rm = TRUE)
    df_p <- df_p %>% dplyr::mutate(DIA_AÑO = as.numeric(Fecha - fecha_min) + 1)
    
    df_temp <- df_p %>% dplyr::filter(!is.na(TEMP)) %>%
      dplyr::select(DIA_AÑO, Profundidad, TEMP)
    if (nrow(df_temp) < 10) return(NULL)
    
    malla_temp <- tryCatch(
      MBA::mba.surf(df_temp, no.X = 730, no.Y = 300,
                    n = 1, m = 1, h = 8, extend = TRUE)$xyz.est,
      error = function(e) NULL)
    if (is.null(malla_temp)) return(NULL)
    
    df_plot_temp <- expand.grid(DIA_AÑO = malla_temp$x, Profundidad = malla_temp$y)
    df_plot_temp$TEMP <- as.vector(malla_temp$z)
    
    df_anom <- df_p %>% dplyr::filter(!is.na(ANOM)) %>%
      dplyr::select(DIA_AÑO, Profundidad, ANOM)
    if (nrow(df_anom) < 10) return(NULL)
    
    malla_anom <- tryCatch(
      MBA::mba.surf(df_anom, no.X = 730, no.Y = 300,
                    n = 1, m = 1, h = 8, extend = TRUE)$xyz.est,
      error = function(e) NULL)
    if (is.null(malla_anom)) return(NULL)
    
    df_plot_anom <- expand.grid(DIA_AÑO = malla_anom$x, Profundidad = malla_anom$y)
    df_plot_anom$ANOM <- as.vector(malla_anom$z)
    
    list(temp = df_plot_temp, anom = df_plot_anom,
         label = paste0("Año: ", años[1], "-", años[2]))
  }
  
  p1 <- preparar_periodo(c(anios_p1[1], anios_p1[2]))
  p2 <- preparar_periodo(c(anios_p2[1], anios_p2[2]))
  
  if (is.null(p1) && is.null(p2)) {
    return(list(status = "insuficiente",
                mensaje = "No hay registros para los años seleccionados"))
  }
  
  p1_temp <- if (!is.null(p1)) p1$temp else NULL
  p2_temp <- if (!is.null(p2)) p2$temp else NULL
  p1_anom <- if (!is.null(p1)) p1$anom else NULL
  p2_anom <- if (!is.null(p2)) p2$anom else NULL
  p1_label <- if (!is.null(p1)) p1$label else paste0("Año: ", anios_p1[1], "-", anios_p1[2])
  p2_label <- if (!is.null(p2)) p2$label else paste0("Año: ", anios_p2[1], "-", anios_p2[2])
  
  df_temp_all <- dplyr::bind_rows(p1_temp, p2_temp)
  df_anom_all <- dplyr::bind_rows(p1_anom, p2_anom)
  
  days_month   <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  starts_y1    <- cumsum(c(0, head(days_month, -1))) + 1
  dias_y1      <- as.numeric(as.Date(paste0(anios_p1[1] + 1, "-01-01")) -
                               as.Date(paste0(anios_p1[1], "-01-01")))
  starts_y2    <- starts_y1 + dias_y1
  month_labels <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
  tick_pos     <- c(starts_y1, starts_y2)
  tick_lab     <- c(month_labels, month_labels)
  
  if (nrow(df_temp_all) > 0) {
    temp_min <- floor(min(df_temp_all$TEMP, na.rm = TRUE))
    temp_max <- ceiling(max(df_temp_all$TEMP, na.rm = TRUE))
  } else { temp_min <- 0; temp_max <- 30 }
  breaks_all    <- seq(temp_min, temp_max, by = 1)
  if (length(breaks_all) < 2) breaks_all <- c(temp_min, temp_min + 1)
  breaks_solid  <- breaks_all[breaks_all %% 2 == 0]
  breaks_dashed <- breaks_all[breaks_all %% 2 != 0]
  ticks_cbar    <- seq(temp_min, temp_max, by = 2)
  n_bins        <- length(breaks_all) - 1
  pal_rainbow   <- colorRampPalette(c(
    "#FF00FF", "#8000FF", "#0000FF", "#00FFFF",
    "#00FF00", "#FFFF00", "#FF8000", "#FF0000"))(n_bins)
  
  if (nrow(df_anom_all) > 0) {
    lim <- ceiling(max(abs(range(df_anom_all$ANOM, na.rm = TRUE))))
    if (!is.finite(lim) || lim == 0) lim <- 3
  } else lim <- 3
  breaks_all_a <- seq(-lim, lim, by = 1)
  if (length(breaks_all_a) < 2) breaks_all_a <- c(-1, 1)
  breaks_pos_a <- breaks_all_a[breaks_all_a > 0]
  breaks_neg_a <- breaks_all_a[breaks_all_a < 0]
  ticks_cbar_a <- seq(-lim, lim, by = 2)
  n_bins_a     <- length(breaks_all_a) - 1
  pal_anom     <- colorRampPalette(c(
    "#053061", "#2166AC", "#4393C3", "#92C5DE",
    "#D1E5F0", "#F7F7F7",
    "#FDDBC7", "#F4A582", "#D6604D", "#B2182B", "#67001F"))(n_bins_a)
  
  list(status = "ok",
       p1_temp = p1_temp, p2_temp = p2_temp, p1_anom = p1_anom, p2_anom = p2_anom,
       p1_label = p1_label, p2_label = p2_label,
       prof_max = prof_max,
       tick_pos = tick_pos, tick_lab = tick_lab,
       temp_min = temp_min, temp_max = temp_max,
       breaks_all = breaks_all, breaks_solid = breaks_solid, breaks_dashed = breaks_dashed,
       ticks_cbar = ticks_cbar, pal_rainbow = pal_rainbow,
       lim = lim, breaks_all_a = breaks_all_a, breaks_pos_a = breaks_pos_a, breaks_neg_a = breaks_neg_a,
       ticks_cbar_a = ticks_cbar_a, pal_anom = pal_anom,
       anio_ini_efec = anio_ini_efec, anio_fin_efec = anio_fin_efec)
}

construir_plots_contorno_t <- function(datos, titulo_hist, titulo_anom,
                                       ylab_hist = "Profundidad [m]",
                                       ylab_anom = "Profundidad [m]",
                                       colores = NULL) {
  if (is.null(colores) || length(colores) < 5) colores <- c("black","grey40","black","grey40","grey30")
  c_temp_solid <- colores[1]; c_temp_dash <- colores[2]
  c_anom_solid <- colores[3]; c_anom_dash <- colores[4]
  c_label      <- colores[5]
  
  if (is.null(datos) || datos$status != "ok") {
    mensaje <- if (!is.null(datos) && !is.null(datos$mensaje)) datos$mensaje else "Sin datos disponibles"
    p_aviso <- ggplot_aviso("Datos insuficientes", mensaje)
    return(list(hist_p1 = p_aviso, hist_p2 = p_aviso, anom_p1 = p_aviso, anom_p2 = p_aviso))
  }
  
  titulo_hist_full <- formar_titulo_con_clim(titulo_hist, datos$anio_ini_efec, datos$anio_fin_efec)
  titulo_anom_full <- formar_titulo_con_clim(titulo_anom, datos$anio_ini_efec, datos$anio_fin_efec)
  
  crear_plot_temp <- function(df, titulo) {
    if (is.null(df) || nrow(df) == 0) return(ggplot_aviso("Datos insuficientes", titulo))
    ggplot(df, aes(x = DIA_AÑO, y = Profundidad)) +
      geom_contour_fill(aes(z = TEMP), breaks = datos$breaks_all) +
      geom_contour(aes(z = TEMP), breaks = datos$breaks_solid, color = c_temp_solid, linewidth = 0.5, linetype = "solid") +
      geom_contour(aes(z = TEMP), breaks = datos$breaks_dashed, color = c_temp_dash, linewidth = 0.6, linetype = "dashed") +
      geom_text_contour(aes(z = TEMP), breaks = datos$breaks_solid, size = 3.4, rotate = TRUE, min.size = 2, skip = 1, fontface = "bold", label.placer = label_placer_n(1)) +
      geom_text_contour(aes(z = TEMP), breaks = datos$breaks_dashed, size = 3.4, rotate = TRUE, min.size = 2, skip = 1, label.placer = label_placer_n(1), fontface = "plain", color = c_label) +
      scale_fill_gradientn(colors = datos$pal_rainbow, limits = c(datos$temp_min, datos$temp_max), breaks = datos$ticks_cbar, name = NULL, guide = guide_colorsteps(show.limits = TRUE, ticks.colour = "black", frame.colour = "black", barheight = unit(10, "cm"), barwidth = unit(0.5, "cm"))) +
      scale_x_continuous(breaks = datos$tick_pos, labels = datos$tick_lab, expand = c(0, 0)) +
      scale_y_reverse(breaks = seq(0, datos$prof_max, by = 50), expand = c(0, 0)) +
      labs(title = titulo, x = NULL, y = ylab_hist) +
      theme_bw(base_size = 12) +
      theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 10))
  }
  
  crear_plot_anom <- function(df, titulo) {
    if (is.null(df) || nrow(df) == 0) return(ggplot_aviso("Datos insuficientes", titulo))
    ggplot(df, aes(x = DIA_AÑO, y = Profundidad)) +
      geom_contour_fill(aes(z = ANOM), breaks = datos$breaks_all_a) +
      geom_contour(aes(z = ANOM), breaks = datos$breaks_pos_a, color = c_anom_solid, linewidth = 0.5, linetype = "solid") +
      geom_contour(aes(z = ANOM), breaks = datos$breaks_neg_a, color = c_anom_dash, linewidth = 0.6, linetype = "dashed") +
      geom_text_contour(aes(z = ANOM), breaks = datos$breaks_pos_a, size = 3.4, rotate = TRUE, min.size = 1, skip = 1, fontface = "bold", label.placer = label_placer_n(1), check_overlap = FALSE) +
      geom_text_contour(aes(z = ANOM), breaks = datos$breaks_neg_a, size = 3.4, rotate = TRUE, min.size = 1, skip = 1, label.placer = label_placer_n(1), fontface = "plain", color = c_label, check_overlap = FALSE) +
      scale_fill_gradientn(colors = datos$pal_anom, limits = c(-datos$lim, datos$lim), breaks = datos$ticks_cbar_a, name = NULL, guide = guide_colorsteps(show.limits = TRUE, ticks.colour = "black", frame.colour = "black", barheight = unit(10, "cm"), barwidth = unit(0.5, "cm"))) +
      scale_x_continuous(breaks = datos$tick_pos, labels = datos$tick_lab, expand = c(0, 0)) +
      scale_y_reverse(breaks = seq(0, datos$prof_max, by = 50), expand = c(0, 0)) +
      labs(title = titulo, x = NULL, y = ylab_anom) +
      theme_bw(base_size = 12) +
      theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 10))
  }
  
  p_hist_p1 <- crear_plot_temp(datos$p1_temp, paste0(titulo_hist_full, "\n", datos$p1_label))
  p_hist_p2 <- crear_plot_temp(datos$p2_temp, paste0(titulo_hist_full, "\n", datos$p2_label))
  p_anom_p1 <- crear_plot_anom(datos$p1_anom, paste0(titulo_anom_full, "\n", datos$p1_label))
  p_anom_p2 <- crear_plot_anom(datos$p2_anom, paste0(titulo_anom_full, "\n", datos$p2_label))
  
  list(hist_p1 = p_hist_p1, hist_p2 = p_hist_p2, anom_p1 = p_anom_p1, anom_p2 = p_anom_p2)
}

# ============================================================
# HELPER: títulos/etiquetas automáticos por variable
# ============================================================
defaults_var <- list(
  sst = list(titulo_hist = "TSM: Promedio Histórico [1991-2020] Vs Años",
             titulo_anom = "Anomalías: Temperatura Superficial del Mar [1991-2020]",
             ylab_hist   = "[°C]", ylab_anom = "Anomalías [°C]"),
  dyn = list(titulo_hist = "Promedio Histórico [1991-2020] Vs Años",
             titulo_anom = "Anomalías: Altura Dinámica [1991-2020]",
             ylab_hist   = "Altura Dinámica [cm]", ylab_anom = "Anomalías [cm]"),
  iso = list(titulo_hist = "Isoterma de 20°C: Promedio Histórico [1991-2020] Vs Años",
             titulo_anom = "Anomalías: Isoterma 20°C [1991-2020]",
             ylab_hist   = "Profundidad [m]", ylab_anom = "Profundidad [m]"),
  t = list(titulo_hist = "Temperatura Sub Superficial del mar [°C]",
           titulo_anom = "Anomalía diaria de Temperatura Sub Superficial del mar [°C]",
           ylab_hist   = "Profundidad [m]", ylab_anom = "Profundidad [m]"),
  w = list(titulo_hist = "Vientos Zonales: Promedio Histórico [1991-2020] Vs Años",
           titulo_anom = "Vientos Zonales: Anomalías [1991-2020]",
           ylab_hist   = "Velocidad [m/s]", ylab_anom = "Anomalía [m/s]")
)
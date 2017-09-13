##' @name gg_bubble
##' @title Thematic map of study region including meteorological stations 
##' location and a climate feature
##' @description Thematic map of study region overlaying points 
##' (e.g. met. stations), political boundaries and raster (e.g. altitude, 
##' vegetation, etc)
##' @keywords hplot
##' @importFrom ggplot2 ggplot geom_polygon geom_raster scale_fill_gradientn 
##' geom_path theme_bw coord_equal geom_point scale_x_continuous 
##' scale_y_continuous theme element_rect element_blank element_text aes
##' @importFrom dplyr scales pretty_breaks
##' @importFrom dplyr rename_
##' @importFrom ggrepel geom_text_repel
##' @export
##' @param data a dataframe with geographical coordinates and climate features 
##' of meteorological stations
##' @param z character name of climate feature or variable column in the data
##' @param raster_bg raster object to be plotted in background (optional)
##' @param boundaries data frame resulting from fortifying a 
##' SpatialPolygonDataFrame 
##' @param colors_z function color pallete for z (default viridis)
##' @param z_legend character title for color bar
##' @param text_color character color for text annotation of stations id
##' @param text_size numeric with size for text annotation of stations id
##' @param color_fill character with background color to be used when raster_bg
##'  is missing
##' @param type character to specify the scale color bar type, use 'gradient' for a gradient color scale (default option) and 'diverging' for a diverging colour gradient.
##' @param ... additional arguments to ggplot2::theme()
##' @seealso \code{\link[ggplot2]{fortify}}, 
##' \code{\link[sp]{SpatialPolygonsDataFrame}} and 
##' \code{\link[raster]{shapefile}}
##' @return ggplot object
##' @author André Parcianelo, Carolina Kannenberg, Jonatan Tatsch (LHMET-UFSM)
##' @references \url{http://rpubs.com/jdtatsch/mapa_tematico}
##' @examples 
##' #TO DO
gg_bubble <- function(data = info
                      ,type = c("gradient", "diverging")
                      ,z = "tavg"
                      ,raster_bg = dem_sul_df
                      ,breaks = pretty(data[["z"]], n = 6)
                      ,limites = sul_df
                      ,colors_z = viridis::viridis
                      ,colors_bg = gray.colors
                      ,text_color = "red"
                      ,text_size = 3
                      ,z_legend = expression(VIÉS~~(degree~~C))
                      ,color_fill = "burlywood3"
                      ,point_size = 3
                      ,point_color = "black"
                      ,point_
                      ,repel_seg_size = 0.5
                      ,repel_min_seg_len = 0.5
                      ,guide_type = c("colourbar", "legend")
                      ,point_text_ff = "plain" # font face labels points
                      ,...){
  
  repel_min_seg_len <- unit(repel_min_seg_len, "lines")
  
  stopifnot(!missing(limites))
  stopifnot("site" %in% names(data))
  data <- dplyr::rename_(.data = data, "z" = z)
  if(missing(type)) type <- "gradient"
  
  # se não tiver raster_bg (raster de background) mas tem limites
  if(missing(raster_bg) & !missing(limites)){
    stopifnot(c("long","lat") %in% names(limites))
    ggp_base <- ggplot2::ggplot(data = limites, 
                                aes(x = long, 
                                    y = lat)) + 
      ggplot2::geom_polygon(aes(group = group),
                            color = "gray57", 
                            fill = color_fill) 
  } # end if missing raster
  
  # se tem raster_bg e tem polígono
  if(!missing(raster_bg) & !missing(limites)){
    
    stopifnot(c("lon","lat", "alt") %in% names(raster_bg))
    brks_bg <- pretty(raster_bg[[3]])
    ggp_base <- ggplot2::ggplot(data = raster_bg) + 
      ggplot2::geom_raster(aes(lon, lat, fill = alt)) +
      ggplot2::scale_fill_gradientn(colors = colors_bg(n = length(brks_bg)),
                                    breaks = brks_bg) +
      ggplot2::geom_path(data = limites,
                         aes(x = long, 
                             y = lat, 
                             group = group)) 
    
  }
  if(missing(breaks)) brks <- pretty(data[["z"]]) else brks <- breaks
  #brks <- pretty(data[["z"]], n = 6)
  
  # adiciona layers de pontos ao ggp_base
  ggp <- ggp_base + 
    ggplot2::theme_bw() +
    ggplot2::coord_equal() +
    ggplot2::geom_point(data = data, 
                        aes(x = lon,
                            y = lat, 
                            fill = z), 
                        size = point_size,
                        shape = 21,
                        colour = point_color,
                        show.legend = TRUE)
  
  if(type == "diverging"){
    ggp <- ggp + 
      ggplot2::scale_fill_gradient2(name = z_legend
                                    ,breaks = brks
                                    ,low = rev(bluepal(n = sum(brks < 0)+1))
                                    ,high = rev(redpal(n = sum(brks > 0)+1)))
  } else {
    ggp <- ggp + 
      ggplot2::scale_fill_gradientn(colors = colors_z(length(brks)),
                                    name = z_legend,
                                    guide = guide_type,
                                    #guide = guide_legend(reverse = TRUE),
                                    #guide = "legend",
                                    space = "Lab",
                                    breaks = brks)
  }
  
  ggp <- ggp +
    #ggplot2::geom_text(data = data, 
    ggrepel::geom_text_repel(data = data,
                             aes(x = lon, 
                                 y = lat, 
                                 label = site), 
                             fontface = point_text_ff,
                             #vjust = -1.4,
                             segment.size = repel_seg_size,
                             #segment.colour = "black",
                             # aumenta msl, diminui segmentos conectando labels (vice-versa)
                             min.segment.length = repel_min_seg_len,
                             col = text_color,
                             size = text_size) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(5),
                                minor_breaks = scales::pretty_breaks(20),
                                #expand = c(0, 0), 
                                name = expression(Latitude~~(degree))
                                #,limits = escala_y
    ) + 
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(5),
                                minor_breaks = scales::pretty_breaks(20),
                                #expand = c(0, 0), 
                                name = expression(Longitude~~(degree))) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "transparent"),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_text(size = 15),
                   axis.title.y = ggplot2::element_text(size = 15, angle = 90),
                   axis.text.x = ggplot2::element_text(size = 13),
                   axis.text.y = ggplot2::element_text(size = 13),
                   # Legend parameters: see ?guide_legend
                   legend.title = ggplot2::element_text(size = 11),
                   # para evitar sobreposição da área da legenda sobre o mapa
                   legend.background = element_rect(fill = "transparent"),
                   ...
    )
  return(ggp)
}
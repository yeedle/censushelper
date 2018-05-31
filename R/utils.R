
#' map it.
#'
#' maps a census variable.
#'
#' variables need to be renamed to value instead of estimate.
#'
#' @param sf An object of class `sf`
#'
#' @export
#'
#' @examples
map_it <- function(sf, palette = "OrRd", provider = "CartoDB.Positron", ...) {
  pal <- leaflet::colorNumeric(palette = palette, domain = sf %>% dplyr::pull(value))
  leaflet::leaflet(sf) %>%
    leaflet::addProviderTiles(provider) %>%
    leaflet::addPolygons(
      fillColor = ~pal(value),
      weight = .1,
      opacity = .5,
      fillOpacity = 0.7,
      stroke = F,
      label = as.character(sf$value),
      highlight = leaflet::highlightOptions(
        weight = 5,
        stroke = T,
        color = "#666",
        fillOpacity = 0.7,
        bringToFront = TRUE
        ),
      ...
      ) %>%
    addLegend("bottomright", pal = pal, values = ~value,
              title = "Number of Yiddish Speakers",
              opacity = 1
    )
}

library(leaflet)
library(htmlwidgets)
library(dplyr)
library(sf)
library(terra)
library(elevatr) 
library(raster)

# --- 1. DATA PREP ---
# Load the packed rasters and unpack them immediately
load("data/environmental_metrics.RData")

r_elev   <- unwrap(r_elev_packed)
r_precip <- unwrap(r_precip_packed)
r_ndvi   <- unwrap(r_ndvi_packed)

# Assign polygons (h3_env_data is now just the polygon geometry)
h3_main <- h3_env_data 

# --- 2. PALETTE DEFINITIONS ---
pal_elev_raster <- colorNumeric(palette = terrain.colors(10), domain = values(r_elev), na.color = "transparent")
pal_precip      <- colorNumeric(palette = "Blues",  domain = values(r_precip), na.color = "transparent")
pal_trees       <- colorNumeric(palette = "Greens", domain = values(r_ndvi), na.color = "transparent")
pal_density     <- colorNumeric(palette = "YlOrRd",  domain = grid_counts$unique_species)
pal_effort      <- colorBin(palette = "Purples",     domain = grid_effort$checklist_count, bins = c(0, 10, 25, 50, 100, 500, 1000, Inf))
pal_rarities    <- colorBin(palette = "YlOrBr",      domain = results$outlier_count,       bins = c(0, 2, 4, 6, 10, 20, Inf))

# --- 3. JAVASCRIPT: LEGEND TOGGLE ONLY ---
js_legend_toggle <- "
function(el, x) {
  var updateLegend = function () {
    var selectedGroup = document.querySelectorAll('.leaflet-control-layers-selector:checked');
    var selectedGroupNames = [];
    for (var i = 0; i < selectedGroup.length; i++) {
      selectedGroupNames.push(selectedGroup[i].nextSibling.innerText.trim());
    }
    var legends = document.querySelectorAll('.legend');
    for (var i = 0; i < legends.length; i++) {
      var legend = legends[i];
      var text = legend.innerText;
      legend.style.display = 'none'; 
      if (selectedGroupNames.includes('Elevation') && text.includes('Elevation')) legend.style.display = 'block';
      if (selectedGroupNames.includes('Annual Precipitation') && text.includes('Precipitation')) legend.style.display = 'block';
      if (selectedGroupNames.includes('Tree Cover %') && text.includes('Tree')) legend.style.display = 'block';
      if (selectedGroupNames.includes('Species Density') && text.includes('Species')) legend.style.display = 'block';
      if (selectedGroupNames.includes('Checklist Density') && text.includes('Checklist')) legend.style.display = 'block';
      if (selectedGroupNames.includes('Regional Rarities (exclusion)') && text.includes('Rarity')) legend.style.display = 'block';
    }
  };
  updateLegend();
  this.on('baselayerchange', updateLegend);
  this.on('overlayadd', updateLegend);
  this.on('overlayremove', updateLegend);
}
"

# --- 4. LEAFLET CONSTRUCTION ---
m <- leaflet() %>% 
  
  # Base Layers
  addProviderTiles(providers$CartoDB.Positron, group = "Grayscale") %>%
  addTiles(urlTemplate = "https://mt1.google.com/vt/lyrs=y&x={x}&y={y}&z={z}", attribution = 'Google', group = "Google Hybrid") %>%
  addTiles(urlTemplate = "https://mt1.google.com/vt/lyrs=p&x={x}&y={y}&z={z}", attribution = 'Google', group = "Google Terrain") %>%
  
  # Panes
  addMapPane("raster_pane", zIndex = 400) %>% 
  addMapPane("boundary_pane", zIndex = 500) %>%
  
  # Rasters
  addRasterImage(raster::raster(r_elev), colors = pal_elev_raster, opacity = 0.8, maxBytes = Inf, group = "Elevation", options = leafletOptions(pane = "raster_pane")) %>%
  addRasterImage(raster::raster(r_precip), colors = pal_precip, opacity = 0.7, maxBytes = Inf, group = "Annual Precipitation", options = leafletOptions(pane = "raster_pane")) %>%
  addRasterImage(raster::raster(r_ndvi), colors = pal_trees, opacity = 0.7, maxBytes = Inf, group = "Tree Cover %", options = leafletOptions(pane = "raster_pane")) %>%
  
  # Vectors
  addPolygons(data = map, fill = FALSE, color = "black", weight = 2, group = "Boundaries", options = pathOptions(pane = "boundary_pane")) %>%
  addPolygons(data = empty_grids_h3, fillColor = "#333333", fillOpacity = 0.4, weight = 0.5, color = "#444444", group = "No Data Zones") %>%
  
  # Data Layers
  addPolygons(data = map_corr, fillColor = ~pal_rarities(outlier_count), fillOpacity = 0.8, weight = 1, color = "white", group = "Regional Rarities (exclusion)", label = ~hover_label, highlightOptions = highlightOptions(weight = 3, color = "orange", bringToFront = TRUE)) %>%
  addPolygons(data = map_density_h3, fillColor = ~pal_density(unique_species), fillOpacity = 0.7, weight = 1, color = "white", group = "Species Density", label = ~hover_label) %>%
  addPolygons(data = map_effort_h3, fillColor = ~pal_effort(checklist_count), fillOpacity = 0.7, weight = 1, color = "white", group = "Checklist Density", label = ~hover_label) %>%
  
  # Legends
  addLegend(pal = pal_elev_raster, values = values(r_elev), title = "Elevation (m)", position = "bottomright", className = "info legend") %>%
  addLegend(pal = pal_precip, values = values(r_precip), title = "Precipitation (mm)", position = "bottomright", className = "info legend") %>%
  addLegend(pal = pal_trees, values = values(r_ndvi), title = "Tree Cover (%)", position = "bottomright", className = "info legend") %>%
  addLegend(pal = pal_rarities, values = map_corr$outlier_count, title = "Rarity Count", position = "bottomright", className = "info legend") %>%
  addLegend(pal = pal_density, values = map_density_h3$unique_species, title = "Species Count", position = "bottomleft", className = "info legend") %>%
  addLegend(pal = pal_effort, values = map_effort_h3$checklist_count, title = "Checklists", position = "bottomleft", className = "info legend") %>%
  
  # Controls
  addLayersControl(
    baseGroups = c("Grayscale", "Google Hybrid", "Google Terrain"),
    overlayGroups = c("Regional Rarities (exclusion)", "Species Density", "Checklist Density", 
                      "Elevation", "Annual Precipitation", "Tree Cover %", 
                      "No Data Zones", "Boundaries"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup(c("Species Density", "Checklist Density", "No Data Zones", "Elevation", "Annual Precipitation", "Tree Cover %")) %>%
  
  # Inject JS (Legend Toggle ONLY)
  htmlwidgets::onRender(js_legend_toggle)

saveWidget(m, file = "outputs/TZ.html", selfcontained = TRUE)

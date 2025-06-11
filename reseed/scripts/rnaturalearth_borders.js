// Initialize the Cesium Viewer
const viewer = new Cesium.Viewer("cesiumContainer", {
  baseLayerPicker: true,
  geocoder: false,
  timeline: false,
  animation: false,
  homeButton: false,
  navigationHelpButton: false,
});

var scene = viewer.scene;

// Disable default visuals
scene.skyBox.show = false;
scene.skyAtmosphere.show = false;
scene.sun.show = false;
scene.moon.show = false;

// Remove default credits
viewer._cesiumWidget._creditContainer.style.display = "none";

// Load GeoJSON as polygons
Cesium.GeoJsonDataSource.load(
  "https://raw.githubusercontent.com/nvkelso/natural-earth-vector/master/geojson/ne_110m_admin_0_countries.geojson",
  {
    clampToGround: false, // Prevent terrain clamping
  }
).then((dataSource) => {
  viewer.dataSources.add(dataSource); // Add the data source

  // Loop through all entities and style them
  const entities = dataSource.entities.values;
  for (let i = 0; i < entities.length; i++) {
    const entity = entities[i];

    // Check if entity has a polygon
    if (entity.polygon) {
      entity.polygon.material = Cesium.Color.RED.withAlpha(0.5); // Red fill with 50% opacity
      entity.polygon.outline = true; // Enable outline
      entity.polygon.outlineColor = Cesium.Color.BLACK; // Black outline color
      entity.polygon.outlineWidth = 1.0; // Outline width

      // Ensure polygons are flat for rendering
      entity.polygon.height = 0; // Flat polygons
      entity.polygon.extrudedHeight = 0; // Avoid extrusions
      entity.polygon.perPositionHeight = false; // Disable terrain adjustment
    }
  }

  // Zoom to fit all polygons
  viewer.zoomTo(dataSource);
});

// Set camera to fit world view
viewer.camera.setView({
  destination: Cesium.Rectangle.fromDegrees(-180, -90, 180, 90),
});
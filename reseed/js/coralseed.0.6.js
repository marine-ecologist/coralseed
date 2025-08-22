// === Minimal Cesium setup ===
const viewer = new Cesium.Viewer("cesiumContainer", {
  animation: false,
  timeline: false,
  baseLayerPicker: false,
  geocoder: false,
  homeButton: false,
  sceneModePicker: false,
  navigationHelpButton: false,
  fullscreenButton: false,
  imageryProvider: false, // no base imagery
  terrainProvider: new Cesium.EllipsoidTerrainProvider() // flat ellipsoid
});

// Hide atmosphere/sky
viewer.scene.skyBox.show = false;
viewer.scene.skyAtmosphere.show = false;
viewer.scene.sun.show = false;
viewer.scene.moon.show = false;

// Camera top-down
viewer.camera.setView({
  destination: Cesium.Cartesian3.fromDegrees(145.4556, -14.637, 2000),
  orientation: {
    heading: 0.0,
    pitch: Cesium.Math.toRadians(-90), // straight down
    roll: 0.0
  }
});

// Load and style your LineStrings
Cesium.GeoJsonDataSource.load("geojson/lizard_particles_linestrings.geojson")
  .then((ds) => {
    viewer.dataSources.add(ds);
    for (const e of ds.entities.values) {
      if (e.polyline) {
        e.polyline.width = 0.5;
        e.polyline.material = Cesium.Color.WHITE.withAlpha(0.7);
        // no clampToGround -> rendered at ellipsoid height
      }
    }
    return viewer.zoomTo(ds);
  })
  .catch((err) => console.error("Failed to load GeoJSON:", err));
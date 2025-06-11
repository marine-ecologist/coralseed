// Initialize the Cesium viewer
const viewer = new Cesium.Viewer("cesiumContainer", {
  baseLayer: Cesium.ImageryLayer.fromProviderAsync(
    Cesium.TileMapServiceImageryProvider.fromUrl(
      Cesium.buildModuleUrl("Assets/Textures/NaturalEarthII")
    )
  ),
  baseLayerPicker: true,
  geocoder: false,
  timeline: false,
  animation: false,
  homeButton: false,
  navigationHelpButton: false,
});

const scene = viewer.scene;

// Disable visuals safely
scene.skyBox.show = false;
scene.skyAtmosphere.show = false;
scene.sun.show = false;
scene.moon.show = false;

// Remove credits
viewer._cesiumWidget._creditContainer.style.display = "none";

// Add SST WMS layer
const sstLayer = viewer.imageryLayers.addImageryProvider(
  new Cesium.WebMapServiceImageryProvider({
    url: `https://pae-paha.pacioos.hawaii.edu/thredds/wms/dhw_5km`,
    layers: "CRW_SST",
    parameters: {
      transparent: true,
      format: "image/png",
    },
     })
);

// Define the handler for click events
const handler = new Cesium.ScreenSpaceEventHandler(viewer.scene.canvas);

handler.setInputAction(async (click) => {
  const cartesian = viewer.camera.pickEllipsoid(click.position, scene.globe.ellipsoid);
 


   
}, Cesium.ScreenSpaceEventType.LEFT_CLICK);
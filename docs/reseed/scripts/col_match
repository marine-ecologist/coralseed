// Cesium setup
Cesium.Ion.defaultAccessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJqdGkiOiJhZTNmOGJmZC0zOTcwLTRhMzYtOTEyMC1jYjc5Yzc5YTcwODMiLCJpZCI6MjY4NTE0LCJpYXQiOjE3MzY3MTg2NzB9.X6fIDdZkrPlD5AGjASkJ-IerCu1BLe8IIQLrwJku4LQ";


// Initialize the Cesium viewer
const viewer = new Cesium.Viewer("cesiumContainer", {
  baseLayerPicker: false,
  timeline: false,
  animation: false,
  homeButton: false,
  navigationHelpButton: false,
  enablePickFeatures: false,
  infoBox: false,
  geocoder: false,
});

const scene = viewer.scene;
scene.skyBox.show = false;
scene.skyAtmosphere.show = false;
scene.sun.show = false;
scene.moon.show = false;

// NOAA DHW WMS Source
const noaaDHWLayer = viewer.imageryLayers.addImageryProvider(
  new Cesium.WebMapServiceImageryProvider({
    url: "https://pae-paha.pacioos.hawaii.edu/thredds/wms/dhw_5km",
    layers: "CRW_DHW",
    parameters: {
      transparent: true,
      format: "image/png",
      time: "2025-03-10T12:00:00.000Z",
      styles: "boxfill/rdbu" // Applying the RdBu palette
    },
  })
);

// Post-processing shader for color remapping
const colorMappingStage = new Cesium.PostProcessStage({
  fragmentShader: `
    uniform sampler2D colorTexture;
    in vec2 v_textureCoordinates;
    out vec4 fragColor;

    void main() {
        vec4 originalColor = texture(colorTexture, v_textureCoordinates);

        // Map original DHW colors to RdBu
        if (originalColor.r > 0.9 && originalColor.g < 0.1 && originalColor.b < 0.1) {
            fragColor = vec4(0.1, 0.2, 0.9, 1.0); // Example remapping red -> blue
        } else if (originalColor.r > 0.7 && originalColor.g > 0.7) {
            fragColor = vec4(0.9, 0.9, 0.1, 1.0); // Example remapping yellow -> new color
        } else {
            fragColor = originalColor; // Default
        }
    }
  `,
});

// Apply the shader to the DHW layer
scene.postProcessStages.add(colorMappingStage);

// Set the camera to focus on a coral reef region
viewer.camera.setView({
  destination: Cesium.Cartesian3.fromDegrees(140, -8, 15000000),
  orientation: {
    heading: Cesium.Math.toRadians(0),
    pitch: Cesium.Math.toRadians(-90),
    roll: 0,
  },
});
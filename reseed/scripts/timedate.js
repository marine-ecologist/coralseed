const viewer = new Cesium.Viewer("cesiumContainer", {
  baseLayerPicker: false,
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

Cesium.Ion.defaultAccessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJqdGkiOiJhZTNmOGJmZC0zOTcwLTRhMzYtOTEyMC1jYjc5Yzc5YTcwODMiLCJpZCI6MjY4NTE0LCJpYXQiOjE3MzY3MTg2NzB9.X6fIDdZkrPlD5AGjASkJ-IerCu1BLe8IIQLrwJku4LQ";
		


// Add SST WMS layer
const sstLayer = viewer.imageryLayers.addImageryProvider(
  new Cesium.WebMapServiceImageryProvider({
    url: `https://pae-paha.pacioos.hawaii.edu/thredds/wms/dhw_5km`,
    layers: "CRW_DHW",
    parameters: {
      transparent: true,
      format: "image/png",
    },
  })
);

// Set the WMS layer alpha to 0.7
const addedLayer = viewer.imageryLayers.get(viewer.imageryLayers.length - 1);
addedLayer.alpha = 0.5;

const styles = `
  #cesiumContainer canvas {
    image-rendering: smooth;
    image-rendering: -webkit-optimize-contrast;
  }
`;
const styleTag = document.createElement("style");
styleTag.textContent = styles;
document.head.appendChild(styleTag);


// Define the handler for click events
const handler = new Cesium.ScreenSpaceEventHandler(viewer.scene.canvas);


handler.setInputAction(async (click) => {
  const pickRay = scene.camera.getPickRay(click.position);
  const imageryLayerFeatures = await scene.imageryLayers.pickImageryLayerFeatures(pickRay, scene);

  if (imageryLayerFeatures && imageryLayerFeatures.length > 0) {
    const rawText = imageryLayerFeatures[0].description;
   // Extract values using regular expressions
const gridCentreLonMatch = rawText.match(/&lt;gridCentreLon&gt;([-0-9.]+)&lt;\/gridCentreLon&gt;/);
const gridCentreLatMatch = rawText.match(/&lt;gridCentreLat&gt;([-0-9.]+)&lt;\/gridCentreLat&gt;/);
const timeMatch = rawText.match(/&lt;time&gt;([\d-]+)T[\d:.]+Z&lt;\/time&gt;/);
const valueMatch = rawText.match(/&lt;value&gt;([-0-9.]+)&lt;\/value&gt;/);

if (gridCentreLonMatch && gridCentreLatMatch && timeMatch && valueMatch) {
  // Extract and round the values
  const samplelon = parseFloat(gridCentreLonMatch[1]).toFixed(5);
  const samplelat = parseFloat(gridCentreLatMatch[1]).toFixed(5);

  // Parse time to dd-mm-yyyy format
  const isoDate = timeMatch[1]; // Extract "2025-01-12"
  const [year, month, day] = isoDate.split("-");
  const formattedDate = `${day}-${month}-${year}`; // Convert to "12-01-2025"

  const sampleval = parseFloat(valueMatch[1]).toFixed(2);

  // Log the extracted and formatted values
  console.log(`Longitude: ${samplelon}`);
  console.log(`Latitude: ${samplelat}`);
  console.log(`Time: ${formattedDate}`);
  console.log(`Value: ${sampleval}`);
} else {
  console.error("Could not extract all values from the raw text.");
}
    
    
  } else {
    console.log("No feature info found at this location.");
  }
}, Cesium.ScreenSpaceEventType.LEFT_CLICK);







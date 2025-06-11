// Cesium setup
Cesium.Ion.defaultAccessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJqdGkiOiJhZTNmOGJmZC0zOTcwLTRhMzYtOTEyMC1jYjc5Yzc5YTcwODMiLCJpZCI6MjY4NTE0LCJpYXQiOjE3MzY3MTg2NzB9.X6fIDdZkrPlD5AGjASkJ-IerCu1BLe8IIQLrwJku4LQ";

const viewer = new Cesium.Viewer("cesiumContainer", {
  baseLayerPicker: false,
  geocoder: false,
  timeline: false,
  animation: false,
  homeButton: false,
  navigationHelpButton: false,
  enablePickFeatures: false,
  infoBox: false,
});

const scene = viewer.scene;

// Disable unnecessary visuals
scene.skyBox.show = false;
scene.skyAtmosphere.show = false;
scene.sun.show = false;
scene.moon.show = false;

// Remove Cesium credits
viewer._cesiumWidget._creditContainer.style.display = "none";

// Styling for smooth rendering
const styles = `
  #cesiumContainer canvas {
    image-rendering: smooth;
    image-rendering: -webkit-optimize-contrast;
  }

  .tooltip {
    position: absolute;
    background: rgba(42, 42, 42, 0.8);
    color: white;
    padding: 5px 10px;
    border-radius: 5px;
    font-size: 12px;
    font-family: Arial, sans-serif;
    pointer-events: none;
    display: none;
  }
`;
const styleTag = document.createElement("style");
styleTag.textContent = styles;
document.head.appendChild(styleTag);

// Tooltip for hover Lon/Lat
const hoverTooltip = document.createElement("div");
hoverTooltip.classList.add("tooltip");
document.body.appendChild(hoverTooltip);

// Tooltip for clicked data
const clickTooltip = document.createElement("div");
clickTooltip.classList.add("tooltip");
document.body.appendChild(clickTooltip);

// Layer management
let activeLayer = null;
let noaaLayer = null;

// Mapping NOAA metrics to names
const noaaMetricNames = {
  CRW_SST: "Sea Surface Temperature",
  CRW_SSTANOMALY: "Sea Surface Temperature Anomaly",
  CRW_SSTTREND: "Sea Surface Temperature Trend",
  CRW_HOTSPOT: "HotSpot",
  CRW_BAA: "Bleaching Alert Area",
  CRW_DHW: "Degree Heating Weeks",
};

// Function to create a WMS Tile Layer with a static date
function createWMSTileLayer(metric) {
  const staticDate = "20250101"; // Static date
  const url = `https://storage.googleapis.com/production-coral-tiles/crw/${metric}/${staticDate}/{z}/{x}/{y}.png`;
  return new Cesium.UrlTemplateImageryProvider({
    url: url,
    credit: "Data from Coral Reef Watch (CRW)",
  });
}

// Function to switch layers
function switchLayers(wmsMetric, noaaMetric) {
  if (activeLayer) viewer.imageryLayers.remove(activeLayer, false);
  if (noaaLayer) viewer.imageryLayers.remove(noaaLayer, false);

  if (wmsMetric !== "none") {
    activeLayer = viewer.imageryLayers.addImageryProvider(createWMSTileLayer(wmsMetric));
    activeLayer.alpha = 0.7;
  }

  if (noaaMetric !== "none") {
    noaaLayer = viewer.imageryLayers.addImageryProvider(
      new Cesium.WebMapServiceImageryProvider({
        url: `https://pae-paha.pacioos.hawaii.edu/thredds/wms/dhw_5km`,
        layers: noaaMetric,
        parameters: {
          transparent: true,
          format: "image/png",
          time: "2025-01-01T12:00:00.000Z",
        },
      })
    );
    noaaLayer.alpha = 0.0;
  }
}

// Define the handler for click events
const clickHandler = new Cesium.ScreenSpaceEventHandler(viewer.scene.canvas);
clickHandler.setInputAction(async (click) => {
  const pickRay = scene.camera.getPickRay(click.position);
  const imageryLayerFeatures = await scene.imageryLayers.pickImageryLayerFeatures(pickRay, scene);

  const mouseX = click.position.x;
  const mouseY = click.position.y;

  if (imageryLayerFeatures && imageryLayerFeatures.length > 0) {
    const rawText = imageryLayerFeatures[0].description;

    const gridCentreLonMatch = rawText.match(/&lt;gridCentreLon&gt;([-0-9.]+)&lt;\/gridCentreLon&gt;/);
    const gridCentreLatMatch = rawText.match(/&lt;gridCentreLat&gt;([-0-9.]+)&lt;\/gridCentreLat&gt;/);
    const timeMatch = rawText.match(/&lt;time&gt;([\d-]+)T[\d:.]+Z&lt;\/time&gt;/);
    const valueMatch = rawText.match(/&lt;value&gt;([-0-9.]+)&lt;\/value&gt;/);

    if (gridCentreLonMatch && gridCentreLatMatch && timeMatch && valueMatch) {
      const samplelon = parseFloat(gridCentreLonMatch[1]).toFixed(5);
      const samplelat = parseFloat(gridCentreLatMatch[1]).toFixed(5);
      const isoDate = timeMatch[1];
      const [year, month, day] = isoDate.split("-");
      const formattedDate = `${day}-${month}-${year}`;
      const sampleval = parseFloat(valueMatch[1]).toFixed(2);
      const metricName = noaaMetricNames[noaaLayer.imageryProvider.layers] || "Value";

      // Update tooltip
      clickTooltip.style.display = "block";
      clickTooltip.style.left = `${mouseX + 15}px`;
      clickTooltip.style.top = `${mouseY + 15}px`;
      clickTooltip.innerHTML = `
        Lon: ${samplelon}<br>
        Lat: ${samplelat}<br>
        ${metricName}: ${sampleval}
      `;
    }
  }
}, Cesium.ScreenSpaceEventType.LEFT_CLICK);

// Lon/Lat Hover Tracker
const hoverHandler = new Cesium.ScreenSpaceEventHandler(scene.canvas);
hoverHandler.setInputAction((movement) => {
  const cartesian = viewer.camera.pickEllipsoid(movement.endPosition, scene.globe.ellipsoid);

  if (cartesian) {
    const cartographic = Cesium.Cartographic.fromCartesian(cartesian);
    const longitudeString = Cesium.Math.toDegrees(cartographic.longitude).toFixed(4);
    const latitudeString = Cesium.Math.toDegrees(cartographic.latitude).toFixed(4);

    // Update tooltip
    hoverTooltip.style.display = "block";
    hoverTooltip.style.left = `${movement.endPosition.x + 15}px`;
    hoverTooltip.style.top = `${movement.endPosition.y + 15}px`;
    hoverTooltip.innerHTML = `
      
      Lon: ${longitudeString}<br>
      Lat: ${latitudeString}
    `;
  } else {
    hoverTooltip.style.display = "none";
  }
}, Cesium.ScreenSpaceEventType.MOUSE_MOVE);

// Add Toolbar
const toolbar = document.createElement("div");
toolbar.style.position = "absolute";
toolbar.style.top = "10px";
toolbar.style.left = "10px";
toolbar.style.backgroundColor = "rgba(42, 42, 42, 0.8)";
toolbar.style.padding = "10px";
toolbar.style.borderRadius = "5px";
toolbar.style.color = "white";
document.body.appendChild(toolbar);

const metrics = [
  { name: "None", value: "none" },
  { name: "Sea Surface Temperature", value: "sst" },
  { name: "Sea Surface Temperature Anomaly", value: "ssta" },
  { name: "Sea Surface Temperature Trend", value: "sstt" },
  { name: "Hot Spots", value: "hs" },
  { name: "Bleaching Alert Area", value: "baa" },
  { name: "Degree Heating Weeks", value: "dhw" },
];

const noaametrics = [
  { name: "None", value: "none" },
  { name: "Sea Surface Temperature", value: "CRW_SST" },
  { name: "Sea Surface Temperature Anomaly", value: "CRW_SSTANOMALY" },
  { name: "Sea Surface Temperature Trend", value: "CRW_SSTTREND" },
  { name: "Hot Spots", value: "CRW_HOTSPOT" },
  { name: "Bleaching Alert Area", value: "CRW_BAA" },
  { name: "Degree Heating Weeks", value: "CRW_DHW" },
];

const metricToNoaa = {};
metrics.forEach((metric, index) => {
  metricToNoaa[metric.value] = noaametrics[index].value;
});

// Create vertically aligned buttons with fixed width
metrics.forEach((metric) => {
  const button = document.createElement("button");
  button.textContent = metric.name;
  button.style.display = "block"; // Align buttons vertically
  button.style.margin = "5px auto"; // Add vertical margin and center align
  button.style.padding = "5px 10px"; // Adjust padding for smaller buttons
  button.style.fontSize = "10px"; // Reduce font size to 10px
  button.style.backgroundColor = "#555";
  button.style.color = "white";
  button.style.border = "none";
  button.style.borderRadius = "5px";
  button.style.cursor = "pointer";
  button.style.width = "150px"; // Set fixed width for buttons

  button.onclick = () => {
    const selectedMetric = metric.value;
    const selectedNoaaMetric = metricToNoaa[selectedMetric];
    switchLayers(selectedMetric, selectedNoaaMetric);
  };

  toolbar.appendChild(button);
});

// Add Transparency Slider
const sliderLabel = document.createElement("div");
sliderLabel.textContent = "Transparency";
sliderLabel.style.textAlign = "center";
sliderLabel.style.color = "white";
sliderLabel.style.margin = "10px 0";
toolbar.appendChild(sliderLabel);

const slider = document.createElement("input");
slider.type = "range";
slider.min = "0";
slider.max = "1";
slider.step = "0.01";
slider.value = "0.7"; // Default alpha value
slider.style.width = "150px";
slider.style.margin = "5px auto";
slider.style.display = "block";

slider.oninput = () => {
  if (activeLayer) {
    activeLayer.alpha = parseFloat(slider.value);
  }
};

toolbar.appendChild(slider);
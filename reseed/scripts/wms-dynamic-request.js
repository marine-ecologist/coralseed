// Initialize the Cesium viewer
const viewer = new Cesium.Viewer("cesiumContainer", {
  baseLayerPicker: false, // Disable base layer picker
  geocoder: false,        // Disable geocoder
  timeline: false,        // Disable timeline
  animation: false,       // Disable animation
  homeButton: false,      // Disable home button
  navigationHelpButton: false, // Disable navigation help button
});

const scene = viewer.scene;

// Disable unnecessary visuals
scene.skyBox.show = false;
scene.skyAtmosphere.show = false;
scene.sun.show = false;
scene.moon.show = false;

// Remove default credits
viewer._cesiumWidget._creditContainer.style.display = "none";

// --------------------------------------------------------------------------------

// Define the WMS URL (unchanged)
const wmsUrl = "https://pae-paha.pacioos.hawaii.edu/thredds/wms/dhw_5km?service=WMS&version=1.3.0&request=GetMap&bbox=-89.99,-179.99,89.99,180.0&crs=EPSG:4326&width=360&height=180&bgcolor=0x808080&layers=CRW_SST&styles=&format=image/png&transparent=TRUE";

// Add the WMS layer to CesiumJS
const sstLayer = viewer.imageryLayers.addImageryProvider(
  new Cesium.WebMapServiceImageryProvider({
    url: wmsUrl.split("?")[0], // Use only the base URL for Cesium
    layers: "CRW_SST",
    parameters: {
      transparent: "true",
      format: "image/png",
    },
    credit: "Data from Coral Reef Watch (CRW)",
  })
);

const getValshandler = new Cesium.ScreenSpaceEventHandler(viewer.scene.canvas);

getValshandler.setInputAction(async (click) => {
  // Project the click position onto the globe's ellipsoid
  const cartesian = viewer.camera.pickEllipsoid(click.position, scene.globe.ellipsoid);
  if (!cartesian) {
    console.log("Invalid click position. Likely clicked outside the globe.");
    return;
  }

  // Convert Cartesian3 to geographic coordinates (longitude, latitude)
  const cartographic = Cesium.Cartographic.fromCartesian(cartesian);
  let longitude = Cesium.Math.toDegrees(cartographic.longitude);
  let latitude = Cesium.Math.toDegrees(cartographic.latitude);

  // Round to 6 decimal places for consistency
  longitude = parseFloat(longitude.toFixed(8));
  latitude = parseFloat(latitude.toFixed(8));
  console.log(`Clicked Location: Longitude = ${longitude}, Latitude = ${latitude}`);

  // Construct the bounding box with a larger area around the clicked location
  const ymin = longitude - 0.005; // Offset by 0.005
  const xmin = latitude - 0.005;
  const ymax = longitude + 0.005;
  const xmax = latitude + 0.005;
  const bbox = `${xmin},${ymin},${xmax},${ymax}`;
  console.log(`bbox = ${bbox}`);

  // Construct the WMS GetFeatureInfo URL
  const featureInfoUrl = `https://pae-paha.pacioos.hawaii.edu/thredds/wms/dhw_5km?SERVICE=WMS&VERSION=1.3.0&REQUEST=GetFeatureInfo` +
  `&LAYERS=CRW_SST&QUERY_LAYERS=CRW_SST&CRS=EPSG:4326&BBOX=${bbox}` +
  `&WIDTH=101&HEIGHT=101&I=50&J=50&INFO_FORMAT=text/xml`;
  
  console.log(`URL = ${featureInfoUrl}`);

  try {
    // Fetch the WMS data
    const response = await fetch(featureInfoUrl);
    if (!response.ok) {
      throw new Error(`HTTP error! Status: ${response.status}`);
    }

    // Parse the XML response
    const xmlText = await response.text();
    const parser = new DOMParser();
    const xmlDoc = parser.parseFromString(xmlText, "application/xml");

    // Extract relevant data (e.g., <value> and <time>)
    const valueNode = xmlDoc.querySelector("value");
    const timeNode = xmlDoc.querySelector("time");

    const sstValue = valueNode ? valueNode.textContent.trim() : "No data";
    const timeValue = timeNode ? timeNode.textContent.trim() : "No time available";

    console.log(`Extracted SST Value: ${sstValue}`);
    console.log(`Extracted Time: ${timeValue}`);
    console.log("------------------------------------------------");

  } catch (error) {
    console.error("Error fetching or parsing WMS data:", error);
  }
}, Cesium.ScreenSpaceEventType.LEFT_CLICK);
// Cesium setup
Cesium.Ion.defaultAccessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJqdGkiOiJhZTNmOGJmZC0zOTcwLTRhMzYtOTEyMC1jYjc5Yzc5YTcwODMiLCJpZCI6MjY4NTE0LCJpYXQiOjE3MzY3MTg2NzB9.X6fIDdZkrPlD5AGjASkJ-IerCu1BLe8IIQLrwJku4LQ";

// Initialize basic Cesium viewer
const viewer = new Cesium.Viewer("cesiumContainer", {
  baseLayerPicker: false,
  timeline: true,
  animation: true,
  homeButton: false,
  navigationHelpButton: false,
  geocoder: true,
  infoBox: false,
  enablePickFeatures: false,
});

const scene = viewer.scene;

// Disable sky, sun, and moon
scene.skyBox.show = false;
scene.skyAtmosphere.show = false;
scene.sun.show = false;
scene.moon.show = false;

// Hide Cesium credits
viewer._cesiumWidget._creditContainer.style.display = "none";


// Add tooltip for lat/lon hover
const hoverTooltip = document.createElement("div");
hoverTooltip.classList.add("tooltip");
hoverTooltip.style.position = "absolute";
hoverTooltip.style.background = "rgba(42, 42, 42, 0.8)";
hoverTooltip.style.color = "white";
hoverTooltip.style.padding = "5px 10px";
hoverTooltip.style.borderRadius = "5px";
hoverTooltip.style.fontSize = "12px";
hoverTooltip.style.fontFamily = "Arial, sans-serif";
hoverTooltip.style.pointerEvents = "none";
hoverTooltip.style.display = "none";
document.body.appendChild(hoverTooltip);

// Hover handler for lat/lon
const dynamicHandler = new Cesium.ScreenSpaceEventHandler(scene.canvas);
dynamicHandler.setInputAction((movement) => {
  const cartesian = viewer.camera.pickEllipsoid(movement.endPosition, scene.globe.ellipsoid);

  if (cartesian) {
    const cartographic = Cesium.Cartographic.fromCartesian(cartesian);
    const longitude = Cesium.Math.toDegrees(cartographic.longitude).toFixed(4);
    const latitude = Cesium.Math.toDegrees(cartographic.latitude).toFixed(4);

    hoverTooltip.style.display = "block";
    hoverTooltip.style.left = `${movement.endPosition.x + 15}px`;
    hoverTooltip.style.top = `${movement.endPosition.y + 15}px`;
    hoverTooltip.innerHTML = `Lon: ${longitude}<br>Lat: ${latitude}`;
  } else {
    hoverTooltip.style.display = "none";
  }
}, Cesium.ScreenSpaceEventType.MOUSE_MOVE);

// Camera default view
viewer.camera.setView({
  destination: Cesium.Cartesian3.fromDegrees(145.4535, -14.647, 1000),
  orientation: {
    heading: Cesium.Math.toRadians(0),
    pitch: Cesium.Math.toRadians(-90),
    roll: 0,
  },
});

// Set date
let globalMinDate = Infinity;
let globalMaxDate = -Infinity;

function updateGlobalTimeBounds(date) {
  const t = date.getTime();
  if (t < globalMinDate) globalMinDate = t;
  if (t > globalMaxDate) globalMaxDate = t;
}


// Layer controls
const layerControls = {
  settlers: null,
  larvalTracks: null,
  seascape: null,
};
const layerToggleContainer = document.createElement("div");
layerToggleContainer.style.position = "absolute";
layerToggleContainer.style.top = "10px";
layerToggleContainer.style.left = "10px";
layerToggleContainer.style.zIndex = "1000";
layerToggleContainer.style.background = "rgba(255, 255, 255, 0.8)";
layerToggleContainer.style.padding = "6px";
layerToggleContainer.style.borderRadius = "5px";
layerToggleContainer.style.fontFamily = "Arial, sans-serif";
layerToggleContainer.style.fontSize = "13px";
document.body.appendChild(layerToggleContainer);  // <- place before the loop

["settlers", "larvalTracks", "seascape"].forEach(layer => {
  const label = document.createElement("label");
  label.style.display = "block";
  label.style.marginBottom = "4px";

  const checkbox = document.createElement("input");
  checkbox.type = "checkbox";
  checkbox.checked = true;
  checkbox.style.marginRight = "5px";
  checkbox.dataset.layer = layer;

  checkbox.addEventListener("change", () => {
    const target = layerControls[layer];
    if (target) target.show = checkbox.checked;
  });

  label.appendChild(checkbox);
  label.appendChild(document.createTextNode(layer));
  layerToggleContainer.appendChild(label);
});
document.body.appendChild(layerToggleContainer);


// Define color maps
const seascapeColors = {
  "Plateau": Cesium.Color.fromCssColorString("cornsilk").withAlpha(0.8),
  "Back Reef Slope": Cesium.Color.DARKCYAN.withAlpha(0.8),
  "Reef Slope": Cesium.Color.fromCssColorString("darkseagreen").withAlpha(0.8),
  "Sheltered Reef Slope": Cesium.Color.fromCssColorString("darkslategrey").withAlpha(0.8),
  "Inner Reef Flat": Cesium.Color.fromCssColorString("darkgoldenrod").withAlpha(0.8),
  "Outer Reef Flat": Cesium.Color.fromCssColorString("goldenrod").withAlpha(0.8),
  "Reef Crest": Cesium.Color.fromCssColorString("coral").withAlpha(0.8),
};

const settlementColors = [
  "#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598",
  "#ffffbf", "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142"
].map(c => Cesium.Color.fromCssColorString(c).withAlpha(0.9));


// 1. Seascape
Cesium.GeoJsonDataSource.load("geojson/lizard_seascape.geojson").then(seascapeDS => {
  layerControls.seascape = seascapeDS;
  seascapeDS.entities.values.forEach(entity => {
    const className = entity.properties?.class?.getValue();
    const color = seascapeColors[className] || Cesium.Color.LIGHTGRAY.withAlpha(0.9);
    if (entity.polygon) {
	  entity.polygon.material = color;
	  entity.polygon.outline = true;
	  entity.polygon.outlineColor = Cesium.Color.BLACK;
	  entity.polygon.outlineWidth = 0.5;
	  entity.polygon.height = undefined;
	  entity.polygon.extrudedHeight = undefined;
	  entity.polygon.heightReference = Cesium.HeightReference.CLAMP_TO_GROUND;
	  entity.polygon.zIndex = 0; // lowest
	}
  });

  return viewer.dataSources.add(seascapeDS);


}).then(() => {

// 2. Larval tracks
return Cesium.GeoJsonDataSource.load("geojson/Llines.geojson", { clampToGround: true }).then(linesDS => {
  layerControls.larvalTracks = linesDS;

  const entities = linesDS.entities.values;
  const valid = entities.map(e => {
    const timeStr = e.properties?.time?.getValue();
    const t = timeStr ? new Date(timeStr) : null;
    const positions = e.polyline?.positions?.getValue(Cesium.JulianDate.now());
    return t && positions?.length > 0 ? { entity: e, date: t, positions } : null;
  }).filter(Boolean);

  valid.forEach(d => updateGlobalTimeBounds(d.date));
  const simEnd = Cesium.JulianDate.fromDate(new Date(globalMaxDate + 3600000));

  valid.forEach(({ entity, date, positions }) => {
    const appear = Cesium.JulianDate.fromDate(date);
    entity.availability = new Cesium.TimeIntervalCollection([
      new Cesium.TimeInterval({ start: appear, stop: simEnd })
    ]);

    entity.polyline = new Cesium.PolylineGraphics({
      positions: positions,
      width: 0.5,
      material: Cesium.Color.WHITE.withAlpha(0.85),
      clampToGround: true
    });
  });

  return viewer.dataSources.add(linesDS);
});

}).then(() => {

  // 3. Settlers
  return Cesium.GeoJsonDataSource.load("geojson/Lpoints.geojson", { clampToGround: true });

}).then(pointsDS => {
  layerControls.settlers = pointsDS;
  const entities = pointsDS.entities.values;
  const valid = entities.map(e => {
    const timeStr = e.properties?.time?.getValue();
    const val = e.properties?.dispersaltime?.getValue();
    const t = timeStr ? new Date(timeStr) : null;
    return t && Number.isFinite(val) ? { entity: e, date: t, val } : null;
  }).filter(Boolean);
  valid.forEach(d => updateGlobalTimeBounds(d.date));
  valid.forEach(({ entity, date, val }) => {
    const appear = Cesium.JulianDate.fromDate(date);
    const norm = (val - Math.min(...valid.map(d => d.val))) /
                 (Math.max(...valid.map(d => d.val)) - Math.min(...valid.map(d => d.val)));
    const color = settlementColors[Math.floor(norm * (settlementColors.length - 1))];
    entity.availability = new Cesium.TimeIntervalCollection([
      new Cesium.TimeInterval({ start: appear, stop: Cesium.JulianDate.fromDate(new Date(globalMaxDate + 3600000)) })
    ]);
    entity.point = new Cesium.PointGraphics({
	  pixelSize: 8,
	  color: color.withAlpha(0.9),
	  outlineColor: Cesium.Color.BLACK,
	  outlineWidth: 1.5,
	  heightReference: Cesium.HeightReference.CLAMP_TO_GROUND,
	  zIndex: 2 // topmost
	});
  });

  return viewer.dataSources.add(pointsDS);

}).then(() => {
  // Finalize clock and timeline
  const simStart = Cesium.JulianDate.fromDate(new Date(globalMinDate));
  const simEnd = Cesium.JulianDate.fromDate(new Date(globalMaxDate + 3600000));
  viewer.clock.startTime = simStart.clone();
  viewer.clock.stopTime = simEnd.clone();
  viewer.clock.currentTime = simStart.clone();
  viewer.clock.clockRange = Cesium.ClockRange.LOOP_STOP;
  viewer.clock.multiplier = 2000;
  viewer.clock.shouldAnimate = true;
  viewer.timeline.zoomTo(simStart, simEnd);
});

// suppress console logs for zindex warning
	const originalWarn = console.warn;
	console.warn = function (msg, ...args) {
	  if (msg && msg.toString().includes("Entity geometry outlines are unsupported on terrain")) return;
	  if (msg && msg.toString().includes("with heightReference must also have a defined height")) return;
	  originalWarn.call(console, msg, ...args);
	};

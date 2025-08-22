// Cesium setup
Cesium.Ion.defaultAccessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJqdGkiOiJhZTNmOGJmZC0zOTcwLTRhMzYtOTEyMC1jYjc5Yzc5YTcwODMiLCJpZCI6MjY4NTE0LCJpYXQiOjE3MzY3MTg2NzB9.X6fIDdZkrPlD5AGjASkJ-IerCu1BLe8IIQLrwJku4LQ";

const viewer = new Cesium.Viewer("cesiumContainer", {
  terrain: Cesium.Terrain.fromWorldTerrain(),
  baseLayerPicker: true,
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
  destination: Cesium.Cartesian3.fromDegrees(145.4556, -14.637, 1200),
  orientation: {
    heading: Cesium.Math.toRadians(180),
    pitch: Cesium.Math.toRadians(-45),  // -90 = straight down, 0 = horizontal
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
  paths: null,
  seascapes: null,
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


const title = document.createElement("div");
title.style.fontWeight = "bold";
title.style.marginBottom = "6px";
title.textContent = "Simulated Larval Reseeding (Lizard Island 2022)";
layerToggleContainer.appendChild(title);

const subtitle = document.createElement("div");
subtitle.style.marginBottom = "8px";
subtitle.style.fontSize = "12px";
subtitle.style.whiteSpace = "normal";
subtitle.style.wordWrap = "break-word";
subtitle.style.width = "280px";
subtitle.innerHTML = `
  - RRAP Moving Corals project <br> 
  - CONNIE particle track input <br>
  - 10am simulated release 16th December 2022 <br>
  - 10k particles tracked for a 12hr period <br><br>
  visualised via coralseed and CesiumJS <br>
   <a href="https://marine-ecologist.github.com/coralseed/" target="_blank">
    marine-ecologist.github.com/coralseed/
  </a> <br><br>
  <b> Select layers </b>
  <br>
`;
layerToggleContainer.appendChild(subtitle);



["Settlers", "Paths", "Seascape"].forEach(layer => {
  const label = document.createElement("label");
  label.style.display = "block";
  label.style.marginBottom = "4px";

  const checkbox = document.createElement("input");
  checkbox.type = "checkbox";
  const checked = layer !== "Seascape";
  checkbox.checked = checked;
  checkbox.style.marginRight = "5px";
  checkbox.dataset.layer = layer;

  const key = layer.toLowerCase();
  checkbox.addEventListener("change", () => {
    const target = layerControls[key];
    if (target) target.show = checkbox.checked;
  });

  // apply initial visibility
  if (layerControls[key]) layerControls[key].show = checked;

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
Cesium.GeoJsonDataSource.load("geojson/lizard_seascape.geojson").then(seascapes => {
  layerControls.seascapes = seascapes;
  seascapes.entities.values.forEach(entity => {
    const className = entity.properties?.class?.getValue();
    const color = seascapeColors[className] || Cesium.Color.LIGHTGRAY.withAlpha(0.05);
    if (entity.polygon) {
      entity.polygon.material = color;
      entity.polygon.outline = true;
      entity.polygon.outlineColor = Cesium.Color.BLACK;
      entity.polygon.outlineWidth = 0.5;
      entity.polygon.height = undefined;
      entity.polygon.extrudedHeight = undefined;
      entity.polygon.heightReference = Cesium.HeightReference.CLAMP_TO_GROUND;
      entity.polygon.zIndex = 0;
    }
  });
  return viewer.dataSources.add(seascapes).then(() => {
    seascapes.show = false;
  });
}).then(() => {

  // 2. Larval tracks
  return Cesium.GeoJsonDataSource.load("geojson/lizard_particles_linestrings.geojson", { clampToGround: true }).then(paths => {
    layerControls.paths = paths;

    const entities = paths.entities.values;
    const valid = entities.map(e => {
      const timeStr = e.properties?.time?.getValue();
      //const t = timeStr ? new Date(timeStr) : null;
      const t = timeStr ? new Date(new Date(timeStr).getTime() + 10 * 3600000) : null;
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
        width: 1,
        material: Cesium.Color.WHITE.withAlpha(0.3),
        clampToGround: true
      });
    });

    return viewer.dataSources.add(paths).then(() => {
      paths.show = true;
    });
  });

}).then(() => {

  // 3. Settlers
  return Cesium.GeoJsonDataSource.load("geojson/lizard_settlers_points.geojson", { clampToGround: true }).then(settlers => {
    layerControls.settlers = settlers;
    const entities = settlers.entities.values;
    const valid = entities.map(e => {
      const timeStr = e.properties?.time?.getValue();
      const val = e.properties?.dispersaltime?.getValue();
      //const t = timeStr ? new Date(timeStr) : null;
      const t = timeStr ? new Date(new Date(timeStr).getTime() + 10 * 3600000) : null;
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
        pixelSize: 6,
        color: color.withAlpha(0.9),
        outlineColor: Cesium.Color.BLACK,
        outlineWidth: 0.5,
        heightReference: Cesium.HeightReference.CLAMP_TO_GROUND,
        zIndex: 2
      });
    });

    return viewer.dataSources.add(settlers).then(() => {
      settlers.show = true;

      const simStart = Cesium.JulianDate.fromDate(new Date(globalMinDate));
      const simEnd = Cesium.JulianDate.fromDate(new Date(globalMaxDate + 3600000));
      viewer.clock.startTime = simStart.clone();
      viewer.clock.stopTime = simEnd.clone();
      viewer.clock.currentTime = simStart.clone();
      viewer.clock.clockRange = Cesium.ClockRange.LOOP_STOP;
      viewer.clock.multiplier = 3000;
      viewer.clock.shouldAnimate = true;
      viewer.timeline.zoomTo(simStart, simEnd);
    });
  });

});



// suppress console logs for zindex warning
	const originalWarn = console.warn;
	console.warn = function (msg, ...args) {
	  if (msg && msg.toString().includes("Entity geometry outlines are unsupported on terrain")) return;
	  if (msg && msg.toString().includes("with heightReference must also have a defined height")) return;
	  originalWarn.call(console, msg, ...args);
	};

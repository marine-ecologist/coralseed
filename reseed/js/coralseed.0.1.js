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



// Define color map for classes
const classColors = {
  "Plateau": Cesium.Color.fromCssColorString("cornsilk").withAlpha(0.8),
  "Back Reef Slope": Cesium.Color.DARKCYAN.withAlpha(0.8),
  "Reef Slope": Cesium.Color.fromCssColorString("darkseagreen").withAlpha(0.8),
  "Sheltered Reef Slope": Cesium.Color.fromCssColorString("darkslategrey").withAlpha(0.8),
  "Inner Reef Flat": Cesium.Color.fromCssColorString("darkgoldenrod").withAlpha(0.8),
  "Outer Reef Flat": Cesium.Color.fromCssColorString("goldenrod").withAlpha(0.8),
  "Reef Crest": Cesium.Color.fromCssColorString("coral").withAlpha(0.8),
};

// Load seascape GeoJSON and apply class-based color styling
viewer.dataSources.add(
  Cesium.GeoJsonDataSource.load("geojson/lizard_seascape.geojson").then(dataSource => {
    dataSource.entities.values.forEach(entity => {
      const className = entity.properties?.class?.getValue();
      const color = classColors[className] || Cesium.Color.LIGHTGRAY.withAlpha(0.9);

      if (entity.polygon) {
        entity.polygon.material = color;
        entity.polygon.outline = true;
        entity.polygon.outlineColor = Cesium.Color.BLACK;
        entity.polygon.outlineWidth = 0.5; // 50% thinner than default
        entity.polygon.height = 0;
      }
    });
    return dataSource;
  })
);

const spectralColors = [
  "#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598",
  "#ffffbf", "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142"
].map(c => Cesium.Color.fromCssColorString(c).withAlpha(0.9));

viewer.dataSources.add(
  Cesium.GeoJsonDataSource.load("geojson/Lpoints.geojson", {
    clampToGround: true
  }).then(dataSource => {
    const entities = dataSource.entities.values;

    const valid = entities.map(e => {
      const timeStr = e.properties?.time?.getValue();
      const val = e.properties?.dispersaltime?.getValue();
      const t = timeStr ? new Date(timeStr) : null;
      return t && Number.isFinite(val) ? { entity: e, date: t, val } : null;
    }).filter(Boolean);

    const minDate = new Date(Math.min(...valid.map(d => d.date.getTime())));
    const maxDate = new Date(Math.max(...valid.map(d => d.date.getTime())));
    const minVal = Math.min(...valid.map(d => d.val));
    const maxVal = Math.max(...valid.map(d => d.val));

    const simStart = Cesium.JulianDate.fromDate(minDate);
    const simEnd = Cesium.JulianDate.fromDate(new Date(maxDate.getTime() + 60 * 60 * 1000)); // +1hr

    viewer.clock.startTime = simStart.clone();
    viewer.clock.stopTime = simEnd.clone();
    viewer.clock.currentTime = simStart.clone();
    viewer.clock.clockRange = Cesium.ClockRange.LOOP_STOP;
    viewer.clock.multiplier = 60;
    viewer.clock.shouldAnimate = true;
    viewer.timeline.zoomTo(simStart, simEnd);

    valid.forEach(({ entity, date, val }) => {
      const appear = Cesium.JulianDate.fromDate(date);

      entity.availability = new Cesium.TimeIntervalCollection([
        new Cesium.TimeInterval({ start: appear, stop: simEnd })
      ]);

      const norm = (val - minVal) / (maxVal - minVal);
      const color = spectralColors[Math.floor(norm * (spectralColors.length - 1))];

      entity.description = undefined;

      entity.point = new Cesium.PointGraphics({
        pixelSize: 8,
        color: color,
        outlineColor: Cesium.Color.BLACK,
        outlineWidth: 1.5,
        heightReference: Cesium.HeightReference.CLAMP_TO_GROUND
      });
    });

    return dataSource;
  })
);
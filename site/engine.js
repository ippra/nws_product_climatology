// engine.js - NWS Alert Climatology.
//
// Loads data/manifest.json and the county, office and state outlines, then the
// one product file the reader picks. A product file holds each county's and
// office's alerts and days by year and by month of the year, and the nation's
// by year and month, all counted by 03_build_counts.R. Every number on the
// page - map, tiles, ranking, card, timeline - is a sum over the selected years
// of those same arrays, divided by the number of years.
//
// The 5 km grid is a second set of arrays, one value per land cell per year
// (04_build_grid.R), drawn as raster tiles this file paints itself: each tile
// pixel is projected to the grid's Albers coordinates and takes its cell's
// color, so cell edges stay exact at every zoom.

import * as maplibregl from "./assets/vendor/maplibre-gl-6.10.0/maplibre-gl.mjs";

window.NAC_ENGINE_LOADED = true;

const DEFAULT_PRODUCT = "TO.W";
const CLASSES = 7;
const RANK_SHORT = 12;
const MONTHS = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
const MONTHS_LONG = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
const SIG_SHORT = { W: "Warning", A: "Watch", Y: "Advisory", S: "Statement", all: "Any" };
const SIG_ORDER = { W: 1, A: 2, Y: 3, S: 4, all: 5 };

// Palettes ---------------------------------------------------------------------
// One blue ramp from the IPPRA dataviz palette, seven steps of even OKLab
// lightness (delta L 0.095). On light maps the busiest counties are darkest; on
// dark maps they are lightest, so the extreme always has the most contrast with
// the map beneath it. Counties with none stay unfilled.
const PALETTE = {
  light: {
    ramp: ["#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b"],
    countyLine: "rgba(40,40,40,0.16)",
    officeLine: "rgba(40,40,40,0.45)",
    stateLine: "rgba(30,30,30,0.55)",
    hover: "#0b0b0b",
    selected: "#8a1538",
    paper: "#fafaf8",
    ink: "#1c1b19",
    sub: "#52514e",
  },
  dark: {
    ramp: ["#0d366b", "#184f95", "#256abf", "#3987e5", "#6da7ec", "#9ec5f4", "#cde2fb"],
    countyLine: "rgba(255,255,255,0.12)",
    officeLine: "rgba(255,255,255,0.4)",
    stateLine: "rgba(255,255,255,0.55)",
    hover: "#ffffff",
    selected: "#e0607f",
    paper: "#0e0e0e",
    ink: "#f4f3ef",
    sub: "#c3c2b7",
  },
};

const CARTO = "https://basemaps.cartocdn.com/gl/";
const BASEMAPS = {
  light: { label: "Light", tone: "light", style: CARTO + "positron-gl-style/style.json" },
  dark: { label: "Dark", tone: "dark", style: CARTO + "dark-matter-gl-style/style.json" },
};

const REGIONS = [
  { id: "conus", label: "Lower 48", bounds: [[-125, 24.2], [-66.8, 49.5]] },
  { id: "ak", label: "Alaska", bounds: [[-179.5, 51], [-129.5, 71.5]] },
  { id: "hi", label: "Hawaii", bounds: [[-160.5, 18.8], [-154.7, 22.4]] },
  { id: "pr", label: "Puerto Rico", bounds: [[-67.4, 17.8], [-64.5, 18.6]] },
  { id: "gu", label: "Guam", bounds: [[144.5, 13.1], [146.1, 15.4]] },
  { id: "as", label: "Samoa", bounds: [[-171.2, -14.6], [-168.9, -11]] },
];

// State ------------------------------------------------------------------------
const prefersDark = window.matchMedia && window.matchMedia("(prefers-color-scheme: dark)").matches;

const state = {
  product: DEFAULT_PRODUCT,
  measure: "days",
  y0: 0,
  y1: 0,
  unit: "counties",
  stateFilter: "",
  selected: null, // { unit: "counties" | "offices", index }
  basemap: prefersDark ? "dark" : "light",
  rankAll: false,
};

let manifest = null;
let years = [];
let products = [];
let productById = new Map();
let countyGeo = null;
let officeGeo = null;
let stateGeo = null;
let data = null; // the loaded product file, unpacked
let view = null; // the current computation: values, classes, ranks
const cache = new Map();

const $ = (id) => document.getElementById(id);
const nf = new Intl.NumberFormat("en-US");
const n1 = new Intl.NumberFormat("en-US", { maximumFractionDigits: 1 });

function fmtRate(v) {
  if (v === 0) return "0";
  if (v < 0.1) return v.toFixed(2);
  if (v < 10) return v.toFixed(1);
  return nf.format(Math.round(v));
}

function compact(n) {
  if (n >= 1e6) return (n / 1e6).toFixed(n >= 1e7 ? 0 : 1) + "M";
  if (n >= 1e4) return Math.round(n / 1e3) + "k";
  if (n >= 1e3) return (n / 1e3).toFixed(1) + "k";
  return nf.format(n);
}

const nYears = () => state.y1 - state.y0 + 1;
// The tiles, ranking and card rank counties while the map shows the grid.
const rankUnit = () => (state.unit === "offices" ? "offices" : "counties");
const isGrid = () => state.unit === "grid";
const yearLabel = () => (state.y0 === state.y1 ? String(years[state.y0]) : `${years[state.y0]}–${years[state.y1]}`);
const product = () => productById.get(state.product);
const tone = () => BASEMAPS[state.basemap].tone;
const unitName = (unit, n = 2) => (unit === "grid" ? (n === 1 ? "cell" : "cells") : unit === "counties" ? (n === 1 ? "county" : "counties") : n === 1 ? "office" : "offices");

// Plural label of a product for sentences: "Tornado Warnings", "Flood
// Warnings (areal)", "Tornado Warnings and Tornado Watches".
function pluralOne(label) {
  const m = label.match(/^(.*?)( \(.*\))?$/);
  const base = m[1];
  const word = /(ch|sh|s|x)$/.test(base) ? base + "es"
    : /[^aeiou]y$/.test(base) ? base.slice(0, -1) + "ies"
    : base + "s";
  return word + (m[2] || "");
}

function plural(p) {
  if (p.kind === "significance") return pluralOne(p.label.replace(/^Any /, "")).replace(/^./, (c) => c.toUpperCase());
  if (p.kind === "phenomenon") {
    const parts = p.label.split(/, | or /);
    return parts.map(pluralOne).join(", ").replace(/, ([^,]*)$/, " and $1");
  }
  return pluralOne(p.label);
}

// "a Tornado Warning", "an Ice Storm Warning", "any warning".
function withArticle(p) {
  if (p.kind === "significance") return p.label.toLowerCase();
  return (/^[AEIOU]/.test(p.label) ? "an " : "a ") + p.label;
}

function unitLabel(unit, i) {
  if (unit === "counties") return `${manifest.counties.name[i]}, ${manifest.counties.state[i]}`;
  return `NWS ${manifest.offices.name[i]}`;
}

// Data -------------------------------------------------------------------------
async function fetchJson(url) {
  const r = await fetch(url);
  if (!r.ok) throw new Error(`${url}: HTTP ${r.status}`);
  return r.json();
}

async function loadProduct(id) {
  const p = productById.get(id);
  if (cache.has(id)) return cache.get(id);
  const raw = await fetchJson(`data/${p.file}`);
  const Y = years.length;
  const unpack = (block, size) => {
    const index = new Int32Array(size).fill(-1);
    block.i.forEach((u, row) => { index[u] = row; });
    return {
      index,
      rows: block.i,
      ay: Int32Array.from(block.ay),
      dy: Int32Array.from(block.dy),
      am: Int32Array.from(block.am),
      dm: Int32Array.from(block.dm),
    };
  };
  const out = {
    id,
    national: { a: Int32Array.from(raw.national.a), d: Int32Array.from(raw.national.d) },
    counties: unpack(raw.counties, manifest.counties.geoid.length),
    offices: unpack(raw.offices, manifest.offices.wfo.length),
    Y,
  };
  cache.set(id, out);
  return out;
}

// Sum of a unit's by-year array over the selected years.
function unitSum(block, row, measure, y0 = state.y0, y1 = state.y1) {
  const arr = measure === "days" ? block.dy : block.ay;
  const Y = years.length;
  let s = 0;
  for (let y = y0; y <= y1; y++) s += arr[row * Y + y];
  return s;
}

// Nice class breaks: septiles of the units with any, rounded to 1-2-5 steps so
// the legend reads cleanly, with duplicates dropped.
function niceRound(v) {
  if (v <= 0) return 0;
  const mag = Math.pow(10, Math.floor(Math.log10(v)));
  const steps = [1, 1.5, 2, 2.5, 3, 4, 5, 6, 7.5, 8, 10];
  const f = v / mag;
  const s = steps.find((x) => x >= f - 1e-9) || 10;
  return +(s * mag).toPrecision(3);
}

function breaksFor(values) {
  return breaksFromSorted(Float64Array.from(values).filter((v) => v > 0).sort());
}

function breaksFromSorted(nz) {
  if (!nz.length) return [];
  const out = [];
  for (let k = 1; k < CLASSES; k++) {
    const q = nz[Math.min(nz.length - 1, Math.floor((k / CLASSES) * nz.length))];
    const b = niceRound(q);
    if (b > (out[out.length - 1] ?? 0) && b <= nz[nz.length - 1]) out.push(b);
  }
  return out;
}

function classOf(v, breaks) {
  if (!(v > 0)) return 0;
  let c = 1;
  for (const b of breaks) if (v >= b) c++;
  return c;
}

function compute() {
  if (!data) return;
  const p = product();
  const n = nYears();
  const unit = rankUnit();
  const block = data[unit];
  const size = unit === "counties" ? manifest.counties.geoid.length : manifest.offices.wfo.length;
  const values = new Float64Array(size);
  const totals = new Float64Array(size);
  const other = new Float64Array(size);
  const otherMeasure = state.measure === "days" ? "alerts" : "days";
  block.rows.forEach((u, row) => {
    const t = unitSum(block, row, state.measure);
    totals[u] = t;
    values[u] = t / n;
    other[u] = unitSum(block, row, otherMeasure) / n;
  });

  const inFilter = (u) => unit !== "counties" || !state.stateFilter || manifest.counties.state[u] === state.stateFilter;
  // Classes follow what is on the map: with a state chosen, its own counties
  // set the breaks, so the state is not one flat color from a national scale.
  const breaks = breaksFor(Array.from(values).filter((_, u) => inFilter(u)));
  const classes = new Uint8Array(size);
  for (let u = 0; u < size; u++) classes[u] = classOf(values[u], breaks);

  // Ranks are national (or within the chosen state for the list); ties share
  // the better rank.
  const order = [...Array(size).keys()].filter((u) => values[u] > 0).sort((a, b) => values[b] - values[a]);
  const rank = new Int32Array(size).fill(0);
  order.forEach((u, k) => { rank[u] = k > 0 && values[order[k - 1]] === values[u] ? rank[order[k - 1]] : k + 1; });

  // National series for the selected years, from the product file.
  const Y = years.length;
  const nat = data.national;
  let natAlerts = 0;
  const perYear = [];
  const perMonth = new Array(12).fill(0);
  for (let y = state.y0; y <= state.y1; y++) {
    let ya = 0;
    for (let m = 0; m < 12; m++) {
      ya += nat.a[y * 12 + m];
      perMonth[m] += nat.a[y * 12 + m];
    }
    perYear.push(ya);
    natAlerts += ya;
  }

  view = { p, unit, size, values, totals, other, breaks, classes, order, rank, inFilter, natAlerts, perYear, perMonth, reached: order.length, n, Y };
}


// Grid -------------------------------------------------------------------------
// Cells are 5 km squares on an Albers equal-area projection per region
// (04_build_grid.R). The browser needs the forward projection to find the cell
// under a pixel and the inverse to draw a cell's outline; both are Snyder's
// ellipsoidal formulas on GRS80, the ellipsoid of NAD83.
const GRS80_A = 6378137;
const GRS80_F = 1 / 298.257222101;
const E2 = GRS80_F * (2 - GRS80_F);
const ECC = Math.sqrt(E2);
const RAD = Math.PI / 180;

const grid = {
  ready: false,
  loading: null,
  domains: [],
  county: null, // cell -> county index
  cellState: null, // cell -> state index
  cellDomain: null, // cell -> domain index
  cellSlot: null, // cell -> slot within its domain
  arrays: new Map(), // "TO.W:days" -> values, cells x years
  values: null, // average per year over the selected years
  classes: null,
  colors: [],
  breaks: [],
  sorted: null,
  reached: 0,
  stateK: -1,
  version: 0,
  hover: null,
};

function albersQ(sinPhi) {
  const es = ECC * sinPhi;
  return (1 - E2) * (sinPhi / (1 - es * es) - (1 / (2 * ECC)) * Math.log((1 - es) / (1 + es)));
}

function albersM(phi) {
  const s = Math.sin(phi);
  return Math.cos(phi) / Math.sqrt(1 - E2 * s * s);
}

const wrapLon = (lon) => ((((lon + 180) % 360) + 360) % 360) - 180;

function makeDomain(d) {
  const m1 = albersM(d.lat_1 * RAD), m2 = albersM(d.lat_2 * RAD);
  const q1 = albersQ(Math.sin(d.lat_1 * RAD)), q2 = albersQ(Math.sin(d.lat_2 * RAD));
  const n = (m1 * m1 - m2 * m2) / (q2 - q1);
  const C = m1 * m1 + n * q1;
  const rho0 = (GRS80_A * Math.sqrt(C - n * albersQ(Math.sin(d.lat_0 * RAD)))) / n;
  return { ...d, n, C, rho0 };
}

function albersRho(dm, lat) {
  return (GRS80_A * Math.sqrt(dm.C - dm.n * albersQ(Math.sin(lat * RAD)))) / dm.n;
}

function project(dm, lon, lat) {
  const rho = albersRho(dm, lat);
  const theta = dm.n * wrapLon(lon - dm.lon_0) * RAD;
  return [rho * Math.sin(theta), dm.rho0 - rho * Math.cos(theta)];
}

function unproject(dm, x, y) {
  const n = dm.n;
  const dy = dm.rho0 - y;
  const rho = Math.sign(n) * Math.hypot(x, dy);
  const theta = n < 0 ? Math.atan2(-x, -dy) : Math.atan2(x, dy);
  const q = (dm.C - (rho * rho * n * n) / (GRS80_A * GRS80_A)) / n;
  let phi = Math.asin(Math.max(-1, Math.min(1, q / 2)));
  for (let i = 0; i < 15; i++) {
    const s = Math.sin(phi), es = ECC * s, d1 = 1 - es * es;
    const step = ((d1 * d1) / (2 * Math.cos(phi))) *
      (q / (1 - E2) - s / d1 + (1 / (2 * ECC)) * Math.log((1 - es) / (1 + es)));
    phi += step;
    if (Math.abs(step) < 1e-12) break;
  }
  return [wrapLon(dm.lon_0 + theta / n / RAD), phi / RAD];
}

// Gzipped binaries are inflated here, unless the host already decoded them.
async function fetchBytes(url) {
  const r = await fetch(url);
  if (!r.ok) throw new Error(`${url}: HTTP ${r.status}`);
  const buf = new Uint8Array(await r.arrayBuffer());
  if (buf[0] !== 0x1f || buf[1] !== 0x8b) return buf;
  const stream = new Blob([buf]).stream().pipeThrough(new DecompressionStream("gzip"));
  return new Uint8Array(await new Response(stream).arrayBuffer());
}

function loadGridBase() {
  if (grid.loading) return grid.loading;
  grid.loading = (async () => {
    const g = manifest.grid;
    const [mask, county] = await Promise.all([
      fetchBytes(`data/grid/${g.mask}?v=${manifest.build}`),
      fetchBytes(`data/grid/${g.county}?v=${manifest.build}`),
    ]);
    const cells = g.cells;
    grid.county = new Uint16Array(county.buffer, county.byteOffset, county.byteLength / 2);
    if (grid.county.length !== cells) throw new Error("grid county file has the wrong length");
    grid.cellDomain = new Uint8Array(cells);
    grid.cellSlot = new Int32Array(cells);
    let byte = 0;
    grid.domains = g.domains.map((d, k) => {
      const dm = makeDomain(d);
      const slots = d.nx * d.ny;
      const index = new Int32Array(slots);
      let cell = d.first_cell;
      for (let slot = 0; slot < slots; slot++) {
        if ((mask[byte + (slot >> 3)] >> (slot & 7)) & 1) {
          index[slot] = cell;
          grid.cellDomain[cell] = k;
          grid.cellSlot[cell] = slot;
          cell++;
        } else index[slot] = -1;
      }
      byte += Math.ceil(slots / 8);
      if (cell !== d.first_cell + d.cells) throw new Error(`grid mask disagrees with grid.json for ${d.domain}`);
      dm.index = index;
      // Longitude-latitude bounds, from points around the domain's edge, so a
      // tile or a pointer outside it is skipped without projecting.
      let w = 180, e = -180, south = 90, north = -90;
      const W = d.nx * g.cell_m, H = d.ny * g.cell_m;
      for (let t = 0; t <= 40; t++) {
        for (const [x, y] of [[d.x0 + (W * t) / 40, d.y_top], [d.x0 + (W * t) / 40, d.y_top - H], [d.x0, d.y_top - (H * t) / 40], [d.x0 + W, d.y_top - (H * t) / 40]]) {
          const [lon, lat] = unproject(dm, x, y);
          w = Math.min(w, lon); e = Math.max(e, lon); south = Math.min(south, lat); north = Math.max(north, lat);
        }
      }
      dm.box = { w: w - 1, e: e + 1, s: south - 1, n: north + 1, wraps: e - w > 180 };
      return dm;
    });
    const stateIndex = new Map(manifest.states.abbr.map((a, k) => [a, k]));
    const countyState = Uint8Array.from(manifest.counties.state, (st) => stateIndex.get(st));
    grid.cellState = Uint8Array.from(grid.county, (c) => countyState[c]);
    grid.ready = true;
  })();
  grid.loading.catch(() => { grid.loading = null; });
  return grid.loading;
}

async function loadGridArray(id, measure) {
  const key = `${id}:${measure}`;
  if (grid.arrays.has(key)) return grid.arrays.get(key);
  const f = productById.get(id).grid[measure];
  const bytes = await fetchBytes(`data/grid/${f.file}`);
  const arr = f.bytes === 1 ? bytes : new Uint16Array(bytes.buffer, bytes.byteOffset, bytes.byteLength / 2);
  if (arr.length !== manifest.grid.cells * years.length) throw new Error(`${f.file} has the wrong length`);
  grid.arrays.set(key, arr);
  // A dense product is several MB unpacked; keep only the last few.
  while (grid.arrays.size > 6) grid.arrays.delete(grid.arrays.keys().next().value);
  return arr;
}

let gridToken = 0;
async function refreshGrid() {
  if (!isGrid() || !manifest.grid) return;
  const p = product();
  if (!hasGrid(p)) return;
  const my = ++gridToken;
  const key = `${p.id}:${state.measure}`;
  const pending = !grid.ready || !grid.arrays.has(key);
  if (pending) {
    $("busy-text").textContent = `the 5 km grid for ${p.label}`;
    $("busy").hidden = false;
  }
  try {
    await loadGridBase();
    await loadGridArray(p.id, state.measure);
  } catch (e) {
    toast("Could not load the grid: " + e.message, 8000);
    return;
  } finally {
    if (my === gridToken && pending) $("busy").hidden = true;
  }
  if (my !== gridToken || !isGrid()) return;
  renderAll();
}

function computeGrid() {
  grid.values = null;
  if (!isGrid() || !grid.ready) return;
  const p = product();
  const arr = hasGrid(p) && grid.arrays.get(`${p.id}:${state.measure}`);
  if (!arr) return;
  const N = manifest.grid.cells;
  const values = new Float32Array(N);
  for (let y = state.y0; y <= state.y1; y++) {
    const off = y * N;
    for (let c = 0; c < N; c++) values[c] += arr[off + c];
  }
  const n = nYears();
  const stateK = state.stateFilter ? manifest.states.abbr.indexOf(state.stateFilter) : -1;
  let count = 0;
  for (let c = 0; c < N; c++) {
    values[c] /= n;
    if (values[c] > 0 && (stateK < 0 || grid.cellState[c] === stateK)) count++;
  }
  const sorted = new Float32Array(count);
  for (let c = 0, k = 0; c < N; c++) {
    if (values[c] > 0 && (stateK < 0 || grid.cellState[c] === stateK)) sorted[k++] = values[c];
  }
  sorted.sort();
  const breaks = breaksFromSorted(sorted);
  const classes = new Uint8Array(N);
  for (let c = 0; c < N; c++) {
    if (stateK >= 0 && grid.cellState[c] !== stateK) continue;
    classes[c] = classOf(values[c], breaks);
  }
  Object.assign(grid, { values, classes, breaks, sorted, reached: count, stateK });
}

const hasGrid = (p) => !!(manifest.grid && p.grid && p.grid.days && p.grid.alerts);

const gridInFilter = (cell) => grid.stateK < 0 || grid.cellState[cell] === grid.stateK;

function gridPercentile(v) {
  const s = grid.sorted;
  let lo = 0, hi = s.length;
  while (lo < hi) {
    const mid = (lo + hi) >> 1;
    if (s[mid] < v) lo = mid + 1; else hi = mid;
  }
  return Math.floor((lo / Math.max(1, s.length)) * 100);
}

function inBox(box, lon, lat) {
  return lat >= box.s && lat <= box.n && (box.wraps || (lon >= box.w && lon <= box.e));
}

function cellAt(lon, lat) {
  lon = wrapLon(lon);
  const cellM = manifest.grid.cell_m;
  for (const d of grid.domains) {
    if (!inBox(d.box, lon, lat)) continue;
    const [x, y] = project(d, lon, lat);
    const col = Math.floor((x - d.x0) / cellM), row = Math.floor((d.y_top - y) / cellM);
    if (col >= 0 && col < d.nx && row >= 0 && row < d.ny) return d.index[row * d.nx + col];
  }
  return -1;
}

function cellCorners(cell) {
  const d = grid.domains[grid.cellDomain[cell]];
  const cellM = manifest.grid.cell_m;
  const slot = grid.cellSlot[cell];
  const row = Math.floor(slot / d.nx), col = slot % d.nx;
  const x0 = d.x0 + col * cellM, y0 = d.y_top - row * cellM;
  return [[x0, y0], [x0 + cellM, y0], [x0 + cellM, y0 - cellM], [x0, y0 - cellM], [x0, y0]].map(([x, y]) => unproject(d, x, y));
}

function cellCentre(cell) {
  const d = grid.domains[grid.cellDomain[cell]];
  const cellM = manifest.grid.cell_m;
  const slot = grid.cellSlot[cell];
  return unproject(d, d.x0 + ((slot % d.nx) + 0.5) * cellM, d.y_top - (Math.floor(slot / d.nx) + 0.5) * cellM);
}

function renderGridMarks() {
  const src = overlaysReady && map.getSource("nac-grid-marks");
  if (!src) return;
  const features = [];
  if (isGrid() && grid.ready) {
    const add = (cell, kind) => features.push({ type: "Feature", properties: { kind }, geometry: { type: "LineString", coordinates: cellCorners(cell) } });
    if (grid.hover != null) add(grid.hover, "hover");
    if (state.selected && state.selected.unit === "grid") add(state.selected.index, "selected");
  }
  src.setData({ type: "FeatureCollection", features });
}

const hexRgb = (h) => [1, 3, 5].map((i) => parseInt(h.slice(i, i + 2), 16));

function repaintGrid() {
  if (!overlaysReady || !isGrid() || !grid.values) return;
  const ramp = PALETTE[tone()].ramp;
  const k = grid.breaks.length + 1;
  grid.colors = [null];
  for (let c = 1; c <= k; c++) grid.colors.push(hexRgb(ramp[Math.round(((c - 1) / Math.max(1, k - 1)) * (ramp.length - 1))]));
  grid.version++;
  const src = map.getSource("nac-grid");
  if (src) src.setTiles([gridTileUrl()]);
}

const gridTileUrl = () => `nacgrid://{z}/{x}/{y}?v=${grid.version}`;

// Each pixel centre is projected into the grid and takes its cell's class.
function paintTile(px, z, tx, ty) {
  const size = 256, scale = 2 ** z;
  const cellM = manifest.grid.cell_m;
  const lons = new Float64Array(size), lats = new Float64Array(size);
  for (let i = 0; i < size; i++) {
    lons[i] = ((tx + (i + 0.5) / size) / scale) * 360 - 180;
    lats[i] = Math.atan(Math.sinh(Math.PI * (1 - (2 * (ty + (i + 0.5) / size)) / scale))) / RAD;
  }
  const west = lons[0], east = lons[size - 1], north = lats[0], south = lats[size - 1];
  const doms = grid.domains.filter((d) => north >= d.box.s && south <= d.box.n && (d.box.wraps || (east >= d.box.w && west <= d.box.e)));
  if (!doms.length) return false;
  const values = grid.classes, colors = grid.colors;
  const rhos = new Float64Array(doms.length);
  let painted = false;
  for (let j = 0; j < size; j++) {
    for (let k = 0; k < doms.length; k++) rhos[k] = albersRho(doms[k], lats[j]);
    for (let i = 0; i < size; i++) {
      for (let k = 0; k < doms.length; k++) {
        const d = doms[k];
        const theta = d.n * wrapLon(lons[i] - d.lon_0) * RAD;
        const x = rhos[k] * Math.sin(theta), y = d.rho0 - rhos[k] * Math.cos(theta);
        const col = Math.floor((x - d.x0) / cellM), row = Math.floor((d.y_top - y) / cellM);
        if (col < 0 || col >= d.nx || row < 0 || row >= d.ny) continue;
        const cell = d.index[row * d.nx + col];
        if (cell >= 0 && values[cell]) {
          const rgb = colors[values[cell]], o = (j * size + i) * 4;
          px[o] = rgb[0]; px[o + 1] = rgb[1]; px[o + 2] = rgb[2]; px[o + 3] = 255;
          painted = true;
        }
        break;
      }
    }
  }
  return painted;
}

function tileCanvas() {
  if (typeof OffscreenCanvas !== "undefined") return new OffscreenCanvas(256, 256);
  const c = document.createElement("canvas");
  c.width = c.height = 256;
  return c;
}

const canvasPng = (c) => (c.convertToBlob ? c.convertToBlob({ type: "image/png" }) : new Promise((res) => c.toBlob(res, "image/png")));

let blankTile = null;
maplibregl.addProtocol("nacgrid", async (params) => {
  const m = /nacgrid:\/\/(\d+)\/(\d+)\/(\d+)/.exec(params.url);
  if (m && isGrid() && grid.ready && grid.classes) {
    const canvas = tileCanvas();
    const ctx = canvas.getContext("2d");
    const img = ctx.createImageData(256, 256);
    if (paintTile(img.data, Number(m[1]), Number(m[2]), Number(m[3]))) {
      ctx.putImageData(img, 0, 0);
      return { data: await (await canvasPng(canvas)).arrayBuffer() };
    }
  }
  blankTile ??= (async () => (await canvasPng(tileCanvas())).arrayBuffer())();
  return { data: (await blankTile).slice(0) };
});

function renderGridCard() {
  const cell = state.selected.index;
  if (!grid.ready || !grid.values) { $("card").hidden = true; return; }
  const p = product();
  const N = manifest.grid.cells;
  const county = grid.county[cell];
  const st = manifest.counties.state[county];
  const [lon, lat] = cellCentre(cell);
  const v = grid.values[cell];
  $("card").hidden = false;
  $("card-kicker").textContent = "5 km grid cell";
  $("card-title").textContent = `${manifest.counties.name[county]}, ${st}`;
  $("card-sub").textContent = `Centred at ${Math.abs(lat).toFixed(2)}°${lat >= 0 ? "N" : "S"}, ${Math.abs(lon).toFixed(2)}°${lon >= 0 ? "E" : "W"} · cell ${cell}`;
  $("card-value").textContent = fmtRate(v);
  $("card-unit").textContent = state.measure === "days"
    ? `days per year with ${withArticle(p)} in effect, ${yearLabel()}`
    : `${plural(p)} per year, ${yearLabel()}`;

  // The other measure, loaded on demand.
  const otherMeasure = state.measure === "days" ? "alerts" : "days";
  const other = grid.arrays.get(`${p.id}:${otherMeasure}`);
  if (other) {
    let sum = 0;
    for (let y = state.y0; y <= state.y1; y++) sum += other[y * N + cell];
    const o = sum / nYears();
    $("card-other").textContent = otherMeasure === "alerts" ? `${fmtRate(o)} ${plural(p)} issued per year` : `${fmtRate(o)} days per year in effect`;
  } else {
    $("card-other").textContent = "";
    loadGridArray(p.id, otherMeasure).then(() => {
      if (state.selected && state.selected.unit === "grid" && state.selected.index === cell) renderCard();
    }).catch(() => {});
  }

  const where = grid.stateK >= 0 ? ` in ${state.stateFilter}` : "";
  $("card-rank").textContent = v > 0
    ? `Higher than ${gridPercentile(v)}% of the ${nf.format(grid.reached)} cells with any${where}`
    : `No ${plural(p)} ${state.measure === "days" ? "in effect" : "issued"} in this cell in ${yearLabel()}.`;

  const arr = grid.arrays.get(`${p.id}:${state.measure}`);
  const word = state.measure === "days" ? "days" : "alerts";
  $("card-years-title").textContent = state.measure === "days" ? "Days in effect, by year" : "Alerts issued, by year";
  const maxY = barChart($("card-years"), years.map((_, y) => arr[y * N + cell]), {
    labels: years.map(String),
    active: (k) => k >= state.y0 && k <= state.y1,
    tipLabel: (k) => years[k],
    format: (x) => `${nf.format(x)} ${word}`,
  });
  $("card-years-max").textContent = `max ${nf.format(maxY)}`;
  $("card-foot").textContent = "Storm-based warnings count when their polygon covered this cell's centre; other alerts cover the cells of every zone and county they name. Days are local calendar days.";
}

function downloadGridCsv() {
  const p = product();
  const arr = grid.arrays.get(`${p.id}:${state.measure}`);
  if (!arr || !grid.values) { toast("The grid is still loading"); return; }
  const N = manifest.grid.cells;
  const sel = [];
  for (let y = state.y0; y <= state.y1; y++) sel.push(y);
  const m = state.measure;
  const head = ["cell", "domain", "lon", "lat", "geoid", "county", "state", "product", "years", `${m}_per_year`, ...sel.map((y) => `${m}_${years[y]}`)];
  const parts = [head.join(",") + "\n"];
  let buf = [];
  const span = yearLabel().replace("–", "-");
  for (let c = 0; c < N; c++) {
    if (!(grid.values[c] > 0) || !gridInFilter(c)) continue;
    const [lon, lat] = cellCentre(c);
    const county = grid.county[c];
    buf.push([
      c, grid.domains[grid.cellDomain[c]].domain, lon.toFixed(4), lat.toFixed(4),
      manifest.counties.geoid[county], csvCell(manifest.counties.name[county]), manifest.counties.state[county],
      p.id, span, grid.values[c].toFixed(3), ...sel.map((y) => arr[y * N + c]),
    ].join(","));
    if (buf.length === 5000) { parts.push(buf.join("\n") + "\n"); buf = []; }
  }
  if (buf.length) parts.push(buf.join("\n") + "\n");
  saveBlob(new Blob(parts, { type: "text/csv" }), `nws_${p.id.replace(".", "_")}_grid5km_${m}_${span}${state.stateFilter ? "_" + state.stateFilter : ""}.csv`);
  toast("Cells with none are left out of the CSV", 4000);
}

// Map --------------------------------------------------------------------------
const initialBasemap = state.basemap;
const map = new maplibregl.Map({
  container: "map",
  style: BASEMAPS[initialBasemap].style,
  bounds: REGIONS[0].bounds,
  fitBoundsOptions: { padding: { top: 60, bottom: 170, left: 20, right: 20 } },
  minZoom: 1.5,
  maxZoom: 11,
  attributionControl: { compact: true },
  canvasContextAttributes: { preserveDrawingBuffer: true },
  dragRotate: false,
  pitchWithRotate: false,
});
map.touchZoomRotate.disableRotation();
map.addControl(new maplibregl.NavigationControl({ showCompass: false }), "top-right");
map.addControl(new maplibregl.FullscreenControl({ container: $("stage") }), "top-right");

function fillColor(P) {
  return ["match", ["coalesce", ["feature-state", "c"], 0], 1, P.ramp[0], 2, P.ramp[1], 3, P.ramp[2], 4, P.ramp[3], 5, P.ramp[4], 6, P.ramp[5], 7, P.ramp[6], "rgba(0,0,0,0)"];
}

function addOverlays() {
  if (map.getSource("nac-counties")) return;
  const P = PALETTE[tone()];
  const layers = map.getStyle().layers;
  const firstSymbol = layers.find((l) => l.type === "symbol");
  const before = firstSymbol ? firstSymbol.id : undefined;

  map.addSource("nac-counties", { type: "geojson", data: countyGeo, promoteId: "i" });
  map.addSource("nac-offices", { type: "geojson", data: officeGeo, promoteId: "i" });
  map.addSource("nac-states", { type: "geojson", data: stateGeo });

  map.addLayer({ id: "county-fill", type: "fill", source: "nac-counties", paint: { "fill-color": fillColor(P), "fill-opacity": 0.9 } }, before);
  map.addSource("nac-grid", { type: "raster", tiles: [gridTileUrl()], tileSize: 256, maxzoom: 14 });
  map.addLayer({ id: "grid-cells", type: "raster", source: "nac-grid", layout: { visibility: "none" }, paint: { "raster-opacity": 0.92, "raster-resampling": "nearest", "raster-fade-duration": 0 } }, before);
  map.addLayer({ id: "county-line", type: "line", source: "nac-counties", minzoom: 3.2, paint: { "line-color": P.countyLine, "line-width": ["interpolate", ["linear"], ["zoom"], 3.2, 0.2, 7, 0.8] } }, before);
  map.addLayer({ id: "office-fill", type: "fill", source: "nac-offices", paint: { "fill-color": fillColor(P), "fill-opacity": 0.9 } }, before);
  map.addLayer({ id: "office-line", type: "line", source: "nac-offices", paint: { "line-color": P.officeLine, "line-width": 0.9 } }, before);
  map.addLayer({ id: "state-line", type: "line", source: "nac-states", paint: { "line-color": P.stateLine, "line-width": ["interpolate", ["linear"], ["zoom"], 3, 0.6, 7, 1.6] } }, before);
  map.addLayer({ id: "county-hover", type: "line", source: "nac-counties", paint: { "line-color": P.hover, "line-width": 1.6, "line-opacity": ["case", ["boolean", ["feature-state", "hover"], false], 1, 0] } });
  map.addLayer({ id: "office-hover", type: "line", source: "nac-offices", paint: { "line-color": P.hover, "line-width": 1.6, "line-opacity": ["case", ["boolean", ["feature-state", "hover"], false], 1, 0] } });
  map.addLayer({ id: "county-selected", type: "line", source: "nac-counties", filter: ["==", ["get", "i"], -1], paint: { "line-color": P.selected, "line-width": 2.6 } });
  map.addLayer({ id: "office-selected", type: "line", source: "nac-offices", filter: ["==", ["get", "i"], -1], paint: { "line-color": P.selected, "line-width": 2.6 } });
  map.addSource("nac-grid-marks", { type: "geojson", data: { type: "FeatureCollection", features: [] } });
  map.addLayer({ id: "grid-marks", type: "line", source: "nac-grid-marks", layout: { visibility: "none" }, paint: { "line-color": ["match", ["get", "kind"], "selected", P.selected, P.hover], "line-width": ["match", ["get", "kind"], "selected", 2.4, 1.4] } });
  overlaysReady = true;
  applyClassColors();
  renderMap();
  // The key's colors follow the base map's tone.
  renderLegend();
}

let overlaysReady = false;
let hovered = null;

function renderMap() {
  if (!overlaysReady || !view) return;
  const counties = state.unit === "counties";
  const offices = state.unit === "offices";
  const gridOn = isGrid();
  const vis = (on) => (on ? "visible" : "none");
  map.setLayoutProperty("county-fill", "visibility", vis(counties));
  map.setLayoutProperty("county-hover", "visibility", vis(counties));
  map.setLayoutProperty("county-selected", "visibility", vis(!offices));
  map.setLayoutProperty("office-fill", "visibility", vis(offices));
  map.setLayoutProperty("office-hover", "visibility", vis(offices));
  map.setLayoutProperty("office-selected", "visibility", vis(offices));
  map.setLayoutProperty("county-line", "visibility", vis(!offices));
  map.setLayoutProperty("grid-cells", "visibility", vis(gridOn));
  map.setLayoutProperty("grid-marks", "visibility", vis(gridOn));

  if (!gridOn) {
    const source = counties ? "nac-counties" : "nac-offices";
    for (let u = 0; u < view.size; u++) {
      const c = view.inFilter(u) ? view.classes[u] : 0;
      map.setFeatureState({ source, id: u }, { c });
    }
  }
  const sel = state.selected;
  map.setFilter("county-selected", ["==", ["get", "i"], sel && sel.unit === "counties" ? sel.index : -1]);
  map.setFilter("office-selected", ["==", ["get", "i"], sel && sel.unit === "offices" ? sel.index : -1]);
  renderGridMarks();
}

function setHover(unit, id) {
  const source = unit === "counties" ? "nac-counties" : "nac-offices";
  if (hovered && (hovered.id !== id || hovered.source !== source)) {
    map.setFeatureState({ source: hovered.source, id: hovered.id }, { hover: false });
    hovered = null;
  }
  if (id != null) {
    map.setFeatureState({ source, id }, { hover: true });
    hovered = { source, id };
  }
}

function clearHover() {
  if (hovered && map.getSource(hovered.source)) map.setFeatureState({ source: hovered.source, id: hovered.id }, { hover: false });
  hovered = null;
  $("hover-tip").hidden = true;
  map.getCanvas().style.cursor = "";
}

function measurePhrase(v, measure = state.measure, short = false) {
  const p = product();
  if (measure === "days") return short ? `${fmtRate(v)} days/yr` : `${fmtRate(v)} days per year in effect`;
  return short ? `${fmtRate(v)}/yr` : `${fmtRate(v)} issued per year`;
}

for (const layer of ["county-fill", "office-fill"]) {
  map.on("mousemove", layer, (e) => {
    const f = e.features && e.features[0];
    if (!f || !view) return;
    const unit = layer === "county-fill" ? "counties" : "offices";
    if (unit !== state.unit) return;
    const u = f.id;
    setHover(unit, u);
    map.getCanvas().style.cursor = "pointer";
    const tip = $("hover-tip");
    const v = view.values[u];
    tip.innerHTML = "";
    const name = document.createElement("div");
    name.className = "tip-name";
    name.textContent = unitLabel(unit, u);
    const meta = document.createElement("div");
    meta.className = "tip-meta";
    meta.textContent = v > 0
      ? `${measurePhrase(v)} · rank ${nf.format(view.rank[u])} of ${nf.format(view.size)}`
      : `No ${plural(product())} ${state.measure === "days" ? "in effect" : "issued"}, ${yearLabel()}`;
    tip.append(name, meta);
    tip.hidden = false;
    const W = $("stage").clientWidth;
    const x = e.point.x + 14, y = e.point.y + 14;
    tip.style.left = Math.min(x, W - tip.offsetWidth - 8) + "px";
    tip.style.top = y + "px";
  });
  map.on("mouseleave", layer, clearHover);
  map.on("click", layer, (e) => {
    const f = e.features && e.features[0];
    if (!f) return;
    const unit = layer === "county-fill" ? "counties" : "offices";
    if (unit !== state.unit) return;
    select(unit, f.id, { fly: false });
  });
}

map.on("mousemove", (e) => {
  if (!isGrid() || !grid.ready || !grid.values) return;
  const cell = cellAt(e.lngLat.lng, e.lngLat.lat);
  if (cell < 0 || !gridInFilter(cell)) {
    if (grid.hover != null) { grid.hover = null; renderGridMarks(); }
    $("hover-tip").hidden = true;
    map.getCanvas().style.cursor = "";
    return;
  }
  map.getCanvas().style.cursor = "pointer";
  if (grid.hover !== cell) { grid.hover = cell; renderGridMarks(); }
  const tip = $("hover-tip");
  const v = grid.values[cell];
  const county = grid.county[cell];
  tip.innerHTML = "";
  const name = document.createElement("div");
  name.className = "tip-name";
  name.textContent = `5 km cell · ${manifest.counties.name[county]}, ${manifest.counties.state[county]}`;
  const meta = document.createElement("div");
  meta.className = "tip-meta";
  meta.textContent = v > 0
    ? `${measurePhrase(v)} · higher than ${gridPercentile(v)}% of cells with any`
    : `No ${plural(product())} ${state.measure === "days" ? "in effect" : "issued"}, ${yearLabel()}`;
  tip.append(name, meta);
  tip.hidden = false;
  const W = $("stage").clientWidth;
  tip.style.left = Math.min(e.point.x + 14, W - tip.offsetWidth - 8) + "px";
  tip.style.top = e.point.y + 14 + "px";
});
map.on("mouseout", () => {
  if (!isGrid()) return;
  grid.hover = null;
  renderGridMarks();
  $("hover-tip").hidden = true;
});
map.on("click", (e) => {
  if (!isGrid() || !grid.ready) return;
  const cell = cellAt(e.lngLat.lng, e.lngLat.lat);
  if (cell >= 0 && gridInFilter(cell)) select("grid", cell, { fly: false });
});

function setBasemap(id, { initial = false } = {}) {
  if (!BASEMAPS[id]) id = "light";
  const changed = id !== state.basemap || initial;
  state.basemap = id;
  for (const b of $("basemaps").children) b.setAttribute("aria-checked", String(b.dataset.basemap === id));
  if (!changed) return;
  overlaysReady = false;
  currentStyleLoaded = false;
  usingFallback = false;
  clearHover();
  map.setStyle(BASEMAPS[id].style, { diff: false });
  writeHash();
}

// If a base map style cannot be fetched, fall back to a plain background so
// the counties still draw.
const FALLBACK_STYLE = (bg) => ({ version: 8, sources: {}, layers: [{ id: "background", type: "background", paint: { "background-color": bg } }] });
let usingFallback = false;
let currentStyleLoaded = false;
map.on("error", (e) => {
  const msg = String((e && e.error && e.error.message) || "");
  if (usingFallback || currentStyleLoaded || !/style|Failed to fetch|NetworkError|Load failed/i.test(msg)) return;
  usingFallback = true;
  toast("The base map could not load; showing counties on a plain background.", 6000);
  map.setStyle(FALLBACK_STYLE(PALETTE[tone()].paper), { diff: false });
});

map.on("style.load", () => {
  currentStyleLoaded = true;
  if (!countyGeo || !officeGeo || !stateGeo) return;
  addOverlays();
});

map.on("moveend", () => writeHash());

function fitRegion(region, animate = true) {
  const pad = { top: 60, bottom: 170, left: 20, right: state.selected && window.innerWidth > 820 ? 380 : 20 };
  map.fitBounds(region.bounds, { padding: pad, duration: animate ? 700 : 0 });
}

function featureBounds(geo, index) {
  const f = geo.features.find((x) => x.properties.i === index);
  if (!f) return null;
  let w = 180, s = 90, e = -180, n = -90;
  const walk = (c) => {
    if (typeof c[0] === "number") {
      w = Math.min(w, c[0]); e = Math.max(e, c[0]); s = Math.min(s, c[1]); n = Math.max(n, c[1]);
    } else c.forEach(walk);
  };
  walk(f.geometry.coordinates);
  return [[w, s], [e, n]];
}

function fitState(abbr, animate = true) {
  let w = 180, s = 90, e = -180, n = -90;
  manifest.counties.state.forEach((st, i) => {
    if (st !== abbr) return;
    const b = featureBounds(countyGeo, i);
    if (!b) return;
    w = Math.min(w, b[0][0]); s = Math.min(s, b[0][1]); e = Math.max(e, b[1][0]); n = Math.max(n, b[1][1]);
  });
  // Alaska's Aleutians cross the antimeridian; leave the view alone there.
  if (e - w < 60) {
    const wide = window.innerWidth > 820;
    map.fitBounds([[w, s], [e, n]], { padding: { top: 70, bottom: 190, left: 40, right: state.selected && wide ? 400 : 40 }, duration: animate ? 700 : 0 });
  }
}

// Selection & card -------------------------------------------------------------
function select(unit, index, { fly = true } = {}) {
  let unitChanged = false;
  if (index == null) {
    state.selected = null;
  } else {
    state.selected = { unit, index };
    // With the grid on, a county picked from the ranking opens its card over
    // the grid rather than switching the map back to counties.
    unitChanged = unit !== state.unit && !(isGrid() && unit === "counties");
    if (unitChanged) setUnit(unit, { quiet: true });
  }
  if (unitChanged) renderAll();
  else {
    renderMap();
    renderCard();
    renderRanking();
  }
  writeHash();
  if (fly && state.selected && unit !== "grid") {
    const b = featureBounds(unit === "counties" ? countyGeo : officeGeo, index);
    if (b && b[1][0] - b[0][0] < 60) {
      const wide = window.innerWidth > 820;
      map.fitBounds(b, { padding: { top: 120, bottom: 240, left: 120, right: wide ? 440 : 80 }, maxZoom: unit === "counties" ? 6.5 : 5.5, duration: 800 });
    }
  }
}

function svgEl(tag, attrs, parent) {
  const el = document.createElementNS("http://www.w3.org/2000/svg", tag);
  for (const [k, v] of Object.entries(attrs)) el.setAttribute(k, v);
  if (parent) parent.appendChild(el);
  return el;
}

// Bars with 4px rounded tops anchored to the baseline and a 2px gap; bars
// outside the selected years are muted. The hit area is the full column.
function barChart(svg, values, { labels, active, tipLabel, format }) {
  svg.innerHTML = "";
  const W = svg.clientWidth || 284, H = 86;
  svg.setAttribute("viewBox", `0 0 ${W} ${H}`);
  const top = 6, bottom = 16;
  const plotH = H - top - bottom;
  const max = Math.max(1, ...values);
  const n = values.length;
  const gap = 2;
  const bw = (W - gap * (n - 1)) / n;
  const css = getComputedStyle(document.documentElement);
  const on = css.getPropertyValue("--bar").trim();
  const off = css.getPropertyValue("--bar-out").trim();
  svgEl("line", { x1: 0, x2: W, y1: top + 0.5, y2: top + 0.5, class: "grid" }, svg);
  svgEl("line", { x1: 0, x2: W, y1: top + plotH + 0.5, y2: top + plotH + 0.5, class: "base" }, svg);
  values.forEach((v, k) => {
    const h = v > 0 ? Math.max(2, (v / max) * plotH) : 0;
    const x = k * (bw + gap);
    const hit = svgEl("rect", { x, y: 0, width: bw + gap, height: H - bottom, class: "hit" }, svg);
    const title = svgEl("title", {}, hit);
    title.textContent = `${tipLabel(k)}: ${format(v)}`;
    if (h > 0) {
      const r = Math.min(4, bw / 2, h);
      const y = top + plotH - h;
      const path = `M${x},${top + plotH} L${x},${y + r} Q${x},${y} ${x + r},${y} L${x + bw - r},${y} Q${x + bw},${y} ${x + bw},${y + r} L${x + bw},${top + plotH} Z`;
      svgEl("path", { d: path, fill: active(k) ? on : off, class: "mark", "pointer-events": "none" }, svg);
    }
  });
  const first = svgEl("text", { x: 0, y: H - 3, class: "axis-label" }, svg);
  first.textContent = labels[0];
  const last = svgEl("text", { x: W, y: H - 3, class: "axis-label", "text-anchor": "end" }, svg);
  last.textContent = labels[labels.length - 1];
  if (labels.length > 2) {
    const mid = svgEl("text", { x: W / 2, y: H - 3, class: "axis-label", "text-anchor": "middle" }, svg);
    mid.textContent = labels[Math.floor(labels.length / 2)];
  }
  return max;
}

function median(arr) {
  if (!arr.length) return 0;
  const s = [...arr].sort((a, b) => a - b);
  const m = Math.floor(s.length / 2);
  return s.length % 2 ? s[m] : (s[m - 1] + s[m]) / 2;
}

function renderCard() {
  const card = $("card");
  const sel = state.selected;
  if (!sel || !view || !data) { card.hidden = true; return; }
  $("card-months-wrap").hidden = sel.unit === "grid";
  if (sel.unit === "grid") { renderGridCard(); return; }
  const p = product();
  const u = sel.index;
  const block = data[sel.unit];
  const row = block.index[u];
  const Y = years.length;
  const v = view.unit === sel.unit ? view.values[u] : 0;
  card.hidden = false;

  if (sel.unit === "counties") {
    const st = manifest.counties.state[u];
    const office = manifest.counties.office[u];
    const oi = manifest.offices.wfo.indexOf(office);
    $("card-kicker").textContent = manifest.states.name[manifest.states.abbr.indexOf(st)] || st;
    $("card-title").textContent = manifest.counties.name[u];
    $("card-sub").textContent = oi >= 0 ? `Forecast office: ${manifest.offices.name[oi]} (${office})` : "";
  } else {
    $("card-kicker").textContent = "NWS forecast office";
    $("card-title").textContent = manifest.offices.name[u];
    $("card-sub").textContent = `Office ${manifest.offices.wfo[u]} · counts every alert it issued, marine zones included`;
  }

  $("card-value").textContent = fmtRate(v);
  $("card-unit").textContent = state.measure === "days"
    ? `days per year with ${withArticle(p)} in effect, ${yearLabel()}`
    : `${plural(p)} per year, ${yearLabel()}`;
  const other = view.other[u] || 0;
  $("card-other").textContent = state.measure === "days"
    ? `${fmtRate(other)} ${plural(p)} issued per year`
    : `${fmtRate(other)} days per year in effect`;

  if (v > 0) {
    let text = `Ranks ${nf.format(view.rank[u])} of ${nf.format(view.size)} ${unitName(sel.unit)}`;
    if (sel.unit === "counties") {
      const st = manifest.counties.state[u];
      const peers = view.order.filter((x) => manifest.counties.state[x] === st);
      const within = peers.indexOf(u) + 1;
      const total = manifest.counties.state.filter((s) => s === st).length;
      const stateVals = [];
      for (let x = 0; x < view.size; x++) if (manifest.counties.state[x] === st) stateVals.push(view.values[x]);
      text += ` · ${within} of ${total} in ${st} (state median ${fmtRate(median(stateVals))})`;
    } else {
      text += ` · national median ${fmtRate(median(Array.from(view.values)))}`;
    }
    $("card-rank").textContent = text;
  } else {
    $("card-rank").textContent = `No ${plural(p)} ${state.measure === "days" ? "in effect" : "issued"} here in ${yearLabel()}.`;
  }

  const arrY = state.measure === "days" ? block.dy : block.ay;
  const arrM = state.measure === "days" ? block.dm : block.am;
  const byYear = years.map((_, y) => (row >= 0 ? arrY[row * Y + y] : 0));
  const byMonth = MONTHS.map((_, m) => (row >= 0 ? arrM[row * 12 + m] : 0));
  const word = state.measure === "days" ? "days" : "alerts";
  $("card-years-title").textContent = state.measure === "days" ? "Days in effect, by year" : "Alerts issued, by year";
  const maxY = barChart($("card-years"), byYear, {
    labels: years.map(String),
    active: (k) => k >= state.y0 && k <= state.y1,
    tipLabel: (k) => years[k],
    format: (x) => `${nf.format(x)} ${word}`,
  });
  $("card-years-max").textContent = `max ${nf.format(maxY)}`;
  $("card-months-title").textContent = `By month, all years ${years[0]}–${years[years.length - 1]}`;
  const maxM = barChart($("card-months"), byMonth, {
    labels: ["Jan", "Jul", "Dec"],
    active: () => true,
    tipLabel: (k) => MONTHS_LONG[k],
    format: (x) => `${nf.format(x)} ${word}`,
  });
  $("card-months-max").textContent = `max ${nf.format(maxM)}`;
  $("card-foot").textContent = sel.unit === "counties"
    ? `A county counts when the alert covers at least ${Math.round(manifest.rules.min_county_share * 100)}% of it. Days are local calendar days.`
    : "Days are local calendar days in the office's time zone.";
}

$("card-close").addEventListener("click", () => select(null, null));

// Panel ------------------------------------------------------------------------
function tile(parent, value, key, sub, onClick) {
  const t = document.createElement("div");
  t.className = "tile";
  const v = document.createElement(onClick ? "button" : "div");
  v.className = "v";
  v.textContent = value;
  if (onClick) { v.type = "button"; v.addEventListener("click", onClick); }
  const k = document.createElement("div");
  k.className = "k";
  k.textContent = key;
  t.append(v, k);
  if (sub) {
    const s = document.createElement("div");
    s.className = "s";
    s.textContent = sub;
    t.appendChild(s);
  }
  parent.appendChild(t);
}

function renderTiles() {
  const el = $("tiles");
  el.innerHTML = "";
  if (!view) return;
  const p = product();
  const n = nYears();
  tile(el, compact(view.natAlerts), `${plural(p)} issued nationwide`, `${nf.format(Math.round(view.natAlerts / n))} per year, ${yearLabel()}`);
  const unit = rankUnit();
  const filtered = state.stateFilter && unit === "counties";
  const where = filtered ? ` in ${state.stateFilter}` : "";
  const pool = filtered ? manifest.counties.state.filter((st) => st === state.stateFilter).length : view.size;
  const reached = view.order.filter((u) => view.inFilter(u));
  tile(el, nf.format(reached.length), `${unitName(unit)} reached${where}`, `of ${nf.format(pool)}`);
  if (reached.length) {
    const top = reached[0];
    tile(el, unitLabel(unit, top).replace(/^NWS /, ""), `Highest${where}`, measurePhrase(view.values[top]), () => select(unit, top));
    const typical = median(reached.map((u) => view.values[u]));
    tile(el, fmtRate(typical), `Typical ${unitName(unit, 1)}${where}`, `median of those reached, ${state.measure === "days" ? "days" : "alerts"} per year`);
  }
  if (n > 1) {
    const k = view.perYear.indexOf(Math.max(...view.perYear));
    tile(el, String(years[state.y0 + k]), "Busiest year", `${nf.format(view.perYear[k])} issued`, () => setYears(state.y0 + k, state.y0 + k));
  }
  const total = view.perMonth.reduce((a, b) => a + b, 0);
  if (total > 0) {
    const m = view.perMonth.indexOf(Math.max(...view.perMonth));
    tile(el, MONTHS_LONG[m], "Peak month", `${Math.round((view.perMonth[m] / total) * 100)}% of those issued`);
  }
}

// The color key sits on the map; the panel keeps the notes that explain it.
function renderLegend() {
  const el = $("legend");
  const key = $("map-key");
  el.innerHTML = "";
  key.innerHTML = "";
  key.hidden = !view;
  if (!view) return;
  const p = product();
  const gridOn = isGrid() && grid.values;
  const add = (parent, tag, cls, text) => {
    const node = document.createElement(tag);
    if (cls) node.className = cls;
    if (text != null) node.textContent = text;
    parent.appendChild(node);
    return node;
  };

  if (p.counties === 0) {
    add(el, "div", "warn", "Marine alerts cover water, not counties, so this product is mapped by forecast office.");
  }

  add(key, "div", "key-title", state.measure === "days" ? "Days per year in effect" : `${plural(p)} per year`);
  const where = state.stateFilter && state.unit !== "offices" ? ` · ${state.stateFilter}` : "";
  const by = isGrid() ? "5 km grid" : state.unit === "counties" ? "By county" : "By forecast office";
  add(key, "div", "key-sub", `${p.label} · ${by} · ${yearLabel()}${where}`);

  if (isGrid() && !grid.values) {
    add(key, "div", "key-empty", "Loading the 5 km grid…");
    return;
  }
  const breaks = gridOn ? grid.breaks : view.breaks;
  if (!breaks.length && !(gridOn ? grid.reached : view.reached)) {
    add(key, "div", "key-empty", `None ${state.measure === "days" ? "in effect" : "issued"} in ${yearLabel()}.`);
    return;
  }

  const ramp = PALETTE[tone()].ramp;
  const k = breaks.length + 1;
  // With fewer classes than steps, spread them across the ramp so the top
  // class is always the ramp's extreme.
  const pick = (c) => ramp[Math.round((c / Math.max(1, k - 1)) * (ramp.length - 1))];
  const steps = add(key, "div", "steps");
  const labels = add(key, "div", "step-labels");
  for (let c = 0; c < k; c++) {
    const lo = c === 0 ? 0 : breaks[c - 1];
    const hi = c < k - 1 ? breaks[c] : null;
    const sw = add(steps, "span");
    sw.style.background = pick(c);
    sw.title = hi == null ? `${fmtRate(lo)} or more` : c === 0 ? `Above 0, under ${fmtRate(hi)}` : `${fmtRate(lo)} to under ${fmtRate(hi)}`;
    add(labels, "span", null, c === 0 ? ">0" : fmtRate(lo));
  }
  const none = add(key, "div", "none");
  add(none, "i");
  none.append("None");

  add(el, "p", "note", `Each color holds about the same number of ${gridOn ? "5 km cells" : unitName(state.unit)} with any; unfilled areas had none. Hover the key for exact ranges.`);
  if (gridOn) {
    add(el, "p", "note", "Storm-based warnings count where their polygon covered a cell; other alerts cover every cell of the zones and counties they name.");
  }
}

// Map classes are 1..k, but the ramp has seven steps: stretch them over it.
function applyClassColors() {
  if (!overlaysReady || !view) return;
  const P = PALETTE[tone()];
  const k = view.breaks.length + 1;
  const pick = (c) => P.ramp[Math.round(((c - 1) / Math.max(1, k - 1)) * (P.ramp.length - 1))];
  const expr = ["match", ["coalesce", ["feature-state", "c"], 0]];
  for (let c = 1; c <= k; c++) expr.push(c, pick(c));
  expr.push("rgba(0,0,0,0)");
  map.setPaintProperty("county-fill", "fill-color", expr);
  map.setPaintProperty("office-fill", "fill-color", expr);
  repaintGrid();
}

function renderRanking() {
  const list = $("rank-list");
  list.innerHTML = "";
  if (!view) return;
  const unit = rankUnit();
  const rows = view.order.filter((u) => view.inFilter(u));
  const shown = state.rankAll ? rows : rows.slice(0, RANK_SHORT);
  const max = rows.length ? view.values[rows[0]] : 1;
  const sel = state.selected;
  if (!rows.length) {
    const li = document.createElement("li");
    li.className = "empty";
    li.textContent = `No ${unitName(unit)} with ${plural(product())} in ${yearLabel()}.`;
    list.appendChild(li);
  }
  shown.forEach((u, k) => {
    const li = document.createElement("li");
    const b = document.createElement("button");
    b.type = "button";
    if (sel && sel.unit === unit && sel.index === u) b.className = "current";
    const r = document.createElement("span");
    r.className = "r";
    r.textContent = String(k + 1);
    const name = document.createElement("span");
    name.textContent = unitLabel(unit, u).replace(/^NWS /, "");
    const n = document.createElement("span");
    n.className = "n";
    n.textContent = fmtRate(view.values[u]);
    const bar = document.createElement("span");
    bar.className = "bar";
    const fill = document.createElement("span");
    fill.style.width = `${(view.values[u] / max) * 100}%`;
    bar.appendChild(fill);
    b.append(r, name, n, bar);
    b.addEventListener("click", () => select(unit, u));
    li.appendChild(b);
    list.appendChild(li);
  });
  const more = $("rank-more");
  more.hidden = rows.length <= RANK_SHORT;
  more.textContent = state.rankAll ? "Show fewer" : `Show all ${nf.format(rows.length)}`;
  more.setAttribute("aria-expanded", String(state.rankAll));
  $("state-filter").hidden = unit !== "counties";
}

$("rank-more").addEventListener("click", () => { state.rankAll = !state.rankAll; renderRanking(); });

function renderProductControls() {
  const p = product();
  $("picker-label").textContent = p.label;
  $("picker-code").textContent = p.id;
  // Siblings: the other significances of the same phenomenon, or the other
  // "any" products.
  const seg = $("sig-seg");
  seg.innerHTML = "";
  const sibs = products
    .filter((q) => (p.kind === "significance" ? q.kind === "significance" : q.phenom === p.phenom))
    .sort((a, b) => SIG_ORDER[a.sig] - SIG_ORDER[b.sig]);
  seg.hidden = sibs.length < 2;
  for (const q of sibs) {
    const b = document.createElement("button");
    b.type = "button";
    b.setAttribute("role", "radio");
    b.setAttribute("aria-checked", String(q.id === p.id));
    b.textContent = p.kind === "significance" ? pluralOne(SIG_SHORT[q.sig]) : SIG_SHORT[q.sig];
    b.title = q.label;
    b.addEventListener("click", () => setProduct(q.id));
    seg.appendChild(b);
  }

  const parts = [];
  const span = p.first === p.last ? `in ${p.first}` : `${p.first}–${p.last}`;
  parts.push(`<b>${nf.format(p.alerts)}</b> issued ${span}`);
  if (p.counties > 0) parts.push(`reaching ${nf.format(p.counties)} counties`);
  let html = parts.join(", ") + ".";
  if (p.note) html += " " + escapeHtml(p.note);
  if (p.first > years[0] || p.last < years[years.length - 1]) {
    html += ` Years without it count as zero.`;
  }
  $("product-note").innerHTML = html;

  $("measure-note").textContent = state.measure === "days"
    ? "Calendar days with the alert in effect for any part of the day, averaged over the selected years."
    : "Separate alerts that covered the county or office, dated by when they first did, averaged over the selected years.";
  for (const b of $("measure-seg").children) b.setAttribute("aria-checked", String(b.dataset.measure === state.measure));

  const unitSeg = $("unit-seg");
  for (const b of unitSeg.children) {
    b.setAttribute("aria-checked", String(b.dataset.unit === state.unit));
    b.disabled = b.dataset.unit !== "offices" && p.counties === 0;
  }
}

function renderYears() {
  $("year-start").value = String(state.y0);
  $("year-end").value = String(state.y1);
  $("range-label").textContent = `${yearLabel()} · ${nYears()} ${nYears() === 1 ? "year" : "years"}`;
  $("step-back").disabled = state.y0 === 0;
  $("step-fwd").disabled = state.y1 === years.length - 1;
  const last = years.length - 1;
  for (const b of $("year-presets").children) {
    const [a, z] = b.dataset.range.split("-").map(Number);
    b.setAttribute("aria-pressed", String(a === state.y0 && z === state.y1));
  }
  void last;
}

function renderAll() {
  compute();
  computeGrid();
  renderProductControls();
  renderYears();
  applyClassColors();
  renderMap();
  renderTiles();
  renderLegend();
  renderRanking();
  renderCard();
  drawTimeline();
}

function escapeHtml(s) {
  return String(s).replace(/[&<>"']/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" })[c]);
}

// Actions ----------------------------------------------------------------------
let loadToken = 0;
async function setProduct(id, { initial = false } = {}) {
  if (!productById.has(id)) id = DEFAULT_PRODUCT;
  const my = ++loadToken;
  state.product = id;
  const p = product();
  if (p.counties === 0 && state.unit !== "offices") state.unit = "offices";
  renderProductControls();
  if (!cache.has(id)) {
    $("busy-text").textContent = p.label;
    $("busy").hidden = false;
    $("tiles").classList.add("pending");
  }
  try {
    const d = await loadProduct(id);
    if (my !== loadToken) return;
    data = d;
  } catch (e) {
    toast("Could not load " + p.label + ": " + e.message, 8000);
    return;
  } finally {
    if (my === loadToken) {
      $("busy").hidden = true;
      $("tiles").classList.remove("pending");
    }
  }
  renderAll();
  if (!initial) writeHash();
  if (isGrid()) refreshGrid();
}

function setMeasure(m) {
  state.measure = m === "alerts" ? "alerts" : "days";
  renderAll();
  writeHash();
  if (isGrid()) refreshGrid();
}

function setUnit(unit, { quiet = false } = {}) {
  if (unit !== "offices" && product().counties === 0) return;
  state.unit = ["offices", "grid"].includes(unit) ? unit : "counties";
  state.rankAll = false;
  clearHover();
  grid.hover = null;
  if (state.selected) {
    const keep = state.selected.unit === state.unit || (isGrid() && state.selected.unit === "counties");
    if (!keep) state.selected = null;
  }
  if (quiet) return;
  renderAll();
  writeHash();
  if (isGrid()) refreshGrid();
}

function setYears(a, b) {
  const last = years.length - 1;
  a = Math.max(0, Math.min(last, a));
  b = Math.max(0, Math.min(last, b));
  if (a > b) [a, b] = [b, a];
  state.y0 = a;
  state.y1 = b;
  renderAll();
  writeHash();
}

function stepYears(dir) {
  const len = nYears();
  const last = years.length - 1;
  let a = state.y0 + dir * len, b = state.y1 + dir * len;
  if (a < 0) { a = 0; b = len - 1; }
  if (b > last) { b = last; a = last - len + 1; }
  setYears(a, b);
}

// Product picker ---------------------------------------------------------------
const picker = { open: false, items: [], active: -1 };

function openPicker() {
  picker.open = true;
  $("picker-pop").hidden = false;
  $("picker-btn").setAttribute("aria-expanded", "true");
  $("picker-search").value = "";
  renderPickerList();
  $("picker-search").focus();
  const cur = $("picker-list").querySelector(".current");
  if (cur) cur.scrollIntoView({ block: "center" });
}

function closePicker(focus = true) {
  picker.open = false;
  $("picker-pop").hidden = true;
  $("picker-btn").setAttribute("aria-expanded", "false");
  if (focus) $("picker-btn").focus();
}

function renderPickerList() {
  const q = $("picker-search").value.trim().toLowerCase();
  const list = $("picker-list");
  list.innerHTML = "";
  picker.items = [];
  const terms = q.split(/\s+/).filter(Boolean);
  // A match in the name or code ranks above one only in the group or note, so
  // "heat" lists Heat Advisory before Wind Chill Advisory (group "Heat and cold").
  const strong = (p) => terms.every((t) => `${p.label} ${p.id}`.toLowerCase().includes(t));
  const match = (p) => {
    const hay = `${p.label} ${p.id} ${p.group} ${p.note}`.toLowerCase();
    return terms.every((t) => hay.includes(t));
  };
  const tiers = terms.length ? [strong, (p) => match(p) && !strong(p)] : [() => true];
  for (const [tier, keep] of tiers.entries()) for (const g of manifest.groups) {
    const inGroup = products.filter((p) => p.group === g && keep(p));
    if (!inGroup.length) continue;
    const head = document.createElement("div");
    head.className = "picker-group";
    head.textContent = tier === 1 ? `${g} · related` : g;
    list.appendChild(head);
    inGroup
      .sort((a, b) => (a.kind === "phenomenon") - (b.kind === "phenomenon") || b.alerts - a.alerts)
      .forEach((p) => {
        const item = document.createElement("div");
        item.className = "picker-item" + (p.id === state.product ? " current" : "");
        item.setAttribute("role", "option");
        item.id = "opt-" + p.id.replace(/\./g, "-");
        const name = document.createElement("span");
        name.className = "name";
        name.textContent = p.label;
        const code = document.createElement("span");
        code.className = "code";
        code.textContent = p.id;
        const meta = document.createElement("span");
        meta.className = "meta";
        const span = p.first === p.last ? `${p.first}` : `${p.first}–${p.last}`;
        meta.textContent = `${compact(p.alerts)} alerts · ${span}${p.counties === 0 ? " · offices only" : ""}`;
        item.append(name, code, meta);
        item.addEventListener("mousedown", (e) => { e.preventDefault(); choose(p.id); });
        item.addEventListener("mousemove", () => setActive(picker.items.indexOf(p.id)));
        list.appendChild(item);
        picker.items.push(p.id);
      });
  }
  if (!picker.items.length) {
    const empty = document.createElement("div");
    empty.className = "picker-empty";
    empty.textContent = "No product matches.";
    list.appendChild(empty);
  }
  setActive(q ? 0 : picker.items.indexOf(state.product));
}

function setActive(k) {
  picker.active = k;
  const list = $("picker-list");
  for (const el of list.querySelectorAll(".picker-item")) el.setAttribute("aria-selected", "false");
  const id = picker.items[k];
  if (id == null) { $("picker-search").removeAttribute("aria-activedescendant"); return; }
  const el = $("opt-" + id.replace(/\./g, "-"));
  if (el) {
    el.setAttribute("aria-selected", "true");
    $("picker-search").setAttribute("aria-activedescendant", el.id);
    const lr = list.getBoundingClientRect(), er = el.getBoundingClientRect();
    if (er.top < lr.top || er.bottom > lr.bottom) el.scrollIntoView({ block: "nearest" });
  }
}

function choose(id) {
  closePicker();
  setProduct(id);
}

$("picker-btn").addEventListener("click", () => (picker.open ? closePicker() : openPicker()));
$("picker-search").addEventListener("input", renderPickerList);
$("picker-search").addEventListener("keydown", (e) => {
  if (e.key === "ArrowDown") { e.preventDefault(); setActive(Math.min(picker.items.length - 1, picker.active + 1)); }
  else if (e.key === "ArrowUp") { e.preventDefault(); setActive(Math.max(0, picker.active - 1)); }
  else if (e.key === "Enter") { e.preventDefault(); if (picker.items[picker.active]) choose(picker.items[picker.active]); }
  else if (e.key === "Escape") { e.preventDefault(); closePicker(); }
});
document.addEventListener("mousedown", (e) => {
  if (picker.open && !$("picker").contains(e.target)) closePicker(false);
  if (!$("find-list").hidden && !e.target.closest(".finder")) $("find-list").hidden = true;
});

// Finder -----------------------------------------------------------------------
const finder = { items: [], active: 0 };

function renderFinder() {
  const q = $("find").value.trim().toLowerCase();
  const list = $("find-list");
  list.innerHTML = "";
  finder.items = [];
  if (q.length < 2) { list.hidden = true; return; }
  const c = manifest.counties;
  for (let i = 0; i < c.name.length && finder.items.length < 8; i++) {
    const label = `${c.name[i]}, ${c.state[i]}`;
    if (label.toLowerCase().includes(q) || c.geoid[i] === q) finder.items.push({ unit: "counties", index: i, label, kind: "County" });
  }
  const o = manifest.offices;
  for (let i = 0; i < o.wfo.length && finder.items.length < 10; i++) {
    if (o.name[i].toLowerCase().includes(q) || o.wfo[i].toLowerCase() === q) finder.items.push({ unit: "offices", index: i, label: `${o.name[i]} (${o.wfo[i]})`, kind: "Office" });
  }
  finder.active = 0;
  finder.items.forEach((it, k) => {
    const li = document.createElement("li");
    li.setAttribute("role", "option");
    li.setAttribute("aria-selected", String(k === 0));
    li.textContent = it.label;
    const kind = document.createElement("span");
    kind.className = "k";
    kind.textContent = it.kind;
    li.appendChild(kind);
    li.addEventListener("mousedown", (e) => { e.preventDefault(); pickFound(it); });
    list.appendChild(li);
  });
  list.hidden = !finder.items.length;
}

function pickFound(it) {
  $("find").value = "";
  $("find-list").hidden = true;
  if (it.unit === "offices" && product().counties === 0) setUnit("offices", { quiet: true });
  select(it.unit, it.index);
}

$("find").addEventListener("input", renderFinder);
$("find").addEventListener("keydown", (e) => {
  const lis = $("find-list").children;
  if (e.key === "ArrowDown" || e.key === "ArrowUp") {
    e.preventDefault();
    finder.active = Math.max(0, Math.min(finder.items.length - 1, finder.active + (e.key === "ArrowDown" ? 1 : -1)));
    [...lis].forEach((li, k) => li.setAttribute("aria-selected", String(k === finder.active)));
  } else if (e.key === "Enter" && finder.items[finder.active]) {
    e.preventDefault();
    pickFound(finder.items[finder.active]);
  } else if (e.key === "Escape") {
    $("find-list").hidden = true;
  }
});

// Timeline ---------------------------------------------------------------------
// National alerts issued per month across the archive, selected years in blue.
const tl = { canvas: $("timeline"), drag: null, layout: null };

function drawTimeline() {
  if (!data || !manifest) return;
  const cv = tl.canvas;
  const dpr = window.devicePixelRatio || 1;
  const W = cv.clientWidth, H = cv.clientHeight;
  if (!W || !H) return;
  cv.width = Math.round(W * dpr);
  cv.height = Math.round(H * dpr);
  const ctx = cv.getContext("2d");
  ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
  ctx.clearRect(0, 0, W, H);
  const css = getComputedStyle(document.documentElement);
  const col = (n) => css.getPropertyValue(n).trim();
  const series = data.national.a;
  const Y = years.length;
  const n = Y * 12;
  const left = 34, right = 6, top = 6, bottom = 18;
  const plotW = W - left - right, plotH = H - top - bottom;
  let max = 0;
  for (let k = 0; k < n; k++) max = Math.max(max, series[k]);
  const niceMax = max > 0 ? niceRound(max) : 1;
  const scale = plotH / niceMax;
  const bw = plotW / n;
  tl.layout = { left, plotW, bw, n };

  ctx.font = `10.5px ${css.getPropertyValue("--font")}`;
  ctx.fillStyle = col("--muted");
  ctx.strokeStyle = col("--grid");
  ctx.lineWidth = 1;
  for (const f of [0.5, 1]) {
    const y = Math.round(top + plotH - f * plotH) + 0.5;
    ctx.beginPath(); ctx.moveTo(left, y); ctx.lineTo(W - right, y); ctx.stroke();
    ctx.textAlign = "right"; ctx.textBaseline = "middle";
    ctx.fillText(compact(Math.round(niceMax * f)), left - 5, y);
  }
  // Year bands behind the selection.
  ctx.fillStyle = col("--hover");
  ctx.fillRect(left + state.y0 * 12 * bw, top, (state.y1 - state.y0 + 1) * 12 * bw, plotH);
  const on = col("--bar"), off = col("--bar-out");
  const gap = bw > 4 ? 1 : 0;
  for (let k = 0; k < n; k++) {
    const v = series[k];
    if (!v) continue;
    const y = Math.floor(k / 12);
    const h = Math.max(1, v * scale);
    ctx.fillStyle = y >= state.y0 && y <= state.y1 ? on : off;
    ctx.fillRect(left + k * bw + gap / 2, top + plotH - h, Math.max(1, bw - gap), h);
  }
  ctx.strokeStyle = col("--axis");
  ctx.beginPath(); ctx.moveTo(left, top + plotH + 0.5); ctx.lineTo(W - right, top + plotH + 0.5); ctx.stroke();
  ctx.fillStyle = col("--muted");
  ctx.textAlign = "center"; ctx.textBaseline = "alphabetic";
  const every = plotW / Y < 34 ? 2 : 1;
  for (let y = 0; y < Y; y++) {
    const x = left + (y * 12 + 6) * bw;
    ctx.strokeStyle = col("--grid");
    if (y > 0) { ctx.beginPath(); ctx.moveTo(Math.round(left + y * 12 * bw) + 0.5, top + plotH); ctx.lineTo(Math.round(left + y * 12 * bw) + 0.5, top + plotH + 4); ctx.stroke(); }
    if (y % every === 0) {
      ctx.fillStyle = y >= state.y0 && y <= state.y1 ? col("--text") : col("--muted");
      ctx.fillText(String(years[y]), x, H - 4);
    }
  }
  const p = product();
  $("timeline-title").textContent = `${plural(p)} issued per month, nationwide`;
  $("timeline-legend").innerHTML = "";
  const peak = [...series].indexOf(max);
  if (max > 0) {
    const s = document.createElement("span");
    s.textContent = `Peak ${MONTHS[peak % 12]} ${years[Math.floor(peak / 12)]}: ${nf.format(max)}`;
    $("timeline-legend").appendChild(s);
  }
}

function yearAt(px) {
  const L = tl.layout;
  if (!L) return 0;
  const k = Math.floor((px - L.left) / L.bw);
  return Math.max(0, Math.min(years.length - 1, Math.floor(k / 12)));
}

tl.canvas.addEventListener("pointerdown", (e) => {
  const r = tl.canvas.getBoundingClientRect();
  const y = yearAt(e.clientX - r.left);
  tl.drag = { from: y };
  tl.canvas.setPointerCapture(e.pointerId);
  state.y0 = y; state.y1 = y;
  drawTimeline();
});
tl.canvas.addEventListener("pointermove", (e) => {
  const r = tl.canvas.getBoundingClientRect();
  const px = e.clientX - r.left;
  if (tl.drag) {
    const y = yearAt(px);
    state.y0 = Math.min(tl.drag.from, y);
    state.y1 = Math.max(tl.drag.from, y);
    drawTimeline();
    renderYears();
  }
  showTimelineTip(px, e.clientY - r.top);
});
const endDrag = () => {
  if (!tl.drag) return;
  tl.drag = null;
  setYears(state.y0, state.y1);
};
tl.canvas.addEventListener("pointerup", endDrag);
tl.canvas.addEventListener("pointercancel", endDrag);
tl.canvas.addEventListener("pointerleave", () => { $("timeline-tip").hidden = true; });
tl.canvas.addEventListener("keydown", (e) => {
  if (e.key === "ArrowLeft") { e.preventDefault(); stepYears(-1); }
  if (e.key === "ArrowRight") { e.preventDefault(); stepYears(1); }
});

function showTimelineTip(px, py) {
  const L = tl.layout;
  if (!L || !data) return;
  const k = Math.floor((px - L.left) / L.bw);
  const tip = $("timeline-tip");
  if (k < 0 || k >= L.n) { tip.hidden = true; return; }
  const y = Math.floor(k / 12), m = k % 12;
  tip.innerHTML = "";
  const b = document.createElement("b");
  b.textContent = nf.format(data.national.a[k]);
  tip.append(b, ` issued · ${MONTHS_LONG[m]} ${years[y]}`);
  const d = data.national.d[k];
  tip.append(document.createElement("br"), `in effect somewhere on ${d} of its days`);
  tip.hidden = false;
  const W = tl.canvas.clientWidth;
  tip.style.left = Math.max(0, Math.min(W - tip.offsetWidth, px - tip.offsetWidth / 2)) + "px";
  tip.style.top = Math.max(-58, py - 62) + "px";
}

// Share, image and CSV ---------------------------------------------------------
function writeHash() {
  if (!manifest) return;
  const p = new URLSearchParams();
  if (state.product !== DEFAULT_PRODUCT) p.set("p", state.product);
  if (state.measure !== "days") p.set("m", state.measure);
  if (state.y0 !== 0 || state.y1 !== years.length - 1) p.set("y", yearLabel().replace("–", "-"));
  if (state.unit !== "counties") p.set("u", state.unit);
  if (state.stateFilter) p.set("st", state.stateFilter);
  if (state.selected) {
    const { unit, index } = state.selected;
    if (unit === "counties") p.set("c", manifest.counties.geoid[index]);
    else if (unit === "offices") p.set("o", manifest.offices.wfo[index]);
    else p.set("g", String(index));
  }
  p.set("b", state.basemap);
  const c = map.getCenter();
  p.set("map", `${map.getZoom().toFixed(2)}/${c.lat.toFixed(3)}/${c.lng.toFixed(3)}`);
  history.replaceState(null, "", "#" + p.toString());
}

function readHash() {
  const p = new URLSearchParams(location.hash.slice(1));
  Object.assign(state, {
    product: DEFAULT_PRODUCT, measure: "days", y0: 0, y1: years.length - 1,
    unit: "counties", stateFilter: "", selected: null, rankAll: false,
  });
  if (productById.has(p.get("p"))) state.product = p.get("p");
  if (p.get("m") === "alerts") state.measure = "alerts";
  const y = (p.get("y") || "").split("-").map((s) => years.indexOf(Number(s)));
  if (p.has("y") && y[0] >= 0) {
    state.y0 = y[0];
    state.y1 = y.length > 1 && y[1] >= 0 ? y[1] : y[0];
  }
  if (["offices", "grid"].includes(p.get("u"))) state.unit = p.get("u");
  if (manifest.states.abbr.includes(p.get("st"))) state.stateFilter = p.get("st");
  if (p.has("c")) {
    const i = manifest.counties.geoid.indexOf(p.get("c"));
    if (i >= 0) state.selected = { unit: "counties", index: i };
  } else if (p.has("o")) {
    const i = manifest.offices.wfo.indexOf(p.get("o"));
    if (i >= 0) state.selected = { unit: "offices", index: i };
  } else if (p.has("g") && manifest.grid) {
    const i = Number(p.get("g"));
    if (Number.isInteger(i) && i >= 0 && i < manifest.grid.cells) state.selected = { unit: "grid", index: i };
  }
  if (BASEMAPS[p.get("b")]) state.basemap = p.get("b");
  const m = (p.get("map") || "").split("/").map(Number);
  return m.length === 3 && m.every(Number.isFinite) ? m : null;
}

function savePng() {
  map.once("render", () => {
    const src = map.getCanvas();
    const dpr = src.width / src.clientWidth;
    const P = PALETTE[tone()];
    const head = Math.round(66 * dpr), foot = Math.round(52 * dpr);
    const out = document.createElement("canvas");
    out.width = src.width;
    out.height = src.height + head + foot;
    const ctx = out.getContext("2d");
    ctx.fillStyle = P.paper;
    ctx.fillRect(0, 0, out.width, out.height);
    ctx.drawImage(src, 0, head);
    const font = getComputedStyle(document.body).fontFamily;
    const p = product();
    ctx.fillStyle = P.ink;
    ctx.font = `650 ${20 * dpr}px ${font}`;
    const title = state.measure === "days" ? `Days per year with a ${p.label} in effect` : `${plural(p)} per year`;
    ctx.fillText(title, 16 * dpr, 28 * dpr);
    ctx.fillStyle = P.sub;
    ctx.font = `${13.5 * dpr}px ${font}`;
    const by = isGrid() ? "5 km grid cell" : unitName(state.unit, 1);
    ctx.fillText(`By ${by}, average of ${yearLabel()}${state.stateFilter && state.unit !== "offices" ? " · " + state.stateFilter : ""} · ${nf.format(view.natAlerts)} issued nationwide`, 16 * dpr, 50 * dpr);
    // Legend strip.
    const breaks = isGrid() && grid.values ? grid.breaks : view.breaks;
    const k = breaks.length + 1;
    const pick = (c) => P.ramp[Math.round((c / Math.max(1, k - 1)) * (P.ramp.length - 1))];
    const sw = 44 * dpr, sh = 10 * dpr, y0 = src.height + head + 12 * dpr;
    ctx.font = `${11 * dpr}px ${font}`;
    for (let c = 0; c < k; c++) {
      ctx.fillStyle = pick(c);
      ctx.fillRect(16 * dpr + c * (sw + 2 * dpr), y0, sw, sh);
      ctx.fillStyle = P.sub;
      ctx.fillText(c === 0 ? ">0" : fmtRate(breaks[c - 1]), 16 * dpr + c * (sw + 2 * dpr), y0 + sh + 12 * dpr);
    }
    ctx.textAlign = "right";
    ctx.fillText("Data: NWS via Iowa Environmental Mesonet · IPPRA, University of Oklahoma · Base map © CARTO, OpenStreetMap", out.width - 16 * dpr, out.height - 10 * dpr);
    out.toBlob((blob) => saveBlob(blob, `nws_${p.id.replace(".", "_")}_${state.measure}_${yearLabel().replace("–", "-")}.png`));
  });
  map.triggerRepaint();
}

function csvCell(s) {
  s = String(s);
  return /[",\n]/.test(s) ? `"${s.replace(/"/g, '""')}"` : s;
}

function downloadCsv() {
  if (!view || !data) return;
  if (isGrid()) { downloadGridCsv(); return; }
  const p = product();
  const unit = state.unit;
  const block = data[unit];
  const Y = years.length;
  const sel = [];
  for (let y = state.y0; y <= state.y1; y++) sel.push(y);
  const head = unit === "counties"
    ? ["geoid", "county", "state", "office"]
    : ["office", "office_name"];
  head.push("product", "years", "days_per_year", "alerts_per_year", ...sel.map((y) => `days_${years[y]}`), ...sel.map((y) => `alerts_${years[y]}`));
  const lines = [head.join(",")];
  for (let u = 0; u < view.size; u++) {
    if (!view.inFilter(u)) continue;
    const row = block.index[u];
    const days = sel.map((y) => (row >= 0 ? block.dy[row * Y + y] : 0));
    const alerts = sel.map((y) => (row >= 0 ? block.ay[row * Y + y] : 0));
    const sum = (a) => a.reduce((x, z) => x + z, 0);
    const ids = unit === "counties"
      ? [manifest.counties.geoid[u], manifest.counties.name[u], manifest.counties.state[u], manifest.counties.office[u]]
      : [manifest.offices.wfo[u], manifest.offices.name[u]];
    lines.push([...ids, p.id, yearLabel().replace("–", "-"), (sum(days) / sel.length).toFixed(3), (sum(alerts) / sel.length).toFixed(3), ...days, ...alerts].map(csvCell).join(","));
  }
  saveBlob(new Blob([lines.join("\n") + "\n"], { type: "text/csv" }), `nws_${p.id.replace(".", "_")}_${unit}_${yearLabel().replace("–", "-")}${state.stateFilter && unit === "counties" ? "_" + state.stateFilter : ""}.csv`);
}

function saveBlob(blob, name) {
  const a = document.createElement("a");
  a.href = URL.createObjectURL(blob);
  a.download = name;
  document.body.appendChild(a);
  a.click();
  setTimeout(() => { URL.revokeObjectURL(a.href); a.remove(); }, 1000);
}

let toastTimer = null;
function toast(msg, ms = 3000) {
  const t = $("toast");
  t.textContent = msg;
  t.hidden = false;
  clearTimeout(toastTimer);
  toastTimer = setTimeout(() => { t.hidden = true; }, ms);
}

// About ------------------------------------------------------------------------
function renderAbout() {
  const r = manifest.rules;
  const first = years[0], last = years[years.length - 1];
  $("about").innerHTML = `
    <p><b>What is counted.</b> Every watch, warning, advisory and statement the National Weather Service issued under VTEC from ${first} through ${last}, as archived by the <a href="https://mesonet.agron.iastate.edu/request/gis/watchwarn.phtml" target="_blank" rel="noopener">Iowa Environmental Mesonet</a>. Each alert names the zones or counties it covers, and each of those rows carries the time it began and ended there.</p>
    <p><b>Days in effect</b> counts calendar days, in the county's own time zone, on which the alert was in effect for any part of the day. A warning from 11 PM to 1 AM counts two days. <b>Alerts issued</b> counts separate alerts (VTEC events) that covered the county, dated by when they first did. Both are averaged over the selected years; a year with none counts as zero.</p>
    <p><b>Zones and counties.</b> Many products are issued for forecast zones rather than counties. A zone counts toward a county when it covers at least ${Math.round(r.min_county_share * 100)}% of the county's area, or when at least ${Math.round(r.min_zone_share * 100)}% of the zone lies inside it, using the outline IEM stored with each alert, so zones redrawn over the years are matched as they were drawn at the time. Counties are the Census 2023 boundaries: Connecticut appears as its nine planning regions.</p>
    <p><b>The 5 km grid</b> covers land in square 5 km cells on an equal-area projection for each region. Storm-based warnings - tornado, severe thunderstorm, flash flood, most flood products, snow squall and dust storm - count in a cell when their polygon covered its centre, at the times each version of the polygon was in force. Every other alert covers all the cells of the zones and counties it names. A day covered by both kinds counts once.</p>
    <p><b>Forecast offices</b> count every alert the office issued, including marine zones, which reach no county. National totals count each SPC tornado and severe thunderstorm watch once, not once per office that issued it.</p>
    <p><b>Stuck alerts.</b> A few alerts in the archive were never closed - a flood advisory for Puerto Rico runs 1,034 days. Each alert is capped at three times the 99.9th percentile duration of its product (never less than 3 days). This shortens 149 of 6.2 million rows; genuinely long river flood warnings are unaffected.</p>
    <p><b>Products change.</b> NWS Hazard Simplification retired and renamed products during these years - Wind Chill became Extreme Cold and Cold Weather, Excessive Heat became Extreme Heat, lake effect snow and freezing rain advisories folded into winter weather advisories. Each keeps its own code here, so check the years a product was issued before comparing periods.</p>
    <p><b>Sources.</b> Alerts: Iowa Environmental Mesonet, NWS watch/warning/advisory archive. Counties: U.S. Census Bureau cartographic boundaries, 2023. Offices and time zones: NWS county and county warning area files, April 2026. Built by the Institute for Public Policy Research and Analysis, University of Oklahoma. Build ${escapeHtml(manifest.build)}.</p>`;
}

// Boot -------------------------------------------------------------------------
function buildControls() {
  for (const b of $("measure-seg").children) b.addEventListener("click", () => setMeasure(b.dataset.measure));
  for (const b of $("unit-seg").children) b.addEventListener("click", () => setUnit(b.dataset.unit));

  const last = years.length - 1;
  const presets = [
    { label: `All ${years.length} years`, range: [0, last] },
    { label: "Last 10", range: [Math.max(0, last - 9), last] },
    { label: "Last 5", range: [Math.max(0, last - 4), last] },
    { label: String(years[last]), range: [last, last] },
  ];
  for (const pr of presets) {
    const b = document.createElement("button");
    b.type = "button";
    b.textContent = pr.label;
    b.dataset.range = pr.range.join("-");
    b.addEventListener("click", () => setYears(pr.range[0], pr.range[1]));
    $("year-presets").appendChild(b);
  }
  for (const sel of [$("year-start"), $("year-end")]) {
    years.forEach((y, k) => sel.add(new Option(String(y), String(k))));
  }
  $("year-start").addEventListener("change", (e) => setYears(Number(e.target.value), Math.max(Number(e.target.value), state.y1)));
  $("year-end").addEventListener("change", (e) => setYears(Math.min(state.y0, Number(e.target.value)), Number(e.target.value)));
  $("step-back").addEventListener("click", () => stepYears(-1));
  $("step-fwd").addEventListener("click", () => stepYears(1));

  const sf = $("state-filter");
  sf.add(new Option("All states", ""));
  manifest.states.abbr.forEach((a, k) => sf.add(new Option(manifest.states.name[k], a)));
  sf.addEventListener("change", () => {
    state.stateFilter = sf.value;
    state.rankAll = false;
    renderAll();
    writeHash();
    if (sf.value) fitState(sf.value);
  });

  for (const r of REGIONS) {
    const b = document.createElement("button");
    b.type = "button";
    b.textContent = r.label;
    b.addEventListener("click", () => fitRegion(r));
    $("regions").appendChild(b);
  }
  for (const [id, bm] of Object.entries(BASEMAPS)) {
    const b = document.createElement("button");
    b.type = "button";
    b.setAttribute("role", "radio");
    b.dataset.basemap = id;
    b.textContent = bm.label;
    b.addEventListener("click", () => setBasemap(id));
    $("basemaps").appendChild(b);
  }

  $("copy-link").addEventListener("click", async () => {
    writeHash();
    try {
      await navigator.clipboard.writeText(location.href);
      toast("Link copied");
    } catch {
      toast("Copy failed - the address bar holds the link");
    }
  });
  $("save-png").addEventListener("click", savePng);
  $("download-csv").addEventListener("click", downloadCsv);

  document.addEventListener("keydown", (e) => {
    if (e.target.closest("input, select, textarea, [contenteditable]") || e.metaKey || e.ctrlKey || e.altKey) return;
    if (e.key === "ArrowLeft") stepYears(-1);
    else if (e.key === "ArrowRight") stepYears(1);
    else if (e.key === "Escape" && state.selected) select(null, null);
    else if (e.key === "/") { e.preventDefault(); openPicker(); }
  });

  let resizeTimer = null;
  window.addEventListener("resize", () => {
    clearTimeout(resizeTimer);
    resizeTimer = setTimeout(() => { drawTimeline(); renderCard(); }, 120);
  });
  if (window.matchMedia) {
    window.matchMedia("(prefers-color-scheme: dark)").addEventListener("change", () => { drawTimeline(); renderCard(); });
  }
}

async function boot() {
  const bust = `?v=${window.NAC_BUILD}`;
  manifest = await fetchJson(`data/manifest.json${bust}`);
  years = manifest.years;
  products = manifest.products;
  productById = new Map(products.map((p) => [p.id, p]));
  state.y0 = 0;
  state.y1 = years.length - 1;

  const view0 = readHash();
  buildControls();
  $("state-filter").value = state.stateFilter;
  renderAbout();
  const built = new Date(manifest.built);
  $("freshness-text").textContent = `${years[0]}–${years[years.length - 1]} · ${products.length} products · built ${built.toLocaleDateString("en-US", { month: "short", day: "numeric", year: "numeric" })}`;
  $("picker-search").placeholder = `Search ${products.length} products — tornado, heat, SV.W…`;

  const geo = Promise.all([
    fetchJson(`data/counties.geojson${bust}`),
    fetchJson(`data/offices.geojson${bust}`),
    fetchJson(`data/states.geojson${bust}`),
  ]);
  const first = setProduct(state.product, { initial: true });
  [countyGeo, officeGeo, stateGeo] = await geo;
  await first;

  if (view0) map.jumpTo({ zoom: view0[0], center: [view0[2], view0[1]] });
  else if (state.selected) select(state.selected.unit, state.selected.index);
  else if (state.stateFilter) fitState(state.stateFilter, false);

  if (state.basemap !== initialBasemap) setBasemap(state.basemap, { initial: true });
  for (const b of $("basemaps").children) b.setAttribute("aria-checked", String(b.dataset.basemap === state.basemap));
  if (currentStyleLoaded) addOverlays();

  $("boot").hidden = true;
  renderAll();
  if (isGrid()) refreshGrid();
}

// A pasted link or the back button changes only the hash; apply it in place.
// replaceState, which this page uses for its own updates, fires no event.
window.addEventListener("hashchange", async () => {
  if (!manifest) return;
  const basemap = state.basemap;
  const v = readHash();
  $("state-filter").value = state.stateFilter;
  await setProduct(state.product, { initial: true });
  if (state.basemap !== basemap) setBasemap(state.basemap, { initial: true });
  if (v) map.jumpTo({ zoom: v[0], center: [v[2], v[1]] });
  else if (state.selected) select(state.selected.unit, state.selected.index);
  else if (state.stateFilter) fitState(state.stateFilter);
});

boot().catch((e) => {
  const l = $("boot");
  l.hidden = false;
  l.textContent = "The dashboard failed to start: " + e.message;
  l.classList.add("boot-failed");
});

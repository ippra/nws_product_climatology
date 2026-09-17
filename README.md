# nws_product_climatology - NWS Alert Climatology

A static site that maps how often the National Weather Service puts each U.S.
county under every kind of watch, warning, advisory and statement, 2010-2025:
148 products, 3,235 counties and 125 forecast offices, for any range of years.

It replaces `wwa_data/wwa_shiny_app/`, a Shiny app that showed one hazard at a
time as average days per year. That app mixed watches, warnings and advisories
under one phenomenon code, dated alerts by the UTC day they were issued, and
matched them to counties with `st_intersects()`, which also counted every
neighbouring county a warning polygon touched along a shared border. Its
tornado figures run about 50% above the equivalent count here (days with a
tornado watch or warning in effect), though the two correlate at 0.95 across
counties. The site handles each of those differently (see Decisions below) and
needs no server.

The other analyses in this folder (`outlooks_data/`, `extended_outlooks_data/`,
`watch_data/`, `tsmf_wwa_data/`) are unchanged and not part of the site.

## What it does

- **Every product.** A searchable picker of all 148 products, grouped by
  hazard, with one-click switching between a phenomenon's warning, watch,
  advisory and "any" (a tornado watch or warning). Also "any warning", "any
  watch" and "any advisory" across all hazards.
- **Two measures.** *Days in effect*: local calendar days with the alert in
  effect for any part of the day. *Alerts issued*: separate alerts (VTEC
  events) that covered the county. Both as an average per year.
- **Any years.** Presets, year fields, arrow keys, or drag across the timeline
  of alerts issued nationwide each month.
- **Counties, a 5 km grid, or forecast offices.** The grid shows storm-based
  warnings where their polygons fell rather than across whole counties. Marine
  products, which cover water rather than land, switch to offices
  automatically.
- **County card.** Click a county or office for its rate, its rank nationally
  and within its state, its values by year and by month of the year.
- **Ranking** of counties or offices, limited to a state if you like, with a
  county and office finder. Choosing a state rescales the map to that state.
- **Share and export.** The address bar always holds the current view. Save
  the map as a PNG with title, legend and credits, or download the table
  behind it as CSV.
- Light and dark base maps; the panel follows the system theme.

## The pipeline

Scripts run in number order. Each sources `00_paths.R`, which resolves paths
against the project root (`.here`), so nothing is machine-specific.

| step | what it is |
|---|---|
| `00_run_pipeline.R` | runs 01-06 |
| `01_refresh_data.R` | makes sure every year's IEM archive, the two boundary windows and the reference geography are on disk. **Incremental.** Reads 2010-2024 in place from `wwa_data/<year>_all/`; fetches any other year from IEM into `data/iem_wwa/` (IEM assembles a year for several minutes, then sends about 350 MB) |
| `02_build_crosswalk.R` | matches every (UGC code, outline area) in the archive to Census counties, and gives each county its time zone and office → `outputs/02_crosswalk/`. About 35 seconds |
| `03_build_counts.R` | reads 6.2 million zone and county rows and counts days and alerts by county, office and nation, product, year and month → `outputs/03_counts/`. About 7 minutes |
| `04_build_grid.R` | counts the same days and alerts for every 5 km land cell, from 651,000 warning polygons and the zone outlines → `outputs/04_grid/`. About 90 seconds |
| `05_build_site_data.R` | reshapes the counts into one JSON file per product, a manifest, and simplified outlines, and copies in the grid → `outputs/05_site_data/` |
| `06_build_dashboard.R` | copies `site/` and the site data into one static directory → `outputs/06_site/` |

```
IEM watch/warning archive       Census counties     NWS counties, CWAs
(wwa_data/, data/iem_wwa/)      (data/reference_raw/)
           │                            │                   │
           └──────────── 02_build_crosswalk.R ──────────────┘
                                  │
                         outputs/02_crosswalk/
                                  │
                          03_build_counts.R   ◄── reference/products.csv
                                  │                reference/issuers.csv
                           outputs/03_counts/ ──────────┐
                                  │                      │
                                  │               04_build_grid.R ◄── warning polygons
                                  │                      │
                                  │               outputs/04_grid/
                                  │                      │
                        05_build_site_data.R ◄───────────┘
                                  │
                         outputs/05_site_data/      site/ (front end)
                                  └──────────┬──────────┘
                                   06_build_dashboard.R
                                             │
                                    outputs/06_site/      the deployable site
```

## Building and previewing

```sh
Rscript 00_run_pipeline.R     # refresh, build, assemble
python3 preview.py            # http://localhost:8903
```

R packages: `tidyverse`, `sf`, `jsonlite`, `curl`, `here`, `rmapshaper`,
`foreign`, `tigris` (only its `fips_codes` table; no download).

02 saves the zone outlines and 03 the cleaned rows and duration caps
(`03_rows.rds`, `03_ceilings.csv`), so 04 counts exactly the rows 03 counted.

To add a year once it has ended (2026, in January 2027), run the pipeline:
`last_year` in `00_paths.R` follows the calendar, and 01 fetches the new year
and a new boundary day. Expect 03 to stop on any product code NWS has
introduced; add its row to `reference/products.csv` and run again.

Every script stops rather than write a quietly wrong file: a CSV that reads
short or has a malformed row (other than the one known case below), CSV and
shapefile rows out of order, a land zone that matches no county, a county with
no NWS time zone, an alert time that does not parse, a product or issuer not in
`reference/`, a span beyond two years, more days counted than a month has, a
combined product that disagrees with its parts, counts for a county or office
the outlines lack, a file the manifest lists but the disk lacks, and R, RDS or
CSV files in the published directory.

## Outputs for analysis

`outputs/03_counts/` holds the counts in tidy form, independent of the site:

| file | contents |
|---|---|
| `03_county_year_counts.csv` | GEOID, product, year, days, alerts |
| `03_national_counts.csv` | product id, year, month, days, alerts |
| `03_products.csv` | product ids, codes, names, groups, notes, years issued |
| `03_capped_rows.csv` | the 149 archive rows shortened as stuck alerts |
| `03_county_counts.rds`, `03_office_counts.rds` | by month as well |
| `04_grid/04_grid_cells.csv` | every grid cell: domain, row, column, centre, county |
| `04_grid/*.bin.gz` | per product and measure, one value per cell per year (see Data format) |

`outputs/02_crosswalk/02_ugc_county.csv` records how every zone was matched,
with its overlap shares.

## Decisions worth knowing

**Counted from the zone and county rows, not the polygons.** IEM's archive has
one row per zone or county an alert names (GTYPE `C`) and one per storm-based
polygon (`P`). Every polygon warning also lists its counties, so the `C` rows
count each alert once per county and need no geometry.

**Zones are matched to counties by area.** A county code maps to its own
county. A zone counts toward a county when it covers at least 5% of the
county's area, or at least 25% of the zone lies in it. Calibrated on 2024's
county-coded rows, whose county is known: boundary slivers between a county
and its neighbours measured 0.2% of county area at the median and 1.75% at the
99th percentile. Matching uses IEM's outline of each code as stored with the
alert, keyed by code and area together, so zones redrawn during the period
match as they were drawn then. Two outlines in the archive are fragments
(PRZ001 at 0.15 km², ASZ003 at 0.81 km²): the first takes its code's full
outline, the second the county 3.6 km away.

**Counties are Census 2023.** Connecticut appears as its nine planning regions;
alerts coded to its old counties are matched by area. A county code always maps
to that county alone: IEM's outline of a Virginia county encloses the
independent cities inside it, which have codes of their own.

**A day is a local calendar day.** Alerts are dated in the county's time zone
(from the NWS county file), so an evening warning belongs to the day it
happened, and one from 11 PM to 1 AM counts two days. The archive includes
December 2009 and 1 January 2026 so the first and last days are complete.

**An alert is in effect from its row's ISSUED to EXPIRED.** Those are per zone
or county: a county added later starts later. A watch upgraded before its start
time ends before it began, so it is an alert issued with no days in effect.

**Alerts are VTEC events.** Keyed by office, phenomenon, significance, event
number and year. National counts key SPC tornado and severe thunderstorm
watches without the office, since each office in a watch issues its own copy
of the same numbered watch.

**Stuck alerts are capped.** Some alerts were never closed: a Puerto Rico
flood advisory runs 1,034 days, a Flash Flood Warning 32, a High Wind Warning
30. Real alerts run long too: James River flood warnings stayed in force from
April 2019 into autumn 2020. Each row is capped at three times its product's
99.9th percentile duration, and never below 3 days, which shortens 149 of 6.2
million rows. They are listed in `03_capped_rows.csv`.

**One malformed row is tolerated.** Files from 2025 end in `FCSTER`, a
forecaster sign-off IEM does not quote; `MGF, JD` splits into two fields. Every
column the build reads comes before it, so that row's values are intact. Any
other parsing problem stops the build.

**Products keep their own codes through renames.** NWS Hazard Simplification
replaced Wind Chill products with Extreme Cold and Cold Weather Advisory (late
2024) and Excessive Heat with Extreme Heat (2025), and earlier retired Blizzard
Watches, Lake Effect Snow Watches and Advisories, and Freezing Rain Advisories
(2017). Each keeps its own series, with the change noted in the picker, so a
reader sees where one ends and the next begins rather than a silent splice.

**Offices count what they issued.** Days are in the office's time zone, and
marine zones count. JSJ is San Juan's older identifier and counts as SJU. The
tsunami and hurricane centers (AAQ, HEB, NHC) count nationally and by county
but have no office outline.

**The grid is 5 km on equal-area projections.** Cells are 5 km squares on an
Albers projection per region: EPSG:5070's parameters for the lower 48,
EPSG:3338's for Alaska, ESRI:102007's for Hawaii, and fitted ones for Puerto
Rico and the Virgin Islands, the Marianas, and American Samoa. A cell is land
when its centre falls in a Census county: 374,097 cells. Nine counties are too
small to hold a centre (five Virginia independent cities among them); their
alerts show in the county view and in the cells around them.

**On the grid, storm-based warnings count where they were drawn.** A cell
counts a polygon warning when the polygon covered its centre, over the times
that version of the polygon was in force, so a warning trimmed by a later
statement stops covering the trimmed-off cells when it was trimmed. That covers
tornado, severe thunderstorm, flash flood, snow squall and dust storm warnings
throughout, and most flood products once they carried polygons. A polygon
narrower than a cell takes the nearest cell within 5 km (2,030 of 2,337 such
polygons; the rest lie offshore). Every other alert covers all the cells of the
zones and counties it names, by cell centre, using the IEM outlines the county
view uses. A day a cell spends under both kinds of alert of one product counts
once. Polygon attributes are read from the CSV, because GDAL returns the DBF's
event number empty for polygon rows; 04 stops if polygons stop matching their
alerts' zone rows, which would count them twice.

**The grid and county views answer different questions.** A county counts a
day when any meaningful part of it was under the alert, so for storm-based
warnings it is at or above its cells: Cleveland County, OK has 2.6 tornado
warning days a year, its cells about 1. For zone products a cell can exceed its
county where a zone covers too little of the county (under 5%) to count for it:
the Sierra snow zone that clips Yuba County, CA gives those cells 29 winter
storm warning days a year against the county's 4.

**Map classes are septiles**, rounded to readable numbers, of the counties (or
offices) with any; counties with none are left unfilled. With a state chosen,
that state's counties set the classes. The ramp is one blue in seven steps of
even lightness from the IPPRA dataviz palette, darkest for the most on the
light map and lightest for the most on the dark map.

## Data format

`05_build_site_data.R` writes one JSON file per product, named with a content
hash so a host can cache it forever:

```
{ id, national: { a: [alerts by year x month], d: [days by year x month] },
  counties: { i: [county index], ay, dy: [by year, flattened], am, dm: [by month] },
  offices:  { i, ay, dy, am, dm } }
```

Only counties and offices with any appear. `manifest.json` lists the years,
counties, states, offices and products (with each file name) and the crosswalk
rules, and the grid's domains and per-product grid files. `counties.geojson` and `offices.geojson` carry an index `i` into the
manifest, and are simplified to about 4% and 0.6% of their vertices.

## The front end

`site/` is hand-edited: `index.html`, `engine.js`, `engine.css`, and MapLibre
GL JS 6.10.0 vendored under `site/assets/vendor/`. The IPPRA bar, panel and
controls are the same as ok_fire_dash's.

`04_build_grid.R` writes, for each land product, `<product>_days_<hash>.bin.gz`
and `<product>_alerts_<hash>.bin.gz`: gzipped unsigned 8-bit integers (16-bit
where a value exceeds 255, noted in the manifest), one per cell per year, year
by year in cell order. Cells are numbered domain by domain, row by row from the
top left. `mask.bin.gz` holds one bit per slot of each domain's rectangle
(least significant first) marking land cells, `county.bin.gz` a 16-bit county
index per cell, and `grid.json` each domain's projection, origin and size. A
file is 0.1-2 MB compressed and loads only when someone views that product on
the grid.

The grid is drawn as raster tiles the page paints itself through a MapLibre
custom protocol (`nacgrid://`): each tile pixel is projected into the grid's
Albers coordinates and takes its cell's color, so cell edges stay exact at
every zoom. Browsers inflate the files with `DecompressionStream`.

The only third-party requests are CARTO base map tiles. If they fail, the
counties still draw on a plain background.

## Hosting

`outputs/06_site/` is the whole site: plain static files, no server code, so it
can be copied to ippra.net like the other dashboards or published with GitHub
Pages.

- `index.html` must be served with `Cache-Control: no-cache`.
- `engine.js`, `engine.css` and the data files carry a `?v=<build>` stamp, and
  product files a content hash, so they can be cached as long as a host likes.
- The site is 68 MB on disk, but a visit loads only the manifest, the outlines
  and the products it opens: about 1 MB compressed for the first view, plus
  0.1-2 MB for each product viewed on the grid.
- The grid files are already gzipped. A host may serve them as they are or with
  `Content-Encoding: gzip`; the page handles both.

`06_build_dashboard.R` builds into `outputs/06_site.next` and swaps it in, so a
host serving `outputs/06_site` never sees a half-copied site.

## Sources

| input | source |
|---|---|
| Watches, warnings and advisories | Iowa Environmental Mesonet, [NWS watch/warning archive](https://mesonet.agron.iastate.edu/request/gis/watchwarn.phtml); 2010-2024 downloaded January 2025, 2025 and boundary windows September 2026 |
| Counties | U.S. Census Bureau, [cartographic boundary file 2023, 1:500,000](https://www2.census.gov/geo/tiger/GENZ2023/shp/cb_2023_us_county_500k.zip) |
| County time zones and offices | NWS, [counties, 16 April 2026](https://www.weather.gov/gis/Counties) |
| Forecast office boundaries | NWS, [county warning areas, 16 April 2026](https://www.weather.gov/gis/CWABounds) |
| Product names and groups | `reference/products.csv`, from NWS Directive 10-1703 |

# Run the Pipeline -------------------------------------------------------------
# Refresh the archive, build the crosswalk, counts and grid, assemble the site.
# Each script still runs standalone, and each stops loudly on a problem, which
# stops the scripts after it: the last good site stays in place rather than
# being replaced by a quietly wrong one.

scripts <- c(
  "01_refresh_data.R",
  "02_build_crosswalk.R",
  "03_build_counts.R",
  "04_build_grid.R",
  "05_build_site_data.R",
  "06_build_dashboard.R"
)

for (script in scripts) {
  message("\n== ", script, " ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  source(here::here(script), local = new.env())
}

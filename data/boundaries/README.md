# County boundaries

Place a county (first-level administrative) boundary file here and set `paths.boundaries_file`
in `config/config.yml`. GeoJSON, GeoPackage or shapefile all work. Requirements:

1. One polygon per county (47 for Kenya), in any coordinate system (it is transformed to WGS84).
2. An attribute with the county name. Set its name in `spatial.boundary_name_field`; common
   alternatives (`shapeName`, `NAME_1`, `ADM1_EN`, `COUNTY_NAM`) are detected automatically.
3. Names are matched to the DHS value labels after lower-casing and removing spaces, hyphens,
   apostrophes and the word "county". If a name still does not match, the pipeline stops and
   lists the unmatched names; add the spelling to `spatial.name_aliases`.

Free sources (check the licence before redistributing): geoBoundaries
(<https://www.geoboundaries.org>, KEN ADM1), GADM (<https://gadm.org>, level 1), the DHS
Spatial Data Repository (<https://spatialdata.dhsprogram.com>, survey regions), or the Kenya
national mapping authority. Boundary files are git-ignored by default.

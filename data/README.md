# Satellite Recon Data

Datasets on US satellite surveillance of nuclear activity from 1941-1985. The satellite imagery datasets were from downloaded from [USGS Earth Explorer](https://earthexplorer.usgs.gov/). 

There are also datasets of nuclear facilities (`facilities.csv`) and missile sites (`missiles.csv`), along with metadata. These were gathered by Quido Haskovec. The document `CoronaProject_Sources.docx` contains information on the methods used to gather these datasets; note that the provided coordinated are of highly limited precision.

The "captures" datasets (`fac_caps_no_unknown.csv`, `fac_caps_with_unknown.csv`, `miss_captures.csv`) were obtained by taking the geographic intersection of the `facilities` and `missiles` coordinates with those of the satellite images. Since the source coordinates are imprecise and much of the imagery is low-definition, low-quality, or cloud-covered, these only represent *candidate* captures, not cases where nuclear targets are actually visible.

## Datasets:
1. `sat.csv` contains various metadata (including geometries) for every satellite image in the dataset. It is a concatenation of `sat1.csv`, `sat2.csv`, and `sat3.csv`, along with various conflict resolutions & some minor cleaning.
2. `sat1.csv` contains metadata for the [first declassification in 1995](https://www.usgs.gov/centers/eros/science/usgs-eros-archive-declassified-data-declassified-satellite-imagery-1).
3. `sat2.csv` contains metadata for the [second declassification in 2002](https://www.usgs.gov/centers/eros/science/usgs-eros-archive-declassified-data-declassified-satellite-imagery-2).
4. `sat3.csv` contains metadata for the [third declassification in 2011](https://www.usgs.gov/centers/eros/science/usgs-eros-archive-declassified-data-declassified-satellite-imagery-3).
4. `facilities.csv` contains data on 157 nuclear facilities.
5. `fac_caps_no_unknown.csv` contains an entry for every image from `sat.csv` where the image's geometry contained the coordinates of a facility in `facilities.csv`, excluding facilities with unknown start dates and cases where facilities' construction began after the photo was taken. This does not mean the facility is visible in the actual photo.
6. `fac_caps_with_unknown.csv` is the same as `fac_captures.csv`, but facilities with unknown start dates are not filtered out.
7. `missiles.csv` contains data on 40 Soviet missile sites.
8. `miss_captures.csv` contains an entry for every image from `sat.csv` where the image's geometry contained the coordinates of a missile site in `missiles.csv`, excluding missiles that did not exist when the image was taken.

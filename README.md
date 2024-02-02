# satellite_data
 
Scripts to explore, clean, preprocess, and create intermediate analytical products for satellite reconnaissance data. Part of a project to explore U.S. satellite reconnaissance of nuclear targets from 1941-1985. The repository for the project's fuly-featured web application is [here]().

`analysis` contains exploratory scripts, descriptive writeups, and other intermediate analytical products. `apps` contains beta versions of some visualization apps. `data` contains all of the data, but I haven't been tracking these with version contorl because of file size constraints.

`find_captures.R` creates datasets containing potential capture occurrences using the information in the facilities, missiles, and satellite imagery datasets. It is these resultant datasets which are used in the web app.
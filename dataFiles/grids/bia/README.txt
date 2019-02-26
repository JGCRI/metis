Title: Global Power Plant Database
Description: A comprehensive, global, open source database of power plants
Version: 1.1.0
Release Date: 2018-06-11
URI: http://datasets.wri.org/dataset/globalpowerplantdatabase
Copyright: Copyright 2018 World Resources Institute and Data Contributors
License: Creative Commons Attribution 4.0 International -- CC BY 4.0
Contact: powerexplorer@wri.org
Citation: Global Energy Observatory, Google, KTH Royal Institute of Technology in Stockholm, Enipedia, World Resources Institute. 2018. Global Power Plant Database. Published on Resource Watch and Google Earth Engine. http://resourcewatch.org/ https://earthengine.google.com/  


Abstract:

An affordable, reliable, and environmentally sustainable power sector is central to modern society.
Governments, utilities, and companies make decisions that both affect and depend on the power sector.
For example, if governments apply a carbon price to electricity generation, it changes how plants run and which plants are built over time.
On the other hand, each new plant affects the electricity generation mix, the reliability of the system, and system emissions.
Plants also have significant impact on climate change, through carbon dioxide (CO2) emissions; on water stress, through water withdrawal and consumption; and on air quality, through sulfur oxides (SOx), nitrogen oxides (NOx), and particulate matter (PM) emissions.

Despite the importance of the power sector, there is no global, open-access database of power plants.
Existing databases fail to be either truly comprehensive or fully open.
Many countries do not report their power sector data at the plant level, and those that do vary wildly in what they report, how they report it, and how frequently they report.
The lack of reporting standards makes data gathering time intensive, as the data are in different formats and must be harmonized.
This creates a barrier for conducting global and national analysis of the power sector.

The Global Power Plant Database is an open-source open-access dataset of grid-scale (1 MW and greater) electricity generating facilities operating across the world.

The Database currently contains over 28,500 power plants in 164 countries, representing about 80% of the world's capacity.
Entries are at the facility level only, generally defined as a single transmission grid connection point.
Generation unit-level information is not currently available. 
The methodology for the dataset creation is given in the World Resources Institute publication "A Global Database of Power Plants" [0].
Associated code for the creation of the dataset can be found on GitHub [1].

To be stay updated with news about the project and future database releases, please sign up for our newsletter for the release announcement [2].


[0] www.wri.org/publication/global-power-plant-database
[1] https://github.com/wri/global-power-plant-database
[2] https://goo.gl/ivTvkd


Package Description:

The Global Power Plant Database is available as an archived directory of 3 files.
	- `global_power_plant_database.csv`: The core dataset of the world's power plants, released as a comma-delimited plain text file.
	- `RELEASE_NOTES.txt`: Information on the changes and history between database versions.
	- `README.txt` is this file: The metadata document.


File Description [global_power_plant_database.csv]

This file is a CSV with the following conventions:
	- file encoding: UTF-8
	- field delimiter: , (comma; 0x2C)
	- line terminator: \r\n (CRLF) (carriage-return line-feed; 0x0D 0x0A)
	- header line: true
	- field quoting: Only in instances where a double-quote (0x22) is contained within a text field; in which case the double-quote is escaped by a double-quote as in RFC 4180 2.7 [4].


[3] https://tools.ietf.org/html/rfc4180#section-2


Fields:

	- `country` (text): 3 character country code corresponding to the ISO 3166-1 alpha-3 specification [4]
	- `country_long` (text): longer form of the country designation
	- `name` (text): name or title of the power plant, generally in Romanized form
	- `gppd_idnr` (text): 10 or 12 character identifier for the power plant
	- `capacity_mw` (number): electrical generating capacity in megawatts
	- `latitude` (number): geolocation in decimal degrees; WGS84 (EPSG:4326)
	- `longitude` (number): geolocation in decimal degrees; WGS84 (EPSG:4326)
	- `fuel1` (text): energy source used in electricity generation or export
	- `fuel2` (text): energy source used in electricity generation or export
	- `fuel3` (text): energy source used in electricity generation or export
	- `fuel4` (text): energy source used in electricity generation or export
	- `commissioning_year` (number): year of plant operation, weighted by unit-capacity when data is available
	- `owner` (text): majority shareholder of the power plant, generally in Romanized form
	- `source` (text): entity reporting the data; could be an organization, report, or document, generally in Romanized form
	- `url` (text): web document corresponding to the `source` field
	- `geolocation_source` (text): attribution for geolocation information
	- `year_of_capacity_data` (number): year the capacity information was reported
	- `generation_gwh_2013` (number): electricity generation in gigawatt-hours reported for the year 2013 
	- `generation_gwh_2014` (number): electricity generation in gigawatt-hours reported for the year 2014
	- `generation_gwh_2015` (number): electricity generation in gigawatt-hours reported for the year 2015 
	- `generation_gwh_2016` (number): electricity generation in gigawatt-hours reported for the year 2016
	- `estimated_generation_gwh` (number): estimated annual electricity generation in gigawatt-hours for the year 2014 (see [0])


[4] https://www.iso.org/iso-3166-country-codes.html


Caveats:

For power plants that have data in multiple fuel fields, the ordering of the fuels should not be taken to indicate any priority or preference of the fuel for operating the power plant or generating units.
Though the fuel columns in the database are numbered sequentially from 1, the ordering is insignificant.

Generation is provided at the year scale for the years 2013-2016.
The generation values may correspond to a calendar year or a fiscal/regulatory year; no distinction is provided in the database.


Updates:

Contributions, suggestions, and corrections to the database are highly encouraged.
The proper channels for contributions are through opening a GitHub issue for the Global Power Plant Database [1] or by email [5].


[5] powerexplorer@wri.org



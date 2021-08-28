# rairtable
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

`rairtable` is an efficient, Tidyverse-friendly interface to Airtable API intended to simplify the integration of Airtable into data science workflows. Other R packages exist for this purpose, but `rairtable` offers the following advantages:

- Create, update, and delete Airtable records in batches of up to 10 at a time
- Optional parallelization for large tables
- Tidyverse conscious development, facilitating Airtable as an endpoint for `dplyr` pipelines
- Support for Airtable views
- Convenient interface for setting and updating Airtable API keys

# Installation

Install using `remotes::install_github('matthewjrogers/rairtable')`.

# Usage

## Get and set your API key
Generate an Airtable API key from your [Airtable account](http://airtable.com/account) page and pass the result to `set_airtable_api_key('MY_KEY_HERE')`. If you would like to store the key in your `.Renviron` file for use in the future, set `set_airtable_api_key('MY_KEY_HERE', install = TRUE)`.

## Connect to a table

```
table <- airtable('TABLE_NAME', 'BASE_ID')

view <- airtable('TABLE_NAME', 'BASE_ID', view = 'VIEW_NAME')

```
`rairtable::airtable()` creates an `airtable` object that is used in a similar fashion to a database connection. 

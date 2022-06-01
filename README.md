
# __rairtable__ 

### Efficient CRUD interface to the Airtable API


[![lifecycle](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://www.tidyverse.org/lifecycle/#stable)

`rairtable` is an efficient, Tidyverse-friendly interface to Airtable API intended to simplify the integration of Airtable into data science workflows. Other R packages exist for this purpose, but `rairtable` offers the following advantages:

- Create, update, and delete Airtable records in batches of up to 10 at a time
- Optional parallelization of JSON encoding for large tables
- Tidyverse conscious development, facilitating Airtable as an endpoint for `dplyr` pipelines
- Support for Airtable views
- Convenient interface for setting and updating Airtable API keys

*****

## __INSTALLATION__

Install using `remotes::install_github('matthewjrogers/rairtable')`.

*****

## __USAGE__

## Get and set your API key
Generate an Airtable API key from your [Airtable account](http://airtable.com/account) page and pass the result to `set_airtable_api_key('MY_KEY_HERE')`. If you would like to store the key in your `.Renviron` file for use in the future, set `set_airtable_api_key('MY_KEY_HERE', install = TRUE)`.

## Connect to a table

```
table <- airtable('TABLE_NAME', 'BASE_ID')

view <- airtable('TABLE_NAME', 'BASE_ID', view = 'VIEW_NAME')

```
`rairtable::airtable()` creates an `airtable` object that is used in a similar fashion to a database connection. The resulting object is then passed to other `rairtable` functions.

## Read a table

```
airtable_data <- read_airtable(airtable_object, id_to_col = TRUE, max_rows = 50000)
```

By default, `read_airtable()` will read all rows in the chosen table and store Airtable records IDs in a column named `airtable_record_id`. Optionally, airtable record IDs can be stored as row names for convenience.

Row names are bad practice in most cases. For this application, they offer the advantage of being sticky through most subset and transform operations which allows us to retain the record ID by default. Some operations will destroy row names (e.g. the use of `dplyr::arrange()`). For these reasons, the default behavior is to store the record IDs in a column. Airtable record IDs are necessary for update and delete operations, but otherwise can be ignored.

## Workflow

`rairtable` is intended to slot into existing data processing workflows.

```
table <- airtable('mtcars', 'appXXXXXXXXXXXXXX')

cars_airtable <- read_airtable(table)

# change units of qsec to minutes
cars_airtable <- cars_airtable %>%
  mutate(qsec = qsec/60) %>%
  update_records(table, columns = qsec)

# remove records where mpg is less than 12
removed_ids <- cars_airtable %>%
  filter(mpg < 12) %>%
  delete_records(table)

cars_airtable %>%
  filter(!airtable_id %in% removed_ids)

```

*****

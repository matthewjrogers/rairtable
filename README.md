
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rairtable

<!-- badges: start -->

[![R-CMD-check](https://github.com/matthewjrogers/rairtable/workflows/R-CMD-check/badge.svg)](https://github.com/matthewjrogers/rairtable/actions)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/rairtable)](https://cran.r-project.org/package=rairtable)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/rairtable)](https://cran.r-project.org/package=rairtable)
<!-- badges: end -->

`rairtable` is an efficient interface to the Airtable API intended to
simplify the integration of Airtable into data science workflows. Using
`rairtable` you can:

- Set and get Airtable API keys and personal access tokens
- Create, update, and delete Airtable records
- Use Airtable as an endpoint for `dplyr` pipelines with
  tidyverse-friendly functions
- Get filtered data from Airtable views

At present, `rairtable` does not support any API endpoints that are
restricted to “Enterprise” accounts (for views, permissions, and org
management) or any of the endpoints for the [webhooks
API](https://airtable.com/developers/web/api/webhooks-overview).
However, the `request_airtable()` and `req_airtable()` functions should
make it straightforward for others to extend this package and expand
coverage of the API. See the [Airtable Web API
changelog](https://airtable.com/developers/web/api/changelog) for any
recent updates to the API.

## Installation

For the stable CRAN release, you can install rairtable with:

``` r
install.packages('rairtable')
```

You can install the development version of rairtable from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pkg_install("matthewjrogers/rairtable@dev")
```

## Usage

``` r
library(rairtable)
```

### Get and set a personal access token (PAT)

Create a personal access token [using your Airtable
account](https://airtable.com/create/tokens) to access the API.

Save the token to your local environment using `set_airtable_api_key()`
and set `install = TRUE` to save the token for reuse in future sessions:

``` r
set_airtable_pat('<your key goes here>')

set_airtable_pat('<your key goes here>', install = TRUE)
```

This package does support Airtable API keys, however, Airtable support
for API keys [ends in January
2024](https://support.airtable.com/docs/airtable-api-key-deprecation-notice).
Any script using an API key should be updated to use a personal access
token instead.

### Create an `airtable` object and reading records

This README uses a sample Airtable copied from the [Emojis
Database](https://www.airtable.com/universe/exphjm5ifnV0bX4Kb/emojis-database)
in the [Airtable Universe](https://www.airtable.com/universe).

Use an Airtable base and table ID to create an `airtable` object:

``` r
atbl <- airtable(
  base = "app8uSq5M3ia6FZUR",
  table = "tblvGyhX2yg0d22gz"
)

atbl
#> <airtable/vctrs_vctr/list>
#> ── Emojis ──────────────────────────────────────────────────────────────────────
#> • Base: app8uSq5M3ia6FZUR
#> • Table: tblvGyhX2yg0d22gz
#> ── <https://airtable.com/app8uSq5M3ia6FZUR/tblvGyhX2yg0d22gz> ──────────────────
```

The airtable object is a list that holds your base ID and a request URL.

You can also use the URL for your base to create an airtable object.
Note, that the URL typically includes a view as well as a table. If a
view is filtered to only show a subset of records, only the records you
can see in that view can be listed if you pass the airtable object to
`read_records()` or `list_records()`.

``` r
atbl <- airtable(
  table = "https://airtable.com/app8uSq5M3ia6FZUR/tblvGyhX2yg0d22gz"
)
```

You can read records to a data frame using `read_airtable()`:

``` r
records <- read_airtable(airtable = atbl)

records
#> # A tibble: 1,165 × 5
#>    airtable_record_id Description               Emoji Type             Name     
#>    <chr>              <chr>                     <chr> <chr>            <chr>    
#>  1 rec06W7BBIWlQOHuu  Wedding                   💒    Travel & Places  💒 Weddi…
#>  2 rec08do2AoZYmCjFu  Stop Sign                 🛑    Symbols          🛑 Stop …
#>  3 rec0BjxZID8hwEoTR  Hand With Fingers Splayed 🖐     Smileys & People 🖐 Hand W…
#>  4 rec0EWZon2777A7qO  Family: Woman, Boy        👩‍👦    Smileys & People 👩‍👦 Famil…
#>  5 rec0EzAj9ZN7NDRBt  Game Die                  🎲    Activity         🎲 Game …
#>  6 rec0GH7OYgPEjRaSb  Family: Woman, Boy, Boy   👩‍👦‍👦    Smileys & People 👩‍👦‍👦 Famil…
#>  7 rec0JiPbOdtdc0kFe  Woman Factory Worker      👩‍🏭    Smileys & People 👩‍🏭 Woman…
#>  8 rec0KMMjZcvjC52cU  Delivery Truck            🚚    Travel & Places  🚚 Deliv…
#>  9 rec0LrntBIo1dC945  Woman Wearing Turban      👳‍♀️    Smileys & People 👳‍♀️ Woman…
#> 10 rec0OurcoK4dQG4DD  Outbox Tray               📤    Objects          📤 Outbo…
#> # ℹ 1,155 more rows
```

By default, `read_airtable()` will read all rows in the chosen table and
store Airtable records IDs in a column named `airtable_record_id`.
Optionally, airtable record IDs can be stored as row names for
convenience. However, this is not recommend in most cases since some
operations such as `dplyr::arrange()` will drop row names. You can
record ID column name by setting the option for `rairtable.id_col`.

You can get a single record with `get_record()`:

``` r
get_record(atbl, record = "rec0GH7OYgPEjRaSb")
#>   airtable_record_id              createdTime             Description  Emoji
#> 1  rec0GH7OYgPEjRaSb 2018-10-25T22:14:35.000Z Family: Woman, Boy, Boy 👩‍👦‍👦
#>               Type                           Name
#> 1 Smileys & People 👩‍👦‍👦 Family: Woman, Boy, Boy
```

`list_records()` is similar to `read_airtable()` but supports some
additional features from the API.

### Insert records

Records can be added to an Airtable base by passing a data frame to
`insert_records()` or `create_records()` (the two functions are
identical):

``` r
data <- tibble::tibble(
  "Emoji" = "🤖",
  "Description" = "README robot"
)

resp <- insert_records(
  airtable = atbl,
  data = data,
  return_data = FALSE
)
#> ℹ 1 record created.
#> ✔ 1 record created. [6ms]
#> 

get_record(airtable = atbl, record = resp$records[[1]]$id)
#>   airtable_record_id              createdTime Emoji  Description
#> 1  recc7OPbDzh7vUib0 2023-08-07T02:08:02.000Z    🤖 README robot
#>              Name
#> 1 🤖 README robot

delete_records(airtable = atbl, records = resp$records[[1]]$id, safely = FALSE)
#> ℹ 1 record deleted.
#> ✔ 1 record deleted. [7ms]
#> 
```

The functions do not currently validate data so the column names and
types must match the names and types in the Airtable where the records
will be created. You can also delete records using `delete_records()`.

### Access the Airtable Metadata API

The Airtable Metadata API requires a personal access token and will not
work with an API key. The `airtable_base()` function returns a list
containing the base ID, `airtable_base_schema` object, and an airtable
object for each table in the base.

``` r
airtable_base(atbl)
#> $base
#> [1] "app8uSq5M3ia6FZUR"
#> 
#> $schema
#> <airtable_base_schema/vctrs_vctr/list>
#> ─ 1 table:
#> • Emojis2 - tblvGyhX2yg0d22gz
#> 
#> $tables
#> $tables[[1]]
#> <airtable/vctrs_vctr/list>
#> ── Emojis ──────────────────────────────────────────────────────────────────────
#> • Base: app8uSq5M3ia6FZUR
#> • Table: "Emojis2" - tblvGyhX2yg0d22gz
#> • 4 fields including Name, Type, Emoji, and Description.
#> ── <https://airtable.com/app8uSq5M3ia6FZUR/tblvGyhX2yg0d22gz> ──────────────────
```

`get_table_models()` is more basic alternative by simply returning a
tibble of tables with list columns for views and fields associated with
each table.

``` r
models <- get_table_models(atbl)

models
#> # A tibble: 1 × 5
#>   id                name    primaryFieldId    fields       views       
#>   <chr>             <chr>   <chr>             <list>       <list>      
#> 1 tblvGyhX2yg0d22gz Emojis2 fld72YY9UFba12LeK <df [4 × 4]> <df [4 × 3]>
```

A table model can be converted into a table configuration object which
is similar but has none of the field IDs that are unique to the source
Airtable:

``` r
config <- copy_table_config(table = models$name, base = atbl$base)

str(config)
#> List of 2
#>  $ name  : chr "Emojis2"
#>  $ fields:List of 4
#>   ..$ :List of 3
#>   .. ..$ name   : chr "Name"
#>   .. ..$ type   : chr "formula"
#>   .. ..$ options:List of 3
#>   .. .. ..$ isValid           : logi TRUE
#>   .. .. ..$ referencedFieldIds:List of 2
#>   .. .. .. ..$ : chr "fldiYDAKVUmtm2csm"
#>   .. .. .. ..$ : chr "fldCunC28DcGDI6vO"
#>   .. .. ..$ result            :List of 1
#>   .. .. .. ..$ type: chr "singleLineText"
#>   ..$ :List of 3
#>   .. ..$ name   : chr "Type"
#>   .. ..$ type   : chr "singleSelect"
#>   .. ..$ options:List of 1
#>   .. .. ..$ choices:List of 8
#>   .. .. .. ..$ :List of 2
#>   .. .. .. .. ..$ name : chr "Smileys & People"
#>   .. .. .. .. ..$ color: chr "blueLight2"
#>   .. .. .. ..$ :List of 2
#>   .. .. .. .. ..$ name : chr "Animals & Nature"
#>   .. .. .. .. ..$ color: chr "cyanLight2"
#>   .. .. .. ..$ :List of 2
#>   .. .. .. .. ..$ name : chr "Food & Drink"
#>   .. .. .. .. ..$ color: chr "tealLight2"
#>   .. .. .. ..$ :List of 2
#>   .. .. .. .. ..$ name : chr "Activity"
#>   .. .. .. .. ..$ color: chr "greenLight2"
#>   .. .. .. ..$ :List of 2
#>   .. .. .. .. ..$ name : chr "Travel & Places"
#>   .. .. .. .. ..$ color: chr "yellowLight2"
#>   .. .. .. ..$ :List of 2
#>   .. .. .. .. ..$ name : chr "Objects"
#>   .. .. .. .. ..$ color: chr "orangeLight2"
#>   .. .. .. ..$ :List of 2
#>   .. .. .. .. ..$ name : chr "Symbols"
#>   .. .. .. .. ..$ color: chr "redLight2"
#>   .. .. .. ..$ :List of 2
#>   .. .. .. .. ..$ name : chr "Flags"
#>   .. .. .. .. ..$ color: chr "pinkLight2"
#>   ..$ :List of 2
#>   .. ..$ name: chr "Emoji"
#>   .. ..$ type: chr "singleLineText"
#>   ..$ :List of 2
#>   .. ..$ name: chr "Description"
#>   .. ..$ type: chr "singleLineText"
```

This table configuration can be used by `create_table()` or
`create_base()` to create a new Airtable base by using the schema of an
existing base.

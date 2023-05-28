
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
  table = "https://airtable.com/app8uSq5M3ia6FZUR/tblvGyhX2yg0d22gz/viw4HALSnD2HQMOHs"
)
```

You can read records to a data frame using `read_airtable()`:

``` r
records <- read_airtable(airtable = atbl)

records
#> # A tibble: 1,160 × 5
#>    airtable_record_id Description       Emoji Type             Name             
#>    <chr>              <chr>             <chr> <chr>            <chr>            
#>  1 recquG0MvrVdIVYqs  Zzz               💤    Symbols          💤 Zzz           
#>  2 recAM3FvVWbi7a4pC  Zombie            🧟    Smileys & People 🧟 Zombie        
#>  3 recCGegvKOI4aoNrO  Zipper-Mouth Face 🤐    Smileys & People 🤐 Zipper-Mouth …
#>  4 rec0SP7HBGjfYE25v  Zebra             🦓    Animals & Nature 🦓 Zebra         
#>  5 recpL1l6rxtI5bb1L  Zany Face         🤪    Smileys & People 🤪 Zany Face     
#>  6 recKoXUv3RKKSSqSI  Yen Banknote      💴    Travel & Places  💴 Yen Banknote  
#>  7 recAyqvduNIUxgc9w  Yen Banknote      💴    Objects          💴 Yen Banknote  
#>  8 rec9oHV0vSvzCsFEd  Yellow Heart      💛    Symbols          💛 Yellow Heart  
#>  9 reczIIl1w5TRYVvj1  Yarn              🧶    Smileys & People 🧶 Yarn          
#> 10 recZa04sGTylDE2yI  Yarn              🧶    Activity         🧶 Yarn          
#> # ℹ 1,150 more rows
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

insert_records(
  airtable = atbl,
  data = data
)
#> ℹ 1 record created.
#> ✔ 1 record created. [12ms]
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
#> • Emojis - tblvGyhX2yg0d22gz
#> 
#> $tables
#> $tables[[1]]
#> <airtable/vctrs_vctr/list>
#> ── Emojis ──────────────────────────────────────────────────────────────────────
#> • Base: app8uSq5M3ia6FZUR
#> • Table: "Emojis" - tblvGyhX2yg0d22gz
#> • 4 fields including Name, Type, Emoji, and Description.
#> ── <https://airtable.com/app8uSq5M3ia6FZUR/tblvGyhX2yg0d22gz> ──────────────────
```

`list_base_tables()` is more basic alternative by simply returning a
tibble of tables with list columns for views and fields associated with
each table.

``` r
list_base_tables(atbl)
#> # A tibble: 1 × 5
#>   id                name   primaryFieldId    fields       views       
#>   <chr>             <chr>  <chr>             <list>       <list>      
#> 1 tblvGyhX2yg0d22gz Emojis fld72YY9UFba12LeK <df [4 × 4]> <df [4 × 3]>
```

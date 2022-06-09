## Resubmission
This is a resubmission. In this version I have:
- Added single quotes around the name 'Airtable' in the DESCRIPTION file
- Added a link to the Airtable API in the Description field of the DESCRIPTION file, per request
- Added `\value` tags to `insert_records.Rd`
- Added `\value` tags to `read_airtable.Rd`
- Added `\value` tags to `set_airtable_api_key.Rd`
- Added `\value` tags to `update_records.Rd`
- Changed calls to `cat` to calls to `message` such that they can be suppressed if desired. Because these processes can be long-running, I have received feedback from users that these messages are helpful and so I opt to retain the verbose behavior by default.


## Test Environments

- Local Windows 10 install, R 4.0.4
- Mac OS (vie GitHub actions, R-devel)
- Ubuntu Linux 20.04.1 LTS (on R-hub), R 4.1.2
- Fedora Linux (on R-hub) R-devel
- Windows (on R-hub, devel and release)

## R CMD check results
There were no ERRORs, WARNINGs. 


### NOTES

There is one NOTE that is only found on Windows (Server 2022, R-devel 64-bit):

First NOTE:

```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```
As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this is likely due to a bug in MiKTeX and can be ignored.

There is an additional NOTE that occurs across multiple platforms.

```
Possibly misspelled words in DESCRIPTION:
  Airtable (3:34, 11:47)
```
"Airtable" is the correct spelling, but is not a dictionary word. For this reason, I believe this note can be safely ignored.

In addition, note that this is my first CRAN submission which may yield an additional NOTE
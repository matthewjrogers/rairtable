## R CMD check results
There were no ERRORs or WARNINGs. 


### NOTES

There were two NOTES.

One NOTE is only found on Windows (Server 2022, R-devel 64-bit):

```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```
As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this is likely due to a bug in MiKTeX and can be ignored.


The second NOTE was found on Fedora Linux (on R-hub) R-devel

```
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
```

This behavior is noted in [R-hub issue #548](https://github.com/r-hub/rhub/issues/548) and I believe is a problem in the R-hub system, not in the `rairtable` code, and can be ignored.

## Test Environments

- Local Windows 10 install, R 4.0.4
- Mac OS (via GitHub actions, R-devel)
- Ubuntu Linux 20.04.1 LTS (on R-hub), R 4.1.2
- Fedora Linux (on R-hub) R-devel
- Windows (on R-hub, devel and release)
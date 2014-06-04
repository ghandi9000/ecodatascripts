Data management
================

This is a work in progress, slowing moving all the data creating/modification scripts to this repo

Dependencies
------------

### Data

* [data](http://github.com/ghandi9000/data)

### Some functions

* [functions](http://github.com/ghandi9000/functions)


Directory Layout
----------------

Ideally each innermost subdirectory has its own readme.

* **read**: scripts for reading data from master files
* **clean**: scripts to do preliminary cleaning of data (i.e. lowercase, remove unecessary columns, rename columns, etc)
* **trans**: scripts that transform the data (i.e. wide->long)
* **vars**: scripts to create new variables
* **recreate**: Scripts to recreate data sets.  These tie the read/clean/transform into the proper order.

Issues
------

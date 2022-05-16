# AnvilDataModels

## R package to work with AnVIL data models

This R package provides utilities for:

- converting a data model from TSV to DBML format
- checking a set of data tables against a data model
- generating a markdown report summarizing data model checks
- importing data tables into AnVIL

## Data model specification

Data models should be specified in one or more TSV (tab separated value) files. The format is inspired by [DBML](https://www.dbml.org/home/) and allows for easy conversion. Column names are:

column | description
--- | ---
entity | 'Table' or 'enum'
table | Table name
column | Column name
type | Column data type; must be one of 'string', 'integer', 'float', 'boolean', 'date', 'dateTime' or the name of an enumerated set defined in this model
required | TRUE if column is required
pk | TRUE if column is a primary key
ref | Cross reference to another table in [DBML format](https://www.dbml.org/docs/#relationships-foreign-key-definitions); e.g., '> sample.sample_id'
note | Description of column

If entity is 'enum', 'table' should contain the name of the enumerated set (e.g., 'sex') and 'column'
should contain a possible value (e.g., 'F', 'M', 'X'), with one row per possible values. All other columns should be left blank.

[Example data model file](inst/extdata/data_model.tsv)

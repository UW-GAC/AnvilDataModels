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
entity | 'Table', 'enum' or 'meta'
table | Table name
column | Column name
type | Column data type; must be one of 'string', 'integer', 'float', 'boolean', 'date', 'dateTime' or the name of an enumerated set defined in this model
required | TRUE if column is required. 'CONDITIONAL (column = value)' indicates a requirement only if any element of 
   'column' contains 'value'.
pk | TRUE if column is a primary key
ref | Reference to other columns in the data model. Either
    1) a cross-table reference string following the [DBML
format](https://www.dbml.org/docs/#relationships-foreign-key-definitions);
e.g., '> sample.sample_id', or
     2) 'from:' followed by a comma-separated list of columns used to
     automatically generate this column using a hash function.
note | Description of column

Enumerated values are specified in rows where entity is 'enum'. 'table' should contain the name of the enumerated set (e.g., 'sex') and 'column'
should contain a possible value (e.g., 'F', 'M', 'X'), with one row per possible value. All other columns should be left blank.

Required tables are specified in rows where entity is 'meta', 'table' is the name of the table, and 'required' is TRUE.  To indicate that table 't2' is only required if table 't1' is 
   present, entity='meta', table='t2' and required='CONDITIONAL (t1)'.

[Example data model file](inst/extdata/data_model.tsv)
[Example data model with an automatically-generated column](inst/extdata/data_model_auto_id.tsv)
[Example data model with conditional requirements](inst/extdata/data_model_conditional.tsv)

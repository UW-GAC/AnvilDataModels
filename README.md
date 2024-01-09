# AnvilDataModels

## R package to work with AnVIL data models

This R package provides utilities for:

- converting a data model from TSV to DBML format
- checking a set of data tables against a data model
- generating a markdown report summarizing data model checks
- importing data tables into AnVIL

## Data model specification

Data models should be specified in one or more JSON files.

The JSON file should contain a list "tables". Each table object should
contain the following elements: 

- table: table name
- columns: list of "column" objects
- required: TRUE indicates the table is required, FALSE or missing if the table
   is optional. 'CONDITIONAL (t1)' indicates that a table is only required if table 't1' is 
   present.

Each column object can contain the following elements. 'column' and 'data_type' are required.

- column: column name
- data_type: "string", "boolean, "integer", "float", "date", "dateTime", or "enumeration"
- enumerations: if data_type is enumeration, list values here
- multi_value_delimiter: if data values may be delimited, this contains the delimiter value
- required: TRUE indicates the column is required, FALSE or missing if the column
   is optional. 'CONDITIONAL (column = value)' indicates a requirement only if any element of 
   'column' contains 'value'. 'CONDITIONAL (column)' indicates a requirement only if any element
   of 'column' is non-missing.
- primary_key: logical where TRUE indicates the column is a primary key, 
    other values may be FALSE or missing
- references: Reference to other columns in the data model. Either of
    - a cross-table reference string following the 
    [DBML](https://www.dbml.org/docs/#relationships-foreign-key-definitions)
    format
    - 'from:' followed by a comma-separated list of columns used to
    automatically generate this column using a hash function.
- description: Note string (column description) following the
    [DBML](https://www.dbml.org/docs/#column-notes) format

Examples

- [Example data model file](inst/extdata/data_model.json)
- [Example data model with an automatically-generated column](inst/extdata/data_model_auto_id.json)
- [Example data model with conditional requirements](inst/extdata/data_model_conditional.json)

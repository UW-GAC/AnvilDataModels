#' Check tables against data model
#' 
#' Read a set of files containing data tables and check them against a data model.
#' 
#' @name check_data_tables
#' @param files Vector of file paths, one per data table.
#' @param table_names Vector of table names associated with \code{files}.
#' @param quiet Logical to control printing results of column parsing from \code{\link{read_tsv}}.
#' @return \code{read_data_tables} returns a named list of data frames.
#' 
#' @examples
#' # read data model
#' json <- system.file("extdata", "data_model.json", package="AnvilDataModels")
#' model <- json_to_dm(json)
#' 
#' # read tables to check
#' table_names <- c("subject", "phenotype", "sample", "sample_set", "file")
#' files <- system.file("extdata", paste0(table_names, ".tsv"), package="AnvilDataModels")
#' names(files) <- table_names
#' tables <- read_data_tables(files)
#' 
#' check_table_names(tables, model)
#' check_column_names(tables, model)
#' check_column_types(tables, model)
#' check_primary_keys(tables, model)
#' check_foreign_keys(tables, model)
#' 
#' @importFrom readr read_tsv cols col_character
#' @export
read_data_tables <- function(files, table_names=names(files), quiet=TRUE) {
    names(files) <- table_names
    lapply(files, function(f) {
        t <- read_tsv(f, col_types=cols(.default=col_character()))
        cols_no_id <- names(t)[!grepl("_id$", names(t))] # id columns should always be character
        for (c in cols_no_id) {
            t[[c]] <- utils::type.convert(t[[c]], as.is=TRUE)
        }
        t
    })
}


.parse_required_tables <- function(table_names, model) {
    required <- attr(model, "required")
    cond <- attr(model, "conditions")
    for (c in names(cond)) {
        # if condition is met, add to 'required'
        if (any(cond[[c]] %in% table_names)) {
            required <- c(required, c)
        }
    }
    optional <- setdiff(names(model), required)
    return(list(required=required, optional=optional))
}

#' @rdname check_data_tables
#' @param tables Named list of data tables
#' @param model \code{\link{dm}} object describing data model
#' @return \code{check_table_names} returns \code{NULL} if \code{tables} matches \code{model}, 
#'     or a list:
#'     \itemize{
#'         \item{missing_tables: }{Vector of tables in \code{model} but not in \code{tables}}
#'         \item{extra_tables: }{Vector of tables in \code{tables} but not in \code{model}}
#'     }
#'     
#' @export
check_table_names <- function(tables, model) {
    if (setequal(names(tables), names(model))) {
        return(NULL)
    } else {
        req <- .parse_required_tables(names(tables), model)
        required <- req$required
        optional <- req$optional
        return(list(missing_required_tables=setdiff(required, names(tables)),
                    missing_optional_tables=setdiff(optional, names(tables)),
                    extra_tables=setdiff(names(tables), names(model))))
    }
}


#' @importFrom stringr str_trim
.parse_condition <- function(x) {
    tmp <- unlist(strsplit(x, "="))
    column <- str_trim(tmp[1])
    value <- str_trim(tmp[2])
    return(list(column=column, value=value))
}

.parse_required_columns <- function(table, model) {
    required <- attr(model, "required")
    cond <- attr(model, "conditions")
    # for all columns in names(cond)
    for (c in names(cond)) {
        for (v in cond[[c]]) {
            cond_parsed <- .parse_condition(v)
            column <- cond_parsed$column
            value <- cond_parsed$value
            # if condition is met, add to 'required'
            if (!is.na(value) & any(!is.na(table[[column]]) & table[[column]] == value)) {
                required <- c(required, c)
            }
            # if value is NA, only requirement is column is non-missing
            if (is.na(value) & any(!is.na(table[[column]]))) {
                required <- c(required, c)
            }
        }
    }
    optional <- setdiff(names(model), required)
    return(list(required=required, optional=optional))
}


#' @rdname check_data_tables
#' @return \code{check_column_names} return a list of all tables in common between data 
#'     and model. Each table element is \code{NULL} if columns in \code{tables} matches \code{model}, 
#'     or a list:
#'     \itemize{
#'         \item{missing_required_columns: }{Vector of required columns in \code{model} but not in \code{tables}}
#'         \item{missing_optional_columns: }{Vector of optional columns in \code{model} but not in \code{tables}}
#'         \item{extra_columns: }{Vector of columns in \code{tables} but not in \code{model}}
#'     }
#'     
#' @export
check_column_names <- function(tables, model) {
    common <- intersect(names(tables), names(model))
    chk <- lapply(common, function(t) {
        if (setequal(names(tables[[t]]), names(model[[t]]))) {
            return(NULL)
        } else {
            req <- .parse_required_columns(tables[[t]], model[[t]])
            required <- req$required
            optional <- req$optional
            return(list(missing_required_columns=setdiff(required, names(tables[[t]])),
                        missing_optional_columns=setdiff(optional, names(tables[[t]])),
                        extra_columns=setdiff(names(tables[[t]]), names(model[[t]]))))
        }
    })
    names(chk) <- common
    return(chk)
}


#' @importFrom stringr str_trim
.parse_delim <- function(x, table_name, column_name, model) {
    delim <- attr(model[[table_name]], "multi_value_delimiters")
    if (column_name %in% names(delim) & length(x) > 0) {
        x <- tryCatch({
            str_trim(unlist(strsplit(x, delim[column_name], fixed=TRUE)))
        }, warning=function(w) w, error=function(e) e)
    }
    return(x)
}

.try_conversion <- function(x, name, type, FUN, na_only=FALSE) {
    err_string <- paste("Some values of", name, "not compatible with", type, "type")
    err_fn <- function(a) {
        ac <- suppressWarnings(FUN(a))
        conv_fails <-  paste(setdiff(a, ac), collapse=", ")
        return(paste(err_string, conv_fails, sep=": "))
    }
    tryCatch({
        if (na_only & all(is.na(FUN(x)) == is.na(x))) {
            return(NULL)
        } else if (all(FUN(x) == x)) {
            return(NULL)
        } else {
            return(err_fn(x))
        }
    }, warning=function(w) err_fn(x), error=function(w) err_fn(x))
}

#' @rdname check_data_tables
#' @return \code{check_column_types} returns a list of all tables in common between data 
#'     and model. Each table element is a list of all columns in common between table and 
#'     model. Each column element is \code{NULL} if values in column are a compatible type 
#'     with the data model, or a string describing the mismatch.
#'     
#' @importFrom lubridate is.Date is.timepoint ymd ymd_hms
#' @importFrom methods is
#' @export
check_column_types <- function(tables, model) {
    common <- intersect(names(tables), names(model))
    chk <- lapply(common, function(t) {
        cols <- intersect(names(tables[[t]]), names(model[[t]]))
        chk2 <- lapply(cols, function(c) {
            name <- paste(t, c, sep=".")
            ct <- na.omit(tables[[t]][[c]]) # only check non-missing values
            ct <- .parse_delim(ct, t, c, model)
            if (is(ct, "error")) {
                return(paste("Error extracting delimited strings from", name, "\n", ct))
            }
            cm <- model[[t]][[c]]
            if (is.character(cm)) {
                .try_conversion(ct, name=name, type="character", FUN=as.character)
            } else if (is.logical(cm)) {
                .try_conversion(ct, name=name, type="boolean", FUN=as.logical)
            } else if (is.integer(cm)) {
                .try_conversion(ct, name=name, type="integer", FUN=as.integer)
            } else if (is.numeric(cm)) {
                .try_conversion(ct, name=name, type="float", FUN=as.numeric)
            } else if (is.Date(cm)) {
                .try_conversion(ct, name=name, type="date", FUN=ymd, na_only=TRUE)
            } else if (is.timepoint(cm)) {
                .try_conversion(ct, name=name, type="datetime", FUN=ymd_hms, na_only=TRUE)
            } else if (is.factor(cm)) {
                conv <- .try_conversion(ct, name=name, type="enum", 
                                FUN=function(x) factor(x, levels=levels(cm)))
                if (!is.null(conv)) {
                    return(paste0(conv, ". Allowed values: ", paste(levels(cm), collapse=", ")))
                } else {
                    return(conv)
                }
            } else {
                stop("unrecognized data type in model for ", t, ".", c)
            }
        })
        names(chk2) <- cols
        return(chk2)
    })
    names(chk) <- common
    return(chk)
}


#' @rdname check_data_tables
#' @return \code{check_column_types} returns a list of all tables in common between data 
#'     and model. Each table element is a list of all columns in common between table and 
#'     model that have min and/or max values. 
#'     Each column element is \code{NULL} if values in column are between min and max, 
#'     or a string describing the mismatch.
#'     
#' @export
check_column_min_max <- function(tables, model) {
    common <- intersect(names(tables), names(model))
    chk <- lapply(common, function(t) {
        min <- attr(model[[t]], "min")
        max <- attr(model[[t]], "max")
        if (is.null(min) & is.null(max)) return(NULL)
        cols <- intersect(names(tables[[t]]), union(names(min), names(max)))
        chk2 <- lapply(cols, function(c) {
            name <- paste(t, c, sep=".")
            ct <- na.omit(tables[[t]][[c]]) # only check non-missing values
            ct <- .parse_delim(ct, t, c, model)
            if (is(ct, "error")) {
                return(paste("Error extracting delimited strings from", name, "\n", ct))
            }
            ct <- tryCatch({
                as.numeric(ct)
            }, warning=function(w) w, error=function(e) e)
            if (is(ct, "error") | is(ct, "warning")) {
                return(paste("Error coercing", name, "to numeric \n", ct))
            }
            lt_min <- unique(ct[ct < min[c]])
            gt_max <- unique(ct[ct > max[c]])
            err_string <- character()
            if (!is.na(min[c]) & length(lt_min) > 0) {
                err_string["min"] <- paste("Values of", name, "<", min[c], ":", 
                                           paste(lt_min, collapse=", "))
            }
            if (!is.na(max[c]) & length(gt_max) > 0) {
                err_string["max"] <- paste("Values of", name, ">", max[c], ":", 
                                           paste(gt_max, collapse=", "))
            }
            if (length(err_string > 0)) {
             return(paste(err_string, collapse=". "))
            } else {
                return(NULL)
            }
        })
        names(chk2) <- cols
        return(chk2)
    })
    names(chk) <- common
    return(chk)
}


#' @rdname check_data_tables
#' @return \code{check_missing_values} returns a list of all tables in common between data 
#'     and model. Each table element is a list of all required columns in common between table and 
#'     model. Each column element is \code{NULL} if the column has no missing values, or 
#'     the number of missing values in the column. If a condition is set on a column, missing values 
#'     are only checked for rows where the condition is met.
#'     
#' @export
check_missing_values <- function(tables, model) {
    common <- intersect(names(tables), names(model))
    chk <- lapply(common, function(t) {
        cond <- attr(model[[t]], "conditions")
        cols <- intersect(names(tables[[t]]), names(model[[t]]))
        req <- .parse_required_columns(tables[[t]], model[[t]])
        cols <- intersect(cols, req$required)
        chk2 <- lapply(cols, function(c) {
            name <- paste(t, c, sep=".")
            ct <- tables[[t]][[c]]
            # if we have a condition, only check values where condition is met
            if (c %in% names(cond)) {
                cond_parsed <- .parse_condition(cond[[c]])
                ref_value <- tables[[t]][[cond_parsed$column]]
                value_req <- cond_parsed$value
                if (is.na(value_req)) {
                    ct <- ct[!is.na(ref_value)]
                } else {
                    ct <- ct[ref_value %in% value_req]
                }
            }
            missing <- sum(is.na(ct))
            if (missing > 0) {
                return(paste(missing, "missing values in required column", name))
            } else {
                return(NULL)
            }
        })
        names(chk2) <- cols
        return(chk2)
    })
    names(chk) <- common
    return(chk)
}


#' @rdname check_data_tables
#' @return \code{check_unique} returns a list of all tables in common between data 
#'     and model. Each table element is a list of all columns in common between table and 
#'     model also defined as unique by the model. Each column element is \code{NULL} if 
#'     the column is unique, or a string listing duplicated elements.
#' @export
check_unique <- function(tables, model) {
    common <- intersect(names(tables), names(model))
    chk <- lapply(common, function(t) {
        cols <- intersect(names(tables[[t]]), attr(model[[t]], "unique"))
        chk2 <- lapply(cols, function(c) {
            name <- paste(t, c, sep=".")
            ct <- na.omit(tables[[t]][[c]]) # only check non-missing values
            dups <- ct[duplicated(ct)]
            if (length(dups) > 0) {
                dup_str <- paste(unique(dups), collapse=", ")
                return(paste0("Duplicated values in unique column ", name, ": ", dup_str))
            } else {
                return(NULL)
            }
        })
        names(chk2) <- cols
        return(chk2)
    })
    names(chk) <- common
    return(chk)
}


#' @rdname check_data_tables
#' @return \code{check_bucket_paths} returns a list of all tables in common between data 
#'     and model. Each table element is a list of all columns in common between table and 
#'     model also defined as containing bucket paths by the model. Each column element is \code{NULL} if 
#'     all paths exist, or a string listing paths that do not exist.
#' @importFrom stringr str_extract str_detect
#' @export
check_bucket_paths <- function(tables, model) {
    # identify all the file paths in the data model
    common <- intersect(names(tables), names(model))
    bucket_cols <- lapply(common, function(t) {
        intersect(names(tables[[t]]), attr(model[[t]], "bucket_path"))
    })
    names(bucket_cols) <- common
    bucket_cols <- bucket_cols[!sapply(bucket_cols, is.null, USE.NAMES=FALSE)]
    
    # get the unique set of buckets
    buckets <- lapply(names(bucket_cols), function(t) {
        sapply(bucket_cols[[t]], function(c) {
            ct <- unique(na.omit(tables[[t]][[c]]))
            unique(na.omit(str_extract(ct, "gs://[[:alnum:]-]+/")))
        }, USE.NAMES = FALSE)
    }) %>% unlist() %>% unique()
    
    # get any buckets with a file in the submissions directory
    submissions <- lapply(names(bucket_cols), function(t) {
        sapply(bucket_cols[[t]], function(c) {
            ct <- unique(na.omit(tables[[t]][[c]]))
            sub("submissions/$", "", unique(na.omit(str_extract(ct, "gs://[[:alnum:]-]+/submissions/"))))
        }, USE.NAMES = FALSE)
    }) %>% unlist() %>% unique()
    
    # gsutil_ls on each bucket, combine to create files_in_buckets
    files_in_buckets <- lapply(buckets, function(b) {
        top_level <- gsutil_ls(b, recursive=FALSE)
        subdirs <- top_level[str_detect(top_level, "/$")]
        subdirs <- subdirs[!str_detect(subdirs, "notebooks")]
        # only check submissions directory if we have to
        if (!(b %in% submissions)) {
            subdirs <- subdirs[!str_detect(subdirs, "submissions")]
        }
        sub_levels <- lapply(subdirs, function(f) gsutil_ls(paste0(f, "**"), recursive=TRUE))
        c(top_level, unlist(sub_levels))
    }) %>% unlist()
    
    # then a simple check like all(df$file_path %in% files_in_buckets)
    chk <- lapply(names(bucket_cols), function(t) {
        chk2 <- lapply(bucket_cols[[t]], function(c) {
            name <- paste(t, c, sep=".")
            ct <- unique(na.omit(tables[[t]][[c]])) # only check unique non-missing values
            ct <- ct[!grepl("^drs://", ct)] # don't check drs ids
            if (length(ct) == 0) return(NULL)
            exists <- ct %in% files_in_buckets
            names(exists) <- ct
            if (all(exists)) {
                return(NULL)
            } else {
                missing <- names(exists)[!exists]
                miss_str <- paste(missing, collapse=", ")
                return(paste0("Bucket paths in ", name, " do not exist: ", miss_str))
            }
        })
        names(chk2) <- bucket_cols[[t]]
        return(chk2)
    })
    names(chk) <- names(bucket_cols)
    return(chk)
}

# alternate method for checking bucket paths using gsutil_exists
# too slow for large tables; gsutil_exists takes ~4 sec per file
check_bucket_paths_indiv <- function(tables, model) {
    common <- intersect(names(tables), names(model))
    chk <- lapply(common, function(t) {
        cols <- intersect(names(tables[[t]]), attr(model[[t]], "bucket_path"))
        chk2 <- lapply(cols, function(c) {
            name <- paste(t, c, sep=".")
            ct <- unique(na.omit(tables[[t]][[c]])) # only check unique non-missing values
            if (length(ct) == 0) return(NULL)
            exists <- sapply(ct, function(uri) {
                tryCatch({
                    gsutil_exists(uri)
                }, warning=function(w) w, error=function(e) FALSE)
            }, USE.NAMES = FALSE)
            
            names(exists) <- ct
            if (all(exists)) {
                return(NULL)
            } else {
                missing <- names(exists)[!exists]
                miss_str <- paste(missing, collapse=", ")
                return(paste0("Bucket paths in ", name, " do not exist: ", miss_str))
            }
        })
        names(chk2) <- cols
        return(chk2)
    })
    names(chk) <- common
    return(chk)
}


#' @importFrom stringr str_detect
.invalid_characters <- function(x) {
    str_detect(x, "[^[:alnum:]_\\-\\.]")
}


#' @rdname check_data_tables
#' @param report_missing_id A logical indicating whether the absence of an entity id is
#'     regarded as an error.
#' @return \code{check_valid_entity_id} returns a list of all tables in common between data 
#'     and model. Each table element is \code{NULL} if the table has a valid AnVIL entity_id, or 
#'     a string describing the error.
#'     
#' @export
check_valid_entity_id <- function(tables, model, report_missing_id=FALSE) {
    common <- intersect(names(tables), names(model))
    chk <- lapply(common, function(t) {
        entity_id <- intersect(names(tables[[t]]), paste0(t, "_id"))
        if (length(entity_id) > 0) {
            entity_col <- na.omit(tables[[t]][[entity_id]]) # only check non-missing values
            chk2 <- .invalid_characters(entity_col)
            if (any(chk2)) {
                fails <- paste(unique(entity_col[chk2]), collapse=", ")
                return(paste0("Invalid characters in ", entity_id, ": ", fails))
            } else {
                return(NULL)
            }
        } else {
            if (report_missing_id) {
                return(paste0("Expected column ", t, "_id not found"))
            } else {
                return(NULL)
            }
        }
    })
    names(chk) <- common
    return(chk)
}



#' @rdname check_data_tables
#' @return \code{check_primary_keys} returns a list with two elements:
#' \itemize{
#'   \item{found_keys}{results of \code{\link{dm_examine_constraints}}
#'     after applying primary keys from \code{model} to \code{tables}}
#'   \item{missing_keys}{list of missing primary keys in each table}
#' }
#' @export
check_primary_keys <- function(tables, model) {
    # set tables will have duplicate values for the set_id before import to AnVIL
    # don't check these
    common <- intersect(names(tables), names(model))
    no_sets <- common[!(grepl("_set$", common))]
    keys <- dm_get_all_pks(model, table=!!no_sets)
    tables_dm <- as_dm(tables)
    missing_keys <- list()
    if (nrow(keys) > 0) {
        for (i in 1:nrow(keys)) {
            this_table <- keys$table[i]
            expected_keys <- keys$pk_col[[i]]
            absent_keys <- setdiff(expected_keys, names(tables_dm[[this_table]]))
            if (length(absent_keys) == 0) {
                tables_dm <- dm_add_pk(tables_dm, table=!!this_table, columns=!!expected_keys)
            } else {
                missing_keys[[this_table]] <- absent_keys
            }
        }
    }
    return(list(found_keys=dm_examine_constraints(tables_dm),
                missing_keys=missing_keys))
}


#' @rdname check_data_tables
#' @return \code{check_foreign_keys} returns a list with two elements:
#' \itemize{
#'   \item{found_keys}{results of \code{\link{dm_examine_constraints}}
#'     after applying foreign keys from \code{model} to \code{tables}}
#'   \item{missing_keys}{list of missing child or parent keys in each table}
#' }
#' @export
check_foreign_keys <- function(tables, model) {
    common <- intersect(names(tables), names(model))
    keys <- dm_get_all_fks(model[common])
    tables_dm <- as_dm(tables)
    missing_keys <- list()
    set_key_problems <- list()
    if (nrow(keys) > 0) {
        for (i in 1:nrow(keys)) {
            child_table <- keys$child_table[i]
            child_keys <- keys$child_fk_cols[[i]]
            missing_child_keys <- setdiff(child_keys, names(tables_dm[[child_table]]))
            if (length(missing_child_keys) > 0) {
                missing_keys[[child_table]] <- unique(c(missing_keys[[child_table]], missing_child_keys))
            }
            parent_table <- keys$parent_table[i]
            parent_keys <- keys$parent_key_cols[[i]]
            missing_parent_keys <- setdiff(parent_keys, names(tables_dm[[parent_table]]))
            if (length(missing_parent_keys) > 0) {
                missing_keys[[parent_table]] <- unique(c(missing_keys[[parent_table]], missing_parent_keys))
            }
            if (length(c(missing_child_keys, missing_parent_keys)) == 0) {
                # foreign keys to set tables will not be unique before import to AnVIL
                if (grepl("_set$", parent_table)) {
                    for (kc in child_keys) {
                        for (kp in parent_keys) {
                            child_vals <- tables[[child_table]][[kc]]
                            parent_vals <- tables[[parent_table]][[kp]]
                            if (!all(child_vals %in% parent_vals)) {
                                miss_str <-  paste(setdiff(child_vals, parent_vals), collapse=", ")
                                set_key_problems[[paste(child_table, kc, sep=".")]] <- 
                                    paste0("Some values not present in ", parent_table, ".", kp, ": ", miss_str)
                            }
                        }
                    }
                # need to expand multi-value delimiters before comparing to parent table
                } else if (any(child_keys %in% names(attr(model[[child_table]], "multi_value_delimiters")))) {
                    for (kc in child_keys) {
                        for (kp in parent_keys) {
                            child_vals <- .parse_delim(na.omit(tables[[child_table]][[kc]]), 
                                                       table_name = child_table,
                                                       column_name = kc,
                                                       model = model)
                            parent_vals <- tables[[parent_table]][[kp]]
                            if (!all(child_vals %in% parent_vals)) {
                                miss_str <-  paste(setdiff(child_vals, parent_vals), collapse=", ")
                                set_key_problems[[paste(child_table, kc, sep=".")]] <- 
                                    paste0("Some values not present in ", parent_table, ".", kp, ": ", miss_str)
                            }
                        }
                    }
                } else {
                    tables_dm <- dm_add_fk(tables_dm, 
                                           table=!!child_table, 
                                           columns=!!child_keys,
                                           ref_table=!!parent_table,
                                           ref_columns=!!parent_keys)
                }
            }
        }
    }
    return(list(found_keys=dm_examine_constraints(tables_dm),
                missing_keys=missing_keys,
                set_key_problems=set_key_problems))
}


#' @rdname check_data_tables
#' @param chk output of \code{check_column_names} or \code{check_column_types}
#' @return \code{parse_column_name_check} and \code{parse_column_type_check} 
#'   each return a tibble with check results suitable for printing
#' @importFrom dplyr .data
#' @export
parse_column_name_check <- function(chk) {
    tibble(Table=names(chk),
           `Missing required columns`=sapply(chk, function(x) paste(x$missing_required_columns, collapse=", ")),
           `Missing optional columns`=sapply(chk, function(x) paste(x$missing_optional_columns, collapse=", ")),
           `Extra columns`=sapply(chk, function(x) paste(x$extra_columns, collapse=", "))
    ) %>%
        filter(.data[["Missing required columns"]] != "" | 
               .data[["Missing optional columns"]] != "" |
               .data[["Extra columns"]] != "")
}


#' @rdname check_data_tables
#' @importFrom dplyr bind_rows
#' @export
parse_column_type_check <- function(chk) {
    lapply(names(chk), function(x) {
        y <- unlist(chk[[x]])
        if (is.null(y)) return(NULL)
        tibble(Table=x,
               Column=names(y),
               Issue=y)
    }) %>%
        bind_rows()
}

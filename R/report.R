#' Internal function to render markdown documents
#'
#' @param markdown_template_name The name of the markdown template, excluding ".Rmd"
#' @param output_file The name of the output file to create
#' @param parameters A named list of parameters to pass to the markdown document
#' @return The value of `return_value` as defined in the markdown document
#'
#' @seealso \code{\link[rmarkdown]{render}}
#'
#' @details
#' The markdown template is expected to live in the \code{inst/rmd} directory and an error
#' is raised if it is not found.
#'
#' @importFrom rmarkdown render
#' @export
# solution for returning a value from a markdown template proposed here:
# https://stackoverflow.com/questions/58315771/can-rmarkdown-return-a-value-to-a-target
custom_render_markdown <- function(markdown_template_name, output_file, parameters=NULL) {
    
    markdown_file <- file.path(system.file(package="AnvilDataModels"), "rmd",
                               sprintf("%s.Rmd", markdown_template_name))
    if (!file.exists(markdown_file)) {
        errmsg <- sprintf("markdown template not found: %s", markdown_file)
        stop(errmsg)
    }
    
    output_file <- sprintf("%s.Rmd", output_file)
    success <- file.copy(markdown_file, output_file, overwrite=TRUE)
    if (!success) {
        errmsg <- sprintf("could not copy markdown template to: %s", output_file)
        stop(errmsg)
    }
    
    rmarkdown::render(output_file, params=parameters, quiet=TRUE)
    return(return_value)
}

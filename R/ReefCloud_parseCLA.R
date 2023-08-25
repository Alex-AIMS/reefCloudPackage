#' @title ReefCloud_parseCLA
#' @description The following function parses the command line arguments
#' 1. Ensure that a bucket has been supplied as --bucket=<bucket>
#' 2. If the bucket is local (/data/.* or /mnt/reefcloud/.*)
#'    - set DATA_FROM <<- "LOCAL"
#'    If the bucket is S3 (s3://dev-aims-gov-au-reefcloud-stats/reefcloud/.*)
#'    - set DATA_FROM <<- "S3"
#' 3. Ensure that a valid domain has been supplied and if so
##    - set DOMAIN_CATEGORY <<- <domain>
#' @param args command line arguments. The call must be of the form:
#' ReefCloud_30_model.R --bucket="<PATH>"
#' --domain=<DOMAIN>
#'   [--by_tier=<NUMBER> --debug=<true|false> --runStage=<NUM> --refresh_data=<true|false>]
#'
#'
#' <PATH>:  	a valid path to a folder containing the input data
#' <DOMAIN>:	either tier (spatio-temporal model for the data provide)
#' or site (hierarchical model for a single site.
#'          <NUMBER>:	for tier analyses, an optional tier number to indicate which
#'          tier level to use in splitting up analyses [n>true|false>:	 whether to operate in debug mode.
#'           If true, progress is provided via a CLI
#' <NUM>:	 which stages of the analysis to run (-1 or missing is all stages
#' @return returned arguments description
#' @examples examples
#' @export
ReefCloud_parseCLA <- function(args) {
    valid_cla <- paste0("The call must be of the form:\n",
                        "ReefCloud_30_model.R --bucket=\"<PATH>\"",
                        "\n\t--domain=<DOMAIN>",
                        "\n\t[--by_tier=<NUMBER> --debug=<true|false> --runStage=<NUM>",
                        " --refresh_data=<true|false>]\n\n",
                        "\n<PATH>:  \ta valid path to a folder containing the input data",
                        "\n<DOMAIN>:\teither tier (spatio-temporal model for the data provide)",
                        "\n\t\tor site (hierarchical model for a single site.",
                        "\n<NUMBER>:\tfor tier analyses, an optional tier number to indicate which ",
                        "\n\t\ttier level to use in splitting up analyses",
                        "[n>true|false>:\t whether to operate in debug mode.  If true,",
                        "\n\t\tprogress is provided via a CLI",
                        "\n<NUM>:\t which stages of the analysis to run (-1 or missing is all stages")
    ## args <- commandArgs()
    ## Ensure that a bucket path is supplied
    bucket <- grep('--bucket=.*', args)
    ## if (length(bucket) != 1)
    ##     stop(paste0('A --bucket argument must be provided with a valid ',
    ##                 'path to a folder containing the input data.\n', valid_cla),
    ##          call. = FALSE)
    ## Ensure that a domain is supplied
    domain <- grep('--domain=.*', args)
    ## if (length(domain) != 1)
    ##     stop(paste0('A --domain argument must be provided with a value of either site or tier.\n',
    ##                 valid_cla),
    ##          call. = FALSE)
    ## ## Ensure that a if a by_tier is provided, it is a valid number
    by_tier <- grep('--by_tier=.*', args)
    ## if (length(by_tier) == 1 & !is.numeric(by_tier) & by_tier %in% 2:5)
    ##     stop(paste0('If a --by_tier argument is provided, it must be a number between 2 and 5.\n',
    ##                 valid_cla),
    ##          call. = FALSE)

    REFRESH_DATA <<- ifelse(any(grepl('--refresh_data ?= ?(true|t|TRUE|T)', args, perl = TRUE)), TRUE, FALSE)
    ReefCloud__change_status(stage = "SETTINGS", item = "REFRESH_DATA",
                             status = "success", update_display = FALSE)

    DEBUG_MODE <<- ifelse(any(grepl('--debug ?= ?(true|t|TRUE|T)', args, perl = TRUE)), TRUE, FALSE)
    ReefCloud__change_status(stage = "SETTINGS", item = "DEBUG_MODE",
                             status = "success", update_display = FALSE)

    runstage <<- grep('--runStage ?=.*', args, perl = TRUE)
    runStage <<- gsub('--runStage ?= ?', '\\1', args[runstage])
    if (length(runStage) == 0) runStage <<- ReefCloud_get_stages()
    if (length(runStage) == 1) if (runStage == -1) runStage <<- ReefCloud_get_stages()

    ## Ensure that a bucket is supplied
    if (length(domain)==0)
        stop(paste0('A bucket (location/path of raw data) needs to be provided as a command line arguement, such as: Rscript <script.R> --bucket=<PATH>\n\n',
                    valid_cla
                    ))
    file <- args[bucket]
    AWS_PATH <<- gsub('--bucket=(.*)','\\1/', file)
    ReefCloud__change_status(stage = "SETTINGS", item = "AWS_PATH",
                             status = "success", update_display = FALSE)

    ## Determine whether data are to be sourced locally or from s3 bucket
    DATA_FROM <<- ifelse(
        grepl('data/synthetic', AWS_PATH, perl = TRUE), 'SYNTHETIC',
                  ifelse(grepl('(^/mnt.*|^/home/data.*)', AWS_PATH, perl=TRUE), 'LOCAL',
                  ifelse(grepl('^[sS]3.*', AWS_PATH, perl=TRUE), 'S3', 'User defined')))
    ReefCloud__change_status(stage = "SETTINGS", item = "DATA_FROM",
                             status = "success", update_display = FALSE)


    ## Ensure that valid domain is supplied
    if (length(domain)==0)
        stop(paste0('A domain needs to be provided as a command line arguement, such as: Rscript <script.R> --domain=<DOMAIN>\n\n',
                    valid_cla
                    ))
    if (!grepl('--domain=(site|tier)',args[domain], perl=TRUE))
        stop("The supplied domain in --domain=<domain> must be either 'site' or 'tier'")

    ## Set domain category global variable
    DOMAIN_CATEGORY <<- gsub('--domain=', '', args[domain])
    ReefCloud__change_status(stage = "SETTINGS", item = "DOMAIN_CATEGORY",
                             status = "success", update_display = FALSE)

    ## Set the AWS_OUTPUT_PATH
    AWS_OUTPUT_PATH <<- paste0(AWS_PATH, "outputs/", DOMAIN_CATEGORY,"/")
    ReefCloud__change_status(stage = "SETTINGS", item = "AWS_OUTPUT_PATH",
                             status = "success", update_display = FALSE)

    ## Get the domain name
    DOMAIN_NAME <<- basename(AWS_PATH)
    ReefCloud__change_status(stage = "SETTINGS", item = "DOMAIN_NAME",
                             status = "success", update_display = FALSE)

    ## Determine whether a report should be generated
    GENERATE_REPORT <<- FALSE
    ReefCloud__change_status(stage = "SETTINGS", item = "GENERATE_REPORT",
                             status = "success", update_display = FALSE)

    ## Model type
    MODEL_TYPE <<- 2   #simple hierachical
    ReefCloud__change_status(stage = "SETTINGS", item = "MODEL_TYPE",
                             status = "success", update_display = FALSE)

    ## Tier level on which to break up analyses
    if (length(by_tier) == 0) {
        BY_TIER <<- 2
    } else {
        BY_TIER <<- gsub('--by_tier=(.*)', '\\1', args[by_tier])
    }
    ReefCloud__change_status(stage = "SETTINGS", item = "BY_TIER",
                             status = "success", update_display = FALSE)

    if(!exists("LEGACY_DATA")) LEGACY_DATA <<- NULL

    ReefCloud__remove_predicates(update_display = FALSE)
    ## if (DEBUG_MODE) ReefCloud_openingBanner()
}

#' Generate presentation
#'
#' Main function for generating the audit filter review report.
#' @param centre.name Character vector of length 1. The name of the centre. No
#'     default.
#' @param this.meeting.date Character vector of length 1. The date whan the
#'     meeting is. Should be formatted as YYYY-MM-DD. No default.
#' @param last.meeting.date Character vector of length 1. The date whan the last
#'     meeting was or NULL, in which case it is assumed that no previous
#'     meetings have been held and all data is used. Should be formatted as
#'     YYYY-MM-DD. Default is NULL.
#' @param action.points.document Character vector of length 1. The name of the
#'     file with the action points from the last meeting. Defaults to
#'     paste0(tolower(centre.name), "-audit-filter-meeting-notes-",
#'     last.meeting.date).
#' @param data.path Character vector of length 1. The path to the raw data
#'     directory. No default.
#' @param codebook.path Character vector of length 1. The path to the data
#'     codebook. No default.
#' @param codebook.file.name Character vector of length 1. The codebook file
#'     name. Defaults to "codebook.csv".
#' @export
GeneratePresentation <- function(centre.name, this.meeting.date,
                                 last.meeting.date = NULL,
                                 action.points.document = paste0(tolower(centre.name), "-audit-filter-meeting-notes-", last.meeting.date),
                                 data.path,
                                 codebook.path,
                                 codebook.file.name = "codebook.csv") {
    ## Load dplyr
    library(dplyr)
    ## Error handling
    date.error.message <- "meeting date is not correctly formatted"
    this.meeting.date <- tryCatch(expr = as.Date(this.meeting.date),
                                  error = function(e) {
                                      stop (date.error.message)
                                  })
    if (!is.null(last.meeting.date)) {
        last.meeting.date <- tryCatch(expr = as.Date(last.meeting.date),
                                      error = function(e) {
                                          stop (date.error.message)
                                      })
    }
    ## Get audit filter data
    all.data <- beehive::compile.centre.dataset(data.path, save = FALSE)
    ## Get ICD 10 mechanism data
    icd.data <- GenerateAuditFilterDataPresentation:::icd.codes
    rownames(icd.data) <- icd.data$code
    ## Get codebook
    codebook <- read.csv(paste0(codebook.path, codebook.file.name),
                         stringsAsFactors = FALSE)
    ## Add time between injury and arrival and time between arrival and
    ## discharge in hours
    if (all(sapply(c("doi","toi","doar","toar","dodd","todd"), function(x) any(x == colnames(all.data))))) {
        datetimes <- list(doi.toi = with(all.data, paste(doi, toi)),
                          doar.toar = with(all.data, paste(doar, toar)),
                          dodd.todd = with(all.data, paste(dodd, todd)))
        datetimes <- lapply(datetimes, function(datetime) {
            datetime[grep("NA|999", datetime)] <- NA
            datetime <- as.POSIXct(datetime)
            return(datetime)
        })
        all.data$teap <- round(as.numeric(with(datetimes, difftime(doar.toar, doi.toi, units = "hours"))), digits = 1)
        all.data$tadd <- round(as.numeric(with(datetimes, difftime(dodd.todd, doar.toar, units = "hours"))), digits = 1)
        empty.entry <- vector("character", ncol(codebook))
        if (!("teap" %in% codebook$name)) {
            teap <- empty.entry
            teap[1] <- "Time elapsed between trauma and arrival in hours"
            teap[2] <- "teap"
            codebook <- rbind(codebook, teap)
            codebook[codebook$name == "teap", "type"] <- "quantitative"
        }
        if (!("tadd" %in% codebook$name)) {
            tadd <- empty.entry
            tadd[1] <- "Time elapsed between arrival and death or discharge in hours"
            tadd[2] <- "tadd"
            codebook <- rbind(codebook, tadd)
            codebook[codebook$name == "tadd", "type"] <- "quantitative"
        }
    }
    rownames(codebook) <- codebook$name
    ## Check for duplicate patient IDs
    duplicate.pids <- all.data$pid[duplicated(all.data$pid)]
    if (length(duplicate.pids) > 0)
        stop (paste0("pid ", paste0(all.data$pid[duplicate.pids], collapse = ", "), " is/are duplicated. Please remove the duplicated files."))
    ## Create audit filter data overview
    all.data$doar <- as.Date(all.data$doar)
    all.data <- all.data[all.data$doar < this.meeting.date, ]
    if (!is.null(last.meeting.date))
        all.data <- all.data[all.data$doar > last.meeting.date, ]
    if (nrow(all.data) == 0)
        stop ("There are no cases between the dates specified")
    rownames(all.data) <- all.data$pid
    audit.filter.data <- all.data[, grep("^taft[0-9]*\\.*[0-9]*$", colnames(all.data), value = FALSE)]
    audit.filter.data[] <- lapply(audit.filter.data, function(filter.data) {
        return.object <- filter.data
        if (all(is.na(filter.data)))
            return.object <- NULL
        return(return.object)
    })
    rownames(audit.filter.data) <- all.data[, "pid"]
    levels <- beehive::get.vector(codebook[names(audit.filter.data)[1], "valid_values"])
    labels <- beehive::get.vector(codebook[names(audit.filter.data)[1], "value_labels"])
    audit.filter.data[] <- lapply(audit.filter.data, factor,
                                  levels = levels,
                                  labels = labels)
    audit.filter.tables <- lapply(audit.filter.data, table)
    audit.filter.prop.tables <- lapply(audit.filter.tables, function(table) {
        prop.table <- round(prop.table(table) * 100)
        return(prop.table)
    })
    tables.list <- list(tables = audit.filter.tables,
                        prop.tables = audit.filter.prop.tables)
    combined.tables <- lapply(tables.list, function(tables) do.call(rbind, tables))
    filter.names <- rownames(combined.tables$tables)
    combined.table <- matrix(with(combined.tables, paste0(tables, " (", prop.tables, ")")), nrow = nrow(combined.tables$tables))
    rownames(combined.table) <- filter.names
    colnames(combined.table) <- paste0(labels, " (%)")
    combined.table <- as.data.frame(combined.table, stringsAsFactors = FALSE)
    audit.filter.data.table <- cbind(rownames(combined.table), combined.table)
    target <- codebook$name[grep("^taft", codebook$name)]
    colnames(audit.filter.data.table)[1] <- "Filter"
    audit.filter.data.table <- audit.filter.data.table[match(target, audit.filter.data.table$Filter), ]
    rownames(audit.filter.data.table) <- NULL
    audit.filter.data.table$Filter <- as.character(audit.filter.data.table$Filter)
    audit.filter.data.table$Filter <- codebook[audit.filter.data.table$Filter, "label"]
    pre.bootstrap.table <- cbind(rep("<input type=\"checkbox\" />", length(filter.names)), audit.filter.data.table)
    colnames(pre.bootstrap.table)[1] <- "Check"
    ## Create bootstrap html header
    formatted.meeting.date <- format(this.meeting.date, "%d %B, %Y")
    bootstrap.header <- paste0(c("<!doctype html>",
                                 "<html lang=\"en\">",
                                 "<head>",
                                 "<meta charset=\"utf-8\">",
                                 "<meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\">",
                                 "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
                                 "<!-- The above 3 meta tags *must* come first in the head; any other head content must come *after* these tags -->",
                                 "<title>", toupper(centre.name), " Audit Filter Review Board Meeting ", formatted.meeting.date, "</title>",
                                 "<!-- Bootstrap -->",
                                 "<link rel=\"stylesheet\" href=\"https://stackpath.bootstrapcdn.com/bootstrap/3.4.1/css/bootstrap.min.css\" integrity=\"sha384-HSMxcRTRxnN+Bdg0JdbxYKrThecOKuH5zCYotlSAcp1+c8xmyTe9GYg1l9a69psu\" crossorigin=\"anonymous\">",
                                 "<!-- HTML5 shim and Respond.js for IE8 support of HTML5 elements and media queries -->",
                                 "<!-- WARNING: Respond.js doesn't work if you view the page via file:// -->", 
                                 "<!--[if lt IE 9]>",
                                 "<script src=\"https://oss.maxcdn.com/html5shiv/3.7.3/html5shiv.min.js\"></script>",
                                 "<script src=\"https://oss.maxcdn.com/respond/1.4.2/respond.min.js\"></script>",
                                 "<![endif]-->",
                                 "</head>",
                                 "<body>",
                                 "<h1>", toupper(centre.name), " Audit Filter Review Board Meeting ", formatted.meeting.date, "</h1>"),
                               collapse = " \n")
    ## Create bootstrap html footer
    bootstrap.footer <- paste0(c("<!-- jQuery (necessary for Bootstrap's JavaScript plugins) -->",
                                 "<script src=\"https://code.jquery.com/jquery-1.12.4.min.js\" integrity=\"sha384-nvAa0+6Qg9clwYCGGPpDQLVpLNn0fRaROjHqs13t4Ggj3Ez50XnGQqc/r8MhnRDZ\" crossorigin=\"anonymous\"></script>",
                                 "<!-- Include all compiled plugins (below), or include individual files as needed -->",
                                 "<script src=\"https://stackpath.bootstrapcdn.com/bootstrap/3.4.1/js/bootstrap.min.js\" integrity=\"sha384-aJ21OjlMXNL5UyIl/XNwTMqvzeRMZH2w8c5cRVpzpU8Y5bApTppSuUkhZXN0VxHd\" crossorigin=\"anonymous\"></script>",
                                 "</body>",
                                 "</html>"),
                               collapse = " \n")
    ## Create agenda
    agenda <- paste0(c("<h2>Agenda</h2>",
                       "<ol>",
                       paste0("<li>",
                              c("Welcome and introductions from the PI and chair",
                                "Summary of action points from last meeting (co-chair) [5 minutes]",
                                "Discussion of flagged cases (chair) [30 minutes]",
                                "Summary of action points (co-chair) [5 minutes]",
                                "Presentation of overall filter data (co-chair) [5-10 minutes]",                                
                                "Next meeting (chair/co-chair)"),
                              "</li>"),
                       "</ol>"),
                     collapse = " \n")
    ## Import action points
    action.points.file.name <- list.files(pattern = paste0("^", action.points.document))
    if (length(action.points.file.name) > 1) {
        warning (paste0("There are more than one file with the name ",
                        action.points.document,
                        " so only the file named ", action.points.file.name[1],
                        " is imported"))
        action.points.file.name <- action.points.file.name[1]
    }
    ## action.points.text <- readtext::readtext(action.points.file.name)$text
    ## first <- regexpr("# Action points", action.points.text, fixed = TRUE)
    ## first <- first[1] + attr(first, "match.length")
    ## last <- gregexpr("- [a-z]* ", action.points.text)
    ## action.points <- substring()
    formatted.action.points <- ""
    if (file.exists(action.points.document)) {
        action.points <- read.csv(action.points.document, header = FALSE, stringsAsFactors = FALSE)
        formatted.action.points <- paste0(c("<h2>Action points from last meeting</h2>",
                                            "<ol>",
                                            paste0("<li>",
                                                   action.points[, 1],
                                                   "</li>"),
                                            "</ol>"),
                                          collapse = "\n")
    }
    ## Create bootstrap table    
    bootstrap.table <- knitr::kable(pre.bootstrap.table, format = "html", escape = FALSE) %>%
        kableExtra::kable_styling(c("striped", "hover")) %>%
        kableExtra::column_spec(column = 2, width_max = "15cm")
    bootstrap.table <- paste0(c("<h3>Overall filter data</h3>",
                                bootstrap.table),
                              collapse = " \n")
    ## Create case tables
    case.variables <- c("pid", "age", "sex", "moi", "teap", "tadd", "doar",
                        "egcs", "vgcs", "mgcs", "sbp", "dbp", "hr", "spo2",
                        "rr", "hd", "dctph", "dcted", "dcticu", "dctiot",
                        "dctiw")
    patterns <- as.list(paste0("^", c("einj", "xrayinj", "uinj", "ctinj", "iopinj"), "[0-9]*$"))
    names(patterns) <- c("External injuries", "X-ray findings", "Ultrasonography findings", "CT-findings", "Intraoperative findings")
    injury.variables <- lapply(patterns, grep, names(all.data), value = TRUE)
    injury.data.list <- lapply(injury.variables, function(variables) {
        injury.data <- all.data[, variables] %>%
            apply(1, function(row) {
                injury.row <- trimws(row)
                injury.row <- paste0(injury.row, collapse = " <br> ")
                return(injury.row)
            }) %>%
            gsub(pattern = "<br> 999|999", replacement = "")
        names(injury.data) <- rownames(all.data)
        return(injury.data)
    })
    treat.as.quantitative <- c("egcs", "vgcs", "mgcs")
    case.data <- all.data[apply(audit.filter.data, 1, function(row) any(row == "Not done")), case.variables]
    case.data[] <- lapply(names(case.data), function(name) {
        entry <- codebook[name, ]
        variable.data <- case.data[, name]
        if (entry$type == "qualitative" & !(name %in% treat.as.quantitative)) {
            levels <- beehive::get.vector(entry$valid_values)
            labels <- beehive::get.vector(entry$value_labels)
            if (length(labels) > 0)
                variable.data <- factor(variable.data, levels, labels)
        }
        return(variable.data)
    })
    case.data$id <- 1:nrow(case.data)
    cases <- split(case.data, case.data$id)
    cases <- lapply(cases, function(case) {
        id <- case[, "pid"]
        case[, "pid"] <- NULL
        case[, "doar"] <- NULL
        icd.mechanism.code <- case[, "moi"]
        search.term <- substring(icd.mechanism.code, 1, 3)
        full.mechanism <- icd.data[search.term, "full_mechanism"]
        if (is.na(full.mechanism))
            full.mechanism <- case[, "moi"]
        case[, "moi"] <- full.mechanism
        injuries <- lapply(injury.data.list, function(injury.data) injury.data[id]) %>%
            do.call(what = rbind) %>% trimws()
        case <- t(case) %>% rbind(injuries)
        audit.filters <- audit.filter.data[id, ]
        audit.filters <- names(audit.filters)[audit.filters == "Not done"]
        violations <- as.list(codebook[audit.filters, "label"])
        names(violations) <- paste("Violation", 1:length(violations))
        violations <- do.call(rbind, violations)
        case <- rbind(case, violations)
        case <- cbind(codebook[rownames(case), "label"], case)
        case[is.na(case[, 1]), 1] <- rownames(case)[is.na(case[, 1])]
        return(case)
    })
    case.tables <- lapply(cases, function(case) {
        id <- case["id", 2]
        case.header <- paste0("<h3 id=\"case-", id, "\">Case ", id, "</h3>")
        case <- case[-grep("id", rownames(case)), ]
        colnames(case) <- NULL
        case.table <- knitr::kable(case, row.names = FALSE, format = "html", escape = FALSE) %>%
            kableExtra::kable_styling(c("striped", "hover"))
        case.footer <- "<a href=\"#case-list\">Back to case list</a>"
        case.table <- paste0(c(case.header, case.table, case.footer),
                             collapse = " \n")
        return(case.table)
    })
    ## Create case list
    case.list <- paste0("<li>",
                        paste0("<a href=\"#case-", 1:length(case.tables), "\">", "Case ", 1:length(case.tables), "</a>"),
                        "</li>", collapse = " \n")
    case.list <- paste0(c("<h2 id=\"case-list\">Case list</h2>",
                          "<ul>",
                          case.list,
                          "</ul>"),
                        collapse = " \n")
    ## Combine bootstrap presentation elements
    bootstrap.presentation <- paste0(c(bootstrap.header, agenda, formatted.action.points, case.list, unlist(case.tables), bootstrap.table, bootstrap.footer),
                                     collapse = " \n")
    file.name <- paste0(tolower(centre.name), "-audit-filter-presentation-", gsub(" ", "-", gsub(",", "", this.meeting.date)), ".html")
    write(bootstrap.presentation, file.name)
    ## Presentation generated
    message(paste0("Presentation generated and saved to disk as ", file.name))
}

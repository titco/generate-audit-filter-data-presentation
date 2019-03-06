#' Generate presentation
#'
#' Main function for generating the audit filter review report.
#' @param centre.name Character vector of length 1. The name of the centre. No
#'     default.
#' @param meeting.date Character vector of length 1. The date whan the meeting
#'     is. No default.
#' @param data.path Character vector of length 1. The path to the raw data
#'     directory. No default.
#' @param codebook.path Character vector of length 1. The path to the data
#'     codebook. No default.
#' @param codebook.file.name Character vector of length 1. The codebook file
#'     name. Defaults to "codebook.csv".
#' @export
GeneratePresentation <- function(centre.name, meeting.date, data.path,
                                 codebook.path,
                                 codebook.file.name = "codebook.csv") {
    ## Get audit filter data
    all.data <- beehive::compile.centre.dataset(data.path, save = FALSE)
    ## Get ICD 10 mechanism data
    icd.data <- read.csv("icd.codes.csv", stringsAsFactors = FALSE)
    rownames(icd.data) <- icd.data$code
    ## Get codebook
    codebook <- read.csv(paste0(codebook.path, codebook.file.name),
                         stringsAsFactors = FALSE)
    rownames(codebook) <- codebook$name
    ## Create audit filter data overview
    audit.filter.data <- all.data[, grep("^taft[0-9]*$", colnames(all.data))]
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
    order.vector <- combined.tables$tables[, "Not done"]
    combined.tables <- lapply(combined.tables, function(tables) tables[order(-order.vector), ])
    filter.names <- rownames(combined.tables$tables)
    combined.table <- matrix(with(combined.tables, paste0(tables, " (", prop.tables, ")")), nrow = nrow(combined.tables$tables))
    rownames(combined.table) <- filter.names
    colnames(combined.table) <- paste0(labels, " (%)")
    combined.table <- as.data.frame(combined.table, stringsAsFactors = FALSE)
    audit.filter.data.table <- cbind(rownames(combined.table), combined.table)
    colnames(audit.filter.data.table)[1] <- "Filter"
    rownames(audit.filter.data.table) <- NULL
    audit.filter.data.table$Filter <- as.character(audit.filter.data.table$Filter)
    audit.filter.data.table$Filter <- codebook[audit.filter.data.table$Filter, "label"]
    pre.bootstrap.table <- cbind(rep("<input type=\"checkbox\" />", length(filter.names)), audit.filter.data.table)
    colnames(pre.bootstrap.table)[1] <- "Check"
    ## Create bootstrap html header
    bootstrap.header <- paste0(c("<!doctype html>",
                                 "<html lang=\"en\">",
                                 "<head>",
                                 "<meta charset=\"utf-8\">",
                                 "<meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\">",
                                 "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
                                 "<!-- The above 3 meta tags *must* come first in the head; any other head content must come *after* these tags -->",
                                 "<title>", toupper(centre.name), " Audit Filter Review Board Meeting ", meeting.date, "</title>",
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
                                 "<h1>", toupper(centre.name), " Audit Filter Review Board Meeting ", meeting.date, "</h1>"),
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
                                "Presentation of overall filter data (co-chair) [5-10 minutes]",
                                "Discussion of flagged cases (chair) [30 minutes]",
                                "Summary of action points (co-chair) [5 minutes]",
                                "Next meeting (chair/co-chair)"),
                              "</li>"),
                       "</ol>"),
                     collapse = " \n")
    ## Create bootstrap table    
    bootstrap.table <- kable(pre.bootstrap.table, format = "html", escape = FALSE) %>%
        kable_styling(c("striped", "hover")) %>%
        column_spec(column = 2, width_max = "15cm")
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
        case[, "moi"] <- icd.data[case[, "moi"], "full_mechanism"]
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
        case.table <- kable(case, row.names = FALSE, format = "html", escape = FALSE) %>%
            kable_styling(c("striped", "hover"))
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
    bootstrap.presentation <- paste0(c(bootstrap.header, agenda, bootstrap.table, case.list, unlist(case.tables), bootstrap.footer),
                                     collapse = " \n")
    file.name <- paste0(tolower(centre.name), "-audit-filter-presentation-", gsub(" ", "-", gsub(",", "", tolower(meeting.date))), ".html")
    write(bootstrap.presentation, file.name)
    ## Presentation generated
    message(paste0("Presentation generated and saved to disk as ", file.name))
}

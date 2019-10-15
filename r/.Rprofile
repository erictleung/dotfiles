# R Configuration
# Example:
# https://www.r-bloggers.com/fun-with-rprofile-and-customizing-r-startup/

# Always pull packages from Oregon State University
local({
    r = getOption("repos")
    r["CRAN"] = "https://ftp.osuosl.org/pub/cran/"
    options(repos = r)
})

# Various options
options(max.print = 100)  # Limit amount of info printed
options(width = 80)  # Keep things nice and narrow
options(editor = "vim")  # Default editor

# Don't save workspace when you quit
q <- function(save = "no", ...) {
    quit(save = save, ...)
}

# ht == headtail, i.e., show the first and last 10 items of object
ht <- function(d) rbind(head(d, 10), tail(d, 10))

# Remap to exit command so you don't have to write quit()
exit <- structure(list(), class = "exit_command")

# Table complete for loading pacakges
utils::rc.settings(ipck = TRUE)

# Exit without saving
print.exit_command <- function(...) {
    q("no")
}

# Prompt settings and how it looks with new line
options(prompt = "> ")
options(continue = "... ")

# Function to run at the beginning
.First <- function(){
    if (interactive()) {
        library(utils)
        timestamp(prefix = paste("##------ [", getwd(), "] ", sep = ""))
    }
}

# Custom function to remove rownames
.env$unrowname <- function(x) {
    rownames(x) <- NULL
    x
}

# Custom function to undo factor() call
.env$unfactor <- function(df){
    id <- sapply(df, is.factor)
    df[id] <- lapply(df[id], as.character)
    df
}

message("n*** Successfully loaded .Rprofile ***n")

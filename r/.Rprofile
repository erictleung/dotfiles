# R Configuration

# Always pull packages from Oregon State University
local({
    r = getOption("repos")
    r["CRAN"] = "https://ftp.osuosl.org/pub/cran/"
    options(repos = r)
})

# ht == headtail, i.e., show the first and last 10 items of object
ht <- function(d) rbind(head(d,10),tail(d,10))

# Remap to exit command so you don't have to write quit()
exit <- structure(list(), class = "exit_command")

# Exit without saving
print.exit_command <- function(...) {
    q("no")
}

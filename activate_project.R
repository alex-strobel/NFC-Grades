# ACTIVATE NFC-Family ---------------------------------------------------------

# 1) Activate R environment via the renv package (default option) -------------

# To activate the R environment this project was written in and have everything 
# working properly, edit the path below to match the directory you cloned the 
# R-Markdown repository in

# Change to project directory via here package
library(here) 
# set top level directory to source file
here::i_am("flag_project_root_RMDcourse.txt")

# Detach other packages if there are any to avoid conflicts with packages of 
# the R environment of this project
if (!is.null(names(sessionInfo()$otherPkgs))) {
  invisible(lapply(paste('package:',names(sessionInfo()$otherPkgs), sep = ""), detach, character.only = TRUE, unload = TRUE))
}

# Activate project
source('renv/activate.R')
renv::activate(getwd())
# Don't ask why whether sourcing the activate function is really necessary.
# In some cases, in can help, in other cases it will be unnecessary, but
# in any case, it will do no harm.

# Restore environment (may require package downloads)
renv::restore()

# Please note that it may be the case that you get an error message if this 
# project's R environment later want to load packages in versions that differ 
# from you system's package versions. If so, it may help to restart R before
# activating the project, i.e., before executing the commands above, execute:
#
# if (exists('.rs.restartR', mode = 'function')) { .rs.restartR() }
#
# Yet, this may cause another problem: R does not (seem to) restart but to 
# freeze. In this case simply close RStudio and reopen it. Now, everything
# should work.

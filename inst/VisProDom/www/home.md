VisProDom: an interactive Shiny/R application to display protein domains with transcriptional features
====

![GPLv3](http://www.gnu.org/graphics/gplv3-88x31.png)
[GNU General Public License, GPLv3](http://www.gnu.org/copyleft/gpl.html)

![Availability](https://github.com/whweve/VisProDom)

# Installation
## install compiler
The `install_github()`, in the R package remotes, requires that you build from source, thus `make` and compilers must be installed on your system -- see the [R FAQ](http://cran.r-project.org/faqs.html) for your operating system; you may also need to install dependencies manually. 

* For Windows system, rtools, a compiler, is required and can be found at https://cran.r-project.org/bin/windows/Rtools/.

* For Ubuntu/Linux system, compilers could be installed by the command: sudo apt-get install libcurl4-openssl-dev libssl-dev. For more information, please see https://stackoverflow.com/questions/20923209/problems-installing-the-devtools-package.

* install R package devtools and remotes
```R
install.packages(c("devtools","remotes"))
```

## install depended packages, including ggplot2, shinydashboard, shiny, ggplot2, rintrojs, dplyr, data.table, ggrepel
```R
#ggplot2, ggrepel, and reshape2 are installed from CRAN
install.packages(c("ggplot2", "shinydashboard", "shiny", "ggplot2", "rintrojs", "dplyr", "data.table", "ggrepel"))
```
## install VisProDom from GitHub:
```R
library(remotes) # version 2.1.0
#download, build, and install VisProDom without creating vignette
install_github("whweve/VisProDom")
#download, build, and install VisProDom with creating vignette
install_github("whweve/VisProDom",build=TRUE,build_vignettes = TRUE)
```

# example
```R
#load VisProDom
library(VisProDom)
# initiate VisProDom shiny appp
runVPDapp()
```

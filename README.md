## actel and RSP workshop - hosted by ETN on January 18th, 2021

Hi! Welcome to the data repository for our workshop. Please download the contents of this repository using the green "Code" button on the top right side of the file list.

In the downloaded folder, you will find data that will be used during our course, as well as a copy of the papers providing some background on actel and RSP. We highly recommend that you read them before the workshop!

### Package installation

Please make sure to install actel and RSP before the course. Here is the code you will need to run:

```r
# Installing packages:
install.packages("patchwork")
install.packages("ggsn")
install.packages("actel")
install.packages("remotes")
remotes::install_github("YuriNiella/RSP", build_opts = c("--no-resave-data", "--no-manual"), build_vignettes = TRUE)
```

### Pandoc

To reduce the chance that something may go wrong while producing actel reports, consider installing the latest version of pandoc [through this link](https://pandoc.org/installing.html).


### All set!

Looking forward to seeing you there!

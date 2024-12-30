# Installing the required environment

Base machine: Ubuntu 22.04 (HP Z-book, 64Gb, Intel i9)

Steps I've taken to install the required R ecosystem:

1. Install RStudio, R-base and R-recommended
    a. Stock Ubuntu version might be outdated, I chose the deb package from www.posit.co
    $ dpkg -i rstudio-[your-rstudio-version]-amd64.deb
    $ sudo apt install r-base r-recommended libcurl4-openssl-dev

2. Install Mark Rutter's R package collection, and upgrade all packages (including R itself):
    $ sudo apt install apt-transport-https
    $ sudo add-apt-repository ppa:c2d4u.team/c2d4u4.0+
    $ sudo add-apt-repository ppa:marutter/rrutter4.0
    $ sudo apt update
    $ sudo apt dist-upgrade
    
3. Install required libraries for RStan:
    $ sudo apt install r-cran-lme4
4. Follow the instructions in rstan how to install
    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started#installation-of-rstan
    
    Execute the following inside the RStudio console (will install for your user only, in ${HOME}/R)
    > Sys.setenv(DOWNLOAD_STATIC_LIBV8 = 1)
    > install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)
5. Install the cmdstan R interface to stan:
    Inside the RStudio console, execute the following:
    > install.packages("devtools")
    > install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
    > cmdstanr::install_cmdstan()
    > install.packages(c("coda","mvtnorm","loo","dagitty"))
6. Optional: If you want to use the development version of cmdstanr, execute the following (instead of the above installation of the released cmdstanr):
    > devtools::install_github("stan-dev/cmdstanr")
7. Optional: If you want to install the rethinking package, also execute the following:
    > devtools::install_github("rmcelreath/rethinking")
8. Optional: If you have the requisite HW, install CUDA support. This example assumes NVidia (choose the latest driver number in your own installation)
    a. Install CUDA/OpenCL drivers
        $ sudo apt install clinfo
        $ sudo apt update
        $ sudo apt install nvidia-driver-535 nvidia-cuda-toolkit
    b. Change setting X to Y in the Rmd to enable cmdstanr to use your GPU for sampling
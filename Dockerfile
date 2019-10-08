FROM rocker/tidyverse:3.6.1

# RUN apt-get update && apt-get install -y cargo

# RUN install2.r --error \
#     --deps TRUE \
#     dismo \
#     XML \
#     knitr \
#     here \
#     ggspatial \
#     tmap \
#     plotGoogleMaps \
#     plotrix \
#     && R -e "devtools::install_github('geocompr/geocompkg')"
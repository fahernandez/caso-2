FROM rocker/tidyverse:3.6.1

RUN install2.r --error \
     --deps TRUE \
     extrafont

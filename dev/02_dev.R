# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "shiny" )
usethis::use_package( "shinythemes" )
usethis::use_package( "shinydashboard" )
usethis::use_package( "shinycssloaders" )
usethis::use_package( "shinyjs" )
usethis::use_package( "shinyalert")
usethis::use_package( "shinipsum")
usethis::use_package( "shinyWidgets" )
usethis::use_package( "ggplot2" )
usethis::use_package( "rintrojs" )
usethis::use_package( "bs4Dash" )
usethis::use_package( "plotly" )
usethis::use_package( "arrow")
usethis::use_package( "gridExtra")
usethis::use_package( "rlang")
usethis::use_package( "RColorBrewer")
usethis::use_package( "reshape2")
usethis::use_package( "waiter")
usethis::use_package( "magrittr")#so precisamos da funcionalidade "%>%"
usethis::use_package("lubridate")
usethis::use_package("forecast")
usethis::use_package("xts")
usethis::use_package("sp")
usethis::use_package("readxl")
usethis::use_package("stringr")
usethis::use_package("astsa")
usethis::use_package("stringr")

## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "inicio" ) # Name of the module
golem::add_module( name = "sobre" ) # Name of the module
golem::add_module( name = "descritiva_sinasc")
golem::add_module( name = "descritiva_sim_materna")
golem::add_module( name = "descritiva_sim_neonatal")
golem::add_module( name = "intervencao_sinasc")
golem::add_module( name = "intervencao_sim_neonatal")
golem::add_module( name = "intervencao_sim_materno")
golem::add_module( name = "analise_geografica")
golem::add_module( name ="intervencao_sif_congenita")

## Add helper functions ----

## Creates fct_* and utils_*
#Os arquivos fct_* contêm funções maiores que são mais centrais para
#o aplicativo.
golem::add_fct( "graficos_analise_impacto" )
#Os arquivos utils_* contêm pequenas funções que podem ser usadas
#várias vezes no aplicativo.
golem::add_utils( "helpers" )

#Add fct_* file to a specific module:
golem::add_fct("mapas", module = "analise_geografica")

#Add utils_* file to a specific module:
golem::add_utils("cookie_warning",module = "apresentacao")

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
# Base de dados do modulo análise descritiva
usethis::use_data_raw( name = "dados_sinasc", open = FALSE )
usethis::use_data_raw( name = "dados_sim_materno", open = FALSE )
usethis::use_data_raw( name = "dados_sim_neonatal", open = FALSE )

# Base de dados do modulo análise de intervenção

usethis::use_data_raw( name = "dados_sinasc_intervencao", open = FALSE )
usethis::use_data_raw( name = "dados_sim_materno_intervention", open = FALSE )
usethis::use_data_raw( name = "dados_sim_neonatal_intervention", open = FALSE )

# Base de dados do modulo análise geográfica

usethis::use_data_raw( name = "dados_map", open = FALSE )


## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("SESA")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis()
usethis::use_travis_badge()

# AppVeyor
usethis::use_appveyor()
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")


# script to run the model
library(targets)
library(tarchetypes)

tar_make()
tar_visnetwork()
tar_read(gas_customer_projections)

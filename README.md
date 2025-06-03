# Hey QC helper!

Thanks for QC-ing this work for me. The two scripts to QC sit within a larger model I am making, but operate in a stand alone way. You don't need to worry about any of the other scripts.

To get started, clone this repository, dowload it locally, and drag the (unzipped) Data folder into the main directory.

## Scripts to QC
R/load_jacobs_demand_data.R and R/non_pipe/calculate_emissions_budget.R

## Intro: Setting up and running the model

Open the run_model.R file in the main directory and run the script. This will likely take a little while to run as it's setting up all of the underlying data. 
This isn't strictly necessary for your QC but I'm trying to make the whole thing as reproducible as possible for later QC so it would be best if we run it this way. 
Let me know if you get any funny errors during this process, this is the first time I've gotten someone else to rebuild the model from scratch (sorry).

Next, lets quickly setup a keyboard shortcut for you that will make debugging really easy. Go to Tools > Modify Keyboard Shortcuts in RStudio. Search for 'Load target at cursor' and assign a shortcut (I use ctrl + l).
This lets you instantly load any object that is defined by the model by putting you cursor on it and pressing the shortcut.

## Getting ready to QC

Targets operates in an environment that is created in the '_targets.R' script. When you're going through and looking at individual scripts you will need to load all the library dependencies at the top of that script before you start. Some scripts also make use of some simple 'helper' functions that are defined in "R/helpers.R" you should run this script before going in to QC an individual script. This makes sure these functions are already defined.

Each script is completely stand alone (except for the librarys and helper functions loaded in the start of the pipeline), and only uses inputs that are defined in the arguments section of the function. Because of this, it's good practice to clear loaded objects out of your environment before you start going through a new script.

## Load in the demand data

Open up the 'load_jacobs_demand_data.R' file. This script takes a file path as its input and reads in all the relevant electricity demand data that we have been sent by the external modelling team,

Load the jacobs_demand_data_file object into your environment by using the load targets shortcut.

Then, run through each line of code in the function as you normally would. The main thing to check here is that we are reading in what we think we are. The file contains weirdly formatted electricity demand
but we're only interested in the totals for each network for the purpose of this QC. As long as those are all read in correctly we are good. There are a few small cleaning lines to get the data in a nice format. 
You should check these haven't introduced any errors.

## Calculate carbon budget

This script uses the emissions intensity of some Climate Change Authority modelling to determine an overall carbon budget for the national electricity demand. 
The basic principle is that the emissions intensity of generating a TWh of electricity in the CCA model represents a reasonable decarbonisation pathway for our modelled electricity grid.
To calculate an overall carbon budget, we multiply the CCA emissions intensity by the projected demand figures that we have just loaded in. This tells us how much carbon can be emitted if the grid decarbonises at the rate modelled by the CCA with the demand modelled by us.

This script can be run through as normal, because it sits outside of the rest of the model pipeline. Let me know if anything is unclear!


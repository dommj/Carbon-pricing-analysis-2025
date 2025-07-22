# Hey Liz!

Thanks a bill for QC-ing this.

To get started, clone this repository, download it locally, and drag the (unzipped) Data folder into the main directory.

## Scripts to QC
Rather than a list of scripts, we want to QC the recipe that is encoded in the _targets.R file. We can discuss the best way to do this when we meet, but essentially that involves going through each object that we load in and create and QCing all the functions that are used to create objects in the pipeline. Every function that is used is stored in a script of the same name so it's easy to find what code you need to look at.

## Intro: Setting up and running the model

Open the run_model.R file in the main directory and run the script. This will likely take a little while to run as it's setting up all of the underlying data. 
Let me know if you get any funny errors during this process.

Next, lets quickly setup a keyboard shortcut for you that will make debugging really easy. Go to Tools > Modify Keyboard Shortcuts in RStudio. Search for 'Load target at cursor' and assign a shortcut (I use ctrl + l).
This lets you instantly load any object that is defined by the model by putting you cursor on it and pressing the shortcut.

## Getting ready to QC

Targets operates in an environment that is created in the '_targets.R' script. When you're going through and looking at individual scripts you will need to load all the library dependencies at the top of that script before you start. Some scripts also make use of some simple 'helper' functions that are defined in "R/helpers.R" you should run this script before going in to QC an individual script. This makes sure these functions are already defined.

Each script is completely stand alone (except for the librarys and helper functions loaded in the start of the pipeline), and only uses inputs that are defined in the arguments section of the function. Because of this, it's good practice to clear loaded objects out of your environment before you start going through a new script.

## Going through code

Open up the file as normal. Load the necessary objects into your environment by using the load targets shortcut.

Then, run through each line of code in the function as you normally would. 




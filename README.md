## ✅ Scripts to QC

- `R/load_jacobs_demand_data.R`
- `R/non_pipe/calculate_emissions_budget.R`

---

## 🧰 Intro: Setting up and Running the Model

1. Open the `run_model.R` file in the main directory and run the script.  
   This step sets up all the underlying data and may take a while. While it's not strictly necessary for this QC, it ensures the work is reproducible and helps with future QC rounds.

2. Let me know if you run into any errors. This is the first time I’ve had someone else rebuild the model from scratch—so your feedback is super helpful (and apologies in advance!).

3. **Optional but helpful:** Set up a debugging shortcut in RStudio:
   - Go to **Tools > Modify Keyboard Shortcuts**
   - Search for **“Load target at cursor”**
   - Assign a shortcut (I use `Ctrl + L`)
   
   This allows you to instantly load any object by placing the cursor on it and hitting the shortcut.

---

## 🔌 Load in the Demand Data

- Open `R/load_jacobs_demand_data.R`.  
  This script takes a file path as input and reads in all the electricity demand data provided by the external modelling team.

- Use the **Load target** shortcut to load the `jacobs_demand_data_file` object into your environment.

- Then step through the function line-by-line:
  - Ensure the data is being read in correctly—specifically, we care about the *totals for each network*.
  - Check that the data cleaning steps haven't introduced any errors.
  - The input file has some odd formatting, so this part is mainly sanity checking.

---

## ♻️ Calculate Carbon Budget

- This script uses emissions intensity values from Climate Change Authority (CCA) modelling to calculate a carbon budget for electricity demand.

- Basic principle:  
  Multiply the CCA emissions intensity (per TWh) by our projected demand to get an overall carbon budget. This assumes the CCA decarbonisation pathway is a reasonable target.

- The script operates independently of the model pipeline, so it can be run through normally.

Let me know if anything is unclear!
"""

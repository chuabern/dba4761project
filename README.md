Note: This is Nico's first group project.

# dba4761project
project group for DBA4761

1. Revise the “calculations” worksheet on the spreadsheet “validation.xlsx”
2. Check how I re-organized for you the formulations on the “formulation” worksheet. So you can understand the formulations in detail.
3. You need to code the function "rd.leases.adjustments.f" on the file “libs/rd_leases.R”
4. The results in the “output” worksheet can be generated using the routines on “main.R”
5. Then you need to validate the results on the “validation” worksheet to make sure your code is perfect.

Deadline: September 30th, 2020 @ 11:59:59 PM


###
How on earth do the files work?
From what I understand:
- validation.xlsx[data] contains the raw data
- validation.xlsx[calculation] contains the calculations for the various formulas in the first row, using data from [data]
- upon successful running of the `main.R` file, the results will be saved in an xlsx file in "/output"
  - before that, we need to fill in the function for `rd.leases.adjustments.f` that can be found in "./libs/rd_leases.R"
- "./data/biotech.xlsx" is a duplicate of "validation.xlsx"

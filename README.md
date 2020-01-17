# imd_2019map
This code contains a function to produce IMD 2019 maps for any local authority in England. The code inside the function is courtesy of Trafford Data Lab (Thank you).

You can view the map output of the code here: https://www.trafforddatalab.io/recipes/importing_data/IMD.html

Instructions for use:

Run the code from the .r file first and then enter your area of interest inside quotation marks:

imd_map("Your area")

e.g. To produce the map for Salford

imd_map("Salford")

This should produce a choropleth map showing the Index of Multiple Deprivation 2019 by decile for your area.

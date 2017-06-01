# Manager_Incentives

The code in this repository computes a comparison of tracking error for equity investment managers before and after periods of high relative performance.

The return data used can be found in the Data folder.

The raw RMarkdown file is names "Manager_Incentive_Results.RMD". There is also a pdf with all the exhibits as well as the code titled "Manager_Incentives_Results.pdf".

For those interested in running the analysis be aware that the calculations take a significant amount of time (about 1.5 hours to run the whole analysis), in addition parallel computing is employed to improve the performance which may cause some issues if you are trying to run on your computer and don't have this properly setup (generally just downloading the parallel package but there could be complications). Also, make sure to change the file path of the data file to your own directory.


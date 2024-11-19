# Carbon Additionality Assessments across the Northeast, Northwoods, Central Appalachians, and Southern Appalachians


### /fiadata/
This is hidden due to size but holds the outputted tables from rFIA. To make the BAU_template.Rmd run, you need to make a new folder for each project you want to run.
**Please note:** the /fiadata/ folder is hidden in the .gitignore file since it gets so big. Please make a local version on your Desktop. 

### /analyses/
This folder holds the BAU_template.Rmd and each project folder. Simply create a new version of the BAU_template.Rmd that is specific for each project. Follow the prompts in lines 18-99 in the blue highlighted and bolded sections. 

#### /analyses/input/
This folder holds all relevant species and forest type .csv files that the BAU_template.Rmd calls on


## Building a new project (e.g., /analyses/newprojectname/)
#### 1) Create a new "output" folder. Keep it empty. Make sure to use all lowercase letters
#### 2) Create a new "figures" folder. Again, this is a new, empty folder and please use all lowercase letters
#### 3) Move a copy of the BAU_template.Rmd file to this new project folder
         3a. **Please note:** You almost always have to modify the BAU_template.Rmd code to be project specific. 
#### 4) Copy an FVS folder from /fvs/ and modify following the README in the folder.


## Recommendations:
#### 1) When listing out the Forest Type Group, it must follow exactly what is listed in the analyses/input/REF_FOREST_TYPE_GROUP.csv "MEANING" column
#### 3) The Forest Type Groups must be listed in ALPHABETICAL order for output to be appropriate. 


## Notes on each region folder:
#### 1) The dynamicbaseline.R file which runs the matching code across 10 "project sites" for each region and forest type combination
#### 2) In the fvs/ folder there are two subfolders: 
#####          a. 01_fvssimulations - holds all Python code to run FVS from the computer
#####          b. 02_carbonaccounting - compiles FVS output into R as .csv files to later compare to the measured, dynamic method

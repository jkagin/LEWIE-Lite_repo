# LEWIE-Lite_repo
 
This repo contains resources to run LEWIE-Lite dashboards. 

Contents:
=======

- The folder "app_current" contains the code files for the most up_to_date. 
This is the only folder you need to run a LEWIE-Lite dashboard. 
 
- The archives folder contains old files.  It may get deleted at some point in the future. 

- The Specific_project_apps contains what the title says.  They are all older versions of the dashboard compared to "app_current", and are of limited use. 

- The Tutorials folder contains links to video tutorials, also here:
https://vimeo.com/showcase/10809827 

- The Unspecified_project_apps contains code to make apps with no specific defined location yet. Their input sheets are online, waiting to be dedicated to a future LEWIE-Lite study site.  



Running a Dashboard:
==============

LEWIE-Lite dashboards are coded in R, and deployed as Shiny apps. 
To compile a dashboard locally, you will need R and RStudio installed. 
To publish a dashboard online, you will additionally need a Shinyapps.io account.  (Technically, you can probably use any other server that supports Shiny, but I have never tried it).

Tu run the dashboard, just open the app.R file in R and hit run. 
Note that the Excel input sheet needs to be in the same folder as the app.R file. 
More detail on how to deploy the code are written in the code itself, as comments.   


=======
Main contributor: Mateusz Filipski
Other contributors: Parth Chawla, Justin Kagin, and J. Ed Taylor. 

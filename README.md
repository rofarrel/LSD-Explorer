# LSD-Explorer-v2.0 (Windows Only)
### 1. Last Update: December 30, 2020
```diff
Version 2 Has Arrived!

@@  Hotfix   @@
      * Updated app.R file to exclude the newly added UR2 column from scraped LSD.org data. The added column had caused 
        interview invite dates to be assigned to decision dates

@@  2.0 Changelog:  @@
      * Now comes with preloaded data (T14 school for cycles 17-18, 18-19, 19-20)
      * Allows multiple cycles to be selected for downloading data
      * Added warnings for downloading too large of a selection of data and ordering the merging datasets
      * The Merge feature now has selectors in which you can choose which selections of data to merge
      * Added a Feedback tab that links to an issue-reporting thread

@@  2.0 Issues and Bugs:  @@
      * The app still often loads to a blank white page when starting. Click "View" then "Reload" to
        load the app.
      * The Electron framework sometimes does not close all windows processes upon exiting the program. Check your
        task manager for "Electron" or "R-Portable Front End" processes lingering after closing the application.
```
Here is a video showing how the app works: https://www.youtube.com/watch?v=GRMn8NR45c4
    
![LSD Explorer](https://i.imgur.com/udwM9nJ.png)


### 2. If you are from a law school, please visit https://rofarrel.shinyapps.io/LSD-Explorer-v-2/ for a web version of the app. Please read below:
```diff
! Enter my LSAC ID as the username and password for access.
! This secure login is only implemented for the web version to prevent public visitors from using server resources.
```

### 3. **How to Use**

1. Click "Code" button to download the .zip folder.

2. Extract the contents of the .zip folder after finished downloading.

3. Run the "LSD-Explorer" application in the LSD-Explorer-RO folder.

4. **Click "View" then "Reload" if only a white page appears**. 

5. **Notice**: The first time you scrape for data online, you will be prompted to allow PhantomJS to access the internet. This is only a headless browser, which the scrape function uses to simulate webpages. It is safe and the full code of the app is available in the Resources folder if you wish to view how it is used.

### 4. **Notes**

* If the app loads only a white screen, press "View" then "Reload" and that should load the app.

* This __only__ runs on Windows. I have not created a MacOS compatible version yet. If you are on a mac, you have 2 options. The easiest option is to run the R code yourself in RStudio. The code is found in the Resource->app folder and is the "app.r" file. The other way for you to run the program is to use a virtual machine on your mac and run windows on the virtual machine.

### 5. **How the program works**

* On the first use, you will need to download data from LawSchoolData.org. Select the application cycle and the schools that you want to analyze. Right now only the top 50 ranked schools are supported. Then click "Get Data" to download the data.
![LSD Explorer](https://i.imgur.com/Oe4PhqC.png)

* **The "Data-View" selector at the top left of the main page will determine what data you are viewing for the other 3 pages (View Data, Scholarship, Predictor).** Below the functions on the main page, there will be a table showing you which schools and cycles are available in the data you have selected. This will change with the "Data-View" selector if you have imported data and scraped data in the same session.
![LSD Explorer](https://i.imgur.com/5HfgEri.png)

* Once you have data, you can use the other tabs (View Data, Scholarship, Predictor, and Twitter). It is advisable to save the data via the "Save" button. This will allow you to download the data as a .csv file and be able to directly import this file on your next use via the 'Import Data' function.

* The "View Data" tab allows you to explroe the data more closely. You can choose the schools and cycles you want to work with for the data. The histogram has both Fill and Stack options. The plots are interactive; you can click and drag to highlight and zoom in on sections of the plots. This is particularly useful for the time-series plot looking at decision "waves". Notice that the top right of each plot there are features you can enable for interacting with the plots.
![LSD Explorer](https://i.imgur.com/J3W2T7E.png)

* The Scholarship tab uses all cycles in available in the selection of data you are using, as selected by the "Data-View" selector on the main page.
![LSD Explorer](https://i.imgur.com/TbpefAB.png)

* The Predictor page has graphs at the top which correspond to the school chosen in the dropdown box. Below the graphs is a table showing you your estimated chance of being admitted or waitlisted to all schools in the data **which is calculated based on the data you are viewing**. This means that it is predicting your chances based on the cycle(s) of data you in your dataset. 
![LSD Explorer](https://i.imgur.com/1bDfNOb.png)

* To get more than 1 cycle in a dataset, you should scrape the schools you want for 1 cycle. Save that data. Load the file you just saved in the Load feature, then scrape data for the same schools but for the next cycle that you want. Once you have data loaded in the Load feature and data in the "Gather" feature, you can "update" the data and it will combine then. You can then save this new merged/updated dataset and can load it anytime in the future. *Remember, you do not need to scrape past cycles more than once. Those data will not change, as it is in the past. You only need to scrape data for any ongoing cycle.*

### 6. **Changelog History**
```diff
@@  1.6.3 Changelog:  @@
      * Hotfix July 29, 5:45pm GST for a missing gginnards package error for the Predictor
      
@@  1.6.2 Changelog:  @@
      * Fixed user table in View Data tab not reacting to LSAT and GPA inputs
      
@@  1.6.1 Changelog:  @@
      * Added a cycle filter to the 'Scholarship' tab
      * Added a dedicated user search sub-panel in the 'View Data' tab
      
@@  1.6.0 Changelog:  @@
      * Twitter functionality disabled
      * Added Show/Hide functionality to prediction curves
      * Improved hover data for 'View Data' plots
      * Improved hover data for prediction plots
      * Added URM filters to 'View Data' and 'Scholarship' tabs
```

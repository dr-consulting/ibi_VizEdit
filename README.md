# Overview of IBI VizEdit

IBI VizEdit is a program built using RShiny. It is designed to assist in the manual editing of inter-beat interval files that are derived from photoplethysmogram (PPG) recordings. Unlike the electrocardiogram signal (EKG or ECG), PPG signals are characterized by a slow-moving waveform, which present a different set of challenges when the true signal becomes contaminated with motion artefacts and other sources of noise. 

Though increasingly popular due to their ease of use, most heart rate editing software that exists to date was designed and optimized for the detection and editing of inter-beat interval files derived from EKG signals. IBI VizEdit provides a new suite of tools for researchers who find themselves working with messy PPG files. 

Please note that IBI VizEdit is beta software. It has not been fully tested, and there are likely numerous bugs and opportunities to optimize code and performance. Any and all feedback is welcome. 

*As of right now, IBI VizEdit is only supported for use on Windows 7/8/10.* 

Please cite as: 

Barstead, M. G. (2018). IBI VizEdit v.0.5: An RShiny Application[Computer software]. University of Maryland.

## Features and Settings

### 1. Supported File Types 

IBI VizEdit can work with any raw PPG (or even EKG) signal that is saved as a tab-delimited `.txt` file. The data should exist in a single column. If header information exists, that is fine as there is an option when loading the raw file to specify the number of header rows (IBI VizEdit will discard these rows when reading in the data).

The program supports files sampled at a rate of up to 2000 Hz, though it is **strongly** recommended that you make use of the downsampling feature within IBI VizEdit to reduce the file size and overall computation time. You will definitely notice the difference in speed when attempting to use any of the advanced imputation features. You may also find some of the graphics slow to refresh when using even the basic features if raw files are particularly large. The current beta version of the program (v.0.5) has an **overall file size limit of 150MB.**

### 2. Selecting a Working Directory

One slight downside in working with RShiny is that it can be slow in navigating a system's file structure. To help speed this process up, before you go to use IBI VizEdit, place all files you would like to edit in a specific folder. This includes the timing file (more on that below). After you have selected the appropriate folder, choose the file you would like to edit. 

When you save your processed and edited files, the program will create a new output folder that will contain a `Case Processing Summary.rtf` file, a raw `.txt` file containing the downsampled version of your PPG data, a separate time series `.csv` for each epoch interval you selected on the opening screen, a overall summary `.csv` file containing heart rate and heart rate variability scores by task, both edited and raw versions of your processed IBI files (as `.txt` files), and finally, separate IBI `.txt` files separated by task. 

### 3. Selecting File Settings

When initially loading your raw file, there are several important inputs and decisions required. First, to standardize file processing and naming, IBI VizEdit requires that files are named using the following convention: `ID#_Timepoint.txt` . The ID of the participant should be the study ID used by the researcher and the `Timepoint` can be any mix of legal filename characters that specify a study time point (e.g., T1, T2, etc), a group assignment (e.g., Tx, Crtl), or a study task or condition (e.g., Task1, Condition2, etc.). 

After you have input the necessary file information, you have the option of specifying an additional identifier for your output files. For instance if your raw file contains several heart rate signals from separate individuals, you may want to specify which one you are working on using this identifier (e.g., subjA, subjB, etc.). 

Timing files are necessary for IBI VizEdit, even if you don't have any tasks you want to use to separate your output.  Absent any meaningful timing files, the researcher should create a timing file with a column of 0's and a column total with rows indexed by the ID names of the files (see above). 

Once all of the files are loaded, you should specify the column in the PPG data file with the relevant channel, the number of header rows (if any), the sampling rate of the original file, and the downsampling rate of the final file. Also be sure to select the number of different epoch lengths you would like to see in the timeseries data files created by the program. 

## Processing Your File

Once you have loaded the various settings for the program, your next task is to process the PPG file and identify the peaks. First, use the View Plot button on the second panel to ensure that the data has loaded correctly. If you are satisfied that you have the right file loaded, click the "Process File" button. The peak detection algorithm will iteratively identify an optimal processing bandwidth and return a raw interbeat interval file. The processing summary will populate on the second panel when finished. The same table will be included in the `Case_Processing_Summary.rtf` output when you have finished editing your file.

## Editing Your File 

More details coming...

## Interpreting Your Output

More details coming...

## Advice on Editing Choices

More details coming... 

## Gaussian Processes, Stan, and Rstan

More details on this coming... 
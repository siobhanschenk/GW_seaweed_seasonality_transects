--------------------------------------
----------- LINKS TO THINGS ----------
--------------------------------------
// Brief intro of the study //

On a monthly basis, we record the abundance of all visible macroalgae species in quadrats (1 m by 1 m squares) that we place at the same intervals along fixed transects in a rocky intertidal zone in Stanley Park (Vancouver British Columbia).
This study was prompted by the lack of baseline data that is publicly available regarding the ecology of seaweeds in the region. To use and cite these data, please go to the Borealis Dataverse https://doi.org/10.5683/SP3/IKGB6E , which cleaner and likely more up to date.  
/


// Shiny app code//
Is in the seaweed_transect_gw folder. This folder contains the app code and the app data for https://siobhanschenk.shinyapps.io/algae_transects_stanley_park/ 
/

--------------------------------------
----- INDIVIDUAL FILE INFORMATION ----
--------------------------------------

//GIAW transect data sheet draft //
Data sheet we use to record the seaweed percent cover during the transect sampling
/

// GW_quadrat_heigth_and_sampling_limit //
R script to link the distance of the quadrat from the seawall in metres to the actual quadrat height as per tidal data.
/

// GW_seaweed_data_cleaning //
R script we use to clean the transect data before uploading to Borealis.
/

// GW_seasonality_transect_data //
year: sampling year
month: sampling month
day: sampling day
low_tide_time_PDT: the time of lowest tide of the day in Pacific Daylight Time (PDT) as reported by the government of Canada tide tables. Sampling started approximately two hours prior to the time listed in this column
low_tide_height_m: the height of the lowest tide in metres (m) as reported by the government of Canada tide tables.
transect_id: the numerical identification of the transect. We started with 5 transects total by cut it down to 3 for all sampling events after 2022-04-20. 
distance_along_transect_m: the distance away from the seawall in m. Quadrat 1 is from 0 to 1 m, quadrat 2 is from 5 to 6 m and so on (see protocol.pdf). Please note that this is not the same as tide height. At this time, we have not yet calculated which quadrats are exposed to air at which tide height.
dominant_seaweed: this is the seaweed that had the highest percent cover in the quadrat. 

The remaining columns list the seaweed genus_species__phylum (brown=Phaeophyceae; green=Chlorophyta; red= Rhodophyta). 
For some genera, we have not resolved to the species level, since molecular work is necessary to do so and there are almost certainly multiple cryptic species (see mastocarpus_sp). 
We also note that our Ulva_species identifications are best guesses based on morphology. We reccomend grouping the Ulvas listed into Ulva_sp for analysis. 
The numbers in the seaweed genus_species columns is the estimated percent cover of that seaweed. Since seaweed overlap, there can be over 100% total cover per quadrat. 
Estimated percent cover at or below 1% was rounded to 1%. For percent cover greater than 1 and below 5%, we reccoded to the nearest percent. Above 5%, we recorded the percent cover to the nearest interval of 5 (e.g 3% was recorded as 3%, 7% was recorded as 5%, and 9% was recorded as 10%).  
/


// kelp_reproductive_timing //
year: sampling year
month: sampling month
day: sampling day

The remaining columns indicate the genus.species of the seaweed (all kelps, Laminariales).
A "y" indicates that reproductive individuals were observed on that sampling occasion. 
Please note that an absence of "y" does not indicate no reproductive individuals and that species not listed here do not mean the species was never reproductive. 
These are opportunistic observations and kelp (Laminariales) reproductive tissue is easy to spot compared to other seaweeds. 
/

// poster_schenk_suprayata_psa2023 //
Our poster about the transect work, which we presented at the Phycological Society of America meeting in 2023.
/

// About the protocol //
This pdf document explains our sampling procedure and indicates the location of the transects near the Girl in a Wetsuit Statue (S.S. Empress of Japan Figurehead Replica) Stanley Park, Vancouver, British Columbia, Canada (49°18'09.9"N 123°07'34.9"W)
/

// standpole_datasheet //
File to record the current time and quadrat height using stand poles so it can be matched to tide height. This is how we obtained the quadrat heights.
/

// About the study_explanation //
This document outlines our current engagement with the Indigenous peoples whose land this research is being conducted on and provides more background regarding why the study is being conducted. 
/

//tide_obs_20230801 //
OBSERVED tide height downloaded from tides.gc.ca to match the observed tide height to the measured quadrat height with the stand poles.
/

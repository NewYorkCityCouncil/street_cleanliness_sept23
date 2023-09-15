# Oversight of Street Cleanliness in NYC
Data analysis and visuals for NYCC 09.12.23 ['Oversight - DSNY’s Initiatives to Address Street Cleanliness'](https://legistar.council.nyc.gov/MeetingDetail.aspx?ID=1115076&GUID=2C89CBEC-AC94-4A45-8EFB-18543811A283&Options=info|&Search=) hearing.

An associated webpage for this analysis can be found on the [council website](https://council.nyc.gov/data/clean-streets/). 

***  

#### Open Data Sources 
- [Vacant storefront registration](https://data.cityofnewyork.us/City-Government/Storefronts-Reported-Vacant-or-Not/92iy-9c3n)
- [DCA licenses](https://nycopendata.socrata.com/Business/Legally-Operating-Businesses/w7w3-xahh/data)
- [Restaurants](https://data.cityofnewyork.us/Health/DOHMH-New-York-City-Restaurant-Inspection-Results/43nn-pn8j)
- [OATH Violations](https://data.cityofnewyork.us/City-Government/OATH-Hearings-Division-Case-Status/jz4z-kudi)
- [311 Complaints](https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2010-to-Present/erm2-nwe9)
- [DSNY Litter Baskets](https://data.cityofnewyork.us/dataset/DSNY-Litter-Basket-Inventory/8znf-7b2c)
- [LION](https://data.cityofnewyork.us/City-Government/LION/2v4z-66xt)
- [PLUTO](https://data.cityofnewyork.us/City-Government/Primary-Land-Use-Tax-Lot-Output-PLUTO-/64uk-42ks)
- [PAD](https://data.cityofnewyork.us/City-Government/Property-Address-Directory/bc8t-ecyu)


##### Summary & Intention
On September 12, 2023, the Committee on Sanitation and Solid Waste Management held an oversight hearing on DSNY's initiatives to address street cleanliness. In addition, the committee heard a series of legislation related to other sanitation concerns (for a full list see the recommendations section).

The data team provided analysis on: 
- The prevalence of several types of sanitation issues by referencing 311 complaints and OATH violations
- Areas of concern, particularly exploring the relation of areas where businesses are to commercial waste

Using data from OATH, 311, and data sources related to where businesses are, we explore the prevalence of sanitation issues and identify areas that need additional support from DSNY. 

#### Main Takeaways
- Dirty conditions were the most prevalent issue in our analysis, receiving both the highest number of OATH violations and 311 complaints. While most NYC streets have few dirty sidewalk OATH violations, less than three, there are eight streets that have more than 50 violations per total number of properties on that street.
- The proportion of OATH violations that go to businesses is xxx. The businesses with the most dirty sidewalk violations/complaints are located in Southwest Queens on Rockaway Parkway, with 70 complaints per business, and in Floral Park on Union Turnpike with 50 violations/complaints, also in Queens.
- 311 derelict vehicle and illegal dumping calls have increased over time but have had fewer OATH violations issued. There were 18 derelict vehicle calls and 39 illegal dumping calls for each violation made. While there was only 1 dirty conditions call for every 4 violations.


#### Recommendations
To address this issue, the Committee on Sanitation and Solid Waste Management will be hearing the following legislation:
- Establishing a tracking system concerning the disposal of yellow and brown grease. [Read the Bill: Int 0413-2022](https://nyc.legistar.com/LegislationDetail.aspx?ID=5656541&GUID=E8BCC947-F3D2-4531-B70F-3AFA5F5AAC0F&G=2FD004F1-D85B-4588-A648-0A736C77D6E3&Options=&Search=)
- Resources for cleanup and enforcement of dumping. [Read the bill: Int 0769-2022](https://nyc.legistar.com/LegislationDetail.aspx?ID=5871072&GUID=B4BF735F-D099-4EB9-9BC8-7AC25A892E10&G=2FD004F1-D85B-4588-A648-0A736C77D6E3&Options=&Search=)
- Increasing the civil penalty for repeated littering violations. [Read the bill: Int 0809-2022](https://nyc.legistar.com/LegislationDetail.aspx?ID=5898982&GUID=30CA2CAA-418D-4785-8BEC-D6BE2A681D51&G=2FD004F1-D85B-4588-A648-0A736C77D6E3&Options=&Search=)
- Emergency and resiliency plans of the department of sanitation. [Read the bill: Int 0861-2022](https://nyc.legistar.com/LegislationDetail.aspx?ID=5971622&GUID=DE19832C-5B9E-423A-9FF6-F8C884928D6F&G=2FD004F1-D85B-4588-A648-0A736C77D6E3&Options=&Search=)
- Expanding the commercial citywide routing system for sidewalk cleanliness violations and technical amendments thereto, including to repeal and reenact subdivision c of section 16-118.1. [Read the bill: Int 0981-2023](https://nyc.legistar.com/LegislationDetail.aspx?ID=6165363&GUID=7DF17D92-232A-4E2D-8D8A-E5E9A0643B3E&G=2FD004F1-D85B-4588-A648-0A736C77D6E3&Options=&Search=)
- Removal of abandoned or derelict vehicles. [Read the bill: Int 1032-2023](https://nyc.legistar.com/LegislationDetail.aspx?ID=6202973&GUID=F6F6C2DF-857C-4815-9605-42A165DF4BFD&G=2FD004F1-D85B-4588-A648-0A736C77D6E3&Options=&Search=)

#### Methodology 
Our timeframe for the analysis is from August 1, 2022 - August 31, 2023.

###### Street Cleanliness Categories
[Fill in]

###### Dirtiest Streets: Streets with the Highest Number of Dirty Sidewalk/Littering OATH Violations
- Using the API, we queried all violations issued by Sanitation and then manually grouped similar charge_1 codes into categories. The complete list of charge codes that were grouped is available [here](https://github.com/NewYorkCityCouncil/street_cleanliness_sept23/blob/main/data/output/oath_codes/oath_charges_grouped.csv).
- The charge_1 codes for Dirty Sidewalk and Littering were used to pull any violations not captured by just querying the issuing agency.
- Joined violations to the property boro, block & lot dataset (PLUTO) to get lat/long for mapping.
- Joined to the property addresses dataset (PAD) to get the street segment id which is needed to then join to the streets (LION) data.
- Aggregated the yearly number of violations to each street. (Dirty Sidewalk/Littering Violations --> BBL --> PAD --> Streets)
- Removed streets with no violations.
- Normalized the aggregated counts by the total number of properties facing, associated with, or that matched to the street.
  
###### Commercial Waste & Businesses
[Fill in]

###### Littering Complaints & Baskets
[Fill in]
#### Scripts

domain=Antarctica

grid_name_tgt=ANT-32KM

#grid_name_src=ANT-16KM
#nc_src=../ice_data/${domain}/${grid_name_src}/${grid_name_src}_REGIONS.nc 

#grid_name_src=ERA5
#nc_src=../data/era5/tas/era5_2m_temperature_1950_monthly.nc

grid_name_src=RACMO-ANT-27KM
nc_src=../data/RACMO2.3p2/ANT-27KM/t2m_monthlyA_ANT27_ERA5-3H_RACMO2.3p2_197901_202212.nc

cdo gencon,grid_${grid_name_tgt}.txt -setgrid,grid_${grid_name_src}.txt ${nc_src} scrip-con_${grid_name_src}_${grid_name_tgt}.nc


# To perform remapping using the weights file
# cdo remap,grid_${grid_name_tgt}.txt,scrip-con_${grid_name_src}_${grid_name_tgt}.nc ${infile} ${outfile}


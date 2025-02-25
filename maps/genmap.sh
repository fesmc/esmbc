domain=Antarctica
grid_name_src=ERA5
grid_name_tgt=ANT-32KM
#nc_src=../ice_data/${domain}/${grid_name_src}/${grid_name_src}_REGIONS.nc 
#nc_src=../data/era5-processed-esmvaltool-historical1/tas_CLIM_mon/native6_ERA5_reanaly_v1_Amon_tas_1951-1980.nc
nc_src=../data/era5/tas/era5_2m_temperature_1950_monthly.nc

cdo gencon,grid_${grid_name_tgt}.txt -setgrid,grid_${grid_name_src}.txt ${nc_src} scrip-con_${grid_name_src}_${grid_name_tgt}.nc


# To perform remapping using the weights file
# cdo remap,grid_${grid_name_tgt}.txt,scrip-con_${grid_name_src}_${grid_name_tgt}.nc ${infile} ${outfile}


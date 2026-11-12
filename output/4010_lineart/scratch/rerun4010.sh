cd "/Users/pedro/Documents/Working/~RCode/MyRCode/LEGO/LEGO_sets_networks/output/4010_lineart"
export LANG=C.UTF-8
Rscript run_plot.R > plot_new.log 2>&1; echo plot=$?
cp 4010_icons.pdf 4010_network.pdf
Rscript run_plot_coords.R > scratch/coords.log 2>&1; echo coords=$?
python3 build_adjacency_xlsx.py > scratch/xlsx.log 2>&1; echo xlsx=$?
echo ALLDONE

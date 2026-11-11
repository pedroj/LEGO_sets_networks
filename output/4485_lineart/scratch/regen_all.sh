cd "/Users/pedro/Documents/Working/~RCode/MyRCode/LEGO/LEGO_sets_networks/output"
export LANG=C.UTF-8
for d in 10174 75181 4010; do cp 4485_lineart/plot_lego_network.R ${d}_lineart/plot_lego_network.R; (cd ${d}_lineart && Rscript run_plot.R > plot_new.log 2>&1; echo "$d EXIT=$?"); done
(cd 4010_lineart && cp 4010_icons.pdf 4010_network.pdf)
cd 4485_lineart
for f in run_plot.R run_plot_split.R run_plot_split_nominifig.R run_plot_coords.R; do Rscript $f > scratch/new_$f.log 2>&1; echo "4485 $f EXIT=$?"; done
python3 scratch/composite.py
for f in 4485_icons.png 4485_network.pdf 4485_sebulba_icons.png 4485_sebulba_icons.pdf 4485_anakin_icons.png 4485_anakin_icons.pdf; do mv -f $f with_minifigs/; done
echo ALLDONE

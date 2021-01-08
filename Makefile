all: experiment1 experiment2 experiment3 experiment4

experiment1: analyze_experiment_1.R lib.R data/exp1/data_training.csv data/exp1/data.csv
	Rscript -e 'knitr::stitch_rhtml("analyze_experiment_1.R")'

experiment2: analyze_experiment_2.R lib.R data/exp2/data_training.csv data/exp2/data.csv
	Rscript -e 'knitr::stitch_rhtml("analyze_experiment_2.R")'

experiment3: analyze_experiment_3.R lib.R data/exp3/data.csv
	Rscript -e 'knitr::stitch_rhtml("analyze_experiment_3.R")'

experiment4: analyze_experiment_4.R lib.R data/exp4/data_training.csv data/exp4/data.csv
	Rscript -e 'knitr::stitch_rhtml("analyze_experiment_4.R")'

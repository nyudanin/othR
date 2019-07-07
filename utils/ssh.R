session <- ssh_connect("naomi@10.0.0.18")
dir <- "naomi@10.0.0.18/mnt/nfs1/SpliceViz/report_data/"
load(paste0(dir, "tnt.RData"))

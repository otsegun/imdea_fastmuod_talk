## table  from: Documents/research_matters/muod_research/muod_submission/submission5_ADAC_revision/New_Accuracy_Test/accuracy_test_new_with_results/manuscript_table1.R
## for model 1 and 2
mm3_cut1 <- mm3[c(1:5, 8:10), 1:4]
write.csv(mm3_cut1, "/home/statimatician/Desktop/IMDEA Talk/gfx/mm_cut1.csv", 
          row.names = F)

mm3_cut2 <- mm3[c(1:5, 8:10), c(1,5:8)]
write.csv(mm3_cut2, "/home/statimatician/Desktop/IMDEA Talk/gfx/mm_cut2.csv", 
          row.names = F)


mm3_cut3 <- mm4[c(1:5, 8:10), c(1:5)]
write.csv(mm3_cut3, "/home/statimatician/Desktop/IMDEA Talk/gfx/mm_cut3.csv", 
          row.names = F)


mm3_cut4 <- mm4[c(1:5, 8:10), c(1,6:9)]
write.csv(mm3_cut4, "/home/statimatician/Desktop/IMDEA Talk/gfx/mm_cut4.csv", 
          row.names = F)

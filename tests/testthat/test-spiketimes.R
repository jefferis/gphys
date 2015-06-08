context("Merge spike times files saved by Igor/Neuromatic")

nmdir=system.file("igor/spikes/nm20120413c0", package = 'gphys')
test_that("merge two blocks of spikes, ", {
  a=CollectSpikesFromSweeps(nmdir,subdir="BLOCKA")
  b=CollectSpikesFromSweeps(nmdir,subdir="BLOCKB")
  
  la=length(a)
  lb=length(b)
  shortest=ifelse(la>lb,lb,la)
  c=merge(a[1:shortest],b[1:shortest])
  expect_true(is.spiketimes(c))
  expect_true(length(c)==shortest)
  
  merged_odours=c("IAA", "cVA", "pro", "PAA", "4ol", "ctr", "vin", "ctr", "fly", 
                  "far", "oen", "pac", "aac", "ger", "lin", "bty", "hxe", "ben", 
                  "met", "oil", "pra", "hxa", "oil", "ehb", "eta", "cit")
  expect_that(attr(c,'oddconf')$odour,equals(merged_odours))
  
  mwi=attr(c,'mergedwaveinfo')
  expect_false(is.null(mwi),
               "Check that merged wave has mergedwaveinfo attribute")
  
  expect_that(mwi[[1,'pxps']],equals(c("000","001","002","003","004","005")))
  expect_that(mwi[[14,'pxps']],equals(c("007","008","009","010","011","012")))
  expect_that(mwi[["0",'pxps']],equals(c("000","001","002","003","004","005")))
  expect_that(mwi[["13",'pxps']],equals(c("007","008","009","010","011","012")))
  expect_equivalent(unlist(mwi[1:13,"merged"]),0:12)
  expect_equivalent(unlist(mwi[14:26,"original"]),0:12)
  expect_equivalent(unlist(mwi[14:26,"merged"]),0:12+12+1)
})
    
test_that("merge two blocks of spikes with unequal lengths (A longer than B) ", {
    a=CollectSpikesFromSweeps(nmdir,subdir="BLOCKA")
    b=CollectSpikesFromSweeps(nmdir,subdir="BLOCKB")
    c=merge(a,b)
    d=merge(b,a)

    expect_true(is.spiketimes(c))
    expect_true(length(c)==7)

    merged_odours=c("IAA", "cVA", "pro", "PAA", "4ol", "ctr", "vin", "ctr", "fly", 
    "far", "oen", "pac", "aac", "ger", "lin", "bty", "hxe", "ben", 
    "met", "oil", "pra", "hxa", "oil", "ehb", "eta", "cit")
    expect_that(attr(c,'oddconf')$odour,equals(merged_odours))
    merged_sweeps=c("000", "001", "002", "003", "004", "005", "006", "007", "008", 
        "009", "010", "011", "012")
    expect_that(attr(c,'sweeps'),equals(merged_sweeps),
        "Check that merged record includes all input sweeps as attribute")
    
    # count spikes
    csa=OdourResponseFromSpikes(a,responseWindow=c(0,4000))
    csb=OdourResponseFromSpikes(b,responseWindow=c(0,4000))
    csc=OdourResponseFromSpikes(c,responseWindow=c(0,4000))
    # should count the same number of spike for odours in a in merged list c
    expect_that(csc[,colnames(csa)],equals(csa))
    # should count the same number of spike for odours in b in merged list c
    # EXCEPT for the last line, which we ignore in this test
    expect_that(csc[rownames(csb),colnames(csb)],equals(csb))
    seven_nas=structure(list(ger = NA_real_, lin = NA_real_, bty = NA_real_, 
            hxe = NA_real_, ben = NA_real_, met = NA_real_, oil = NA_real_, 
            pra = NA_real_, hxa = NA_real_, oil.1 = NA_real_, ehb = NA_real_, 
            eta = NA_real_, cit = NA_real_), .Names = c("ger", "lin", 
            "bty", "hxe", "ben", "met", "oil", "pra", "hxa", "oil.1", "ehb", 
            "eta", "cit"), row.names = 7L, class = "data.frame")
    expect_that(csc[7,colnames(csb)],equals(seven_nas))
    
    # check that merge in the oppposite order is identical
    csd=OdourResponseFromSpikes(d,responseWindow=c(0,4000))
    expect_that(csd[,colnames(csc)],equals(csc),
        'merge unequal number sweeps in opposite order gives same result')
    
    })
    
test_that("merge two blocks of spikes with unequal lengths (B longer than A) ", {
    nmdir=system.file("igor/spikes/nm20121020c2", package = 'gphys')
    a=CollectSpikesFromSweeps(nmdir,subdir="BLOCKA")
    b=CollectSpikesFromSweeps(nmdir,subdir="BLOCKB")
    c=merge(a,b)
    d=merge(b,a)

    expect_true(is.spiketimes(c))
    expect_true(length(c)==8)
    
    merged_odours=c("cVA", "IAA", "pro", "PAA", "4ol", "ctr", "vin", "ctr", "C10", 
    "far", "oen", "pac", "aac", "ger", "lin", "bty", "hxe", "ben", 
    "met", "oil", "pra", "hxa", "oil", "ehb", "eta", "cit")
    expect_that(attr(c,'oddconf')$odour,equals(merged_odours))
    
    # count spikes
    csa=OdourResponseFromSpikes(a,responseWindow=c(0,4000))
    csb=OdourResponseFromSpikes(b,responseWindow=c(0,4000))
    csc=OdourResponseFromSpikes(c,responseWindow=c(0,4000))
    csd=OdourResponseFromSpikes(c,responseWindow=c(0,4000))
    # should count the same number of spike for odours in b in merged list c
    expect_that(csc[,colnames(csb)],equals(csb))
    expect_that(csd[,colnames(csb)],equals(csb))
    # and for a (excluding the last lines(s) with NAs
    expect_that(csc[1:nrow(csa),colnames(csa)],equals(csa))
    expect_that(csd[1:nrow(csa),colnames(csa)],equals(csa))
    })

context("Counting spike times")
test_that("Count spikes - large reponse window", {
  a=CollectSpikesFromSweeps(nmdir,subdir="BLOCKA")
  csa=OdourResponseFromSpikes(a,responseWindow=c(0,4000))

  csa_baseline<-structure(list(IAA = c(4L, 1L, 2L, 1L, 3L, 1L, 4L), cVA = c(4L, 
              7L, 6L, 8L, 8L, 7L, 10L), pro = c(1L, 1L, 4L, 2L, 3L, 3L, 5L), 
          PAA = c(0, 0, 0, 0, 0, 0, 0), `4ol` = c(2L, 4L, 3L, 2L, 2L, 
              3L, 3L), ctr = c(0, 0, 1, 0, 1, 1, 1), vin = c(0, 0, 0, 0, 
              1, 1, 2), ctr.1 = c(0, 0, 0, 0, 0, 1, 0), fly = c(0, 0, 1, 
              0, 0, 1, 0), far = c(0, 0, 0, 0, 1, 1, 1), oen = c(0, 0, 
              2, 0, 1, 2, 1), pac = c(2, 0, 0, 0, 0, 1, 1), aac = c(0, 
              0, 2, 1, 2, 1, 1)), .Names = c("IAA", "cVA", "pro", "PAA", 
          "4ol", "ctr", "vin", "ctr.1", "fly", "far", "oen", "pac", "aac"
      ), row.names = c(NA, -7L), class = "data.frame")
  expect_that(csa,equals(csa_baseline))
})

test_that("Count spikes - NULL baseline", {
      a=CollectSpikesFromSweeps(nmdir,subdir="BLOCKA")
      csa=OdourResponseFromSpikes(a,responseWindow=c(0,4000))
      csa.nb=OdourResponseFromSpikes(a,responseWindow=c(0,4000),baselineWindow = NULL)
      
      expect_that(csa.nb,equals(csa))
    })

test_that("Count spikes - with baseline", {
      nmdir=system.file("igor/spikes/nm20110914c4", package = 'gphys')
      spikes=CollectSpikesFromSweeps(nmdir,subdir="BLOCKI",sweeps=0:4)
      od=OdourResponseFromSpikes(spikes,response=c(2200,2700),baseline=c(0,2000))
      
      od_baseline<-structure(list(ctr = c(-1.5, -0.25, 0, 0.75, -0.75), fly = c(-0.25, 
                  -1.25, -0.75, -0.25, -0.75), `4ol` = c(-0.25, -0.5, -0.5, -1, 
                  -1.5), cVA = c(0, -1.75, -0.5, -1, 0), IAA = c(-0.25, -0.25, 
                  -1.25, 0.25, 2), PAA = c(24, 23, 20.5, 18, 20)), .Names = c("ctr", 
              "fly", "4ol", "cVA", "IAA", "PAA"), row.names = c(NA, -5L), class = "data.frame")
      expect_that(od,equals(od_baseline))
    })

test_that("Count spikes - Shahar data with repeated block", {
      nmdir=system.file("igor/spikes/nm20120906c0", package = 'gphys')
      b8=CollectSpikesFromSweeps(nmdir,8,xlim=c(0,3000),stimRange=c(500,1000))
      # divide into separate data frames for each repeat
      b8s=divide(b8)
      b10=CollectSpikesFromSweeps(nmdir,10,xlim=c(0,3000),stimRange=c(500,1000))
      od8=OdourResponseFromSpikes(b8s,response=c(700,1500))
      od8_baseline=structure(list(OilBl = c(0, 0, 0, 0), BeZal = c(0, 0, 0, 0), 
              bCitr = c(0, 0, 1, 2), `1HxOl` = c(0, 0, 1, 0), Frnsl = c(1, 
                  0, 1, 1), WatBl = c(0, 2, 0, 1), Cdvrn = c(10, 7, 9, 9), 
              Sprmn = c(8, 8, 14, 9)), .Names = c("OilBl", "BeZal", "bCitr", 
              "1HxOl", "Frnsl", "WatBl", "Cdvrn", "Sprmn"), row.names = c(NA, 
              -4L), class = "data.frame")
      expect_that(od8,equals(od8_baseline))
    })

context("Dividing spike times")
test_that("divide spiketimes with n presentations into n separate dataframes",{
      # This organisation is typical for Shahar's data when a single PXP file
      # contains repeated presentations for the same set of odours
      nmdir=system.file("igor/spikes/nm20120906c0", package = 'gphys')
      b8=CollectSpikesFromSweeps(nmdir,8,xlim=c(0,3000),stimRange=c(500,1000))
      # divide into separate data frames for each repeat
      b8s=divide(b8)
      
      expect_warning(b8s_wsplit<-split(b8))
      expect_equal(b8s_wsplit, b8s)
      
      b8s_baseline=structure(list(structure(list(Time = c(85.48837, NA, NA, NA, 
                          1009.242, NA, 852.1313, 895.276, 906.8022, 915.5463, 936.9566, 
                          1010.212, 1022.351, 1085.436, 1123.234, 1140.751, 819.5104, 840.275, 
                          908.2111, 1020.582, 1091.242, 1123.574, 1205.959, 1262.735), 
                      Wave = c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 6L, 6L, 6L, 6L, 6L, 
                          6L, 6L, 6L, 6L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L), OldWave = c(0L, 
                          1L, 2L, 3L, 4L, 5L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 
                          7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L), Rep = c(0, 0, 0, 0, 0, 0, 
                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), .Names = c("Time", 
                      "Wave", "OldWave", "Rep"), row.names = c(1L, 3L, 5L, 7L, 9L, 
                      11L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 24L, 25L, 
                      26L, 27L, 28L, 29L, 30L, 31L), class = "data.frame"), structure(list(
                      Time = c(NA, NA, NA, NA, NA, 998.7688, 1348.279, 833.4576, 
                          857.7372, 920.1404, 944.7571, 978.6253, 1031.302, 1123.073, 
                          828.5359, 863.4034, 925.2823, 950.6212, 1106.836, 1145.393, 
                          1262.927, 1379.848), Wave = c(0L, 1L, 2L, 3L, 4L, 5L, 5L, 
                          6L, 6L, 6L, 6L, 6L, 6L, 6L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L
                      ), OldWave = c(8L, 9L, 10L, 11L, 12L, 13L, 13L, 14L, 14L, 
                          14L, 14L, 14L, 14L, 14L, 15L, 15L, 15L, 15L, 15L, 15L, 15L, 
                          15L), Rep = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                          1, 1, 1, 1, 1, 1, 1)), .Names = c("Time", "Wave", "OldWave", 
                      "Rep"), row.names = c(33L, 35L, 37L, 39L, 41L, 43L, 44L, 46L, 
                      47L, 48L, 49L, 50L, 51L, 52L, 54L, 55L, 56L, 57L, 58L, 59L, 60L, 
                      61L), class = "data.frame"), structure(list(Time = c(NA, NA, 
                          1056.15, 862.0561, 901.2249, NA, 849.8798, 872.0212, 888.6719, 
                          909.6631, 947.6135, 993.7155, 1071.568, 1080.201, 1145.942, 831.3203, 
                          843.1108, 866.9832, 876.169, 907.0812, 932.1031, 957.0385, 985.181, 
                          1015.86, 1056.867, 1099.511, 1141.714, 1203.21, 1499.235), Wave = c(0L, 
                          1L, 2L, 3L, 4L, 5L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 7L, 7L, 
                          7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L), OldWave = c(16L, 
                          17L, 18L, 19L, 20L, 21L, 22L, 22L, 22L, 22L, 22L, 22L, 22L, 22L, 
                          22L, 23L, 23L, 23L, 23L, 23L, 23L, 23L, 23L, 23L, 23L, 23L, 23L, 
                          23L, 23L), Rep = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
                          2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)), .Names = c("Time", 
                      "Wave", "OldWave", "Rep"), row.names = c(63L, 65L, 67L, 69L, 
                      71L, 73L, 75L, 76L, 77L, 78L, 79L, 80L, 81L, 82L, 83L, 85L, 86L, 
                      87L, 88L, 89L, 90L, 91L, 92L, 93L, 94L, 95L, 96L, 97L, 98L), class = "data.frame"), 
              structure(list(Time = c(NA, NA, 895.4297, 1090.732, NA, 915.6794, 
                          1371.687, 835.1253, 849.8801, 893.4182, 906.6436, 970.3796, 
                          1063.738, 1077.448, 1120.681, 1169.363, 833.2636, 873.7192, 
                          917.7266, 938.9572, 977.3084, 1021.811, 1033.517, 1067.995, 
                          1203.008, 1532.626), Wave = c(0L, 1L, 2L, 2L, 3L, 4L, 5L, 
                          6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 7L, 7L, 7L, 7L, 7L, 7L, 
                          7L, 7L, 7L, 7L), OldWave = c(24L, 25L, 26L, 26L, 27L, 28L, 
                          29L, 30L, 30L, 30L, 30L, 30L, 30L, 30L, 30L, 30L, 31L, 31L, 
                          31L, 31L, 31L, 31L, 31L, 31L, 31L, 31L), Rep = c(3, 3, 3, 
                          3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 
                          3, 3, 3, 3)), .Names = c("Time", "Wave", "OldWave", "Rep"
                  ), row.names = c(100L, 102L, 104L, 105L, 107L, 109L, 111L, 
                      113L, 114L, 115L, 116L, 117L, 118L, 119L, 120L, 121L, 123L, 
                      124L, 125L, 126L, 127L, 128L, 129L, 130L, 131L, 132L), class = "data.frame")), oddconf = structure(list(
                  odour = c("OilBl", "BeZal", "bCitr", "1HxOl", "Frnsl", "WatBl", 
                      "Cdvrn", "Sprmn"), del = c(0, 0, 0, 0, 0, 0, 0, 0), dur = c(250, 
                      250, 250, 250, 250, 250, 250, 250), chan = c(16, 17, 18, 
                      19, 20, 21, 22, 23), del.1 = c(0, 0, 0, 0, 0, 0, 0, 0), dur.1 = c(0, 
                      0, 0, 0, 0, 0, 0, 0), chan.1 = c(0, 0, 0, 0, 0, 0, 0, 0), 
                  del.2 = c(0, 0, 0, 0, 0, 0, 0, 0), dur.2 = c(0, 0, 0, 0, 
                      0, 0, 0, 0), chan.2 = c(0, 0, 0, 0, 0, 0, 0, 0), del.3 = c(0, 
                      0, 0, 0, 0, 0, 0, 0), dur.3 = c(0, 0, 0, 0, 0, 0, 0, 0), 
                  chan.3 = c(0, 0, 0, 0, 0, 0, 0, 0), del.4 = c(0, 0, 0, 0, 
                      0, 0, 0, 0), dur.4 = c(0, 0, 0, 0, 0, 0, 0, 0), chan.4 = c(0, 
                      0, 0, 0, 0, 0, 0, 0)), .Names = c("odour", "del", "dur", 
                  "chan", "del.1", "dur.1", "chan.1", "del.2", "dur.2", "chan.2", 
                  "del.3", "dur.3", "chan.3", "del.4", "dur.4", "chan.4"), row.names = c(NA, 
                  8L), class = "data.frame"), sweeps = "008", sweepdir = "../igor/spikes/nm20120906c0", stimRange = c(500, 
              1000), xlim = c(0, 3000), class = c("spiketimes", "list"))
      expect_that(b8s,is_equivalent_to(b8s_baseline))
      
      names_baseline=c("008.000", "008.001", "008.002", "008.003")
      expect_that(names(b8s),equals(names_baseline),
          "Check that after dividing we make sensible names for each block")
    })

test_that("Merge 2 blocks that have been divided",{
      nmdir=system.file("igor/spikes/nm20120906c0", package = 'gphys')
      b8=CollectSpikesFromSweeps(nmdir,8,xlim=c(0,3000),stimRange=c(500,1000))
      # divide into separate data frames for each repeat
      b8s=divide(b8)
      b10=CollectSpikesFromSweeps(nmdir,10,xlim=c(0,3000),stimRange=c(500,1000))
      b10s=divide(b10)
      bs=merge(b8s,b10s)
      merged_names=c("008.000,010.000", "008.001,010.001", "008.002,010.002", 
          "008.003,010.003")
      expect_that(names(bs),equals(merged_names),
          "Check that we get sensible names after divide and merge")
      merged_lengths=c(56,52,68,63)
      expect_that(sapply(bs,nrow),is_equivalent_to(merged_lengths),
          "Check that we get the right number of spikes in the right blocks")
      
    })

context("Subset spike times")
test_that("subset spikes by odour or channel",{
  fixVec=structure(c(31, 30, 29, 27, 26, 25), .Names = c("empty", "IAA", "cVA", "PAA", "4ol", "ctr"))
  nmdir=system.file("igor/spikes/nm20110907c3", package = 'gphys')
  a=CollectSpikesFromSweeps(nmdir,subdir='BLOCKI',fixChannels=fixVec)
  b=CollectSpikesFromSweeps(nmdir,subdir='BLOCKII')
  ab=merge(a,b)
  
  cVAPAA=subset(ab,c("cVA","PAA"))
  chs2927=subset(ab,c(29,27))
  
  expect_that(chs2927,is_equivalent_to(cVAPAA),
              "Check that we get the same result subsetting by odour or channel")
  
  expect_warning(subset(ab,c(29,21)),regex='Will use first sweep for duplicated channels',
                 info="Check that we get a warning when subsetting using a duplicated channel")
})
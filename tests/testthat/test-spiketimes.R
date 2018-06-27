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
               info = "Check that merged wave has mergedwaveinfo attribute")
  
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
      
      b8s_baseline=readRDS('testdata/b8s_baseline.rds')
      expect_that(b8s,is_equivalent_to(b8s_baseline))
      
      names_baseline=c("008.000", "008.001", "008.002", "008.003")
      expect_that(names(b8s),equals(names_baseline),
          info="Check that after dividing we make sensible names for each block")
      
    })

test_that("divide spiketimes works with shuffled presentations",{
  nmdir=system.file("igor/spikes/nm20151231c0/", package = 'gphys')
  b8=CollectSpikesFromSweeps(nmdir,4,xlim=c(0,3000),stimRange=c(500,750))
  # divide into separate data frames for each repeat
  b8s=divide(b8)
  
  # Let's check that the order that waves appear in the 4 separate blocks
  # matches what we predict looking at the shuffled odd
  baseline=list(0:7, 
                c(0, 3, 2, 4, 6, 1, 7, 5), 
                c(0, 7, 1, 6, 4, 2, 5, 3),
                c(0, 6, 5, 3, 1, 4, 2, 7))
  expect_equivalent(lapply(b8s, function(x) unique(x$Wave)), baseline)
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
          info="Check that we get sensible names after divide and merge")
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
  
  od_subset=c("ger",'lin', 'oil')
  expect_warning(sb<-subset(b, odours = od_subset))
  expect_equal(attr(sb, 'oddconf')$odour, od_subset)
  
  expect_that(chs2927,is_equivalent_to(cVAPAA),
              info="Check that we get the same result subsetting by odour or channel")
  
  expect_warning(subset(ab,c(29,21)),regex='Will use first sweep for duplicated channels',
                 info="Check that we get a warning when subsetting using a duplicated channel")
})

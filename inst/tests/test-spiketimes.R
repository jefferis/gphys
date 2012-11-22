context("Test handling of spike times files saved by Igor/Neuromatic")

test_that("merge two blocks of spikes, ", {
    nmdir='../igor/spikes/nm20120413c0'
    a=CollectSpikesFromSweeps(nmdir,subdir="BLOCK A")
    b=CollectSpikesFromSweeps(nmdir,subdir="BLOCK B")
    
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
    })
    
test_that("merge two blocks of spikes with unequal lengths (A longer than B) ", {
    nmdir='../igor/spikes/nm20120413c0'
    a=CollectSpikesFromSweeps(nmdir,subdir="BLOCK A")
    b=CollectSpikesFromSweeps(nmdir,subdir="BLOCK B")

    la=length(a)
    lb=length(b)
    c=merge(a,b)

    expect_true(is.spiketimes(c))
    expect_true(length(c)==7)

    merged_odours=c("IAA", "cVA", "pro", "PAA", "4ol", "ctr", "vin", "ctr", "fly", 
    "far", "oen", "pac", "aac", "ger", "lin", "bty", "hxe", "ben", 
    "met", "oil", "pra", "hxa", "oil", "ehb", "eta", "cit")
    expect_that(attr(c,'oddconf')$odour,equals(merged_odours))
    
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
    
    })
    
test_that("merge two blocks of spikes with unequal lengths (B longer than A) ", {
    nmdir='../igor/spikes/nm20121020c2'
    a=CollectSpikesFromSweeps(nmdir,subdir="BLOCK A")
    b=CollectSpikesFromSweeps(nmdir,subdir="BLOCK B")
    
    la=length(a)
    lb=length(b)

    c=merge(a,b)
    expect_true(is.spiketimes(c))
    expect_true(length(c)==8)
    
    merged_odours=c("cVA", "IAA", "pro", "PAA", "4ol", "ctr", "vin", "ctr", "C10", 
    "far", "oen", "pac", "aac", "ger", "lin", "bty", "hxe", "ben", 
    "met", "oil", "pra", "hxa", "oil", "ehb", "eta", "cit")
    expect_that(attr(c,'oddconf')$odour,equals(merged_odours))
    })

test_that("Count spikes - large reponse window", {
  nmdir='../igor/spikes/nm20120413c0'
  a=CollectSpikesFromSweeps(nmdir,subdir="BLOCK A")
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

test_that("Count spikes - with baseline", {
      nmdir='../igor/spikes/nm20110914c4'
      spikes=CollectSpikesFromSweeps(nmdir,subdir="BLOCK I",sweeps=0:4)
      od=OdourResponseFromSpikes(spikes,response=c(2200,2700),baseline=c(0,2000))
      
      od_baseline<-structure(list(ctr = c(-1.5, -0.25, 0, 0.75, -0.75), fly = c(-0.25, 
                  -1.25, -0.75, -0.25, -0.75), `4ol` = c(-0.25, -0.5, -0.5, -1, 
                  -1.5), cVA = c(0, -1.75, -0.5, -1, 0), IAA = c(-0.25, -0.25, 
                  -1.25, 0.25, 2), PAA = c(24, 23, 20.5, 18, 20)), .Names = c("ctr", 
              "fly", "4ol", "cVA", "IAA", "PAA"), row.names = c(NA, -5L), class = "data.frame")
      expect_that(od,equals(od_baseline))
    })

test_that("Count spikes - Shahar data with repeated block", {
      nmdir='../igor/spikes/nm20120906c0'
      b8=CollectSpikesFromSweeps(nmdir,8,xlim=c(0,3000),stimRange=c(500,1000))
      # split into separate data frames for each repeat
      b8s=split(b8)
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

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
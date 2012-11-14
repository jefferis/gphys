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
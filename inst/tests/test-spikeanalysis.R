context("spike analysis")

test_that("lifetime_sparseness",{
  spikes=CollectSpikesFromSweeps(
    system.file('igor','spikes','nm20110914c4',package='gphys'),
    subdir='BLOCKI',sweeps=0:4)
  od=OdourResponseFromSpikes(spikes, responseWindow =c(2200,2700),
                             baselineWindow = c(0,2000))
  expect_equal(lifetime_sparseness(od), 
               c(1, 1, 1, 0.977589526376588, 0.96039603960396))
  
  # check what happens with NAs
  od2=od
  # -ve response -> NA
  od2[5,'cVA']=NA
  # +ve response -> NA
  od2[4,'IAA']=NA
  # nb difference in baseline for cell 4 is due to loss of a positive response
  expect_equal(lifetime_sparseness(od2),
               c(1, 1, 1, 0.983362218370884, 0.96039603960396))
})
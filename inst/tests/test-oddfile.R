# TODO: Add comment
# 
# Author: jefferis
###############################################################################

context("Test handling of ODD config files saved by Igor XOP")

test_that("Read simple odd config file", {
      odd<-read.odd("../igor/oddfiles/simple_odd.txt")
      odd_baseline=structure(list(
              odour = c("4ol", "IAA", "ctr", "PAA", "fly", "cVA"),
              del = c(0L, 0L, 0L, 0L, 0L, 0L), 
              dur = c(500L, 500L, 500L, 500L, 500L, 500L), 
              chan = c(30L, 26L, 31L, 29L, 25L, 27L),
              del.1 = c(0L, 0L, 0L, 0L, 0L, 0L), 
              dur.1 = c(0L, 0L, 0L, 0L, 0L, 0L), 
              chan.1 = c(0L, 0L, 0L, 0L, 0L, 0L),
              del.2 = c(0L, 0L, 0L, 0L, 0L, 0L), 
              dur.2 = c(0L, 0L, 0L, 0L, 0L, 0L), 
              chan.2 = c(0L, 0L, 0L, 0L, 0L, 0L), 
              del.3 = c(0L, 0L, 0L, 0L, 0L, 0L), 
              dur.3 = c(0L, 0L, 0L, 0L, 0L, 0L), 
              chan.3 = c(0L, 0L, 0L, 0L, 0L, 0L), 
              del.4 = c(0L, 0L, 0L, 0L, 0L, 0L), 
              dur.4 = c(0L, 0L, 0L, 0L, 0L, 0L), 
              chan.4 = c(0L, 0L, 0L, 0L, 0L, 0L)), .Names = c("odour", 
              "del", "dur", "chan", "del.1", "dur.1", "chan.1", "del.2", "dur.2", 
              "chan.2", "del.3", "dur.3", "chan.3", "del.4", "dur.4", "chan.4"
          ), class = "data.frame", row.names = c(NA, -6L))
      
      expect_that(odd, equals(odd_baseline))
    })

test_that("Fix channels in ODD config file", {
      odd<-read.odd("../igor/oddfiles/simple_odd.txt")
      fixVec=c(empty=31,IAA=30,cVA=29,PAA=27,`4ol`=26,ctr=25)
      fixed=fix.odd(odd,fixVec)
      fixed_odour=c('IAA','4ol','empty','cVA','ctr','PAA')
      expect_true(is.data.frame(fixed))
      expect_that(fixed$odour,equals(fixed_odour))
    })
      
test_that("Read complex ODD config file", {
      odd<<-read.odd("../igor/oddfiles/nm20120125c1_000_odd_5times_Sput_1.txt")
      odd_baseline=structure(list(odour = c("OilBl", "E2Hex", "GerAc", "Prpyl", 
                  "IPenA", "Et3HB", "Nonnl", "CiVAc", "OilBl", "E2Hex", "GerAc", 
                  "Prpyl", "IPenA", "Et3HB", "Nonnl", "CiVAc", "OilBl", "E2Hex", 
                  "GerAc", "Prpyl", "IPenA", "Et3HB", "Nonnl", "CiVAc", "OilBl", 
                  "E2Hex", "GerAc", "Prpyl", "IPenA", "Et3HB", "Nonnl", "CiVAc", 
                  "OilBl", "E2Hex", "GerAc", "Prpyl", "IPenA", "Et3HB", "Nonnl", 
                  "CiVAc"), del = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                  0, 0, 0, 0), dur = c(500, 500, 500, 500, 500, 500, 500, 500, 
                  500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 
                  500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 
                  500, 500, 500, 500, 500, 500), chan = c(0, 1, 2, 3, 4, 5, 6, 
                  7, 0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 
                  4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7), del.1 = c(0, 0, 0, 0, 0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), dur.1 = c(0, 0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), chan.1 = c(0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), del.2 = c(0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), dur.2 = c(0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), chan.2 = c(0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), del.3 = c(0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), dur.3 = c(0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), chan.3 = c(0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), del.4 = c(0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), dur.4 = c(0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), chan.4 = c(0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), .Names = c("odour", 
              "del", "dur", "chan", "del.1", "dur.1", "chan.1", "del.2", "dur.2", 
              "chan.2", "del.3", "dur.3", "chan.3", "del.4", "dur.4", "chan.4"
          ), row.names = c(NA, -40L), class = "data.frame")
      
      expect_that(odd, equals(odd_baseline))
    })

test_that("Read ODD config file with missing zeros in a line", {
      odd<<-read.odd("../igor/oddfiles/missingzeros.txt")
      odd_baseline=structure(list(odour = c("OilBl", "E2Hex", "GerAc", "Prpyl", 
                  "IPenA", "Et3HB", "Nonnl", "CiVAc", "OilBl", "E2Hex", "GerAc", 
                  "Prpyl", "IPenA", "Et3HB", "Nonnl", "CiVAc", "OilBl", "E2Hex", 
                  "GerAc", "Prpyl", "IPenA", "Et3HB", "Nonnl", "CiVAc", "OilBl", 
                  "E2Hex", "GerAc", "Prpyl", "IPenA", "Et3HB", "Nonnl", "CiVAc", 
                  "OilBl", "E2Hex", "GerAc", "Prpyl", "IPenA", "Et3HB", "Nonnl", 
                  "CiVAc"), del = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                  0, 0, 0, 0), dur = c(500, 500, 500, 500, 500, 500, 500, 500, 
                  500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 
                  500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 
                  500, 500, 500, 500, 500, 500), chan = c(0, 1, 2, 3, 4, 5, 6, 
                  7, 0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 
                  4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7), del.1 = c(0, 0, 0, 0, 0, 
                  NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), dur.1 = c(0, 0, 
                  0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), chan.1 = c(0, 
                  0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), del.2 = c(0, 
                  0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), dur.2 = c(0, 
                  0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), chan.2 = c(0, 
                  0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), del.3 = c(0, 
                  0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), dur.3 = c(0, 
                  0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), chan.3 = c(0, 
                  0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), del.4 = c(0, 
                  0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), dur.4 = c(0, 
                  0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), chan.4 = c(0, 
                  0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), .Names = c("odour", 
              "del", "dur", "chan", "del.1", "dur.1", "chan.1", "del.2", "dur.2", 
              "chan.2", "del.3", "dur.3", "chan.3", "del.4", "dur.4", "chan.4"
          ), row.names = c(NA, -40L), class = "data.frame")
      
      expect_that(odd, equals(odd_baseline))
    })

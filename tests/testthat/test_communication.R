context("Communication")

test_that("Number of calls", {
  i = 8
  ids = c("1", "2")
  toydata = data.frame(userId = rep(ids, length = i), source = rep("PHONE", length = i),
    phone.event = rep(c("INCOMING", "OUTGOING", "REJECTED", "MISSED", "RINGING"), length = i),
    stringsAsFactors = FALSE)

  expect_equal(communicationNumberCalls(loggingdata = toydata)[, 2], c(4, 4))
  expect_equal(communicationNumberCalls(loggingdata = toydata, call_type = "INCOMING")[, 2], c(1, 1))
  expect_equal(communicationNumberCalls(loggingdata = toydata, call_type = "OUTGOING")[, 2], c(1, 1))
  expect_equal(communicationNumberCalls(loggingdata = toydata, call_type = "REJECTED")[, 2], c(1, 1))
  expect_equal(communicationNumberCalls(loggingdata = toydata, call_type = "RINGING")[, 2], c(1, 0))
  expect_equal(communicationNumberCalls(loggingdata = toydata, call_type = "MISSED")[, 2], c(0, 1))

  # test single-row data
  toydata2 = data.frame(userId = "1", source = "PHONE", phone.event = "INCOMING",
    stringsAsFactors = FALSE)
  expect_equal(communicationNumberCalls(loggingdata = toydata2, call_type =  "all", colname = "NumberCalls")[, "NumberCalls"], 1)
  #test wrong call_type
  expect_error(communicationNumberCalls(loggingdata = toydata, call_type = "SOMETHING"))
  #test missing source
  toydata$source = rep("SMS", i)
  expect_equal(communicationNumberCalls(loggingdata = toydata)[, 2], c(0, 0))
  #test data missing columns
  toydata$source = rep("PHONE", i)
  toydata$phone.event = NULL
  expect_error(communicationNumberCalls(loggingdata = toydata))
})

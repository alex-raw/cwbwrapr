mock_freqlist <- data.frame(1:9, c("this", "is", "a", "frequency", "list",
                      '"', "\\haha", "#", "stuff withspace"))
colnames(mock_freqlist) <- c("f", "type")
write.table(mock_freqlist, "mock.tsv", sep = "\t", quote = FALSE,
            col.names = FALSE, row.names = FALSE)
test_mock <- read_freqs("mock.tsv")
expect_identical(mock_freqlist, test_mock)

write_freqs(mock_freqlist, "mock2.tsv")
mock2 <- read_freqs("mock2.tsv", header = TRUE)
expect_identical(mock2, mock_freqlist)

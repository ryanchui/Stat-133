# Functions to create the derived variables

isRe = function(msg) {
   # The subject line begins with Re: or Fwd: Re:  ... 
   # Returns TRUE if begins with Re
   grepl("\\<Re\\>", msg$header["subject"])
}

percentCapitals = function(msg) {
  # The percentage of capital letters in subject
  # Do not count punctuation and numbers in denominator 
  # Returns a numeric of length 1
  subject = gsub("[[:punct:]]", "", msg$header["Subject"])
  subject1 = gsub("[[:blank:]]", "", subject)
  subject2 = gsub("[[:lower:]]+", "", subject1)
  return(nchar(subject2)/nchar(subject1))
}

fromNumericEnd = function(msg) {
  # The From address ends in a number 
  # for example marty66@aol.com
  # Returns a logical of length 1
  grepl("[[:digit:][:punct:]]+@[[:alpha:]]",msg$header['From'])
}

exclaimCount = function(msg) {
  # The number of exclamation marks in body 
  # Returns a numeric of length 1
  # msg$body = na.omit(msg$body))
  sum(nchar(gsub("[^!]", "", msg$body)))
}

# When ready to create data frame
# run the following code
if (TRUE) {
emailDF = data.frame( 
  sapply(emails, function(x) x$isSpam), 
  sapply(emails, isRe),
  sapply(emails, percentCapitals),
  sapply(emails, fromNumericEnd),
  sapply(emails, exclaimCount))

names(emailDF) = c("isSpam", "isRe", "percentCapitals",
                   "fromNumericEnd", "exclaimCount")

save(emailDF, file = "emailDF.rda")
}


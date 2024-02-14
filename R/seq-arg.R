##' DNA sequence complement
##' @title Complement of the Sequence
##' @param seq The sequence in a character
##' @return Character of the complement sequence
##' @examples
##' bioseq_complement("ATGCGATGTCGAGAGCAGCGATGC")
##' # Function will not be affected by case of the characters,
##' # return will be of all Upcased characters.
##' bioseq_complement("agtGTGacGCT")
##' @export
##' @author Dilip
bioseq_complement <- function(seq) {
  stopifnot(is.character(seq))
  seq_input <- tolower(seq)
  seq_comp <- gsub(
    "a", "T",
    gsub(
      "t", "A",
      gsub(
        "c", "G",
        gsub("g", "C", seq_input)
      )
    )
  )
  return(seq_comp)
}

##' Reverse complement for a sequence
##' @title Reverse Complement
##' @param seq The sequence in a character
##' @return Reverse complement for input sequence
##' @examples
##' bioseq_rcomplement("ATGAGTGCGTGCT")
##' bioseq_rcomplement("AGagaTgcAgATGCacgca")
##' @export
##' @author Dilip
bioseq_rcomplement <- function(seq) {
  stopifnot(is.character(seq))
  seq_comp <- bioseq_complement(seq)
  # to reverse the seqyuence/characters
  seq_rcomp <- intToUtf8(rev(utf8ToInt(seq_comp)))
  return(seq_rcomp)
}

##' Transcription of a DNA sequence
##' @title Transcription of DNA
##' @param seq The sequence in a character
##' @return mRna sequence that is transcribed by input sequence.
##' @examples
##' bioseq_transcribe("AGTGACGAGCGAagcgaGACGGTGagagcg")
##' @export
##' @author Dilip
bioseq_transcribe <- function(seq) {
  stopifnot(is.character(seq))
  seq_input <- toupper(seq)
  seq_mrna <- gsub("T", "U", seq_input)
  return(seq_mrna)
}

##' Basic Bioseq interactive function
##' @title Interactive Bio Sequence
##' @return Interactive function gives DNA complement, Reverse complement or Transcribed Sequence or more detailed Information based on the option passed.
##' @examples
##' bioseq()
##' @export
##' @author Dilip
bioseq <- function() {
  user_input <- gsub(" ", "", readline("Please, Input the DNA sequence: "))

  user_opt <- ""
  while (!is.element(user_opt, c("C", "R", "T", "A"))) {
    print("Choose either 'C', 'R', or 'T' only")
    user_opt <- toupper(readline("Do you want to find Complement(C), Reverse complement(R) or Transcription(T) or All details(A) of the DNA Sequence: "))
  }

  if (user_opt == "C") {
    bioseq_complement(user_input)
  } else if (user_opt == "R") {
    bioseq_rcomplement(user_input)
  } else if (user_opt == "T") {
    bioseq_transcribe(user_input)
  } else {
    print(paste("Given DNA sequence: ", user_input))

    leng <- nchar(user_input)
    print(paste("The count/length of the DNA sequence: ", leng))

    comp <- bioseq_complement(user_input)
    print(paste("The complement strand for given DNA sequence: ", comp))

    rcomp <- bioseq_rcomplement(user_input)
    print(paste("The reverse complement for DNA sequence: ", rcomp))

    mrna <- bioseq_transcribe(user_input)
    print(paste("The mRna translated from DNA sequence: ", mrna))
  }
}

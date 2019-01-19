# Haskell-Project: macroprocessor
## Introduction
##### A macroprocessor is a program that transforms a text file by replacing specially identified keywords within the file by a specified piece of text. Keywords are strings that begin with the character ’$’. Each definition comprises a keyword followed by a (single) space character and then the keyword’s definition, which is a sequence of words, possibly including punctuation and whitespace characters. For example, given from the info file 
$name William Shakespeare<br />
$birth-date 1564<br />
$town Stratford upon Avon


##### and the text file
Welcome to $town, where $name was born in $birth-date.
##### Macroprocessor will rewrite the text file to produce
Welcome to Stratford upon Avon, where William Shakespeare was born in 1564.

### Definitions and Functions

#### The defined types:
<body>
type FileContents = String<br />
type Keyword      = String<br />
type KeywordValue = String<br />
type KeywordDefs  = [(Keyword, KeywordValue)]<br />
</body>

#### lookUp
lookUp :: String -> [(String, a)] -> [a] <br />
given a search string and a list of string/item pairs, returns the list of items whose associated string matches the search string
#### split
split :: [Char] -> String -> (String, [String])<br />
break up a string in a way that returns a pair comprising the separator characters, i.e. a String, and the list of words, a [String], in the order that they were encountered in the input
#### combine
combine :: String -> [String] -> [String] <br />
combine the components of a string from its constituent separator characters and words, as generated by a call to split. 
#### getKeywordDefs
getKeywordDefs :: [String] -> KeywordDefs <br />
takes the contents of an information file in the form of a list of lines (each line is a string), and which returns a list of keyword/definition pairs.
#### expand
expand :: FileContents -> FileContents -> FileContents <br />
takes the contents of a text file and an info file and combines them using the above functions to build a string representing the output file.
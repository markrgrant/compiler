

type LexicalSpec = [(TokenClass, RegExp)]
data TokenClass = Identifier | Number | Operator | Keyword | Whitespace |
                  OpenParen | CloseParen | Semicolon | Assignment

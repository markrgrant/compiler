module Scanner where


type Program        = ByteString
type SourceProgram  = Program
data Category       = Category Name -- syntactic category
type Lexeme         = ByteString
type Name           = ByteString
type Token          = (Category, Lexeme)

-- the scanner converts a source program into a sequence
-- of tokens according to a set of regular expressions
type Scanner = SourceProgram -> [Token]



-- convert a RE to an NFA
toNFA :: RE -> NFA
toNFA = undefined

-- convert an NFA to a DFA
toDFA :: NFA -> DFA
toDFA = undefined

-- take a DFA and minimize it according to Hopcroft's algorithm
minimize :: DFA -> DFA
minimize = undefined

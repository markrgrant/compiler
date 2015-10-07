module Compiler where

type SourceLanguage = ByteString
type TargetLanguage = ByteString
data IR = IR Tree
data Token = Token SyntacticCategory Lexeme
data SyntacticCategory = SyntacticCategory Name -- this depends on the microsyntax in the language
type Lexeme = ByteString
type Name = ByteString
type Compiler = SourceLanguage -> TargetLanguage

-- A compiler translates a source program into a destination program.
-- The destination program is typically assembly language or bytecode.
compiler :: Compiler
compiler = frontEnd >>= optimizer >>= backEnd


-- The front end is responsible for creating an intermediate representation
-- from a source program.
frontEnd :: SourceProgram -> IR
frontEnd = scanner >>= parser >>= elaboration


-- A scanner translates a program into a sequence of tokens.  Scanning
-- typically takes O(n) time and space.
scanner :: ScanningTable -> SourceProgram -> Maybe [Token]
scanner =


-- A scanner generator takes regular expressions and produces a
-- scanner.  It is essentially a compiler that compiles from the 
-- language of regular expressions to the target language.
scannerGenerator :: Compiler RELanguage HaskellLanguage



-- A parser translates a sequence of tokens into an intermediate
-- representation. Scanning typically takes O(n) time and space.
parser :: [token] -> IR
parser = undefined


elaboration :: IR -> IR
elaboration = undefined


-- An optimizer improves the efficiency of the code in a particular respect,
-- such as reduced energy usage, or faster execution, etc.  Multiple
-- optimizers are usually composed to achieve particular goals.  Optimization
-- proceeds in two parts: analysis, and then transformation.
optimizer :: IR -> IR
optimizer = undefined


-- The back end takes an intermediate representation and generates the
-- desired program.  A back end is specific to the output program.
-- This specific back-end is for generating machine language code.
backEnd :: IR -> Program
backend = instructionSelector >>= registerAllocator >>= instructionScheduler


-- Instruction selection maps each IR operation, in its context, into one
-- or more target language (typically machine language) operations.
instructionSelector :: IR -> VirtualProgram
instructionSelector = undefined


-- registerAllocator decides at each point in the code which valeus should
-- reside in the target and 
registerAllocator :: VirtualProgram -> DestinationProgram
registerAllocator = undefined
--
-- The instruction scheduler reorders the operations in the code.  It attempts
-- to minimize the number of cycles wasted waiting for operands by exploiting
-- the parallelism provided by some architectures.  This may benefit from
-- using more registers.
instructionScheduler :: DestinationProgram -> DestinationProgram
instructionScheduler = undefined

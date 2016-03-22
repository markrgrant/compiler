module Compiler where

import Data.ByteString

import Scanner (Scanner, Token)
import Parser (Parser)

type Compiler               = SourceProgram -> TargetProgram
data FrontEnd               = SourceProgram -> IR
type Scanner                = SourceProgram -> [Token]
type Parser                 = [Token] -> Grammar -> IR
type Elaborator             = IR -> IR
type Optimizer              = IR -> IR
type BackEnd                = IR -> TargetProgram
type InstructionSelector    = IR -> VirtualProgram
type RegisterAllocator      = VirtualProgram -> TargetProgram
type InstructionScheduler   = TargetProgram -> TargetProgram
type Program                = ByteString 
type SourceProgram          = Program
type VirtualProgram         = Program
type TargetProgram          = Program
type IR                     = ByteString -- TODO: determine IR representation

-- A compiler translates a source program into a destination program.
-- The destination program is typically assembly language or bytecode.
compiler :: Compiler
compiler = frontEnd >>> optimizer >>> backEnd


-- The front end encodes the knowledge of the source language
-- in an intermediate representation, both the syntax and its
-- semantics.
frontEnd :: FrontEnd
frontEnd = scanner >>> parser >>> elaborator


-- A scanner translates a program into a sequence of tokens.  Scanning
-- typically takes O(n) time and space.  Scanning can fail if some
-- sequence of input is not a part of the language.
scanner :: Scanner
scanner = undefined


-- A parser translates a sequence of tokens into an intermediate
-- representation. Scanning typically takes O(n) time and space.
parser :: [Token] -> IR
parser = undefined


-- Perform additional checking that involves considering each statement 
-- in its actual context such as type checking and control flow.
elaborator :: Elaborator
elaborator = undefined


-- An optimizer improves the efficiency of the code in a particular respect,
-- such as reduced energy usage, or faster execution, etc.  Multiple
-- optimizers are usually composed to achieve particular goals.  Optimization
-- proceeds in two parts: analysis, and then transformation.
optimizer :: Optimizer
optimizer = undefined


-- The back end takes an intermediate representation and generates the
-- desired program.  A back end is specific to the output program.
-- This specific back-end is for generating machine language code.
backEnd :: BackEnd
backEnd = instructionSelector >>= registerAllocator >>= instructionScheduler


-- Instruction selection maps each IR operation, in its context, into one
-- or more target language (typically machine language) operations.
-- The instruction selector is aware of the target machine and if the target
-- language is assembly, it chooses machine language operations.  Typically
-- virtual registers are used here and these must be replaced in the
-- register allocator phase.
instructionSelector :: InstructionSelector
instructionSelector = undefined


-- The register allocator maps the virtual registers assigned during 
-- instruction selection to the actual registers of the target machine.
registerAllocator :: Allocator
registerAllocator = undefined


-- The instruction scheduler reorders the operations in the code.  It attempts
-- to minimize the number of cycles wasted waiting for operands by exploiting
-- the parallelism provided by some architectures.  This may benefit from
-- using more registers.
instructionScheduler :: InstructionScheduler
instructionScheduler = undefined

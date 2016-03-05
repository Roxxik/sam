--import Text.ParserCombinators.Parsec
import System.Environment
--import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Char
import Data.List hiding (unfoldr)
import Text.Regex.TDFA
import Data.Array

data Sam = Sam [Element]

data Element = BlockElement Block | InstructionElement Instruction

data Block = Block Identifier [Element]

type Identifier = String

data Instruction = Instruction InstructionMnemonic [InstructionParameter]

data InstructionParameter
    = Register Register
    | Identifier Identifier
    | Constant Int

data Register = ByteRegister ByteRegister | WordRegister WordRegister | DoubleRegister DoubleRegister | QuadRegister QuadRegister
    deriving Show

data ByteRegister = AL | AH | BL | BH
    deriving Show

data WordRegister = AX | BX
    deriving Show

data DoubleRegister = EAX | EBX
    deriving Show

data QuadRegister = RAX | RBX
    deriving Show


data Token
    = TokLBrace
    | TokRBrace
    | TokIdentifier Identifier
    | TokImmediate Int
    | TokInstructionMnemonic InstructionMnemonic
    | TokRegister Register
    | TokSemicolon
    | TokWhiteSpace
    deriving Show

data InstructionMnemonic
    = Mov
    | Const
    | Call
    | Ret
    deriving Show

data Span = Span
    { spanToken :: Token
    , spanStart :: SourcePos
    , spanLen   :: SourcePos
    } deriving Show

type SourcePos = Int


type Pattern = String

data LexerF n =
      Start (String -> n)
    | Next Span (String -> n)
    | Fin


instance Functor for LexerF where
    fmap f (Start g) = Start (f . g)
    fmap f (Next span g) = Next span (f . g)
    fmap f (Fin) = Fin

type Lexer = Free LexerF

lex :: Lexer ()
lex = do
    str <- get'
    unless (null str) $ do
        put' $ getSpan str
        lex


lex' str = do
    case extract str of
        Just span -> next' span
        Nothing   -> fin'




runLexer :: Lexer r -> String -> [Span]
runLexer (Pure r)             s = []
runLexer (Free (Start g))     s = runLexer (g s)
runLexer (Free (Next span g)) s = span : runLexer (g s)
runLexer (Free Fin)           s = []




data Lexeme = Lexeme
    { lexPattern :: Pattern
    , lexTokFunc :: (String -> Token)
    }

lexemeConst :: String -> Token -> Lexeme
lexemeConst s t = Lexeme s (const t)

lexemes =
    [ lexemeConst "mov" (TokInstructionMnemonic Mov)
    , lexemeConst "const" (TokInstructionMnemonic Const)
    , lexemeConst "call" (TokInstructionMnemonic Call)
    , lexemeConst "ret" (TokInstructionMnemonic Ret)
    , lexemeConst "{" TokLBrace
    , lexemeConst "}" TokRBrace
    , lexemeConst "ax" (TokRegister (WordRegister AX))
    , lexemeConst "[ \t\n]" TokWhiteSpace
    , Lexeme "[0-9]+"        (TokImmediate . foldl' (\a i -> a * 10 + digitToInt i) 0)
    , Lexeme "0x[a-fA-F0-9]" (TokImmediate . foldl' (\a i -> a * 16 + digitToInt i) 0)
    , Lexeme "[a-zA-Z_][a-zA-Z0-9_]*" TokIdentifier
    ]


unfoldr :: (b -> Either err (Maybe (a, b))) -> b -> Either err [a]
unfoldr f x = case f x of
    Left err -> Left err
    Right (Just (y, x')) -> fmap (y :) (unfoldr f x')
    Right Nothing -> Right []

lex :: [Lexeme] -> String -> Either String [Span]
lex = unfoldr . lex1 . (,) 0

lex1 :: [Lexeme] -> (Int, String) -> Either String (Maybe (Span, (Int, String)))
lex1 rules = apply . second ((>>= headMay) . fmap (filter ((== 0) . fst . snd . snd) . zip [0..]) . tailMay . getAllTextSubmatches . match re)
    where
        re = makeRegex (intercalate "|" . map ((\s -> concat ["(", s, ")"]) . lexPattern) $ rules) :: Regex

apply :: [Lexeme] -> (Int, Maybe (Int, (String, (MatchOffset, MatchLength)))) -> Either String (Maybe (Span, (Int, String)))
apply rules (c, Just (n, (s, (0, len)))) = Right ()
apply _     (c, Nothing) = Left ("error at: " ++ show c)
apply _ _ = undefined --this never happens

{-
lex :: [Lexeme] -> String -> [Span]
lex rules str = map (apply rules') (matchAllText re str)
    where
        re = makeRegex (intercalate "|" . map ((\s -> concat ["(", s, ")"]) . lexPattern) $ rules') :: Regex
        rules' = rules--Lexeme "." (error . (++) "Invalid Token: ") : rules

apply :: [Lexeme] -> Array Int (String, (MatchOffset, MatchLength)) -> Span
apply rules = toSpan rules . head . filter ((/= -1) . fst . snd . snd) . zip [0..] . tail . elems

toSpan :: [Lexeme] -> (Int, (String, (MatchOffset, MatchLength))) -> Span
toSpan rules (n, (s, (start, len))) = Span ((lexTokFunc (rules !! n)) s) start len
-}
main :: IO ()
main = print 2


{-
main {
    const foo 32
    mov bx 2
    mov ax bx
    mov bx foo
    call add2
    ret

    add2 {
        inc ax
        inc ax
        ret
    }
}


-}

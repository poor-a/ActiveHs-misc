-- |Conversion from 'GenericData' to 'Doc'
{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module Data.Data.GenRep.Doc
    ( Doc
    , Style(..)
    , stringCharPlain
    , stringCharAsLiterals
    , showCharLit
    , showCharLitInString
    , toDoc
    , valueToDoc
    , errorToDoc
    ) where

import Data.Data.GenRep
import Data.Char (isPrint, showLitChar)

import Text.PrettyPrint.HughesPJ hiding (Style)
import Data.List (intersperse)

----------------

-- |'IsString' instance for 'Doc'
-- instance IsString Doc where fromString = text

-------------------------

-- |Show a character literal. Unicode characters are not escaped.
showCharLit :: Char -> String
showCharLit c | isPrint c = [c]
              | otherwise = showLitChar c ""

-- |Show a character in a string literal. Unicode characters are not escaped.
showCharLitInString :: Char -> String
showCharLitInString '"'   = "\\\""
showCharLitInString c     = showCharLit c

----------------------------------------------

-- | Strings and Chars are shown as literals (with escapes and
--   quotes), as in GHCi.
stringCharAsLiterals :: Style
stringCharAsLiterals = Style {
    charLiterals   = True
  , stringLiterals = True
  }

-- | Strings and Chars are shown without escapes and quotes. Useful for
--   error messages, like the argument of 'error'.
stringCharPlain :: Style
stringCharPlain = Style {
    charLiterals   = False
  , stringLiterals = False
  }

-- | Controls how 'toDoc' behaves.
data Style = Style {
    charLiterals   :: Bool
  , stringLiterals :: Bool
  }

errorToDoc :: GenericData -> Doc
errorToDoc = toDoc stringCharPlain stringCharPlain

valueToDoc :: GenericData -> Doc
valueToDoc = toDoc stringCharAsLiterals stringCharPlain

-- | Converts a 'GenericData' to a 'Doc'.
toDoc :: Style -> Style -> GenericData -> Doc
toDoc defaultStyle errorStyle val =
  case val of
    NestedError _ -> showsP errorStyle 0 val
    _             -> showsP defaultStyle 0 val
  where
    showsP :: Style -> Int -> GenericData -> Doc
    showsP style j x = case x of
        Hole           -> text "…"       -- !!! ragadás
        ListHole       -> text "……"
        Timeout _      -> text "⊥"
        NestedError e  -> text "⊥:" <+> showsP errorStyle 0 e
        Error e        -> text e
        Detail s       -> showParen_ (j > 10) $ text "……" <+> showsP style 0 s <+> text "……"
        Constructor (Char c) []         -> if charLiterals style
                                           then quotes $ text $ showCharLit c
                                           else char c
        Constructor Nil []              -> text "[]"
        Constructor (Prefix f) []       -> text f
        Constructor (Infix i f)  [a,b]  -> showParen_ (j > i) $ showsP style (i+1) a <+> text f <+> showsP style (i+1) b
        Constructor (Infixr i f) [a,b]  -> showParen_ (j > i) $ showsP style (i+1) a <+> text f <+> showsP style i b
        Constructor (Infixl i f) [a,b]  -> showParen_ (j > i) $ showsP style i a <+> text f <+> showsP style (i+1) b
        Constructor (Tuple _) xs        -> showParen_ True $ list $ map (showsP style 0) xs
        Constructor Cons [_,_]          -> fsep $ intersperse (text "++") $ elems style x -- showListEnd "[]" "\"" "[" s
        Constructor (Prefix f) l        -> showParen_ (j > 10) $ text f <+> fsep (map (showsP style 11) l)
        _                               -> error $ "showsP: " ++ show x

    showParen_ True  = parens
    showParen_ False = id

    list = fsep . punctuate comma

    collectChars :: GenericData -> (String, GenericData)
    collectChars (Constructor Cons [Constructor (Char c) [],b])
        | (cs, x) <- collectChars b
        = (c: cs, x)
    collectChars x = ([], x)

    collectElems :: GenericData -> ([GenericData], GenericData)
    collectElems x@(Constructor Cons [Constructor (Char _) [], _]) = ([], x)
    collectElems (Constructor Cons [a,b])
        | (cs, x) <- collectElems b
        = (a: cs, x)
    collectElems (Detail b) 
        | (cs, x) <- collectElems b
        = (ListHole: cs, x)
    collectElems Hole 
        = ([ListHole], Constructor Nil [])
    collectElems x = ([], x)

    elems :: Style -> GenericData -> [Doc]
    elems style x
        | (cs@(_:_), rest) <- collectChars x =
            if stringLiterals style
            then doubleQuotes (text $ concatMap showCharLitInString cs): elems style rest
            else text cs : elems style rest
        | (es@(_:_), rest) <- collectElems x =
            (brackets . list . map (showsP style 0) $ es): elems style rest
    elems style (Constructor Nil []) = []
    elems style (Detail x) = [text "...", showsP style 0 x]
    elems style x = [showsP style 0 x]



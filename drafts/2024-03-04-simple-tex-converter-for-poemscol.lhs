---
title: Simple Converter for latex
description:
keywords: literate haskell, haskell, lhs, latex, parser
author: Myoungjin Jeon
---

What Does This Do?
------------------
This simple program converts sections of poetry written in text into LaTeX code compatible with the [poemscol package](https://ctan.org/pkg/poemscol). It's important to note that it only converts the poetry sections, so you'll still need to write other LaTeX commands manually.

- wrap around the poem with *"\begin{poem}"* ... *"\end{poem}"*
- add title latex command if a argument is given
- read the poem text from STDIN
- split lines by stanza and add latex command *"\begin{stanza}"* ... *"\end{stanza}"*
- add "/verseline at the end of each line, except last line of a stanza


Why I made this?
----------------
Manually converting text to LaTeX is tedious, so I'm looking for a converter to streamline the process. Interestingly, I tried to adding a command within ChatGPT4 to convert my text into poemscol syntax, but it was too slow and became slower overtime. Recognizing that the task wasn't complex, I decided to tackle it myself using my favorite language (one I'm also eager to learn more about).

- summary: A converter text to Latex is required. but ChatGPT4 performance is not great, so I made it by myself.

\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment               as ENV
\end{code}

This program may not be large, but using `Data.Text` is a good idea for handling text. I believe this [answer](https://stackoverflow.com/a/22623325) provides a good explanation for this approach. and this is a summary from GhatGPT v4

>  Data.Text is more space-efficient than Haskell's native String, which is a linked list of Chars with high space overhead. It is also more performant, providing better memory locality and interfacing more efficiently with native system libraries, especially for IO-heavy programs. Additionally, Data.Text offers text-specific functions not available with native Strings.

So, `Data.Text`, `Data.Text.IO` is imported.

\begin{code}
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import Data.Text (Text)

\end{code}

Additionally, there are a few more modules to handle syntax. I prefer to use `(>>>)` instead of `.` for longer statements, especially when the code consists of more than *four steps*. This is because it's easier to follow than backward function combination when using `.`, and I use `(<&>)` for similar reasons (forward function composition).

\begin{code}
import Data.Either (isRight)
import Control.Arrow ((>>>))
import Data.Functor ((<&>))

\end{code}

Generally speaking, translating some values into English words can provide a better understanding when reading the code.
And this approach offers better control over options when retrieving values from the command line input by a user.
"So, I'd like to recommend the following lines:"
\begin{code}
beginTitle :: Text
beginTitle = "\\sequencetitle{"

endTitle :: Text
endTitle = "}\n\\poemtitle{}"

beginStanza :: Text
beginStanza = "\\begin{stanza}"

endStanza :: Text
endStanza = "\\end{stanza}"

beginPoem :: Text
beginPoem = "\\begin{poem}"

endPoem :: Text
endPoem = "\\end{poem}"

middleEOLSurfix :: Text
middleEOLSurfix = "\\verseline"

aIndent :: Text
aIndent = " "

-- ind: indent
ind :: Int -> Text
ind = (flip T.replicate) aIndent

\end{code}

The title will be retrieved from the command line, while the rest of the poem will be provided through STDIN. So firtly, I made getArgs for `Data.Text` version.

\begin{code}
getArgsText :: IO [Text]
getArgsText = map T.pack <$> ENV.getArgs

\end{code}

It is always a good idea for a program to have a help message. Even if you are the person who created the program, you could forget how to use it and have to open the code again, LOL.

\begin{code}
parseOpts :: [Text] -> IO (Either Text Text)
parseOpts ["-h"] = do
  progName <- T.pack <$> ENV.getProgName
  return $ Left $ "Usage: " <> progName <> " [OPTION] [A Poem Title]" <> "\n"
    <> "Return a poem structured for a LaTeX package, `poemscol'" <> "\n"
    <> "Read text data from STDIN." <> "\n\n"
    <> "-h        show this message." <> "\n\n"
    <> ":^]\n"

\end{code}

While it may not be the best design, the title will be retrieved from the entire command line arguments. The advantage is that you don't need to use quotes around the title.

\begin{code}
parseOpts ts =
  return $ case (mkt ts) of "" -> Right ""
                            tt -> Right $ (addtex tt) <> "\n\n"
  where
    mkt    = T.unwords
    addtex = (beginTitle <>) . (flip (<>)) endTitle

\end{code}

And the poem text will be read from the STDIN!
`parseContents will take previously parsed data, which is the poem title, and combine it with the parsed poem text."

\begin{code}
parseContents :: (Either Text Text) -> IO (Either Text Text)
parseContents ei =
  if isRight ei then
    do
      pb <- parseBody
      return . ((<> pb) <$>) $ ei
  else
    return ei

\end{code}
"parseBody is the main part of the program, which:

1. Groups by stanza.
2. Adds a special command for each line (which is "\verseline") except for the last line of a stanza.
3. Adds syntax for the stanza.
4. Adds indentation to each line for better readability.

\begin{code}
  where
    parseBody = T.getContents <&>
                (     T.splitOn "\n\n"  -- Group by stanza
                  >>> map T.lines       -- and then divide into lines
                                        -- within each group, NB: *map*
                  >>> map foldLinesWithTex
                  >>> foldStanzasWithTex
                )
    sil = 1 -- stanza indent level
    lil = sil + 1 -- line indent level

    -- Add "\verseline" to the end of each line except the last line of a stanza.
    foldLinesWithTex = T.intercalate (middleEOLSurfix <> "\n" <> (ind lil))

    -- Wrap each stanza in a stanza structure with indentation.
    foldStanzasWithTex =
      foldr (\n acc ->
                if T.null n then
                  acc
                else
                  (ind sil) <> beginStanza <> "\n"
                  <> (ind lil) <> n <> "\n"
                  <> (ind sil) <> endStanza <> "\n"
                  <> acc
            ) ""

main :: IO ()
main = getArgsText >>= parseOpts >>= parseContents >>=
  (\x -> case x of
      Left l -> T.putStr l
      Right r -> T.putStr (beginPoem <> "\n" <> r <> endPoem <> "\n") )
\end{code}

Another Advantage
-----------------
I could use this programme inside of org-mode in emacs so I could write down the text and generate syntaxed poem in the same place. I'll post about this sooner or later.

module Phantom where

import Prelude

--data Language = English | Spanish
--data Censored = Censored | NotCensored
--data Encoding = Plain | EncodingA | EncodingB

data English
data Spanish

data Censored
data NotCensored

data Plain
data EncodingA
data EncodingB

data Message (constraints :: # *) = Message String

--data Message (language :: Language, censored :: Censored, encoding :: Encoding) = Message String

mkMessage :: String -> Message (language :: English, censored :: NotCensored, encoding :: Plain)
mkMessage a = Message a

sendMessage :: forall msgProps. Message (encoding :: EncodingA | msgProps) -> Unit
sendMessage a = unit

--sentMessage = sendMessage (mkMessage "test")

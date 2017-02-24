module WebForm (
    Web, Form, Element(P, Input), Arribute, Name, Value, Answers, runWeb
    ) where

import Replay

type Web a = Replay Form Answers a

type Form = (String, [Element])

data Element = P String | Input [Attribute]

type Attribute = (Name, Value)

type Name = String

type Value = String

type Answers = String

runWeb :: Web a -> ActionM a
runWeb = undefined
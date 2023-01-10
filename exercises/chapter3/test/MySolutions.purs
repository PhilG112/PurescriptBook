module Test.MySolutions where

import Prelude

import Data.AddressBook (Entry, AddressBook)
import Data.List (filter, head, nubByEq, null)
import Data.Maybe (Maybe)

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet s = head <<< filter (\e -> _.address.street e == s)

isInBook :: String -> String -> AddressBook -> Boolean
isInBook fName lName = not null <<< filter f
    where
        f :: Entry -> Boolean
        f entry = entry.firstName == fName && entry.lastName == lName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq (\a b -> a.firstName == b.firstName && a.lastName == b.lastName)
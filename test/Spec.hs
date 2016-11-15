{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main (main) where

import  Test.Hspec
import  Test.Hspec.Wai
import  Test.Hspec.Wai.JSON
import  Data.Aeson (Value(..), object, (.=))

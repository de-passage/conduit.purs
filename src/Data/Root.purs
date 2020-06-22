module Data.Root (Root(..)) where

data Root
  = PublicApi
  | LocalHost Int
  | CustomBackend String

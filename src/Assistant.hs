module Assistant
  where

import Universum

import Time.Units (Second)

import Assistant.Config

type Assistant m = ReaderT (Config Second) m
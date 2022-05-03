module Assistant where

import Universum

type AssistantM r a = ReaderT r IO a

module Bluefin.Capability
  ( -- * Historical commentary

    -- | Bluefin is in a transitionary phase moving away from the old
    -- terminology of "handle" and naming handles/effects based on
    -- MTL\/transformers style names
    -- (e.g. @Exception@\/@Reader@\/@Stream@) and moving towards
    -- calling these things "capabilities" and naming them after their
    -- main operation (e.g. @Throw@\/@Ask@\/@Yield@).  You are
    -- encouraged to use the API beneath @Bluefin.Capability@ because
    -- that will be the supported API in the future.
    --
    -- You are encouraged to change your usage of the old modules on
    -- the left to the new modules on the right:
    --
    -- +------------------------+------------------------------------------+
    -- | Old                    | New                                      |
    -- +========================+==========================================+
    -- | "Bluefin.Reader"       | "Bluefin.Capability.Ask"                 |
    -- +------------------------+------------------------------------------+
    -- | "Bluefin.HandleReader" | "Bluefin.Capability.AskCapability"       |
    -- +------------------------+------------------------------------------+
    -- | "Bluefin.Consume"      | "Bluefin.Capability.Await"               |
    -- +------------------------+------------------------------------------+
    -- | "Bluefin.Jump"         | "Bluefin.Capability.JumpTo"              |
    -- +------------------------+------------------------------------------+
    -- | "Bluefin.State"        | "Bluefin.Capability.Modify"              |
    -- +------------------------+------------------------------------------+
    -- | "Bluefin.Coroutine"    | "Bluefin.Capability.Request"             |
    -- +------------------------+------------------------------------------+
    -- | "Bluefin.EarlyReturn"  | "Bluefin.Capability.ReturnEarly"         |
    -- +------------------------+------------------------------------------+
    -- | "Bluefin.Writer"       | "Bluefin.Capability.Tell"                |
    -- +------------------------+------------------------------------------+
    -- | "Bluefin.Exception"    | "Bluefin.Capability.Throw"               |
    -- +------------------------+------------------------------------------+
    -- | "Bluefin.Stream"       | "Bluefin.Capability.Yield"               |
    -- +------------------------+------------------------------------------+
  )
where

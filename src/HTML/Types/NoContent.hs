module HTML.Types.NoContent
  ( NoContent
      ( OmitTag
      , WithTag
      )
  ) where

-- | This represents an element that, for one reason or another, does not
-- contain child elements.
data NoContent
  -- | OmitTag means the tag is self closing, and thus omits a closing tag.
  = OmitTag
  -- | WithTag means the tag requires an explicit closing tag despite not being
  -- able to contain child elements.
  | WithTag


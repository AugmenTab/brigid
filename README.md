# brigid

A library for constructing type-safe and (mostly) spec-compliant HTML.

### TODO

1. Remove Fleece dependency.
2. Create benchmarks for Brigid HTML generation. Compare against Blaze and Lucid.
3. Change HTML entities to an ADT. Change the `ChildHTML` constructor to accept
   this, and add use `ToText` and `ToBytes`. Decide between either an `entity`
   combinator and the ADT exposed, or combinator functions that do this.

### TODO (HTML)

1. For each of global, HTMX, event listeners, arias, and scoped attributes:
    1. Make constructors for `Attribute` GADT.
    2. Make render functions.
    3. Modify `ValidElementsFor`.
    4. Modify the attribute selector functions in `HTML.Types.QuerySelector` to
       take their appropriate types.
2. Add relevant type class instances (Bounded, Enum, Eq, Ord, Show, etc) and
   from/to text functions to applicable types.
3. Consider a `contenteditable/inputmode` dual attribute type for the `Safe`
   module.
4. Add safe builder functions for any HTML elements with complex conditions for
   its permitted content, such as what was done with
   `Brigid.HTML.Elements.Safe.Table`. Go through the entire list of elements
   again to determine what qualifies for this.
5. Add support for the SVG tag.
6. Add support for MathML.
7. Look into safely constructing [Shadow DOM](https://developer.mozilla.org/en-US/docs/Web/API/Web_components/Using_shadow_DOM) elements.
8. Consider `Default` type class that returns the default value for a given
   type or attribute.
9. Review common classes (`Id`, `Class`, `Name`, etc) and maybe make smart
   constructors that prevent users from creating them with invalid characters,
   rather than escaping them after the fact.
10. Consider writing Aeson instances for common types.
11. Give `ChildHTML` and `Attribute` an `NFData` instance - this seems to be
    required for benchmarking.

### TODO (XML)

1. Add support for XML.
2. Add function to produce an XML WSDL from a provided Brigid XML structure?

### TODO (HXML)

1. For each attribute:
    1. Make constructors for `Attribute` GADT.
    2. Make render functions.
    3. Modify `ValidElementsFor`.
2. Add safe builder functions for any HXML elements with complex conditions for
   its permitted content. Go through the entire list of elements again to
   determine what qualifies for this.
3. Consider `Default` type class that returns the default value for a given
   type or attribute.

### Possible optimizations

1. Change all `ToText` and `ToBytes` functions to use builders? Export them for
   use in render, but not from `Brigid.HTML.Types`.
2. Switch to strict types for element combinators.
3. Inline newtypes.
4. Interpolate hard-coded string types everywhere.
5. Parallel rendering for DOMs branches with more than 3 children.
6. Replace `nubOrdOn attributeText` in element combinators with a
   custom fold in rendering that dedupes as it folds.


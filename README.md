# brigid

A library for constructing type-safe and (mostly) spec-compliant HTML.

This library is in active development; the API is not yet stable.

### TODO

1. Change HTML entities to an ADT. Change the `ChildHTML` constructor to accept
   this, and add use `ToText` and `ToBytes`. Decide between either an `entity`
   combinator and the ADT exposed, or combinator functions that do this.

### TODO (HTML)

1. For each of ~~global~~, ~~HTMX~~, event listeners, arias, and ~~scoped~~ attributes:
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
12. Make type synonyms for Shrubbery union member constraints.
13. `Maybe member` for members of Shrubbery union member constraints causes
    compile issues when `Nothing` is passed, because it can't determine what
    the union member should be. These unions should include `None` as an
    option, and that can be used for `Nothing` instead.

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

1. Switch the `[Attribute tag]` and `[ChildHTML tag parent]` fields in the `ChildHTML`
   and `ChildHXML` GADTs from lazy linked lists to `SmallArray` (from the `primitive`
   package). `SmallArray` is backed by GHC's `SmallArray#` primitive: single-pass
   construction via `smallArrayFromList`, no offset/length slice machinery (unlike
   `Vector`), and better cache locality than cons cells during render traversal.
   The ~128-element GC guidance for `SmallArray#` applies only to mutable arrays
   with frequent point writes; since these fields are written once and never mutated,
   there is no functional or performance concern at any element count. The change is
   entirely internal — GADT constructors are not exposed, so combinator signatures
   (`[Attribute tag] -> [ChildHTML tag parent] -> ChildHTML parent gp`) remain
   unchanged. Estimated impact: 5–15% improvement on construct+render benchmarks
   (higher for wide, flat trees; lower for narrow, deeply nested ones); 3–8% on
   render-only benchmarks. Requires adding `primitive` as an explicit dependency
   (it is already in the transitive closure via `text` and `hashable`).
2. Add `{-# INLINE #-}` pragmas to small functions called across module boundaries,
   particularly the `xToBytesBuilder` family and `buildAttribute` /
   `buildBooleanAttribute` in `Render/ByteString.hs`, to ensure GHC can fuse
   surrounding Builder operations.
3. Parallel rendering for DOMs branches with more than 3 children.
4. Replace `nubOrdOn attributeText` in element combinators with a
   custom fold in rendering that dedupes as it folds.
5. Switch to using `text-builder-linear` for all building functions.


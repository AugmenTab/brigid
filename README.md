# safe-html

A library for constructing type-safe and (mostly) spec-compliant HTML.

### TODO

1. Consider adding safer builder functions for any HTML elements with complex conditions for its permitted content, such as what was done with <del>`table`</del> <ins>`HTML.Elements.Table`</ins>. Go through the entire list of elements again to determine what qualifies for this.
2. For each of global, HTMX, event listeners, arias, and scoped
attributes:
    1. Make constructors for `Attribute` GADT.
    2. Make render functions.
    3. Modify `ValidElementsFor`.
    4. Modify the attribute functions in `HTML.Types.AttributeSelector` to take their appropriate types.
3. Add support for the SVG tag.
4. Add support for MathML.
5. Look into safely constructing [Shadow DOM](https://developer.mozilla.org/en-US/docs/Web/API/Web_components/Using_shadow_DOM) elements.
6. Consider adding support for an `XML` constructor for `ChildHTML`. It would take only custom elements and attributes.

### Possible optimizations

1. Change all `ToText` and `ToBytes` functions to use builders; export them for use in render, but not from `HTML.Types`.


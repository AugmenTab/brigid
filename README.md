# safe-html

A library for constructing type-safe HTML.

### TODO

1. Resolve all incomplete `ValidChildrenFor` instances.
2. Remove unused `TagGroups`.
3. Consider adding safer builder functions for any HTML elements with
complex conditions for its permitted content, such as what was done with <del>`table`</del> <ins>`HTML.Elements.Table`</ins>.
4. For each of global, HTMX, event listeners, arias, and scoped
attributes:
    1. Make constructors for `Attribute` GADT.
    2. Make render functions.
    3. Modify `ValidElementsFor`.
5. Add support for the SVG tag.
6. Add support for MathML.
7. Look into safely constructing [Shadow DOM](https://developer.mozilla.org/en-US/docs/Web/API/Web_components/Using_shadow_DOM) elements.


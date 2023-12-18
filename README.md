# safe-html

A library for constructing type-safe HTML.

### TODO

1. Resolve all incomplete `ValidChildrenFor` instances.
2. Remove type family instances for non-elements, void elements, and contentless elements.
3. Remove unused `TagGroups`.
4. Consider adding safer builder functions for any HTML elements with
complex conditions for its permitted content, such as what was done with <del>`table`</del> <ins>`Data.HTML4.Elements.Table`</ins>.
5. For each of global, HTMX, event listeners, arias, and scoped
attributes:
  1. Make constructors for `Attribute` GADT.
  2. Make render functions.
  3. Modify `ValidElementsFor`.
6. Add support for the SVG tag.
7. Add support for MathML.


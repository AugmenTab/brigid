# safe-html

A library for constructing type-safe HTML.

### TODO

1. Resolve all incomplete `ValidChildrenFor` instances.
2. Remove type family instances for non-elements, void elements, and contentless elements.
3. Remove unused `TagGroups`.
4. Consider adding safer builder functions for any HTML elements with
complex conditions for its permitted content, such as what was done with <del>`table`</del> <ins>`HTML.Elements.Table`</ins>.
5. For each of global, HTMX, event listeners, arias, and scoped
attributes:
    1. Make constructors for `Attribute` GADT.
    2. Make render functions.
    3. Modify `ValidElementsFor`.
6. Add support for the SVG tag.
7. Add support for MathML.
8. Complete functions to add attributes and child elements to an existing `ChildHTML`.
9. Support custom attributes.
    * Attr_Custom :: T.Text -> T.Text -> Attribute tag
10. Support custom elements?
    * Tag_Custom :: T.Text -> [Attribute 'Custom] -> [ChildHTML 'Custom] -> ChildHTML parent
    * Will have to be added to all lists as a valid child.


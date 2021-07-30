# Clutter
### Annotations/decorators/attributes for CLOS classes and slots

Clutter is a library to allow us Lispers to, well, clutter our beautiful CLOS classes and their slots with additional information (attributes or, as they're called in other languages, annotations or decorators).

One may thus define a class like the following:
```
(defclass my-class (my-superclass)
  ((some-slot :attributes ((pretty-name . "My precious") (validator . #'valid-treasure-p))))
  (:attributes (pretty-name . "My class") (version . "1.1"))
  (:metaclass clutter:standard-class-with-attributes))
```

Then, we can query the class and slot for information about their attributes:
```
(attribute 'pretty-name 'my-class) => "My class"
(attribute 'pretty-name (slot 'some-slot 'my-class)) => "My precious"
```
`attribute` is also a SETFable place.

Note that attributes in subclasses can override attributes in superclasses.

Internally, attributes are stored in hash tables. However, for convenience, we provide them as alists in defclass forms.

Note that defclass doesn't evaluate the attributes. If they're not literal constants you have to set them programmatically with `setf attribute`.

Also note that effective attributes are computed by `finalize-inheritance`, which is invoked by the MOP at some point between the declaration of the class and the creation of its first instance. When in doubt, call `finalize-inheritance` manually to ensure that you can query and update attributes.

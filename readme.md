How to deal with method/field aliasing?  Like Ruby?

Should I implement forward?

How would documentation work? 

* Add roxygen doclet that recogises @class (or @ioclass)
* This would add an alias so that class?method (internally converted to "class-method") would work

What should modifier functions return?  Should they always return self, so that you can chain methods?

To do
=====

* has_slot should also check if slot is not null - that ensures that it's possible to overwrite a parent's slot

* once rewritten get_slot is in place, it should be possible to use emptyenv() as the base environment for objects - that will prevent matching of any R function

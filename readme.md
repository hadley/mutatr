How to deal with method/field aliasing?  Like Ruby?

Should I implement forward?

How would documentation work? 

* Add roxygen doclet that recogises @class (or @ioclass)
* This would add an alias so that class?method (internally converted to "class-method") would work

What should modifier functions return?  Should they always return self, so that you can chain methods?

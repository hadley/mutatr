This is a prototype-based programming package for R, heavily inspired by [io](http://iolanguage.com/) and javascript.

To do
======

 * Implement deeper clone that calls clone/init all the way down

Ideas for documentation
=======================

With roxygen, add a doclet that recognises @class (or @ioclass).  This would add an alias so that class?method (internally converted to "class-method") would work.  This requires changes to roxygen so that it parses functions defined inside other functions.

Alternatively, documentation could be generated purely with proto.  This would probably involve defining some doc function that takes a roxygen tagged doc string and adds it to the specified function.  Yet another approach would be to parse do blocks, extract comments, use internal roxygen functions to parse and expose.

Other ideas
===========

 * Create new and inherit methods to illustrate how prototype based programming can be used to create a regular class based system

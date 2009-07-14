This is a prototype-based programming package for R, heavily inspired by (io)[http://iolanguage.com/].

To do
======

 * Proper iteration over ancestors (avoid cycles!)
 * Implement forward
 * Implement gettor and settor methods
 * Implement has_slot methods similarly


Ideas for documentation
=======================

With roxygen, add a doclet that recogises @class (or @ioclass).  This would add an alias so that class?method (internally converted to "class-method") would work.  This requires changes to roxygen so that it parses functions defined inside other functions.

Alternatively, documentation could be generated purely with proto.  This would probably involve defining some doc function that takes a roxygen tagged doc string and adds it to the specified function.  Yet another approach would be to parse do blocks, extract comments, use internal roxygen functions to parse and expose.

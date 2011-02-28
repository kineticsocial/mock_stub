-module(my_module).
-compile(export_all).
foo()    -> foo0.
foo(_)   -> foo1.
foo(_,_) -> foo2.

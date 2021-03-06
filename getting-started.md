---
layout: page
title: Getting started with LuvvieScript
tagline:  - its easy to do
---
{% include JB/setup %}

Getting Started
===============

You need to <a href="install.html">install</a> LuvvieScript and all its dependencies, obviously.

And you should read about the <a href="toolchain.html">toolchain</a> carefully.

Then go into ``test/not_passing/src/`` and have a good nosey around. Find a test you want to make pass and copy it into ``test/passing/src``.

Fire up a browser and point it to the online Javascript parser <a href="http://esprima.org/demo/parse">esprima</a>. Type in the Javascript you think you will need to compile to and that will give you a target Javascript AST.

You can use the erlang module ``test_utils.erl`` to help you make a unit test in ``to_js_ast.erl`` for that.

Then it is a matter of making and running the tests:
```
./build.sh
rebar compile
rebar make_tests
rebar ct skip_deps=true
```

It will crash and burn (obviously, otherwise the test would already be in ``test/passing/src``. But once you fix that, commit, get yer t-shirt, and move onto the next test.

  <div class='well'>
     <h4 class='text-info'>If you have read this far you should follow <a href='http://twitter.com/luvviescript'>@LuvvieScript</a> or <a href='http://twitter.com/gordonguthrie'>@gordonguthrie</a> on Twitter.</h4>
  </div>

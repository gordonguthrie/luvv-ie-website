---
layout: page
title: Installing And Running LuvvieScript
tagline:  - its easy to do
---
{% include JB/setup %}

Installing LuvvieScript
=======================

LuvvieScript is installed from github as so:

``git clone git@github.com:/hypernumbers/LuvvieScript.git``

Having installed it you need to pull down its dependencies by running the rebar command in the root directory where you have cloned it:
```
cd luvviescript/
rebar get_deps
```

The intention is to make it easier for non-Erlang programmers to join in this project. As a result it is written in Literate Erlang and not plain Erlang.

Literate Erlang is designed to work better in GitHub - the source code is properly readable - it is basically Erlang embedded inside Markdown.

Please see the <a href='http://github.com/hypernumbers/literate-erlang'>README</a> of Literate Erlang.

There are still some glitches which mean its a bit cumbersome and not working fully to snuff at the mo. As the project continues these will be ironed out and detailed documentation will be built into the code.

At the moment the literate Erlang toolchain is a bit immature, and we are using a bundled version not the GitHub one!. Bear with!

To get started run:
```
rebar get-deps
rebar compile
rebar compile-literate
rebar compile skip_deps=true
```

This will generate the erlang source files that can be compiled. The best way to develop literate Erlang (at the moment) is to then copy the ``src/*.erl`` files and ``include/*.hrl`` files into the ``src_md/.erl/`` and ``include_md/.hrl`` directories.

You can then edit the Erlang sources in ``src_md/.erl`` and ``include_md/.hrl`` and build as so:
```
./build.sh
rebar make_luvv
```

(The file ``build.sh`` runs a rebar plugin called ``markup`` which converts the erlang files to markdown - which are then compiled back to erlang... It seems like a bit round-the-houses but the purpose is to make a GitHub project that is easier for non-Erlang developers to join, *bear with*! *bear with*!).

To make tests you run:
```
./build.sh
rebar make_tests
rebar ct skip_deps=true
```

Remember to commit the changes as ``src_md/something.erl.md``!

Installing Non-Erlang Dependencies
==================================

At the moment the dependencies are a bit of a guddle.

You will need to install:
* ``NodeJs``
* ``npm``    (the node package manager)
* ``rhino``  (I know, two Javascript engines...)
* ``python``

(There is a json pretty printer in the toolchain which require Python)

You need to install some node modules:
```
sudo npm install -g escodegen
sudo npm install -g esparse-cl2
sudo npm install -g source-map
```

Getting Started
===============

You can then read about the <a href="toolchain.html">toolchain</a> and <a href="getting-started.html">get started</a>.
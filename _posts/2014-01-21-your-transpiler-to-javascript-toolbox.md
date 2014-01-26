---
layout: default
title: Your Transpiler-To-Javascript Toolbox
tagline:  - you need a special set of tools for transpiling
---
{% include JB/setup %}

### So You're Writing Your Own Transpile-To-Javascript Language?

Or maybe you are just thinking about it, but writing transpilers requires a few specialist tools. Elsewhere I talk about the <a href='http://luvv.ie/toolchain.html'>toolchain</a> in LuvvieScript in great detail, but here I will talk about some of the tools I use to build and test that toolchain.

Lets define terms first - what do I mean by transpiling? We all know what compiling is - taking a high-level format (a programme in a human readable language, like ``Ruby`` or ``C`` or the mighty ``Fortran``) doing some monkey business on it and outputting a form of that programme in a lower-level (maybe ``.elf`` or ``Java Byte Code`` or ``IRL``) which a computer can run.

Well transpiling is a bit like that on its side - instead of converting a programme down the complexity stack you are translating it sideways - more like Swedish-into-Swahili.

LuvvieScript is *Erlang-transpiled-to-Javascript* - so we have a lot of syntax tools already. The Erlang language is defined, the Javascript language is defined, there are parsers and lexers and a whole lot of good stuff on both sides of the house.

On the Erlang side there is:
* comprehensive <a href='http://www.erlang.org/doc/apps/syntax_tools/chapter.html'>Standard Syntax Tools</a> (which we don't use in LuvvieScript)
* a great <a href='http://www.erlang.org/doc/man/compile.html'>compiler</a> (which we do use)
* a whole range of stuff in ``stdlibs`` including a tokenizer <a href='http://www.erlang.org/doc/man/erl_scan.html'>erl_scan</a> (which we also use)

On the Javascript side we have:
* the Javascript parser <a href='http://esprima.org/'>esprima</a> which turns Javascript into an Abstract Syntax Tree (AST)
* the reverse of esprima <a href='https://github.com/Constellation/escodegen'>escodegen</a> which turns the Javascript AST back into Javascript
* <a href='https://npmjs.org/package/continuation.js'>Continuation.js</a> which implements tail-call eliminations

These are pretty complex, but well documented tools, of the sort you will have to master if you want to write your own transpiler.

But what I really want to dig into is the actual day-to-day grunt work - what do you ***actually need to do*** and ***how do you actually do it***.

With LuvvieScript it not quite the straight journey. First we compile down a bit: from Erlang to Core Erlang to a Core Erlang Abstract Syntax tree. Then we transpile sideways from the Core Erlang AST to the Javascript AST that the nice people at Mozilla have <a href='https://developer.mozilla.org/en-US/docs/SpiderMonkey/Parser_API'>defined</a>. Finally we build up from that Javascript AST to your actual Javascript.

The actual core of the work I am doing is the little sidie-ways bit between the AST's.

Its a bit like digging a tunnel through a mountain. On the West is the Vale D'Erlang, on the right StrathJavascript. I get a fragment of Erlang and think 'what Javascript pattern is going to implement that. I then write that Javascript and compile to the Javascript JSON API.

Then I push through the Erlang down my tunnel from the west getting a native Erlang data structure that represents the Erlang AST. I then prod my toolchain to actually emit the JSON representation of that and compare it to the one I am expecting to get - I am looking to see if the West and East tunnels are perfectly aligned - which of course they never are.


So there three tools that help here. The first is a json pretty printer. My Erlang libraries produce json that looks like this:

```javascript
{"type":"Program","body":[{"type":"VariableDeclaration","declarations":[{"type":"VariableDeclarator","id":{"type":"Identifier","name":"exports"},"init":{"type":"ObjectExpression","properties":[]}},{"type":"VariableDeclarator","id":{"type":"Identifier","name":"not_exported_fn"},"init":{"type":"ObjectExpression","properties":[]}},{"type":"VariableDeclarator","id":{"type":"Identifier","name":"simple_fn"},"init":{"type":"ObjectExpression","properties":[]}}],"kind":"var"},{"type":"ExpressionStatement","expression":{"type":"AssignmentExpression","operator":"=","left":{"type":"MemberExpression","computed":false,"object":{"type":"Identifier","name":"exports"},"property":{"type":"Identifier","name":"simple_fn"}},"right":{"type":"FunctionExpression","id":null,"params":[],"defaults":[],"body":{"type":"BlockStatement","body":[{"type":"ExpressionStatement","expression":...
```

A simple json pretty printer is a godsend - I use a Python not-quite-one-liner:

```python
#!/usr/bin/env python
"""
Convert JSON data to human-readable form.

(Reads from stdin and writes to stdout)
"""

import sys
import json


print json.dumps(json.loads(sys.stdin.read()), indent=4)
sys.exit(0)
```

Suddenly, my json is not quite so impenetrable:

```javascript
{
    "body": [
        {
            "kind": "var",
            "declarations": [
                {
                    "init": {
                        "type": "ObjectExpression",
                        "properties": []
                    },
                    "type": "VariableDeclarator",
                    "id": {
                        "type": "Identifier",
                        "name": "exports"
                    }
                },
```

This brings me to my next tool. JSON is notoriously hard on the scroll button, particularly highly-nested JSON like Abstract Syntax Trees. It is not unusual for a **simple** JSON AST expression so split over a hundred lines. An editor that can fold out the embedded bits is critical - enter <a href='http://www.sublimetext.com'>Sublime Text</a>:

<img class='img-responsive' src='/assets/img/sublime-text-code-folding-json.png' alt='Folding JSON In The Sublime Text Editor' />

There is still one little problem - the json you are producing on one side of the house is often not formatted the same way as that produced on the other side. Old fashioned ``diff`` is hard to get to work even with purty json - what we need is a <a href="http://tlrobinson.net/projects/javascript-fun/jsondiff/">json-diff</a> tool.

Lets see this in action. Here is the simple erlang test function I am trying to get passing:
```erlang
-module('1b_simpler').

-export([
         simple_fn/0
        ]).

simple_fn() ->
    not_exported_fn().

not_exported_fn() ->
    "django".
```

I write a fragment of my target output in the <a href='http://esprima.org/demo/parse.html'>Esprima Online Parser</a>:

<img class='img-responsive' src='/assets/img/esprima-online-parser-generating-javascript-abstract-syntax-tree.png' alt='Esprima Online Parser Generating Javascript Abstract Syntax Tree' />

I can now compare a fragment from my transpiler with what it ought to be:

<img class='img-responsive' src='/assets/img/json-diff-tool-comparing-abstract-syntax-trees.png' alt='Comparing Abstract Syntax Trees With JSON-Diff' />

The tool shows one of the typical hard-to-spot-by-inspection bugs - returning an object instead of a list of objects:

<img class='img-responsive' src='/assets/img/json-diff-tool-showing-differences.png' alt='JSON-Diff tool showing differences' />

That's it folks, a quick run through of the day-to-day work of developing a ***something-to-javascript*** transpiler.

If you are interested in ***this*** Erlang-to-javascript transpiler you can read about the <a href='http://luvv.ie/toolchain.html'>toolchain</a> and <a href='http://luvv.ie/mission.html'>mission</a> and, mebbies, <a href='http://luvv.ie/install.html'>install</a> it and <a href='http://luvv.ie/getting-started.html'>get started</a>.

Toodle-pip!



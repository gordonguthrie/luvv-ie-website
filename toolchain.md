---
layout: widepage
title: LuvvieScript
tagline:  - help you contribute
---
{% include JB/setup %}

The LuvvieScript Toolchain
--------------------------

This page takes you through the production process for LuvvieScript and describes each stage. This will help you understand how the compiler is supposed to work and help you contribute to the development.

Erlang
------

The first step is to define a simple Erlang module which is to be compiled to Javascript. This is in ``test/passing/src/demo.src``.

The luvviescript compiler can be run in two modes ``production`` or ``debug``. In ``debug`` mode it writes out all the intermediate stages to file - whis page will take you through them.

```erlang
-module(demo).

-export([
         test/0
        ]).

test() ->
    A = first(),
    B = second(),
    C = third(),
    A + B / C.

first() ->
    1.

second() ->
    2.

third() ->
    3.
````

The problem with Erlang source is that the ``-include`` attributes haven't been processed and the code can have arbitraty whitespace in it. As we will see later this causes a problem when you are trying to generate source maps.

We solve this problem by using an intermediate output of the Erlang compiler called the ``.P`` format - this file is written to ``test/passing/debug/demo.P`` directory. That ``.P`` format file is then slightly tidied up to what is known internally as the ``dotP2`` format which is written out to ``test/psrc/demo.src``. This format is the actual Erlang that will be used to build the source maps and the javascript and wil be displayed in the browser.

The ``psrc`` variant is:

```erlang
-file("test/passing/src/demo.erl", 1).

-module(demo).

-export([test/0]).

test() ->
    A = first(),
    B = second(),
    C = third(),
    A + B / C.

first() ->
    1.

second() ->
    2.

third() ->
    3.
```

This doesn't look so different from the original source. However if you where to *untidify* the original by adding additional white space in strange ways, you would see that the layout and style is *normalised*.

At this point two seperate transformations are going to be applied to the source:

* a compilation to a **Core Erlang** Asbtract Syntax Tree
* a loexical tokensing of the source

The AST will have line information in it, but no column information (source maps need both). The lexical tokens will be transformed in a source of column information and this will be used to annotate the AST.

Before diving into this it is worth explaining what Core Erlang is. Basically it is a language that is semantically equivalent to Erlang but which has all the syntactic sugar stripped out. It is designed to be a target language for compile-to-Erlang languages. Core Erlang is not actually generated during the LuvvieScript compilation process, but the ``debug`` option writes out the core representation anyway. Here is ``demo.erl`` in Core Erlang:

```
module 'demo' ['module_info'/0,
	       'module_info'/1,
	       'test'/0]
    attributes []
'test'/0 =
    %% Line 7
    fun () ->
	let <A> =
	    %% Line 8
	    apply 'first'/0
		()
	in  let <B> =
		%% Line 9
		apply 'second'/0
		    ()
	    in  let <C> =
		    %% Line 10
		    apply 'third'/0
			()
		in  let <_cor3> =
			%% Line 11
			call 'erlang':'/'
			    (B, C)
		    in  %% Line 11
			call 'erlang':'+'
			    (A, _cor3)
'first'/0 =
    %% Line 13
    fun () ->
	%% Line 14
	1
'second'/0 =
    %% Line 16
    fun () ->
	%% Line 17
	2
'third'/0 =
    %% Line 19
    fun () ->
	%% Line 20
	3
'module_info'/0 =
    fun () ->
	call 'erlang':'get_module_info'
	    ('demo')
'module_info'/1 =
    fun (_cor0) ->
	call 'erlang':'get_module_info'
	    ('demo', _cor0)
end
```

Core Erlang is a great choice for transpiling from (or to) because of the small number of primitives it has - a mere 21:

    literal, binary, bitstr, cons, tuple,
    var, values, fun, seq, let, letrec,
    case, clause, alias, receive, apply,
    call, primop, try, catch, module

If you are not familiar with functional programming you might be freaked out that the ***only*** flow of control primitive is ``case``, there is no ``if`` or ``for`` but, trust me, you won't miss them.

You can read more about Core Erlang at http://www.it.uu.se/research/group/hipe/cerl/

Erlang has two Abstract Syntax Trees (it might confused you, it certainly confused me). One is the normal Erlang one and the other is the Core Erlang one - and the Syntax Tools that come with Erlang are correspondingly split. For LuvvieScript we are going down the Core Erlang route. Here is the code after it has been compiled to the AST as ``passed/debug/demo.ast``:

```
{c_module,[],
    {c_literal,[],demo},
    [{c_var,[],{module_info,0}},
     {c_var,[],{module_info,1}},
     {c_var,[],{test,0}}],
    [],
    [{{c_var,[],{test,0}},
      {c_fun,
          [7,{file,"test/passing/src/demo.erl"}],
          [],
          {c_let,[],
              [{c_var,[8,{file,"test/passing/src/demo.erl"}],'A'}],
              {c_apply,
                  [8,{file,"test/passing/src/demo.erl"}],
                  {c_var,[8,{file,"test/passing/src/demo.erl"}],{first,0}},
                  []},
              {c_let,[],
                  [{c_var,[9,{file,"test/passing/src/demo.erl"}],'B'}],
                  {c_apply,
                      [9,{file,"test/passing/src/demo.erl"}],
                      {c_var,
                          [9,{file,"test/passing/src/demo.erl"}],
                          {second,0}},
                      []},
                  {c_let,[],
                      [{c_var,[10,{file,"test/passing/src/demo.erl"}],'C'}],
                      {c_apply,
                          [10,{file,"test/passing/src/demo.erl"}],
                          {c_var,
                              [10,{file,"test/passing/src/demo.erl"}],
                              {third,0}},
                          []},
                          ...
```

This is where we hit our first Erlang/Javascript roadblock. Erlang functions are defined by a ``name/arity`` combination, eg ``somefn/3`` which means the function ``somefn`` that accepts 3 arguments. ``somefn/3`` is a different function to ``somefn/2``. In javascript there is no such thing. I can define a function as taking three arguments and then call it with 2, or 4 or 11. If I redefine it as a function taking 4 arguements I (might) silently overwrite the original definition...

We get round this by sorting the AST to group all the functions with the same root name together - when we generate the Javascript AST we will wrap the function call with a case scaffold that will see how many arguments the function was called with and direct the call to the appropriate function body. (Whilst we are at it we also strip off the ``module_info/0`` and ``module_info/1`` export declarations). This new sorted version of the AST is written as ``passing/debug/demo.ast2`` but it is not very interesting.

We now have an AST which we could transpile into Javascript - but it doesn't contain the vital column information we need to generate source maps. Source maps are used in the browser's debugger to enable the developer to debug in the orginal language (LuvvieScript would be unusable as a dom scripting language without them).

What we need to do is annotate the AST with line/col information. The second element of every record type in the AST is a list of annotations. If you look at the AST you will see some records have an annotation like ``{c_literal,[14,{file,"test/passing/src/demo.erl"}],1}}}`` what this means is that the ``literal value 1`` appears on ``line 14`` of the file ``test/passing/src/demo.erl``.

To get the column information we transform the source code into lexical tokens, which are written to the file ``test/passing/debug/demo.tks``:

```
[{'-',[{line,1},{text,"-"}]},
 {atom,[{line,1},{text,"file"}],file},
 {'(',[{line,1},{text,"("}]},
 {string,[{line,1},{text,"\"test/passing/src/demo.erl\""}],
         "test/passing/src/demo.erl"},
 {',',[{line,1},{text,","}]},
 {white_space,[{line,1},{text," "}]," "},
 {integer,[{line,1},{text,"1"}],1},
 {')',[{line,1},{text,")"}]},
 {dot,[{line,1},{text,".\n"}]},
 {white_space,[{line,2},{text,"\n"}],"\n"},
 {'-',[{line,3},{text,"-"}]},
 ...
]
```

Each line describes a token - the tuple ``{text, "\n"}`` gives the actual text in the source code. By counting the length of each text string we can build the column information we need, we do that and output the new token information in the file ``test/passing/debug/demo.tks2``:

```
[{1,
  [{'-',{1,{1,2}},operator},
   {file,{1,{3,7}},atom},
   {'(',{1,{8,9}},operator},
   {"test/passing/src/demo.erl",{1,{10,37}},string},
   {',',{1,{38,39}},operator},
   {" ",{1,40},white_space},
   {1,{1,{41,42}},integer},
   {')',{1,{43,44}},operator},
   {dot,{1,{45,47}},operator}]},
 {2,[{"\n",{2,48},white_space}]},
 {3,
  [{'-',{3,{1,2}},operator},
   {module,{3,{3,9}},atom},
   {'(',{3,{10,11}},operator},
   {demo,{3,{12,16}},atom},
   {')',{3,{17,18}},operator},
   {dot,{3,{19,21}},operator}]},
 {4,[{"\n",{4,22},white_space}]},
 {5,
  [{'-',{5,{1,2}},operator},
   {export,{5,{3,9}},atom},
   {'(',{5,{10,11}},operator},
   {'[',{5,{12,13}},operator},
   {test,{5,{14,18}},atom},
   {'/',{5,{19,20}},operator},
   {0,{5,{21,22}},integer},
   {']',{5,{23,24}},operator},
   {')',{5,{25,26}},operator},
   {dot,{5,{27,29}},operator}]},
 {6,[{"\n",{6,30},white_space}]},
 ...
```

We can now merge this line column information to make the final version of the ast ``test/passing/debug/demo.ast3`` - which is otherwise uninteresting.

Finally the actual work starts, each element in the AST needs to be transiled to the Javascript AST. There are no tools to do this, we have to write the code by hand.

The transpiled AST for ``demo.erl`` is created in the ``.jast`` format which is an otherwise uniteresting Erlang represntation of JSON. We convert that to proper json (the ``.jast2`` format) which has no formatting and is therefore unreadable by normal people. Finally we call a pretty printer on that and emit the usable JSON file ``test/passing/debug/demo.js``:
```
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
                }
            ],
            "type": "VariableDeclaration"
        },
        {
            "expression": {
                "operator": "=",
                "right": {
                    "body": {
                        "body": [
                            {
                                "expression": {
                                    "operator": "=",
                                    "right": {
                                        "property": {
                                            "type": "Identifier",
                                            "name": "length"
                                        },
                                        "object": {
                                            "type": "Identifier",
                                            "name": "arguments"
                                        },
                                        "type": "MemberExpression",
                                        "computed": false
                                    },
                                    "type": "AssignmentExpression",
                                    "left": {
                                        "type": "Identifier",
                                        "name": "_args"
                                    }
                                },
                                "type": "ExpressionStatement"
                            },
                            ...
```

Calling ``escodegen.js`` on this Javascript AST gives us our final productions, the javascript (``test/passing/js/demo.js``) file and the source map (``test/passing/js/demo.js.map``):

```javascript
var exports = {};
exports.test = function () {
    _args = arguments.length;
    switch (_args) {
    case 0:
        return test();
        break;
    default:
    }
};
first = function () {
    _args = arguments.length;
    switch (_args) {
    case 0:
        return 1;
        break;
    default:
        return 'throw error';
    }
};
second = function () {
    _args = arguments.length;
    switch (_args) {
    case 0:
        return 2;
        break;
    default:
        return 'throw error';
    }
};
test = function () {
    _args = arguments.length;
    switch (_args) {
    case 0:
        var A;
        var B;
        var C;
        var cor3;
        A = first();
        B = second();
        C = third();
        cor3 = B / C;
        return A + cor3;
        break;
    default:
        return 'throw error';
    }
};
third = function () {
    _args = arguments.length;
    switch (_args) {
    case 0:
        return 3;
        break;
    default:
        return 'throw error';
    }
};
```

The source map is a funny compressed format which, (thankfully for us) is generated automatically.

```
{"version":3,"file":"test/passing/src/../psrc/demo.erl","sources":["test/passing/src/../psrc/demo.erl"],"names":[],"mappings":";;;;;;;;;;;;;;eAaK,C;;;;;;;;;;eAGA,C;;;;;;;;;;;;;;QATA,C;QACA,C;QACA,C;;;;;;;;;;;;eAUA,C"}
```

Decisions, Decisions
====================

We still don't really know how we are going to handle types, there are some suggestions:

    Erlang &             Javascript
    LuvvieSript

    Integer        <---> Double
                         ! Erlang supports big nums
                         Also Erlang supports radix of 2..36 check in js
    Float          <---> Double
    Char           <---> Double
    String         <---> Array of UTF16 character points
                         All fn calls out to external fns will be cast on return
                         Is this sensible or should be try and use native strings?
                         Would we need to wrap a cast around all list operations?
    Atom           <---> object {atom: Atom}
    List           <---> Array
    Records        <---> Objects
                         with a helper fn to map position to element name
    Funs           <---> Funs
    Tuple          <---> {tuple: Array}
    Dict Of Dicts  <---> Objects/JSON
                         All Fn calls out to external fns will be cast on return

    Binaries       <--->  none

The approach used by <a href='http://github.com/rustyio/bert-js'>Bert-JS</a> is also being examined.

Testing Framework
-----------------

Because LuvvieScript is a strict subset of Erlang, all LuvvieScript modules will also compile to Erlang. The test framework takes advantage of this. Tests simply consist of Erlang modules. Any zero-arity functions exported from them is regarded as a test. The module is compiled to ``.beam`` files - every zero-arity function is called and the return values are collected.

An Erlang test file is then called which executes the zero-arity functions in the compiled Javascript and checks that the Javascript returns the same values.

  <div class='well'>
     <h4 class='text-info'>If you have read this far you should follow <a href='http://twitter.com/luvviescript'>@LuvvieScript</a> on Twitter.</h4>
  </div>

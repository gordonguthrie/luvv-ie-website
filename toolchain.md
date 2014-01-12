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

You can read more about Core Erlang at http://www.it.uu.se/research/group/hipe/cerl/

Erlang (confusingly) has two Abstract Syntax Trees - the normal Erlang one and the Core Erlang one - and the Syntax Tools that come with Erlang are correspondingly split. For LuvvieScript we are going down the Core Erlang route. Here is the code after it has been compiled to the AST as ``passed/debug/demo.ast``:

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
                      {c_let,[],
                          [{c_var,[],cor3}],
                          {c_call,
                              [11,{file,"test/passing/src/demo.erl"}],
                              {c_literal,
                                  [11,{file,"test/passing/src/demo.erl"}],
                                  erlang},
                              {c_literal,
                                  [11,{file,"test/passing/src/demo.erl"}],
                                  '/'},
                              [{c_var,
                                   [11,{file,"test/passing/src/demo.erl"}],
                                   'B'},
                               {c_var,
                                   [11,{file,"test/passing/src/demo.erl"}],
                                   'C'}]},
                          {c_call,
                              [11,{file,"test/passing/src/demo.erl"}],
                              {c_literal,
                                  [11,{file,"test/passing/src/demo.erl"}],
                                  erlang},
                              {c_literal,
                                  [11,{file,"test/passing/src/demo.erl"}],
                                  '+'},
                              [{c_var,
                                   [11,{file,"test/passing/src/demo.erl"}],
                                   'A'},
                               {c_var,[],cor3}]}}}}}}},
     {{c_var,[],{first,0}},
      {c_fun,
          [13,{file,"test/passing/src/demo.erl"}],
          [],
          {c_literal,[14,{file,"test/passing/src/demo.erl"}],1}}},
     {{c_var,[],{second,0}},
      {c_fun,
          [16,{file,"test/passing/src/demo.erl"}],
          [],
          {c_literal,[17,{file,"test/passing/src/demo.erl"}],2}}},
     {{c_var,[],{third,0}},
      {c_fun,
          [19,{file,"test/passing/src/demo.erl"}],
          [],
          {c_literal,[20,{file,"test/passing/src/demo.erl"}],3}}},
     {{c_var,[],{module_info,0}},
      {c_fun,
          [0,{file,"test/passing/src/demo.erl"}],
          [],
          {c_call,
              [0,{file,"test/passing/src/demo.erl"}],
              {c_literal,[0,{file,"test/passing/src/demo.erl"}],erlang},
              {c_literal,
                  [0,{file,"test/passing/src/demo.erl"}],
                  get_module_info},
              [{c_literal,[0,{file,"test/passing/src/demo.erl"}],demo}]}}},
     {{c_var,[],{module_info,1}},
      {c_fun,
          [0,{file,"test/passing/src/demo.erl"}],
          [{c_var,[0,{file,"test/passing/src/demo.erl"}],cor0}],
          {c_call,
              [0,{file,"test/passing/src/demo.erl"}],
              {c_literal,[0,{file,"test/passing/src/demo.erl"}],erlang},
              {c_literal,
                  [0,{file,"test/passing/src/demo.erl"}],
                  get_module_info},
              [{c_literal,[0,{file,"test/passing/src/demo.erl"}],demo},
               {c_var,[0,{file,"test/passing/src/demo.erl"}],cor0}]}}}]}
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
 {atom,[{line,3},{text,"module"}],module},
 {'(',[{line,3},{text,"("}]},
 {atom,[{line,3},{text,"demo"}],demo},
 {')',[{line,3},{text,")"}]},
 {dot,[{line,3},{text,".\n"}]},
 {white_space,[{line,4},{text,"\n"}],"\n"},
 {'-',[{line,5},{text,"-"}]},
 {atom,[{line,5},{text,"export"}],export},
 {'(',[{line,5},{text,"("}]},
 {'[',[{line,5},{text,"["}]},
 {atom,[{line,5},{text,"test"}],test},
 {'/',[{line,5},{text,"/"}]},
 {integer,[{line,5},{text,"0"}],0},
 {']',[{line,5},{text,"]"}]},
 {')',[{line,5},{text,")"}]},
 {dot,[{line,5},{text,".\n"}]},
 {white_space,[{line,6},{text,"\n"}],"\n"},
 {atom,[{line,7},{text,"test"}],test},
 {'(',[{line,7},{text,"("}]},
 {')',[{line,7},{text,")"}]},
 {white_space,[{line,7},{text," "}]," "},
 {'->',[{line,7},{text,"->"}]},
 {white_space,[{line,7},{text,"\n    "}],"\n    "},
 {var,[{line,8},{text,"A"}],'A'},
 {white_space,[{line,8},{text," "}]," "},
 {'=',[{line,8},{text,"="}]},
 {white_space,[{line,8},{text," "}]," "},
 {atom,[{line,8},{text,"first"}],first},
 {'(',[{line,8},{text,"("}]},
 {')',[{line,8},{text,")"}]},
 {',',[{line,8},{text,","}]},
 {white_space,[{line,8},{text,"\n    "}],"\n    "},
 {var,[{line,9},{text,"B"}],'B'},
 {white_space,[{line,9},{text," "}]," "},
 {'=',[{line,9},{text,"="}]},
 {white_space,[{line,9},{text," "}]," "},
 {atom,[{line,9},{text,"second"}],second},
 {'(',[{line,9},{text,"("}]},
 {')',[{line,9},{text,")"}]},
 {',',[{line,9},{text,","}]},
 {white_space,[{line,9},{text,"\n    "}],"\n    "},
 {var,[{line,10},{text,"C"}],'C'},
 {white_space,[{line,10},{text," "}]," "},
 {'=',[{line,10},{text,"="}]},
 {white_space,[{line,10},{text," "}]," "},
 {atom,[{line,10},{text,"third"}],third},
 {'(',[{line,10},{text,"("}]},
 {')',[{line,10},{text,")"}]},
 {',',[{line,10},{text,","}]},
 {white_space,[{line,10},{text,"\n    "}],"\n    "},
 {var,[{line,11},{text,"A"}],'A'},
 {white_space,[{line,11},{text," "}]," "},
 {'+',[{line,11},{text,"+"}]},
 {white_space,[{line,11},{text," "}]," "},
 {var,[{line,11},{text,"B"}],'B'},
 {white_space,[{line,11},{text," "}]," "},
 {'/',[{line,11},{text,"/"}]},
 {white_space,[{line,11},{text," "}]," "},
 {var,[{line,11},{text,"C"}],'C'},
 {dot,[{line,11},{text,".\n"}]},
 {white_space,[{line,12},{text,"\n"}],"\n"},
 {atom,[{line,13},{text,"first"}],first},
 {'(',[{line,13},{text,"("}]},
 {')',[{line,13},{text,")"}]},
 {white_space,[{line,13},{text," "}]," "},
 {'->',[{line,13},{text,"->"}]},
 {white_space,[{line,13},{text,"\n    "}],"\n    "},
 {integer,[{line,14},{text,"1"}],1},
 {dot,[{line,14},{text,".\n"}]},
 {white_space,[{line,15},{text,"\n"}],"\n"},
 {atom,[{line,16},{text,"second"}],second},
 {'(',[{line,16},{text,"("}]},
 {')',[{line,16},{text,")"}]},
 {white_space,[{line,16},{text," "}]," "},
 {'->',[{line,16},{text,"->"}]},
 {white_space,[{line,16},{text,"\n    "}],"\n    "},
 {integer,[{line,17},{text,"2"}],2},
 {dot,[{line,17},{text,".\n"}]},
 {white_space,[{line,18},{text,"\n"}],"\n"},
 {atom,[{line,19},{text,"third"}],third},
 {'(',[{line,19},{text,"("}]},
 {')',[{line,19},{text,")"}]},
 {white_space,[{line,19},{text," "}]," "},
 {'->',[{line,19},{text,"->"}]},
 {white_space,[{line,19},{text,"\n    "}],"\n    "},
 {integer,[{line,20},{text,"3"}],3},
 {dot,[{line,20},{text,".\n"}]},
 {white_space,[{line,21},{text,"\n"}],"\n"},
 {white_space,[{line,22},{text,"\n"}],"\n"},
 {white_space,[{line,23},{text,"\n"}],"\n"}]
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
 {7,
  [{test,{7,{1,5}},atom},
   {'(',{7,{6,7}},operator},
   {')',{7,{8,9}},operator},
   {" ",{7,10},white_space},
   {'->',{7,{11,13}},operator},
   {"\n    ",{7,14},white_space}]},
 {8,
  [{'A',{8,{5,6}},var},
   {" ",{8,7},white_space},
   {'=',{8,{8,9}},operator},
   {" ",{8,10},white_space},
   {first,{8,{11,16}},atom},
   {'(',{8,{17,18}},operator},
   {')',{8,{19,20}},operator},
   {',',{8,{21,22}},operator},
   {"\n    ",{8,23},white_space}]},
 {9,
  [{'B',{9,{5,6}},var},
   {" ",{9,7},white_space},
   {'=',{9,{8,9}},operator},
   {" ",{9,10},white_space},
   {second,{9,{11,17}},atom},
   {'(',{9,{18,19}},operator},
   {')',{9,{20,21}},operator},
   {',',{9,{22,23}},operator},
   {"\n    ",{9,24},white_space}]},
 {10,
  [{'C',{10,{5,6}},var},
   {" ",{10,7},white_space},
   {'=',{10,{8,9}},operator},
   {" ",{10,10},white_space},
   {third,{10,{11,16}},atom},
   {'(',{10,{17,18}},operator},
   {')',{10,{19,20}},operator},
   {',',{10,{21,22}},operator},
   {"\n    ",{10,23},white_space}]},
 {11,
  [{'A',{11,{5,6}},var},
   {" ",{11,7},white_space},
   {'+',{11,{8,9}},operator},
   {" ",{11,10},white_space},
   {'B',{11,{11,12}},var},
   {" ",{11,13},white_space},
   {'/',{11,{14,15}},operator},
   {" ",{11,16},white_space},
   {'C',{11,{17,18}},var},
   {dot,{11,{19,21}},operator}]},
 {12,[{"\n",{12,22},white_space}]},
 {13,
  [{first,{13,{1,6}},atom},
   {'(',{13,{7,8}},operator},
   {')',{13,{9,10}},operator},
   {" ",{13,11},white_space},
   {'->',{13,{12,14}},operator},
   {"\n    ",{13,15},white_space}]},
 {14,[{1,{14,{5,6}},integer},{dot,{14,{7,9}},operator}]},
 {15,[{"\n",{15,10},white_space}]},
 {16,
  [{second,{16,{1,7}},atom},
   {'(',{16,{8,9}},operator},
   {')',{16,{10,11}},operator},
   {" ",{16,12},white_space},
   {'->',{16,{13,15}},operator},
   {"\n    ",{16,16},white_space}]},
 {17,[{2,{17,{5,6}},integer},{dot,{17,{7,9}},operator}]},
 {18,[{"\n",{18,10},white_space}]},
 {19,
  [{third,{19,{1,6}},atom},
   {'(',{19,{7,8}},operator},
   {')',{19,{9,10}},operator},
   {" ",{19,11},white_space},
   {'->',{19,{12,14}},operator},
   {"\n    ",{19,15},white_space}]},
 {20,[{3,{20,{5,6}},integer},{dot,{20,{7,9}},operator}]},
 {21,[{"\n",{21,10},white_space}]},
 {22,[{"\n",{22,1},white_space}]},
 {23,[{"\n",{23,1},white_space}]}]
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
                            {
                                "cases": [
                                    {
                                        "test": {
                                            "raw": "0",
                                            "type": "Literal",
                                            "value": 0
                                        },
                                        "type": "SwitchCase",
                                        "consequent": [
                                            {
                                                "type": "ReturnStatement",
                                                "argument": {
                                                    "callee": {
                                                        "type": "Identifier",
                                                        "name": "test"
                                                    },
                                                    "type": "CallExpression",
                                                    "arguments": []
                                                }
                                            },
                                            {
                                                "type": "BreakStatement",
                                                "label": null
                                            }
                                        ]
                                    },
                                    {
                                        "test": null,
                                        "type": "SwitchCase",
                                        "consequent": {
                                            "type": "ReturnStatement",
                                            "argument": {
                                                "raw": "\"throw error\"",
                                                "type": "Literal",
                                                "value": "throw error"
                                            }
                                        }
                                    }
                                ],
                                "type": "SwitchStatement",
                                "discriminant": {
                                    "type": "Identifier",
                                    "name": "_args"
                                }
                            }
                        ],
                        "type": "BlockStatement"
                    },
                    "generator": false,
                    "expression": false,
                    "rest": null,
                    "params": [],
                    "defaults": [],
                    "type": "FunctionExpression",
                    "id": null
                },
                "type": "AssignmentExpression",
                "left": {
                    "property": {
                        "type": "Identifier",
                        "name": "test"
                    },
                    "object": {
                        "type": "Identifier",
                        "name": "exports"
                    },
                    "type": "MemberExpression",
                    "computed": false
                }
            },
            "type": "ExpressionStatement"
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
                            {
                                "cases": [
                                    {
                                        "test": {
                                            "raw": "0",
                                            "type": "Literal",
                                            "value": 0
                                        },
                                        "type": "SwitchCase",
                                        "consequent": [
                                            {
                                                "type": "ReturnStatement",
                                                "argument": {
                                                    "raw": "1",
                                                    "type": "Literal",
                                                    "value": 1,
                                                    "loc": {
                                                        "start": {
                                                            "column": 5,
                                                            "line": 14
                                                        },
                                                        "end": {
                                                            "column": 6,
                                                            "line": 14
                                                        }
                                                    }
                                                }
                                            },
                                            {
                                                "type": "BreakStatement",
                                                "label": null
                                            }
                                        ]
                                    },
                                    {
                                        "test": null,
                                        "type": "SwitchCase",
                                        "consequent": [
                                            {
                                                "type": "ReturnStatement",
                                                "argument": {
                                                    "raw": "\"throw error\"",
                                                    "type": "Literal",
                                                    "value": "throw error"
                                                }
                                            }
                                        ]
                                    }
                                ],
                                "type": "SwitchStatement",
                                "discriminant": {
                                    "type": "Identifier",
                                    "name": "_args"
                                }
                            }
                        ],
                        "type": "BlockStatement"
                    },
                    "generator": false,
                    "expression": false,
                    "rest": null,
                    "params": [],
                    "defaults": [],
                    "type": "FunctionExpression",
                    "id": null
                },
                "type": "AssignmentExpression",
                "left": {
                    "type": "Identifier",
                    "name": "first"
                }
            },
            "type": "ExpressionStatement"
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
                            {
                                "cases": [
                                    {
                                        "test": {
                                            "raw": "0",
                                            "type": "Literal",
                                            "value": 0
                                        },
                                        "type": "SwitchCase",
                                        "consequent": [
                                            {
                                                "type": "ReturnStatement",
                                                "argument": {
                                                    "raw": "2",
                                                    "type": "Literal",
                                                    "value": 2,
                                                    "loc": {
                                                        "start": {
                                                            "column": 5,
                                                            "line": 17
                                                        },
                                                        "end": {
                                                            "column": 6,
                                                            "line": 17
                                                        }
                                                    }
                                                }
                                            },
                                            {
                                                "type": "BreakStatement",
                                                "label": null
                                            }
                                        ]
                                    },
                                    {
                                        "test": null,
                                        "type": "SwitchCase",
                                        "consequent": [
                                            {
                                                "type": "ReturnStatement",
                                                "argument": {
                                                    "raw": "\"throw error\"",
                                                    "type": "Literal",
                                                    "value": "throw error"
                                                }
                                            }
                                        ]
                                    }
                                ],
                                "type": "SwitchStatement",
                                "discriminant": {
                                    "type": "Identifier",
                                    "name": "_args"
                                }
                            }
                        ],
                        "type": "BlockStatement"
                    },
                    "generator": false,
                    "expression": false,
                    "rest": null,
                    "params": [],
                    "defaults": [],
                    "type": "FunctionExpression",
                    "id": null
                },
                "type": "AssignmentExpression",
                "left": {
                    "type": "Identifier",
                    "name": "second"
                }
            },
            "type": "ExpressionStatement"
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
                            {
                                "cases": [
                                    {
                                        "test": {
                                            "raw": "0",
                                            "type": "Literal",
                                            "value": 0
                                        },
                                        "type": "SwitchCase",
                                        "consequent": [
                                            {
                                                "kind": "var",
                                                "declarations": [
                                                    {
                                                        "init": null,
                                                        "type": "VariableDeclarator",
                                                        "id": {
                                                            "type": "Identifier",
                                                            "name": "A"
                                                        }
                                                    }
                                                ],
                                                "type": "VariableDeclaration"
                                            },
                                            {
                                                "kind": "var",
                                                "declarations": [
                                                    {
                                                        "init": null,
                                                        "type": "VariableDeclarator",
                                                        "id": {
                                                            "type": "Identifier",
                                                            "name": "B"
                                                        }
                                                    }
                                                ],
                                                "type": "VariableDeclaration"
                                            },
                                            {
                                                "kind": "var",
                                                "declarations": [
                                                    {
                                                        "init": null,
                                                        "type": "VariableDeclarator",
                                                        "id": {
                                                            "type": "Identifier",
                                                            "name": "C"
                                                        }
                                                    }
                                                ],
                                                "type": "VariableDeclaration"
                                            },
                                            {
                                                "kind": "var",
                                                "declarations": [
                                                    {
                                                        "init": null,
                                                        "type": "VariableDeclarator",
                                                        "id": {
                                                            "type": "Identifier",
                                                            "name": "cor3"
                                                        }
                                                    }
                                                ],
                                                "type": "VariableDeclaration"
                                            },
                                            {
                                                "expression": {
                                                    "operator": "=",
                                                    "right": {
                                                        "callee": {
                                                            "type": "Identifier",
                                                            "name": "first"
                                                        },
                                                        "type": "CallExpression",
                                                        "arguments": []
                                                    },
                                                    "type": "AssignmentExpression",
                                                    "left": {
                                                        "loc": {
                                                            "start": {
                                                                "column": 5,
                                                                "line": 8
                                                            },
                                                            "end": {
                                                                "column": 6,
                                                                "line": 8
                                                            }
                                                        },
                                                        "type": "Identifier",
                                                        "name": "A"
                                                    }
                                                },
                                                "type": "ExpressionStatement"
                                            },
                                            {
                                                "expression": {
                                                    "operator": "=",
                                                    "right": {
                                                        "callee": {
                                                            "type": "Identifier",
                                                            "name": "second"
                                                        },
                                                        "type": "CallExpression",
                                                        "arguments": []
                                                    },
                                                    "type": "AssignmentExpression",
                                                    "left": {
                                                        "loc": {
                                                            "start": {
                                                                "column": 5,
                                                                "line": 9
                                                            },
                                                            "end": {
                                                                "column": 6,
                                                                "line": 9
                                                            }
                                                        },
                                                        "type": "Identifier",
                                                        "name": "B"
                                                    }
                                                },
                                                "type": "ExpressionStatement"
                                            },
                                            {
                                                "expression": {
                                                    "operator": "=",
                                                    "right": {
                                                        "callee": {
                                                            "type": "Identifier",
                                                            "name": "third"
                                                        },
                                                        "type": "CallExpression",
                                                        "arguments": []
                                                    },
                                                    "type": "AssignmentExpression",
                                                    "left": {
                                                        "loc": {
                                                            "start": {
                                                                "column": 5,
                                                                "line": 10
                                                            },
                                                            "end": {
                                                                "column": 6,
                                                                "line": 10
                                                            }
                                                        },
                                                        "type": "Identifier",
                                                        "name": "C"
                                                    }
                                                },
                                                "type": "ExpressionStatement"
                                            },
                                            {
                                                "expression": {
                                                    "operator": "=",
                                                    "right": {
                                                        "operator": "/",
                                                        "right": {
                                                            "type": "Identifier",
                                                            "name": "C"
                                                        },
                                                        "type": "BinaryExpression",
                                                        "left": {
                                                            "type": "Identifier",
                                                            "name": "B"
                                                        }
                                                    },
                                                    "type": "AssignmentExpression",
                                                    "left": {
                                                        "type": "Identifier",
                                                        "name": "cor3"
                                                    }
                                                },
                                                "type": "ExpressionStatement"
                                            },
                                            {
                                                "type": "ReturnStatement",
                                                "argument": {
                                                    "operator": "+",
                                                    "right": {
                                                        "type": "Identifier",
                                                        "name": "cor3"
                                                    },
                                                    "type": "BinaryExpression",
                                                    "left": {
                                                        "type": "Identifier",
                                                        "name": "A"
                                                    }
                                                }
                                            },
                                            {
                                                "type": "BreakStatement",
                                                "label": null
                                            }
                                        ]
                                    },
                                    {
                                        "test": null,
                                        "type": "SwitchCase",
                                        "consequent": [
                                            {
                                                "type": "ReturnStatement",
                                                "argument": {
                                                    "raw": "\"throw error\"",
                                                    "type": "Literal",
                                                    "value": "throw error"
                                                }
                                            }
                                        ]
                                    }
                                ],
                                "type": "SwitchStatement",
                                "discriminant": {
                                    "type": "Identifier",
                                    "name": "_args"
                                }
                            }
                        ],
                        "type": "BlockStatement"
                    },
                    "generator": false,
                    "expression": false,
                    "rest": null,
                    "params": [],
                    "defaults": [],
                    "type": "FunctionExpression",
                    "id": null
                },
                "type": "AssignmentExpression",
                "left": {
                    "type": "Identifier",
                    "name": "test"
                }
            },
            "type": "ExpressionStatement"
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
                            {
                                "cases": [
                                    {
                                        "test": {
                                            "raw": "0",
                                            "type": "Literal",
                                            "value": 0
                                        },
                                        "type": "SwitchCase",
                                        "consequent": [
                                            {
                                                "type": "ReturnStatement",
                                                "argument": {
                                                    "raw": "3",
                                                    "type": "Literal",
                                                    "value": 3,
                                                    "loc": {
                                                        "start": {
                                                            "column": 5,
                                                            "line": 20
                                                        },
                                                        "end": {
                                                            "column": 6,
                                                            "line": 20
                                                        }
                                                    }
                                                }
                                            },
                                            {
                                                "type": "BreakStatement",
                                                "label": null
                                            }
                                        ]
                                    },
                                    {
                                        "test": null,
                                        "type": "SwitchCase",
                                        "consequent": [
                                            {
                                                "type": "ReturnStatement",
                                                "argument": {
                                                    "raw": "\"throw error\"",
                                                    "type": "Literal",
                                                    "value": "throw error"
                                                }
                                            }
                                        ]
                                    }
                                ],
                                "type": "SwitchStatement",
                                "discriminant": {
                                    "type": "Identifier",
                                    "name": "_args"
                                }
                            }
                        ],
                        "type": "BlockStatement"
                    },
                    "generator": false,
                    "expression": false,
                    "rest": null,
                    "params": [],
                    "defaults": [],
                    "type": "FunctionExpression",
                    "id": null
                },
                "type": "AssignmentExpression",
                "left": {
                    "type": "Identifier",
                    "name": "third"
                }
            },
            "type": "ExpressionStatement"
        }
    ],
    "type": "Program"
}
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

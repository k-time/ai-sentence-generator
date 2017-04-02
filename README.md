# ai-sentence-generator
Proof of concept Lisp program that generates random sentences using a [context-free grammar](https://en.wikipedia.org/wiki/Context-free_grammar).

## Features
* Defines more advanced production rules.
* Implements non-terminal symbols (ex. adjective, preposition, RelativeClause, InfinitivePhrase, etc.).
* Non-terminal symbols handle singular/plural nouns, present/past tense verbs, transitive/intransitive verbs, and more.
* Accepts/rejects generated sentences based on certain criteria, including length, tree depth, repetition, and subordinating/coordinating conjunction counts.
* Prefixes valid sentences with a "+" and rejected sentences with a "-" in output file.

## Files
File|Description
:-:|---
*grammar_ksx2101.lisp*|Defines the context-free grammar and validation functions. 
*q1_ksx2101.txt*|Contains 1000 randomly generated sentences. 
*report_ksx2101.txt*|Comprehensive report on the project.
